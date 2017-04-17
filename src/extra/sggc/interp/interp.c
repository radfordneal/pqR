/* SGGC - A LIBRARY SUPPORTING SEGMENTED GENERATIONAL GARBAGE COLLECTION.
          Simple interpreter used to test SGGC

   Copyright (c) 2016, 2017 Radford M. Neal.

   The SGGC library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA. */


#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "sggc-app.h"


/* INTERPRETER FOR A SIMPLE LISP-LIKE LANGUAGE.  Used to test SGGC, and
   assess its performance.

   Syntax of data and programs:

     ()             The nil object

     (a b (x y) c)  list of a, b, (x y), and c

     a, b, c, ...   Symbols: which are a single character from the set 
                      a-z, A-Z, ', ?, !, @, %, $, =, ., :, &, `, +

     # comment      Rest of line from # on is a comment
       
   Expressions:

     ()             Nil evaluates to itself

     a, b, c, ...   Symbols evaluate to their most recent binding value

     (f a1 a2 ...)  Evaluate f, a1, a2, ..., then call value of f with arguments
                    a1, a2, ...; f must not be a special symbol as listed below,
                    and the value f evaluates to must have the $ described below

     ($ (a1 a2 ...) e1 e2 ...)  As an expression, evaluates to itself; as a 
                                function, takes arguments for a1, a2, ..., makes
                                bindings for them, evaluates e1, e2, ..., and
                                returns the last of them

     (% (v1 v2 ...) e1 e2 ...)  Creates bindings for v1, v2, ... (initially ());
                                evaluates e1, e2, ...; returns the last of them

     (? w a b)       Conditional expression, evaluates w, then returns result of
                     evaluating a if w is a list (not () or a symbol), and
                     otherwise returns result of evaluing b (default ())

     (! w e1 e2 ...) Looping expression, evaluates w, then if it is a list
                     (not () or a symbol), e1, e2, ..., then does it again,
                     repeating until w is () or a symbol, returning ().

     (' a)          Returns a unevaluated

     (` a)          Evaluates a, then returns the result of evaluating that

     (@ v e)        Assignment expression, evaluates e, then changes the most
                    recent binding of symbol v to be the value of e; value
                    returned is v (unevaluated)

     (= a b)        Returns '(=) if the results of evaluating a and b are equal,
                    () if not

     (. a)          If a evaluates to a list, returns its first element

     (: a)          If a evaluates to a list, returns this list with the
                    first element dropped, () if the list had only one element

     (& x a)        If a evaluates to a list or (), returns the list with the
                    result of evaluating x prepended to the front of this list

  Bindings for all symbols exists globally, with initial values of ().

  The interpreter repeatedly reads an expression, evaluates it, and
  prints the value returned (after a sequence number and "\"), until
  end-of-file.  Changes to the global bindings from evaluation of one
  such expression may affect the evaluation of the next.  
*/


/* TYPE OF A POINTER USED IN THIS APPLICATION.  Uses compressed pointers.
   The OLD_TO_NEW_CHECK macro can therefore just call sggc_old_to_new
   and TYPE is just SGGC_TYPE. */

typedef sggc_cptr_t ptr_t;

#define OLD_TO_NEW_CHECK(from,to) sggc_old_to_new_check(from,to)
#define TYPE(v) SGGC_TYPE(v)


/* TYPES FOR THIS APPLICATION. */

struct type_nil { int dummy; };              /* Nil, () */
struct type_list { ptr_t head, tail; };      /* List, not including () */
struct type_symbol { char symbol; };         /* Symbol, only 64 of them */
struct type_binding { ptr_t value, next; };  /* Binding, symbol is in aux1 */

#define TYPE_NIL 0
#define TYPE_LIST 1
#define TYPE_SYMBOL 2
#define TYPE_BINDING 3

#define KIND_GLOBAL_BINDING 4  /* Used if UNCOLLECTED_NIL_SYMS_GLOBALS defined*/


#ifdef CHECK_VALID
#define CHK(x) sggc_check_valid_cptr(x)
#else
#define CHK(x) (x)
#endif

#define LIST(v) ((struct type_list *) SGGC_DATA(CHK(v)))
#define SYMBOL(v) ((struct type_symbol *) SGGC_DATA(CHK(v)))
#define BINDING(v) ((struct type_binding *) SGGC_DATA(CHK(v)))
#define BOUND_SYMBOL(v) (* (char *) SGGC_AUX1(CHK(v)))


/* VALID SYMBOLS. */

static const char symbol_chars[SGGC_CHUNKS_IN_SMALL_SEGMENT+1] =
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'`?!@%$=.:&+";

static ptr_t symbols[SGGC_CHUNKS_IN_SMALL_SEGMENT];


/* GLOBAL VARIABLES. */

static ptr_t nil;              /* The nil object, () */
static ptr_t global_bindings;  /* Global bindings of symbols with values */

#if CALL_NEWLY_FREED
static unsigned freed_count;  /* Count of freed objects, if doing that */
#endif


/* SCHEME FOR PROTECTING POINTERS FROM GARBAGE COLLECTION. */

static struct ptr_var { ptr_t *var; struct ptr_var *next; } *first_ptr_var;

#define PROT1(v) \
  struct ptr_var *saved_first_ptr_var = first_ptr_var; \
  struct ptr_var prot1 = { .var = &v, .next = first_ptr_var }; \
  first_ptr_var = &prot1;

#define PROT2(v) \
  struct ptr_var prot2 = { .var = &v, .next = first_ptr_var }; \
  first_ptr_var = &prot2;

#define PROT3(v) \
  struct ptr_var prot3 = { .var = &v, .next = first_ptr_var }; \
  first_ptr_var = &prot3;

#define PROT4(v) \
  struct ptr_var prot4 = { .var = &v, .next = first_ptr_var }; \
  first_ptr_var = &prot4;

#define PROT_END (first_ptr_var = saved_first_ptr_var)


/* FUNCTIONS THAT THE APPLICATION NEEDS TO PROVIDE TO THE SGGC MODULE. 

   Note that sggc_kind and sggc_nchunks are macros in sggc-app.h. */

char *sggc_aux1_read_only (sggc_kind_t kind)
{
  static const char spaces[SGGC_CHUNKS_IN_SMALL_SEGMENT+1] =
    "                                                                ";

  return kind >= TYPE_BINDING ? NULL : (char *) spaces;
}

void sggc_find_root_ptrs (void)
{ 
# if UNCOLLECT_LEVEL < 1
    sggc_mark (nil);  
# endif

# if UNCOLLECT_LEVEL < 2
    int i;
    for (i = 0; symbol_chars[i]; i++)
    { sggc_mark (symbols[i]);
    }
# endif

# if UNCOLLECT_LEVEL < 3
    sggc_look_at (global_bindings);  
# endif

  struct ptr_var *p;
  for (p = first_ptr_var; p != NULL; p = p->next)
  { sggc_look_at (*p->var);
  }
}

void sggc_find_object_ptrs (sggc_cptr_t cptr)
{
  if (SGGC_TYPE(cptr) == TYPE_LIST)
  { sggc_look_at (LIST(cptr)->head);
    sggc_look_at (LIST(cptr)->tail);
  }

  else if (SGGC_TYPE(cptr) == TYPE_BINDING)
  { sggc_look_at (BINDING(cptr)->value);
    sggc_look_at (BINDING(cptr)->next);
  }
}

/* ALLOCATE FUNCTION FOR THIS APPLICATION.  Calls the garbage collector
   when necessary, or otherwise every 100th allocation, with every 500th
   being level 1, and every 2000th being level 2. */

static unsigned alloc_count = 1;  /* 1 for allocation of nil at init */

static ptr_t alloc (sggc_type_t type)
{
  sggc_cptr_t a;

  /* Do optional garbage collections according to the scheme.  Do this first,
     not after allocating the object, which would then get collected! */

  alloc_count += 1;
  if (0) /* can enable for debugging */
  { if (alloc_count > 1) /* don't try it before nil is created */
    { sggc_collect(2); 
    }
  }
  else if (alloc_count % 100 == 0)
  { sggc_collect (alloc_count % 2000 == 0 ? 2 : alloc_count % 500 == 0 ? 1 : 0);
  }

  /* Try to allocate object, calling garbage collector if this initially
     fails. */

# if USE_ALLOC_SMALL_KIND_QUICKLY
    a = sggc_alloc_small_kind_quickly(type);  /* kind always same as type */
    if (a == SGGC_NO_OBJECT)
    { a = sggc_alloc_small_kind(type);
    }
# elif USE_ALLOC_SMALL_KIND
    a = sggc_alloc_small_kind(type);  /* kind always same as type in this app */
# elif USE_ALLOC_KIND
    a = sggc_alloc_kind(type,1);      /* kind always same as type in this app */
# else
    a = sggc_alloc(type,1); /* length argument is ignored */
# endif

  if (a == SGGC_NO_OBJECT)
  { sggc_collect(2);
    a = sggc_alloc(type,1); /* length argument is ignored */
    if (a == SGGC_NO_OBJECT)
    { printf("CAN'T ALLOCATE\n");
      exit(1);
    }
  }

  /* Initialize the object. */

  if (type == TYPE_LIST)
  { LIST(a)->head = nil;
    LIST(a)->tail = nil;
  }
  else if (type == TYPE_BINDING)
  { BINDING(a)->value = nil;
    BINDING(a)->next = nil;
  }

  return a;
}


/* PRINT AN OBJECT.  Bindings are not normally printed, but are handled
   here for debugging purposes. */

static void print (ptr_t a)
{
  switch (TYPE(a))
  { case TYPE_NIL:
    { printf ("()");
      break;
    }
    case TYPE_SYMBOL:
    { printf ("%c", SYMBOL(a)->symbol);
      break;
    }
    case TYPE_LIST:
    { ptr_t p;
      printf ("(");
      print (LIST(a)->head);
      for (p = LIST(a)->tail; p != nil; p = LIST(p)->tail)
      { printf (" ");
        print (LIST(p)->head);
      }
      printf (")");
      break;
    }
    case TYPE_BINDING:
    { ptr_t p;
      printf ("[");
      printf ("%c=", BOUND_SYMBOL(a));
      print (BINDING(a)->value);
      for (p = BINDING(a)->next; p != nil; p = BINDING(p)->next)
      { printf (" %c=", BOUND_SYMBOL(p));
        print (BINDING(p)->value);
      }
      printf ("]");
      break;
    }
  }
}


/* READ A CHARACTER.  Skips white space and comments.  Calls end_prog on EOF. */

static void end_prog(void);

static char read_char (void)
{
  char c;

  for (;;)
  {
    if (scanf(" %c",&c) != 1) 
    { end_prog();
    }

    if (c != '#')
    { return c;
    }

    scanf ("%*[^\n]");
  }
}


/* READ AN OBJECT.  Passed the next character of input; reads more if
   appropriate, but doesn't read past end of expression.  Prints an
   error message and exits if there is a syntax error. 

   As a special fudge for testing, if the first character is '^', a
   level 2 garbage collection is done, and then this character is
   skipped. */

static ptr_t read (char c)
{
  while (c == '^') 
  { sggc_collect(2);
    c = read_char();
  }

  if (strchr(symbol_chars,c))
  { return symbols [strchr(symbol_chars,c) - symbol_chars];
  }

  if (c == '(')
  {
    c = read_char();

    if (c == ')')
    { return nil;
    }

    ptr_t list = alloc (TYPE_LIST);
    ptr_t last = list;
    PROT1(list);

    for (;;)
    { LIST(last) -> head = read(c);
      OLD_TO_NEW_CHECK (last, LIST(last) -> head);
      c = read_char();
      if (c == ')')
      { PROT_END;
        return list;
      }
      ptr_t n = alloc (TYPE_LIST);
      LIST(last) -> tail = n;
      OLD_TO_NEW_CHECK (last, n);
      last = n;
    }
  }

  printf ("Syntax error %d\n", __LINE__);
  exit(1);
}


/* FIND THE LATEST BINDING OF A SYMBOL IN A SET OF BINDINGS. */

static ptr_t find_binding (char sym, ptr_t b)
{
  while (b != nil)
  { if (BOUND_SYMBOL(b) == sym)
    { return b;
    }
    b = BINDING(b) -> next;
  }

  abort();
}


/* CHECK WHETHER TWO OBJECTS ARE EQUAL. */

static int equal (ptr_t a, ptr_t b)
{
  if (TYPE(a) != TYPE(b)) 
  { return 0;
  }

  switch (TYPE(a))
  { case TYPE_NIL:    return 1;
    case TYPE_SYMBOL: return SYMBOL(a)->symbol == SYMBOL(b)->symbol;
    case TYPE_LIST:   return equal (LIST(a)->head, LIST(b)->head)
                               && equal (LIST(a)->tail, LIST(b)->tail);
    default: abort();
  }
}


/* EVALUATE AN EXPRESSION. */

static void error (int n)
{ printf ("Evaluation error %d\n", n);
  exit(2);
}

static ptr_t eval (ptr_t e, ptr_t b)
{
  ptr_t r = nil;
  ptr_t f = nil;
  PROT1(e);
  PROT2(b);
  PROT3(r);
  PROT4(f);

  /* () evaluates to itself. */

  if (e == nil)
  { r = nil;
    goto done;
  }

  /* Symbols evaluate to the result of looking them up in the bindings. */

  if (TYPE(e) == TYPE_SYMBOL)
  { r = BINDING (find_binding (SYMBOL(e)->symbol, b)) -> value;
    goto done;
  }

  /* Lists evaluate to the result of a special or defined function call. */

  if (TYPE(e) == TYPE_LIST)
  { 
    f = LIST(e) -> head;

    /* Check for special function. */

    if (TYPE(f) == TYPE_SYMBOL)
    { char sym = SYMBOL(f) -> symbol;
      ptr_t a1 = LIST(e) -> tail;
      ptr_t a2 = TYPE(a1) == TYPE_LIST ? LIST(a1) -> tail : nil;
      ptr_t a3 = TYPE(a2) == TYPE_LIST ? LIST(a2) -> tail : nil;
      ptr_t a4 = TYPE(a3) == TYPE_LIST ? LIST(a3) -> tail : nil;
      ptr_t n;
      switch (sym)
      { case '$':
        { r = e;
          goto done;
        }
        case '%':
        { if (a1 == nil) error(__LINE__);
          ptr_t a = LIST(a1) -> head;
          if (a != nil && TYPE(a) != TYPE_LIST) error(__LINE__);
          ptr_t old_b = b;
          while (a != nil)
          { if (TYPE (LIST(a)->head) != TYPE_SYMBOL) error(__LINE__);
            ptr_t n = alloc (TYPE_BINDING);
            BINDING(n) -> next = b;  /* old-to-new check not needed */
            BOUND_SYMBOL(n) = SYMBOL(LIST(a)->head) -> symbol;
            b = n;
            a = LIST(a) -> tail;
          }
          r = nil;
          ptr_t t;
          for (t = a2; t != nil; t = LIST(t)->tail)
          { r = nil;  /* free memory now */
            r = eval (LIST(t)->head, b);
          }
          goto done;
        }
        case '?':
        { if (a1 == nil || a2 == nil || a4 != nil) error(__LINE__);
          if (TYPE(eval(LIST(a1)->head,b)) == TYPE_LIST)
          { r = eval (LIST(a2)->head, b);
          }
          else
          { r = a3 == nil ? nil : eval (LIST(a3)->head, b);
          }
          goto done;
        }
        case '!':
        { if (a1 == nil) error(__LINE__);
          while (TYPE(eval(LIST(a1)->head,b)) == TYPE_LIST)
          { ptr_t s = a2;
            while (s != nil)
            { (void) eval (LIST(s)->head, b);
              s = LIST(s) -> tail;
            }
          }
          r = nil;
          goto done;
        }
        case '\'':
        { if (a1 == nil) error(__LINE__);
          r = LIST(a1) -> head;
          goto done;
        }
        case '`':
        { if (a1 == nil) error(__LINE__);
          r = eval (eval (LIST(a1)->head, b), b);
          goto done;
        }
        case '@':
        { if (a1 == nil || a2 == nil || a3 != nil) error(__LINE__);
          if (TYPE(LIST(a1)->head) != TYPE_SYMBOL) error(__LINE__);
          ptr_t g = find_binding (SYMBOL(LIST(a1)->head) -> symbol, b);
          n = eval (LIST(a2)->head, b);
          BINDING(g) -> value = n;
          OLD_TO_NEW_CHECK (g, n);
          r = LIST(a1) -> head;
          goto done;
        }
        case '=':
        { if (a1 == nil || a2 == nil || a3 != nil) error(__LINE__);
          r = eval (LIST(a1)->head, b);  /* put in r for protection */
          if (equal (r, eval(LIST(a2)->head, b)))
          { r = alloc(TYPE_LIST);
            LIST(r) -> head = f;  /* r will be (=), no old-to-new check req'd */
          }
          else
          { r = nil;
          }
          goto done;
        }
        case '.':
        { if (a1 == nil || a2 != nil) error(__LINE__);
          n = eval (LIST(a1)->head, b);
          if (TYPE(n) != TYPE_LIST) error(__LINE__);
          r = LIST(n) -> head;
          goto done;
        }
        case ':':
        { if (a1 == nil || a2 != nil) error(__LINE__);
          n = eval (LIST(a1)->head, b);
          if (TYPE(n) != TYPE_LIST) error(__LINE__);
          r = LIST(n) -> tail;
          goto done;
        }
        case '&':
        { if (a1 == nil || a2 == nil || a3 != nil) error(__LINE__);
          r = alloc (TYPE_LIST);
          n = eval (LIST(a1)->head, b);
          LIST(r) -> head = n;
          OLD_TO_NEW_CHECK (r, n);
          n = eval (LIST(a2)->head, b);
          if (n != nil && TYPE(n) != TYPE_LIST) error(__LINE__);
          LIST(r) -> tail = n;
          OLD_TO_NEW_CHECK (r, n);
          goto done;
        }
      }
    }

    /* If function is not special, evaluate head to get function. */

    f = eval(f,b);

    if (TYPE(f) != TYPE_LIST) error(__LINE__);
    if (TYPE(LIST(f)->head) != TYPE_SYMBOL) error(__LINE__);
    if (SYMBOL(LIST(f)->head)->symbol != '$') error(__LINE__);

    /* Set e to be list of unevaluated arguments. */

    e = LIST(e) -> tail;

    /* Look for formal arguments. */

    ptr_t t = LIST(f) -> tail;

    if (t == nil) error(__LINE__);
    ptr_t a = LIST(t) -> head;
    if (a != nil && TYPE(a) != TYPE_LIST) error(__LINE__);

    /* Create bindings of formal arguments to the results of evaluating actual
       arguments (with original bindings), */

    ptr_t old_b = b;

    while (a != nil)
    { if (TYPE (LIST(a)->head) != TYPE_SYMBOL) error(__LINE__);
      ptr_t n = alloc (TYPE_BINDING);
      BINDING(n) -> next = b;  /* old-to-new check not needed */
      BOUND_SYMBOL(n) = SYMBOL(LIST(a)->head) -> symbol;
      b = n;
      if (e == nil) error(__LINE__);
      BINDING(b) -> value = eval (LIST(e)->head, old_b);
      OLD_TO_NEW_CHECK (b, BINDING(b)->value);
      e = LIST(e) -> tail;
      a = LIST(a) -> tail;
    }

    if (e != nil) error(__LINE__);

    /* printf("bindings: "); print(b); printf("\n"); */

    /* Evaluate expressions in the body of the function, returning last. */

    r = nil;
    for (t = LIST(t)->tail; t != nil; t = LIST(t)->tail)
    { r = nil;  /* free memory now */
      r = eval (LIST(t)->head, b);
    }

    goto done;
  }

  abort();

done:
  PROT_END;
  return r;
}


/* FUNCTION TO CALL FOR NEWLY-FREED OBJECTS. */

#if CALL_NEWLY_FREED

int freed_fun (sggc_cptr_t cptr)
{
  freed_count += 1;
  return 0;
}

#endif


/* MAIN PROGRAM. */

int main (void)
{
  int seqno = 1;

# if NO_REUSE
    sggc_init (SGGC_MAX_SEGMENTS);
# else
    sggc_init (10000);
# endif

# if NO_REUSE
    sggc_no_reuse(1);
# endif

# if CALL_NEWLY_FREED
  { sggc_kind_t k;
    for (k = 0; k < SGGC_N_KINDS; k++) 
    { sggc_call_for_newly_freed_object(k,freed_fun);
    }
  }
# endif

  nil = sggc_alloc (TYPE_NIL, 1);

# ifndef SGGC_NO_OBJECT_ZERO
    if (nil != 0) abort();
# endif

  global_bindings = nil;

  ptr_t n;
  int i;

  for (i = 0; symbol_chars[i]; i++)
  { symbols[i] = alloc (TYPE_SYMBOL);
    SYMBOL(symbols[i]) -> symbol = symbol_chars[i];
#   if UNCOLLECT_LEVEL >= 3
      n = sggc_alloc_small_kind (KIND_GLOBAL_BINDING);
      alloc_count += 1;
#   else
      n = alloc (TYPE_BINDING);
#   endif
    BOUND_SYMBOL(n) = symbol_chars[i];     /* no old-to-new check needed:   */
    BINDING(n) -> next = global_bindings;  /*   either n is new, or n is    */
    BINDING(n) -> value = nil;             /*   uncollected, and so is "to" */
    global_bindings = n;
  }

  sggc_collect(2);

  /* The read / eval / print loop. */

  for (;;)
  { ptr_t expr = read(read_char());
    printf ("%d \\ ", seqno++);
    print (eval (expr, global_bindings));
    printf ("\n");
  }

  return 0;
}


/* COUNT OBJECTS OF AN UNCOLLECTED KIND. */

static unsigned count_uncol (sggc_kind_t kind)
{
  unsigned cnt = 0;
  sggc_cptr_t u = sggc_first_uncollected_of_kind(kind);

  while (u != SGGC_NO_OBJECT)
  { cnt += 1;
    u = sggc_next_uncollected_of_kind(u);
  }

  return cnt;
}


/* TERMINATE PROGRAM.  Prints number of allocations and current usage,
   and other stuff in sggc_info, plus freed objects, if that's
   enabled, then checks that the numbers and types of uncollected
   objects (if any) are as they ought to be, then exits. */

static void end_prog (void)
{
  unsigned total;

  printf("Allocated objects: %u\n",alloc_count);
  printf("Counts... Gen0: %u, Gen1: %d, Gen2: %d, Uncollected: %d\n",
          sggc_info.gen0_count, sggc_info.gen1_count, 
          sggc_info.gen2_count, sggc_info.uncol_count);
  printf("Big chunks... Gen0: %u, Gen1: %d, Gen2: %d, Uncollected: %d\n",
   (unsigned) sggc_info.gen0_big_chunks, (unsigned) sggc_info.gen1_big_chunks, 
   (unsigned) sggc_info.gen2_big_chunks, (unsigned) sggc_info.uncol_big_chunks);
  printf("Number of segments: %u,  Total memory usage: %llu bytes\n",
          sggc_info.n_segments, (unsigned long long) sggc_info.total_mem_usage);
# if CALL_NEWLY_FREED
    printf("Number of freed objects: %u\n",freed_count);
    total =  sggc_info.gen0_count + sggc_info.gen1_count + sggc_info.gen2_count
              + sggc_info.uncol_count + freed_count;
    printf("Freed + still around: %u\n",total);
    if (total != alloc_count)
    { printf("DOESN'T MATCH ALLOC COUNT!\n");
    }
# endif

# if UNCOLLECT_LEVEL >= 1
    if (count_uncol(TYPE_NIL) != 1) abort();
# endif

# if UNCOLLECT_LEVEL >= 2
    if (count_uncol(TYPE_SYMBOL) + 1 != sizeof symbol_chars) abort();
# endif

# if UNCOLLECT_LEVEL >= 3
    if (count_uncol(KIND_GLOBAL_BINDING) + 1 != sizeof symbol_chars) abort();
# endif

  exit(0);
}


/* CODE GENERATION TEST.  Not actually executed, but compiled here to
   be disassembled to see what code gets generated. */

ptr_t code_gen_test (ptr_t a, ptr_t b)
{
  if (TYPE(a) == TYPE_LIST)
  { return LIST(a)->tail == nil ? LIST(a)->head : LIST(b)->head;
  }

  if (TYPE(a) == TYPE_SYMBOL)
  { return SYMBOL(a)->symbol == 'a' ? a : b;
  }

  if (TYPE(a) == TYPE_BINDING)
  { return BOUND_SYMBOL(a) == ' ' ? BINDING(a)->value : b;
  }

  return nil;
}
