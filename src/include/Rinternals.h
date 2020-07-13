/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018, 2019 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2010   The R Core Team.
 *
 *  The changes in pqR from R-2.15.0 distributed by the R Core Team are
 *  documented in the NEWS and MODS files in the top-level source directory.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#ifndef R_INTERNALS_H_
#define R_INTERNALS_H_


#ifdef __cplusplus
# include <cstdio>
# ifdef __SUNPRO_CC
using std::FILE;
# endif
# include <climits>
extern "C" {
#else
# include <stdio.h>
# include <limits.h> /* for INT_MAX */
#endif


/* Redefinition of "fork" to handle helper threads properly, using the
   Rf_fork function in system.c.  Starts by including the header file
   that declares the system "fork" (if such exists), so that the
   redefinition won't affect that.  Declares Rf_fork as returning a
   value of type pid_t if the header file defining that type exists.
   If not, it is allowed to default to returning int.

   For some mysterious reason, things go wrong if this is done after
   the includes of headers in R_ext below.  Also, letting Rf_fork
   default to returning int, rather than declaring it so, is deliberate,
   in case "fork" (now really "Rf_fork") is somehow declared later,
   in which case we don't want to conflict with the return type it is
   given then, which we hope is the same as it's declared as in util.c.
   (You'd think this couldn't be an issue, but something funny happens
   with C++ ...) */

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#define fork Rf_fork

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
pid_t Rf_fork(void);
#endif


#include <R_ext/Arith.h>
#include <R_ext/Boolean.h>
#include <R_ext/Complex.h>
#include <R_ext/Error.h>
#include <R_ext/Memory.h>
#include <R_ext/PrtUtil.h>
#include <R_ext/Utils.h>

#include <R_ext/libextern.h>

#ifndef NEED_SGGC_FUNCTIONS
#define SGGC_NO_FUNCTIONS
#endif
#include <R_ext/sggc-app.h>


#define ConstExtern extern    /* Used for variables defined in const-objs.c */


/* The NOT_LVALUE macro is used to disallow assignment to CDR(s), etc.
   even when USE_RINTERNALS is defined (SETCDR, etc. must be used instead
   for GC old-to-new to work properly).  It can be redefined as the identity
   function in those modules that actually need to assign (eg, memory.c). */

#define NOT_LVALUE(x) ((void)0,(x)) /* Make use on left of assignment an error*/


typedef unsigned char Rbyte;


/* Type for length of vectors, etc.  For compatibility with R-3.0.0+, the
   "long vector" versions are also defined, same as the regular versions. */

typedef int R_len_t;
typedef int R_xlen_t;

#define R_LEN_T_MAX INT_MAX
#define R_XLEN_T_MAX R_LEN_T_MAX


/* Fundamental Data Types: These are largely Lisp influenced
   structures, with the exception of LGLSXP, INTSXP, REALSXP, CPLXSXP,
   RAWSXP, and STRSXP which are the element types for S-like data
   objects.

   Note that the gap of 11 and 12 below is because of
   the withdrawal of native "factor" and "ordered" types.

   The exact numeric values are seldom used, but they are, e.g., in
   ../main/subassign.c

   See TypeTable[] in ../main/util.c for typeof() */

typedef unsigned int SEXPTYPE;  /* used in serialize.c for things that aren't
                                   actual types */
#define MAX_NUM_SEXPTYPE 32

#define NILSXP	     0	  /* nil = NULL */
#define SYMSXP	     1	  /* symbols */
#define LISTSXP	     2	  /* lists of dotted pairs */
#define CLOSXP	     3	  /* closures */
#define ENVSXP	     4	  /* environments */
#define PROMSXP	     5	  /* promises: [un]evaluated closure arguments */
#define LANGSXP	     6	  /* language constructs (special lists) */
#define SPECIALSXP   7	  /* special forms */
#define BUILTINSXP   8	  /* builtin non-special forms */
#define CHARSXP	     9	  /* "scalar" string type (internal only)*/
#define LGLSXP	    10	  /* logical vectors */
#define INTSXP	    13	  /* integer vectors */
#define REALSXP	    14	  /* real variables */
#define CPLXSXP	    15	  /* complex variables */
#define STRSXP	    16	  /* string vectors */
#define DOTSXP	    17	  /* dot-dot-dot object */
#define ANYSXP	    18	  /* make "any" args work.
			     Used in specifying types for symbol
			     registration to mean anything is okay  */
#define VECSXP	    19	  /* generic vectors */
#define EXPRSXP	    20	  /* expressions vectors */
#define BCODESXP    21    /* byte code */
#define EXTPTRSXP   22    /* external pointer */
#define WEAKREFSXP  23    /* weak reference */
#define RAWSXP      24    /* raw bytes */
#define S4SXP       25    /* S4, non-vector */

/* used for detecting PROTECT issues in memory.c */

#define FREESXP     31    /* node released by GC */

#define FUNSXP      99    /* Closure or Builtin or Special */



#if USE_COMPRESSED_POINTERS

#if 1
typedef sggc_cptr_t SEXP;
#else /* experimental idea */
typedef enum SEXP_enum { SEXP_low = -2147483647, SEXP_high=2147483647 } SEXP;
#endif

typedef SEXP VECSEXP;

typedef SEXP SEXP32;
#define SEXP32_FROM_SEXP(x) (x)
#define SEXP_FROM_SEXP32(x) (x)
#define R_NoObject32 R_NoObject

#define CPTR_FROM_SEXP(x) (x)
#define CPTR_FROM_SEXP32(x) (x)
#define UPTR_FROM_SEXP(x) ((SEXPREC *) SGGC_DATA(x))
#define SEXP_FROM_CPTR(x) (x)

#else /* !USE_COMPRESSED_POINTERS */

typedef struct SEXPREC *SEXP;
typedef struct VECTOR_SEXPREC *VECSEXP;

#if SIZEOF_CHAR_P == 4
typedef SEXP SEXP32;
#define CPTR_FROM_SEXP32(x) CPTR_FROM_SEXP(x)
#define SEXP32_FROM_SEXP(x) (x)
#define SEXP_FROM_SEXP32(x) (x)
#define R_NoObject32 R_NoObject
#else
typedef sggc_cptr_t SEXP32;
#define CPTR_FROM_SEXP32(x) (x)
#define SEXP32_FROM_SEXP(x) CPTR_FROM_SEXP(x)
#define SEXP_FROM_SEXP32(x) SEXP_FROM_CPTR(x)
#define R_NoObject32 SGGC_NO_OBJECT
#endif

#define CPTR_FROM_SEXP(x) ((x)->cptr) 
#define UPTR_FROM_SEXP(x) (x)
#define SEXP_FROM_CPTR(x) ((SEXP) SGGC_DATA(x))

#endif


/* Information in the header of every object.  Size is 32 bits = 4 bytes. */

struct sxpinfo_struct {

    /* First byte:   Whole of it allows quick checks for, eg, simple scalars. 
                     Parts give type and other information */

    unsigned int type_et_cetera : 8;    /* Bit fields as defined, 0 if use 
                                           is not specified below */

#   define TYPE_ET_CETERA_TYPE 0x1f /* Type of object, narrower than SEXPTYPE */

#   define TYPE_ET_CETERA_BEING_COMPUTED 0x20 /* Obj being computed in helper?*/

#   define TYPE_ET_CETERA_VEC_DOTS_TR 0x40 /* Symbol:   is ... or ..1, ..2, etc.
                                              Vector:   not scalar (length != 1)
                                              Function: is being traced 
                                              CHARSXP:  may need translation 
                                              PROMSXP:  has been forced */

#   define TYPE_ET_CETERA_HAS_ATTR 0x80 /* Vector type, LISTSXP, or PROMSXP: 
                                             1 if ATTRIB != R_NilValue */

    /* Second byte.  Various flags, some of whose meaning varies with the
       type of the object. */

    unsigned int nmcnt : 3;   /* Count of "names" referring to object */

    unsigned int in_use: 1;   /* whether contents may be in use by a helper */

    unsigned int debug : 1;       /* Function/Environment: is being debugged
                                     Symbol: maybe use fast subassign */
    unsigned int rstep_pname : 1; /* Function: is to be debugged just once
                                     Symbol: subassign counterpart follows it
                                     CHARSXP: is used as a symbol's printname
                                     Envir: store gradient with variables
                                     Promise: evaluate with VARIANT_GRADIENT */
    unsigned int base_sym_env : 1;/* Symbol: has base binding in global cache,
                                     Envir: R_BaseEnv or R_BaseNamespace 
                                     VECSXP: Holds gradients wrt list variable,
                                        as opposed to gradients _of_ list var */

    unsigned int obj : 1;     /* Set if this is an S3 or S4 object */

    /* The "general purpose" field, used for miscellaneous purposes */

    unsigned int gp : 16;     /* The "general purpose" field */
};

struct listsxp_struct {
    SEXP carval;
    SEXP cdrval;
    SEXP tagval;
};

struct closxp_struct {
    SEXP formals;
    SEXP body;
    SEXP env;
};

struct promsxp_struct {
    SEXP value;
    SEXP expr;
    SEXP env;
};

#if USE_COMPRESSED_POINTERS

/* Every node must have a set of sxpinfo flags in the header.  They also
   all have an attribute field and a length (possibly a shared constant),
   but these are in auxiliary information 1 and auxiliary information 2. */

#define SEXPREC_HEADER \
    struct sxpinfo_struct sxpinfo

#endif

#if !USE_COMPRESSED_POINTERS && !USE_AUX_FOR_ATTRIB

/* Every node must have a set of sxpinfo flags and an attribute field,
   plus a cptr field giving the compressed pointer to the node.  They
   also all have length fields at present, though this is supposed to
   be used only for vectors (but having always suppresses some bugs). */

#define SEXPREC_HEADER \
    struct sxpinfo_struct sxpinfo; \
    uint32_t cptr; \
    SEXP attrib; \
    R_len_t length

#endif

#if !USE_COMPRESSED_POINTERS && USE_AUX_FOR_ATTRIB

/* Every node must have a set of sxpinfo flags and a cptr field giving 
   the compressed pointer to the node.  They also all have an attribute
   field, but it is in auxiliary information 1.  Not all nodes have
   a length field, so it's not in the header. */

#define SEXPREC_HEADER \
    struct sxpinfo_struct sxpinfo; \
    uint32_t cptr

#endif

/* The standard node structure consists of a header followed by the
   node data. */


/* SEXPREC for types not specially handled below. */

typedef struct SEXPREC {
    SEXPREC_HEADER;
#if !USE_COMPRESSED_POINTERS && SIZEOF_CHAR_P == 8 && !USE_AUX_FOR_ATTRIB
    int32_t padding;
#endif
    union {
	struct listsxp_struct listsxp;
	struct closxp_struct closxp;
	struct promsxp_struct promsxp;
    } u;
#if !USE_COMPRESSED_POINTERS && SIZEOF_CHAR_P == 4
    int32_t padding;
#endif
} SEXPREC;


/* Version of SEXPREC used for environments. */

#if 1 /* currently no space for this */
#define USE_ENV_TUNECNTS 0  /* Must be kept as 0 */
#else
#define USE_ENV_TUNECNTS 0  /* May be 0 or 1 - normally 0 to avoid slowdown */
#endif

typedef uint64_t R_symbits_t;

typedef struct ENV_SEXPREC {
    SEXPREC_HEADER;
#if !USE_COMPRESSED_POINTERS && SIZEOF_CHAR_P == 8 && !USE_AUX_FOR_ATTRIB
    int32_t padding;
#endif
    SEXP frame;
    SEXP enclos;
    SEXP hashtab;
    int32_t hashlen;
    SEXP32 gradvars;   /* N.B.: not looked at by GC - must protect otherwise */
#if !USE_COMPRESSED_POINTERS && SIZEOF_CHAR_P == 4
    int32_t padding;
#endif
    R_symbits_t envsymbits;
} ENV_SEXPREC, *ENVSEXP;


/* Version of SEXPREC used for primitives. */

struct primsxp_struct {    /* table offset of this and other info is in gp  */
    /* The two function pointers below can't use SEXP, since not defined yet*/
    void *(*cfun)();          /* c-code address for prim fun, from table   */
    void *(*fast_cfun)();     /* c-code addr for fast interface, or NULL*/
    unsigned short var1;      /* variant for eval of unary primitive arg */
    short code;               /* operation code, from table                */
    signed char arity;        /* function arity (-1 for any), from table   */
    unsigned char pending_ok; /* whether args can have computation pending */
    unsigned int vis_on:1;    /* whether to force visibility on */
    unsigned int vis_off:1;   /* whether to force vis off (internal only) */
    unsigned int variant:1;   /* pass variant to cfun, from table     */
    unsigned int internal:1;  /* call with .Internal flag, from table */
    unsigned int fast_sub:1;  /* subassign/subset fn that can use fast method*/
    unsigned int dsptch1:1;   /* might dispatch on 1st argument (only
                                 for when fast_cfun != NULL) */
    unsigned int whole:1;     /* Do special processing for .Internal
                                 when VARIANT_WHOLE_BODY (BUILTIN only) */
    unsigned int padbit:1;
    unsigned int gradn:5;     /* gradn&7 is # of internal arguments that might 
                                 have gradient; gradn>>3 is number of initial
                                 args that don't have gradient. See names.c */
#if USE_COMPRESSED_POINTERS && SIZEOF_CHAR_P == 4
    int32_t padding1, padding2;
#endif
};

typedef struct PRIM_SEXPREC {
    SEXPREC_HEADER;
#if USE_COMPRESSED_POINTERS
    int32_t padding;
#endif
#if !USE_COMPRESSED_POINTERS && SIZEOF_CHAR_P == 8 && !USE_AUX_FOR_ATTRIB
    int32_t padding;
#endif
    struct primsxp_struct primsxp;
} PRIM_SEXPREC, *PRIMSEXP;


/* Version of SEXPREC used for symbols. */

#if 1 /* currently no space for this */
#define USE_SYM_TUNECNTS 0   /* Must be kept as 0 */
#else
#define USE_SYM_TUNECNTS 0   /* May be 0 or 1 - normally 0 to avoid slowdown */
#endif

#if 1 /* currently no space for this */
#define USE_SYM_TUNECNTS2 0  /* Must be kept as 0 */
#else
#define USE_SYM_TUNECNTS2 0  /* May be 0 or 1 - normally 0 to avoid slowdown */
#endif

#if USE_COMPRESSED_POINTERS || SIZEOF_CHAR_P == 8 && USE_AUX_FOR_ATTRIB
#define SYM_HASH_IN_SYM 0    /* No room for it */
#else
#define SYM_HASH_IN_SYM 1
#endif

typedef struct SYM_SEXPREC {
    SEXPREC_HEADER;
#if !USE_COMPRESSED_POINTERS && SIZEOF_CHAR_P == 8 && !USE_AUX_FOR_ATTRIB
    SEXP32 lastenv;
#endif
    SEXP pname;
    SEXP value;
    SEXP lastbinding;
#if USE_COMPRESSED_POINTERS || SIZEOF_CHAR_P == 4 || USE_AUX_FOR_ATTRIB
    SEXP32 lastenv;
#endif
#if SYM_HASH_IN_SYM
    uint32_t sym_hash;
#endif
    SEXP32 lastenvnotfound;
    R_symbits_t symbits;
} SYM_SEXPREC, *SYMSEXP;


/* Version of SEXPREC used for external pointers. */

typedef struct EXTPTR_SEXPREC {
    SEXPREC_HEADER;
#if !USE_COMPRESSED_POINTERS && SIZEOF_CHAR_P == 8 && !USE_AUX_FOR_ATTRIB
    int32_t padding;
#endif
#if USE_COMPRESSED_POINTERS
    SEXP unused;             /* Include 'unused' to match the offsets with    */
    SEXP prot;               /*   uncompressed pointers, in case of wrong use */
    SEXP tag;                /*   of CDR and TAG to get prot and tag fields   */
    void *ptr;               /* The actual exernal pointer */
#else
    void *ptr;               /* The actual exernal pointer */
    SEXP prot;
    SEXP tag;
#endif
#if SIZEOF_CHAR_P == 4
    int32_t padding;
#endif
#if USE_COMPRESSED_POINTERS
    int32_t padding1, padding2;
#endif
} EXTPTR_SEXPREC, *EXTPTRSEXP;

/* Version of SEXPREC used as a header in vector nodes.  MUST be kept 
   consistent with the SEXPREC definition, and with VECTOR_SEXPREC_C. */

typedef struct VECTOR_SEXPREC {
    SEXPREC_HEADER;
#if !USE_COMPRESSED_POINTERS && SIZEOF_CHAR_P == 8 && USE_AUX_FOR_ATTRIB
    R_len_t length;       /* The length, only in vectors for this setup */
#endif
    R_len_t truelength;   /* The mis-named "truelength" field */
#if !USE_COMPRESSED_POINTERS && SIZEOF_CHAR_P == 4
    int32_t padding;
#endif
} VECTOR_SEXPREC;

typedef union { VECTOR_SEXPREC s; double align; } SEXPREC_ALIGN;

/* Version of VECTOR_SEXPREC used for defining constants in const-objs.c. 
   MUST be kept consistent with the VECTOR_SEXPREC definition. */

#define R_CONST const  /* Define as 'const' to get actual read-only constants,
                          or nothing if you don't want them to be read-only */
typedef struct VECTOR_SEXPREC_C {
    SEXPREC_HEADER;
#if !USE_COMPRESSED_POINTERS && SIZEOF_CHAR_P == 8 && USE_AUX_FOR_ATTRIB
    R_len_t length;       /* The length, only in vectors for this setup */
#endif
    R_len_t truelength;   /* The mis-named "truelength" field */
#if !USE_COMPRESSED_POINTERS && SIZEOF_CHAR_P == 4
    int32_t padding;
#endif
    union { double d; int w[2]; int i; char c; char s[8]; SEXP p; } data;
#if USE_AUX_FOR_ATTRIB
    double const_padding; /* Needed to fill out full chunk */
#endif
} VECTOR_SEXPREC_C;

#define DATAPTR(x)	(((SEXPREC_ALIGN *) UPTR_FROM_SEXP(x)) + 1)

/* Use gcc's facility for specifying alignment if present, and not disabled.
   This may assert an incorrect assumption about alignment in the case of
   scalars - since they may be constants, or may be held in one chunk
   when compressed pointers are used.  But it's difficult to see how
   that could cause a problem in practice... */

#if __GNUC__ && !defined(NO_ASSUME_ALIGNED)
#define DATAPTR_WITH_ALIGNMENT(x) __builtin_assume_aligned \
            (DATAPTR(x), SGGC_DATA_ALIGNMENT, SGGC_DATA_ALIGNMENT_OFFSET)
#else
#define DATAPTR_WITH_ALIGNMENT(x) DATAPTR(x)
#endif


/* Pairlist and data access macros / static inline functions that are now 
   used everywhere (rather than non-inline functions, though those still
   exist in memory.c).  Type checks are done for data access unless 
   USE_RINTERNALS is defined. */

#ifdef USE_RINTERNALS

#define TAG(e)     NOT_LVALUE(UPTR_FROM_SEXP(e)->u.listsxp.tagval)
#define CAR(e)     NOT_LVALUE(UPTR_FROM_SEXP(e)->u.listsxp.carval)
#define CDR(e)     NOT_LVALUE(UPTR_FROM_SEXP(e)->u.listsxp.cdrval)

#define TYPE_ETC(x)(UPTR_FROM_SEXP(x)->sxpinfo.type_et_cetera)
#define TYPEOF(x)  (UPTR_FROM_SEXP(x)->sxpinfo.type_et_cetera \
                     & TYPE_ET_CETERA_TYPE)

#define LOGICAL(x) ((int *) DATAPTR_WITH_ALIGNMENT(x))
#define INTEGER(x) ((int *) DATAPTR_WITH_ALIGNMENT(x))
#define RAW(x)     ((Rbyte *) DATAPTR_WITH_ALIGNMENT(x))
#define COMPLEX(x) ((Rcomplex *) DATAPTR_WITH_ALIGNMENT(x))
#define REAL(x)    ((double *) DATAPTR_WITH_ALIGNMENT(x))

#if USE_COMPRESSED_POINTERS
#define LENGTH(x)  NOT_LVALUE(* (R_len_t *) SGGC_AUX1(x))
#else
#define LENGTH(x)  NOT_LVALUE(((VECTOR_SEXPREC *) UPTR_FROM_SEXP(x))->length)
#endif

#else /* USE_RINTERNALS not defined */

static inline SEXP TAG (SEXP e) 
  { return UPTR_FROM_SEXP(e)->u.listsxp.tagval; }
static inline SEXP CAR (SEXP e) 
  { return UPTR_FROM_SEXP(e)->u.listsxp.carval; }
static inline SEXP CDR (SEXP e) 
  { return UPTR_FROM_SEXP(e)->u.listsxp.cdrval; }

static inline SEXPTYPE TYPEOF (SEXP x) 
  { return UPTR_FROM_SEXP(x)->sxpinfo.type_et_cetera & TYPE_ET_CETERA_TYPE; }

extern R_NORETURN void Rf_LOGICAL_error(SEXP);
static inline int *LOGICAL(SEXP x) 
{   if (TYPEOF(x) != LGLSXP) Rf_LOGICAL_error(x);
    return (int *) DATAPTR_WITH_ALIGNMENT(x);
}
extern R_NORETURN void Rf_INTEGER_error(SEXP);
static inline int *INTEGER(SEXP x) /* allows logical too, due to common usage */
{   if (TYPEOF(x) != INTSXP && TYPEOF(x) != LGLSXP) Rf_INTEGER_error(x);
    return (int *) DATAPTR_WITH_ALIGNMENT(x);
}
extern R_NORETURN void Rf_RAW_error(SEXP);
static inline Rbyte *RAW(SEXP x) 
{   if (TYPEOF(x) != RAWSXP) Rf_RAW_error(x);
    return (Rbyte *) DATAPTR_WITH_ALIGNMENT(x);
}
extern R_NORETURN void Rf_COMPLEX_error(SEXP);
static inline Rcomplex *COMPLEX(SEXP x) 
{   if (TYPEOF(x) != CPLXSXP) Rf_COMPLEX_error(x);
    return (Rcomplex *) DATAPTR_WITH_ALIGNMENT(x);
}
extern R_NORETURN void Rf_REAL_error(SEXP);
static inline double *REAL(SEXP x) 
{   if (TYPEOF(x) != REALSXP) Rf_REAL_error(x);
    return (double *) DATAPTR_WITH_ALIGNMENT(x);
}

#if USE_COMPRESSED_POINTERS
static inline int LENGTH (SEXP x) 
  { return * (R_len_t *) SGGC_AUX1(x); }
#else
static inline int LENGTH (SEXP x) 
  { return ((VECTOR_SEXPREC *) UPTR_FROM_SEXP(x))->length; }
#endif

#endif /* USE_RINTERNALS */

/* Long/short versions for compatibility with R-3.x.y. */

#define XLENGTH(x) LENGTH(x)
#define XTRUELENGTH(x) TRUELENGTH(x)
#define SHORT_VEC_LENGTH(x) LENGTH(x)
#define SET_SHORT_VEC_LENGTH SETLENGTH
#define SET_SHORT_VEC_TRUELENGTH(x,v) SET_TRUELENGTH((x),(v))
#define IS_LONG_VEC(x) 0
#define IS_SCALAR(x,type) (TYPEOF(x) == (type) && LENGTH(x) == 1)

#define CAAR(e)		CAR(CAR(e))
#define CDAR(e)		CDR(CAR(e))
#define CADR(e)		CAR(CDR(e))
#define CDDR(e)		CDR(CDR(e))
#define CADDR(e)	CAR(CDR(CDR(e)))
#define CADDDR(e)	CAR(CDR(CDR(CDR(e))))
#define CAD4R(e)	CAR(CDR(CDR(CDR(CDR(e)))))

/* Macros for accessing and changing NAMEDCNT.

   Be careful not to write to an object with NAMEDCNT equal to MAX_NAMEDCNT, 
   even if the new value is also MAX_NAMEDCNT, since it might be a constant 
   object in a read-only memory area.

   When a variable may be being used by a helper, it is treated as temporarily
   having a nmcnt value that is greater than the value stored.  If the value
   of a macro is the same regardless of whether this temporary increment is
   counted, the value can be returned immediately.  Otherwise, it's necessary
   to wait for the variable to not be in use, and then return the value based
   on the stored nmcnt. 

   A declaration for helpers_wait_until_not_in_use is put here, so that the
   helpers.h file needn't be included. 

   When helper threads are disabled, helpers_wait_until_not_in_use is normally
   a null macro, but a stub for it will also be defined in main.c for linking 
   to by any modules that don't know that helper threads are disabled. */

#define MAX_NAMEDCNT 7	/* Must be either 2 or a power of 2 minus 1, limited 
                           by the number of bits in the nmcnt sxpinfo field */

#ifdef HELPERS_DISABLED
#define helpers_wait_until_not_in_use(v) 0
#else
extern void helpers_wait_until_not_in_use(SEXP);
#endif

/* Below for when Defn.h is not included, where helpers_is_in_use is defined. */

#ifndef helpers_is_in_use  
#define helpers_is_in_use(x)  (UPTR_FROM_SEXP(x)->sxpinfo.in_use)
#endif

#define NAMEDCNT(x) \
( helpers_is_in_use(x) && UPTR_FROM_SEXP(x)->sxpinfo.nmcnt != MAX_NAMEDCNT \
     ? (helpers_wait_until_not_in_use(x), UPTR_FROM_SEXP(x)->sxpinfo.nmcnt) \
     : UPTR_FROM_SEXP(x)->sxpinfo.nmcnt )

#define NAMEDCNT_EQ_0(x) \
( UPTR_FROM_SEXP(x)->sxpinfo.nmcnt == 0 && !helpers_is_in_use(x) ? 1 \
    : UPTR_FROM_SEXP(x)->sxpinfo.nmcnt != 0 ? 0 \
    : (helpers_wait_until_not_in_use(x), 1) )

#define NAMEDCNT_EQ_MAX(x) \
( UPTR_FROM_SEXP(x)->sxpinfo.nmcnt == MAX_NAMEDCNT)

#define NAMEDCNT_GT_0(x) \
( UPTR_FROM_SEXP(x)->sxpinfo.nmcnt == 0 && !helpers_is_in_use(x) ? 0 \
    : UPTR_FROM_SEXP(x)->sxpinfo.nmcnt != 0 ? 1 \
    : (helpers_wait_until_not_in_use(x), 0) )

#if MAX_NAMEDCNT!=2 && 1     /* Change 1 to 0 to disable "optimized" version */
#define NAMEDCNT_GT_1(x) \
( (UPTR_FROM_SEXP(x)->sxpinfo.nmcnt & (MAX_NAMEDCNT-1)) != 0 ? 1 \
    : !helpers_is_in_use(x) ? 0 \
    : (helpers_wait_until_not_in_use(x), 0) )
#else
#define NAMEDCNT_GT_1(x) \
( UPTR_FROM_SEXP(x)->sxpinfo.nmcnt > 1 ? 1 : !helpers_is_in_use(x) ? 0 \
    : (helpers_wait_until_not_in_use(x), 0) )
#endif

#define SET_NAMEDCNT_0(x)     (UPTR_FROM_SEXP(x)->sxpinfo.nmcnt = 0)
#define SET_NAMEDCNT_1(x)     (UPTR_FROM_SEXP(x)->sxpinfo.nmcnt = 1)

/* if 0, change to 1, without waiting */ \
#if 1                        /* Change 1 to 0 to disable "optimized" version */
#define SET_NAMEDCNT_NOT_0(x) do { \
    SEXPREC *_p_ = UPTR_FROM_SEXP(x); \
    if (_p_->sxpinfo.nmcnt == 0) \
        _p_->sxpinfo.nmcnt |= 1; \
  } while (0)
#else
    SEXPREC *_p_ = UPTR_FROM_SEXP(x); \
    if (_p_->sxpinfo.nmcnt == 0) \
        _p_->sxpinfo.nmcnt = 1; \
  } while (0)
#endif

#if MAX_NAMEDCNT!=2 && 1     /* Change 1 to 0 to disable "optimized" version */
#define SET_NAMEDCNT_MAX(x) do { \
    SEXPREC *_p_ = UPTR_FROM_SEXP(x); \
    if (_p_->sxpinfo.nmcnt != MAX_NAMEDCNT) \
        _p_->sxpinfo.nmcnt |= MAX_NAMEDCNT; \
  } while (0)
#else
#define SET_NAMEDCNT_MAX(x) do { \
    SEXPREC *_p_ = UPTR_FROM_SEXP(x); \
    if (_p_->sxpinfo.nmcnt != MAX_NAMEDCNT) \
        _p_->sxpinfo.nmcnt = MAX_NAMEDCNT; \
  } while (0)
#endif


/* Macros for compatibility with later R Core versions. */

#define MAYBE_SHARED(x)     NAMEDCNT_GT_1(x)
#define NO_REFERENCES(x)    NAMEDCNT_EQ_0(x)
#define MAYBE_REFERENCED(x) NAMEDCNT_GT_0(x)
#define NOT_SHARED(x)       (! NAMEDCNT_GT_1(x))
#define MARK_NOT_MUTABLE(x) SET_NAMEDCNT_MAX(x)


#ifdef USE_RINTERNALS

/* This is intended for use only within R itself.
 * It defines internal structures that are otherwise only accessible
 * via SEXP, and macros to replace many (but not all) of accessor functions
 * (which are always defined).
 *
 * Making R_NilValue a constant has necessitated exposing more of the
 * internals (some of what is above used to be after this ifdef).  Also
 * the case for defining MAYBE_REFERENCED, etc. without creating more 
 * functions (rather than macros).
 */

#define SET_NAMEDCNT(x,v) do { \
    SEXPREC *_p_ = UPTR_FROM_SEXP(x); \
    int _v_ = v; \
    if (_p_->sxpinfo.nmcnt != _v_) \
        _p_->sxpinfo.nmcnt = _v_; \
  } while (0)

#define INC_NAMEDCNT(x) do { \
    SEXPREC *_p_ = UPTR_FROM_SEXP(x); \
    if (_p_->sxpinfo.nmcnt < MAX_NAMEDCNT) \
        _p_->sxpinfo.nmcnt += 1; \
  } while (0)

#define INC_NAMEDCNT_0_AS_1(x) do { \
    SEXPREC *_p_ = UPTR_FROM_SEXP(x); \
    if (_p_->sxpinfo.nmcnt < 2) \
        _p_->sxpinfo.nmcnt = 2; \
    else if (_p_->sxpinfo.nmcnt < MAX_NAMEDCNT) \
        _p_->sxpinfo.nmcnt += 1; \
  } while (0)

#define DEC_NAMEDCNT(x) do { \
    SEXPREC *_p_ = UPTR_FROM_SEXP(x); \
    if (_p_->sxpinfo.nmcnt < MAX_NAMEDCNT && _p_->sxpinfo.nmcnt != 0) \
        _p_->sxpinfo.nmcnt -= 1; \
  } while (0)

/* Backward-compatible NAMED macros.  To mimic the 0/1/2 scheme, any nmcnt 
   greater than 1 must be converted to 2, and a value of 2 must be convert back
   to the maximum.  Furthermore, when NAMED returns 2, the actual nmcnt must 
   be set to the maximum - this is necessary to mimic the effect such code as

                      if (NAMED(v)<2) SET_NAMED(v,2)

   which should have the effect of keeping v with maximum nmcnt even if later
   a DEC_NAMEDCNT(v) is done.
*/

#if MAX_NAMEDCNT == 2	/* Gives the old scheme */

#define NAMED(x)	NAMEDCNT((x))
#define SET_NAMED(x,v)	SET_NAMEDCNT((x),(v))

#else 			/* New scheme with MAX_NAMEDCNT > 2 */

#define NAMED(x) \
  ( UPTR_FROM_SEXP(x)->sxpinfo.nmcnt > 1 \
     && UPTR_FROM_SEXP(x)->sxpinfo.nmcnt < MAX_NAMEDCNT \
      ? ((UPTR_FROM_SEXP(x)->sxpinfo.nmcnt = MAX_NAMEDCNT), 2) \
      : NAMEDCNT((x)) )

#define SET_NAMED(x,v) do { \
    if ((v) > 1) SET_NAMEDCNT_MAX((x)); else SET_NAMEDCNT((x),(v)); \
  } while (0)

#endif

/* Decrement NAMEDCNT for object and for PRVALUE if object is a promise. */

#if 0  /* temporarily disabled, pending problem resolution */
#define DEC_NAMEDCNT_AND_PRVALUE(x) do { \
    SEXP _q_ = (x); \
    DEC_NAMEDCNT(_q_); \
    if (TYPEOF(_q_) == PROMSXP && NAMEDCNT_EQ_0(_q_) \
                               && PRVALUE(_q_) != R_UnboundValue) \
        DEC_NAMEDCNT(PRVALUE(_q_)); \
  } while (0)
#else
#define DEC_NAMEDCNT_AND_PRVALUE(x) DEC_NAMEDCNT((x))
#endif

/* Set an element in VECSXP or EXPRSXP to a given value or to an element from
   another vector.  The element is duplicated or namedcnt is adjusted as 
   appropriate, and as specified by the setting of DUPVE. */

#define DUPVE 0  /* Set to 1 to duplicate values, to 0 to adjust namedcnt */

/* Set element to a value whose namedcnt is properly set.  One can lazily
   let namedcnt of zero stay zero, rather than increment, for the moment. 

   Refers to nmcnt directly, rather than with NAMEDCNT macros, since no
   need here to wait until not in use. */

#define SET_VECTOR_ELEMENT_TO_VALUE(_dst_,_i_,_val_) do { \
    SEXP _v_ = _val_; \
    if (!DUPVE || UPTR_FROM_SEXP(_v_)->sxpinfo.nmcnt == 0) { \
        SET_VECTOR_ELT (_dst_, _i_, _v_); \
        if (UPTR_FROM_SEXP(_v_)->sxpinfo.nmcnt > 0) \
            INC_NAMEDCNT(_v_); \
    } \
    else \
        SET_VECTOR_ELT (_dst_, _i_, duplicate(_v_)); \
} while (0)

/* Set element to a value from another list.  If the value has namedcnt of
   zero, it must be regarded as actually being one, unless the list it came
   from has namedcnt of zero (which for proper operation must be the real
   namedcnt for it, not a lazy zero).

   Refers to nmcnt directly, rather than with NAMEDCNT macros, since no
   need here to wait until not in use. */

#define SET_VECTOR_ELEMENT_FROM_VECTOR(_dst_,_i_,_src_,_j_) do { \
    SEXP _s_ = _src_; \
    SEXP _v_ = VECTOR_ELT(_s_,_j_); \
    if (!DUPVE || UPTR_FROM_SEXP(_s_)->sxpinfo.nmcnt == 0) { \
        SET_VECTOR_ELT (_dst_, _i_, _v_); \
        if (UPTR_FROM_SEXP(_s_)->sxpinfo.nmcnt > 0 \
         || UPTR_FROM_SEXP(_v_)->sxpinfo.nmcnt > 0) \
            INC_NAMEDCNT_0_AS_1(_v_); \
    } \
    else \
        SET_VECTOR_ELT (_dst_, _i_, duplicate(_v_)); \
} while (0)


/* General Cons Cell Attributes */

#if USE_COMPRESSED_POINTERS
#define ATTRIB_W(x)     (* (SEXP *) SGGC_AUX2(x))
#elif SIZEOF_CHAR_P==8 && USE_AUX_FOR_ATTRIB
#define ATTRIB_W(x)     (* (SEXP *) SGGC_AUX1(CPTR_FROM_SEXP(x)))
#else
#define ATTRIB_W(x)     (UPTR_FROM_SEXP(x)->attrib)
#endif

#define ATTRIB(x)       NOT_LVALUE(TYPEOF(x)==SYMSXP ? R_NilValue : ATTRIB_W(x))

#define IS_PRINTNAME(x) NOT_LVALUE(UPTR_FROM_SEXP(x)->sxpinfo.rstep_pname)

/* HAS_ATTR(x) check for existence of an attribute not R_NilValue, but only
   for vector types, LISTSXP, and PROMSXP. */

#define HAS_ATTRIB(x)   ((UPTR_FROM_SEXP(x)->sxpinfo.type_et_cetera \
                          & TYPE_ET_CETERA_HAS_ATTR) != 0)
#define SET_HAS_ATTRIB(x)     (UPTR_FROM_SEXP(x)->sxpinfo.type_et_cetera \
                                 |= TYPE_ET_CETERA_HAS_ATTR)
#define UNSET_HAS_ATTRIB(x)   (UPTR_FROM_SEXP(x)->sxpinfo.type_et_cetera \
                                 &= ~TYPE_ET_CETERA_HAS_ATTR)

#define SET_VEC_DOTS_TR_BIT(x)   (UPTR_FROM_SEXP(x)->sxpinfo.type_et_cetera \
                                   |= TYPE_ET_CETERA_VEC_DOTS_TR)
#define UNSET_VEC_DOTS_TR_BIT(x) (UPTR_FROM_SEXP(x)->sxpinfo.type_et_cetera \
                                   &= ~TYPE_ET_CETERA_VEC_DOTS_TR)

#define OBJECT(x)	NOT_LVALUE(UPTR_FROM_SEXP(x)->sxpinfo.obj)
#define RTRACE(x)	NOT_LVALUE(UPTR_FROM_SEXP(x)->sxpinfo.type_et_cetera \
                                                   & TYPE_ET_CETERA_VEC_DOTS_TR)
#define LEVELS(x)	NOT_LVALUE(UPTR_FROM_SEXP(x)->sxpinfo.gp)

#define SUBASSIGN_FOLLOWS(x) NOT_LVALUE(UPTR_FROM_SEXP(x)->sxpinfo.rstep_pname)
#define SET_SUBASSIGN_FOLLOWS(x) (UPTR_FROM_SEXP(x)->sxpinfo.rstep_pname = 1)
#if USE_COMPRESSED_POINTERS
#define SUBASSIGN_THAT_FOLLOWS(x) ((x)+SGGC_SYM_CHUNKS)
#else
#define SUBASSIGN_THAT_FOLLOWS(x) ((SEXP)((SYMSEXP)(x)+1))
#endif

#define MAYBE_FAST_SUBASSIGN(x) NOT_LVALUE(UPTR_FROM_SEXP(x)->sxpinfo.debug)
#define SET_MAYBE_FAST_SUBASSIGN(x) (UPTR_FROM_SEXP(x)->sxpinfo.debug = 1)

  /* For SET_OBJECT and SET_TYPEOF, don't set if new value is the current value,
     to avoid crashing on an innocuous write to a constant that may be stored
     in read-only memory. */

#define SET_OBJECT(x,v) do { \
    SEXPREC *_x_ = UPTR_FROM_SEXP(x); int _v_ = (v); \
    if (_x_->sxpinfo.obj!=_v_) _x_->sxpinfo.obj = _v_; \
  } while (0)

#define SET_TYPEOF(x,v) do { \
    SEXP _x_ = (x); SEXPREC * _X_ = UPTR_FROM_SEXP(_x_); int _v_ = (v); \
    if (TYPEOF(_x_) != _v_) { \
        _X_->sxpinfo.type_et_cetera = \
          (_X_->sxpinfo.type_et_cetera & ~TYPE_ET_CETERA_TYPE) | _v_; \
        if (((HAS_ATTRIB_TYPES>>_v_)&1) == 0 || ATTRIB(_x_) == R_NilValue) \
            UNSET_HAS_ATTRIB(_x_); \
        else \
            SET_HAS_ATTRIB(_x_); \
    } \
  } while (0)

#define SET_TYPEOF0(x,v) /* don't check if same as previous; expr not stmt */ \
    (UPTR_FROM_SEXP(x)->sxpinfo.type_et_cetera = \
      (UPTR_FROM_SEXP(x)->sxpinfo.type_et_cetera & ~TYPE_ET_CETERA_TYPE) | v)

#define SET_RTRACE(x,v)	((v) ? \
  (UPTR_FROM_SEXP(x)->sxpinfo.type_et_cetera |= TYPE_ET_CETERA_VEC_DOTS_TR) \
   : (UPTR_FROM_SEXP(x)->sxpinfo.type_et_cetera &= ~TYPE_ET_CETERA_VEC_DOTS_TR))

#define SETLEVELS(x,v)	(UPTR_FROM_SEXP(x)->sxpinfo.gp=(v))

/* The TRUELENGTH is seldom used, and usually has no connection with length. */
#define TRUELENGTH(x)	\
  NOT_LVALUE(((VECTOR_SEXPREC *) UPTR_FROM_SEXP(x))->truelength)
#define SET_TRUELENGTH(x,v) \
  (((VECTOR_SEXPREC *) UPTR_FROM_SEXP(x))->truelength = (v))

/* S4 object bit, set by R_do_new_object for all new() calls.  Avoid writes
   of what's already there, in case object is a constant in read-only memory. */
#define S4_OBJECT_BIT_POS 4
#define S4_OBJECT_MASK (1<<S4_OBJECT_BIT_POS)
#define IS_S4_OBJECT(x) ((UPTR_FROM_SEXP(x)->sxpinfo.gp & S4_OBJECT_MASK)!=0)
#define SET_S4_OBJECT(x) SET_S4_OBJECT_inline(x)
static inline void SET_S4_OBJECT_inline (SEXP x) {
    if (!IS_S4_OBJECT(x)) UPTR_FROM_SEXP(x)->sxpinfo.gp |= S4_OBJECT_MASK;
}
#define UNSET_S4_OBJECT(x) UNSET_S4_OBJECT_inline(x)
static inline void UNSET_S4_OBJECT_inline (SEXP x) {
    if (IS_S4_OBJECT(x)) UPTR_FROM_SEXP(x)->sxpinfo.gp &= ~S4_OBJECT_MASK;
}

/* Vector Access Macros */

/* Under the generational allocator the data for vector nodes comes
   immediately after the node structure, so the data address is a
   known offset from the node SEXP. */
#define CHAR(x)		((const char *) DATAPTR(x))
#define STRING_ELT(x,i)	NOT_LVALUE(((SEXP *) DATAPTR(x))[i])
#define VECTOR_ELT(x,i)	NOT_LVALUE(((SEXP *) DATAPTR(x))[i])
#define STRING_PTR(x)	((SEXP *) DATAPTR(x))
#define VECTOR_PTR(x)	((SEXP *) DATAPTR(x))

/* List Access Macros.  Some are now found above, outside USE_RINTERNALS. */
/* These also work for ... objects */
#define LISTVAL(x)	(UPTR_FROM_SEXP(x)->u.listsxp)
#define MISSING_MASK	15 /* reserve 4 bits--only 2 uses now */
#define MISSING(x)	(UPTR_FROM_SEXP(x)->sxpinfo.gp & MISSING_MASK)/* for closure calls */
#define SET_MISSING(x,v) do { \
  SEXPREC *__x__ = UPTR_FROM_SEXP(x); \
  int __v__ = (v); \
  int __other_flags__ = __x__->sxpinfo.gp & ~MISSING_MASK; \
  __x__->sxpinfo.gp = __other_flags__ | __v__; \
} while (0)

/* Closure Access Macros */
#define FORMALS(x)	NOT_LVALUE(UPTR_FROM_SEXP(x)->u.closxp.formals)
#define BODY(x)		NOT_LVALUE(UPTR_FROM_SEXP(x)->u.closxp.body)
#define CLOENV(x)	NOT_LVALUE(UPTR_FROM_SEXP(x)->u.closxp.env)
#define RDEBUG(x)	NOT_LVALUE(UPTR_FROM_SEXP(x)->sxpinfo.debug)
#define SET_RDEBUG(x,v)	(UPTR_FROM_SEXP(x)->sxpinfo.debug=(v))
#define RSTEP(x)	NOT_LVALUE(UPTR_FROM_SEXP(x)->sxpinfo.rstep_pname)
#define SET_RSTEP(x,v)	(UPTR_FROM_SEXP(x)->sxpinfo.rstep_pname=(v))

/* Symbol Access Macros */
#define PRINTNAME(x)	NOT_LVALUE(((SYMSEXP) UPTR_FROM_SEXP(x))->pname)
#define SYMVALUE(x)	NOT_LVALUE(((SYMSEXP) UPTR_FROM_SEXP(x))->value)
#define LASTSYMENV(x)	(((SYMSEXP) UPTR_FROM_SEXP(x))->lastenv)
#define LASTSYMBINDING(x) (((SYMSEXP) UPTR_FROM_SEXP(x))->lastbinding)
#define LASTENVNOTFOUND(x) (((SYMSEXP) UPTR_FROM_SEXP(x))->lastenvnotfound)
#define SYMBITS(x)      NOT_LVALUE((((SYMSEXP) UPTR_FROM_SEXP(x))->symbits))
#define SET_SYMBITS(x,v)  (((SYMSEXP) UPTR_FROM_SEXP(x))->symbits = (v))
#define DDVAL_MASK	1
#define DDVAL(x)	(UPTR_FROM_SEXP(x)->sxpinfo.gp & DDVAL_MASK) /* for ..1, ..2 etc */
#define SET_DDVAL_BIT(x) ((UPTR_FROM_SEXP(x)->sxpinfo.gp) |= DDVAL_MASK)
#define UNSET_DDVAL_BIT(x) ((UPTR_FROM_SEXP(x)->sxpinfo.gp) &= ~DDVAL_MASK)
#define SET_DDVAL(x,v) ((v) ? SET_DDVAL_BIT(x) : UNSET_DDVAL_BIT(x)) /* for ..1, ..2 etc */
#define BASE_CACHE(x)  NOT_LVALUE(UPTR_FROM_SEXP(x)->sxpinfo.base_sym_env) /* 1 = base binding
                                                               in global cache*/
#define SET_BASE_CACHE(x,v) (UPTR_FROM_SEXP(x)->sxpinfo.base_sym_env = (v))

/* Environment Access Macros */
#define FRAME(x)	NOT_LVALUE(((ENVSEXP)UPTR_FROM_SEXP(x))->frame)
#define ENCLOS(x)	NOT_LVALUE(((ENVSEXP)UPTR_FROM_SEXP(x))->enclos)
#define HASHTAB(x)	NOT_LVALUE(((ENVSEXP)UPTR_FROM_SEXP(x))->hashtab)
#define ENVFLAGS(x)	NOT_LVALUE(UPTR_FROM_SEXP(x)->sxpinfo.gp)
#define SET_ENVFLAGS(x,v)	((UPTR_FROM_SEXP(x)->sxpinfo.gp)=(v))
#define ENVSYMBITS(x)   NOT_LVALUE(((ENVSEXP)UPTR_FROM_SEXP(x))->envsymbits)
#define SET_ENVSYMBITS(x,v)   (((ENVSEXP)UPTR_FROM_SEXP(x))->envsymbits=(v))
#define IS_BASE(x)	NOT_LVALUE(UPTR_FROM_SEXP(x)->sxpinfo.base_sym_env)
                           /* 1 = R_BaseEnv or R_BaseNamespace */
#define IS_USER_DATABASE(rho) \
  ( OBJECT((rho)) && inherits((rho), "UserDefinedDatabase") )


/* Gradient-related stuff. */

#define GRADIENT_IN_CELL(x) ATTRIB_W(x)
#define HAS_GRADIENT_IN_CELL(x) \
(!HAS_ATTRIB(x) ? 0 : \
 TYPEOF(ATTRIB(x))==LISTSXP && TYPEOF(TAG(ATTRIB(x)))==ENVSXP ? 1 : (abort(),0))

#define GRADIENT_WRT_LEN(g) (TRUELENGTH(g) + 1)
#define SET_GRADIENT_WRT_LEN(g,l) SET_TRUELENGTH(g,l-1)

#define STORE_GRAD(x)	NOT_LVALUE(UPTR_FROM_SEXP(x)->sxpinfo.rstep_pname)
#define SET_STORE_GRAD(x,v) (UPTR_FROM_SEXP(x)->sxpinfo.rstep_pname=(v))

#define GRADVARS(x)	SEXP_FROM_SEXP32(((ENVSEXP)UPTR_FROM_SEXP(x))->gradvars)
#define SET_GRADVARS(x,v)  (((ENVSEXP)UPTR_FROM_SEXP(x))->gradvars \
                             = SEXP32_FROM_SEXP(v))
#define GRADVARS_LEN(nv)(2*(nv))       /* length of GRADVARS for nv variables */
#define GRADVARS_NV(v)	(LENGTH(v)/2)  /* number of variables in GRADVARS */

#define GRADINDEX(x)	((unsigned)LEVELS(x) >> 8)
#define SET_GRADINDEX(x,v) SETLEVELS(x,(LEVELS(x)&0xff)|((v)<<8))

#define GRAD_WRT_LIST(x) NOT_LVALUE(UPTR_FROM_SEXP(x)->sxpinfo.base_sym_env)
#define SET_GRAD_WRT_LIST(x,v) (UPTR_FROM_SEXP(x)->sxpinfo.base_sym_env=(v))


#else /* not USE_RINTERNALS */

#define CHAR(x)		R_CHAR(x)
const char *(R_CHAR)(SEXP x);

/* Various tests with macro versions below */
Rboolean (Rf_isNull)(SEXP s);
Rboolean (Rf_isRaw)(SEXP s);
Rboolean (Rf_isSymbol)(SEXP s);
Rboolean (Rf_isLogical)(SEXP s);
Rboolean (Rf_isReal)(SEXP s);
Rboolean (Rf_isComplex)(SEXP s);
Rboolean (Rf_isExpression)(SEXP s);
Rboolean (Rf_isEnvironment)(SEXP s);
Rboolean (Rf_isString)(SEXP s);
Rboolean (Rf_isObject)(SEXP s);

#endif /* USE_RINTERNALS */

/* Accessor functions.  Many are declared using () to avoid the macro
   definitions in the USE_RINTERNALS section.
   The function STRING_ELT is used as an argument to arrayAssign even
   if the macro version is in use.
*/

#ifdef TESTING_WRITE_BARRIER
#ifndef PROTECTCHECK
# define PROTECTCHECK
#endif
#endif

#ifdef PROTECTCHECK
SEXP Rf_chk_valid_SEXP (SEXP x);
#else
#define Rf_chk_valid_SEXP(x) (x)
#endif

const char *Rf_translateChar_nontrivial (SEXP);

/* General Cons Cell Attributes */
SEXP (ATTRIB)(SEXP x);
int  (OBJECT)(SEXP x);
int  (NAMED)(SEXP x);
void (SET_OBJECT)(SEXP x, int v);
void (SET_TYPEOF)(SEXP x, int v);
void (SET_NAMED)(SEXP x, int v);
void SET_ATTRIB(SEXP x, SEXP v);
void DUPLICATE_ATTRIB(SEXP to, SEXP from);

/* S4 object testing */
int (IS_S4_OBJECT)(SEXP x);
void (SET_S4_OBJECT)(SEXP x);
void (UNSET_S4_OBJECT)(SEXP x);

/* Vector Access Functions */
int  (TRUELENGTH)(SEXP x);
void (SETLENGTH)(SEXP x, int v);
void (SET_TRUELENGTH)(SEXP x, int v);
int  (LEVELS)(SEXP x);
int  (SETLEVELS)(SEXP x, int v);

SEXP (STRING_ELT)(SEXP x, int i);
SEXP (VECTOR_ELT)(SEXP x, int i);
void SET_STRING_ELT(SEXP x, int i, SEXP v);
void copy_string_elements(SEXP x, int i, SEXP v, int j, int n);
void Rf_rep_one_string_element(SEXP x, int i, SEXP e, int n);
void Rf_rep_string_elements(SEXP x, int i, SEXP v, int n);
SEXP SET_VECTOR_ELT(SEXP x, int i, SEXP v);
void copy_vector_elements(SEXP x, int i, SEXP v, int j, int n);
SEXP *(STRING_PTR)(SEXP x);
SEXP *(VECTOR_PTR)(SEXP x);

/* List Access Functions. Declared here even if there are macro versions. */
/* These also work for ... objects */
#define CONS(a, b)	cons((a), (b))		/* data lists */
#define LCONS(a, b)	lcons((a), (b))		/* language lists */
int  (MISSING)(SEXP x);
void (SET_MISSING)(SEXP x, int v);
void (SET_TAG)(SEXP x, SEXP y);
SEXP SETCAR(SEXP x, SEXP y);
SEXP SETCDR(SEXP x, SEXP y);
SEXP SETCADR(SEXP x, SEXP y);
SEXP SETCADDR(SEXP x, SEXP y);
SEXP SETCADDDR(SEXP x, SEXP y);
SEXP SETCAD4R(SEXP e, SEXP y);

/* Closure Access Functions */
SEXP (FORMALS)(SEXP x);
SEXP (BODY)(SEXP x);
SEXP (CLOENV)(SEXP x);
int  (RDEBUG)(SEXP x);
int  (RSTEP)(SEXP x);
int  (RTRACE)(SEXP x);
void (SET_RDEBUG)(SEXP x, int v);
void (SET_RSTEP)(SEXP x, int v);
void (SET_RTRACE)(SEXP x, int v);
void SET_FORMALS(SEXP x, SEXP v);
void SET_BODY(SEXP x, SEXP v);
void SET_CLOENV(SEXP x, SEXP v);

/* Symbol Access Functions */
SEXP (PRINTNAME)(SEXP x);
SEXP (SYMVALUE)(SEXP x);
SEXP (INTERNAL)(SEXP x);
int  (DDVAL)(SEXP x);
void (SET_DDVAL)(SEXP x, int v);
void SET_PRINTNAME(SEXP x, SEXP v);
void SET_SYMVALUE(SEXP x, SEXP v);
void SET_INTERNAL(SEXP x, SEXP v);

/* Environment Access Functions */
SEXP (FRAME)(SEXP x);
SEXP (ENCLOS)(SEXP x);
SEXP (HASHTAB)(SEXP x);
int  (ENVFLAGS)(SEXP x);
void (SET_ENVFLAGS)(SEXP x, int v);
void SET_FRAME(SEXP x, SEXP v);
void SET_ENCLOS(SEXP x, SEXP v);
void SET_HASHTAB(SEXP x, SEXP v);

/* Promise Access Functions */
/* First five have macro versions in Defn.h */
SEXP (PRCODE)(SEXP x);
SEXP (PRENV)(SEXP x);
SEXP (PRVALUE)(SEXP x);
int  (PRSEEN)(SEXP x);
void (SET_PRSEEN)(SEXP x, int v);
void SET_PRENV(SEXP x, SEXP v);
void SET_PRVALUE(SEXP x, SEXP v);
void SET_PRCODE(SEXP x, SEXP v);
void SET_PRSEEN(SEXP x, int v);

/* External pointer access macros */
#define EXTPTR_PTR(x)	(((EXTPTRSEXP)UPTR_FROM_SEXP(x))->ptr)
#define EXTPTR_PROT(x)	(((EXTPTRSEXP)UPTR_FROM_SEXP(x))->prot)
#define EXTPTR_TAG(x)	(((EXTPTRSEXP)UPTR_FROM_SEXP(x))->tag)

/* Bytecode access macros */
#define BCODE_CODE(x)	CAR(x)
#define BCODE_CONSTS(x) CDR(x)
#define BCODE_EXPR(x)	TAG(x)
#define isByteCode(x)	(TYPEOF(x)==BCODESXP)

/* Pointer Protection and Unprotection */
#define PROTECT(s)		Rf_protect(s)
#define PROTECT2(s1,s2)		Rf_protect2(s1,s2) /* BEWARE! All args eval'd */
#define PROTECT3(s1,s2,s3)	Rf_protect3(s1,s2,s3) /* before any protected */
#define PROTECT4(s1,s2,s3,s4)	Rf_protect4(s1,s2,s3,s4)
#define PROTECT5(s1,s2,s3,s4,s5) Rf_protect5(s1,s2,s3,s4,s5)
#define UNPROTECT(n)		Rf_unprotect(n)
#define UNPROTECT_PTR(s)	Rf_unprotect_ptr(s)

/* We sometimes need to coerce a protected value and place the new
   coerced value under protection.  For these cases PROTECT_WITH_INDEX
   saves an index of the protection location that can be used to
   replace the protected value using REPROTECT. */
typedef int PROTECT_INDEX;
#define PROTECT_WITH_INDEX(x,i) R_ProtectWithIndex(x,i)
#define REPROTECT(x,i) R_Reprotect(x,i)


/* New protection scheme, that records local variables containing pointers
   that should be protected, rather that recording the pointers themselves. 

   Note that these definitions include some defensive measures designed to
   produce compile errors, or at least unwanted behaviour, when the rules
   for using these macros are violated.  (But not all violations are caught.) */

struct R_local_protect {
    const struct R_local_protect *next;  /* next in list, or NULL */
    int cnt;                             /* number of pointers present below */
    SEXP *Protected[9];                  /* array of ptrs to protected vars  */
};                                       /*    (may be a shorter array)      */

#define R_local_protect_start R_high_frequency_globals.local_protect_start


#define CHK_IS_SEXP(v) \
    if (0) (UPTR_FROM_SEXP(v)->sxpinfo) /* try to get error if v not a SEXP */

#define BEGIN_PROTECT0() \
    do { \
        BEGIN_PROTECT_label: if (0) goto END_PROTECT_label; \
        const struct { /* not actually put in local_protect list */ \
            const struct R_local_protect *next; \
        } R_local_protect = { R_local_protect_start }; \
        struct { int outer; } R_protect_check; /* for error check */ \
        do {

#define BEGIN_PROTECT1(v1) \
    do { \
        SEXP v1 = R_NilValue; \
        BEGIN_PROTECT_label: if (0) goto END_PROTECT_label; /* error check */ \
        const struct { \
            const struct R_local_protect *next; int cnt; SEXP *Protected[1]; \
        } R_local_protect = { R_local_protect_start, \
              1, { &v1 } }; \
        R_local_protect_start = (const struct R_local_protect *) \
                                  &R_local_protect; \
        struct { int outer; } R_protect_check; /* for error check */ \
        do {

#define BEGIN_PROTECT2(v1,v2) \
    do { \
        SEXP v1 = R_NilValue, v2 = R_NilValue; \
        BEGIN_PROTECT_label: if (0) goto END_PROTECT_label; /* error check */ \
        const struct { \
            const struct R_local_protect *next; int cnt; SEXP *Protected[2]; \
        } R_local_protect = { R_local_protect_start, \
              2, { &v1, &v2 } }; \
        R_local_protect_start = (const struct R_local_protect *) \
                                  &R_local_protect; \
        struct { int outer; } R_protect_check; /* for error check */ \
        do {

#define BEGIN_PROTECT3(v1,v2,v3) \
    do { \
        SEXP v1 = R_NilValue, v2 = R_NilValue, v3 = R_NilValue; \
        BEGIN_PROTECT_label: if (0) goto END_PROTECT_label; /* error check */ \
        const struct { \
            const struct R_local_protect *next; int cnt; SEXP *Protected[3]; \
        } R_local_protect = { R_local_protect_start, \
              3, { &v1, &v2, &v3 } }; \
        R_local_protect_start = (const struct R_local_protect *) \
                                  &R_local_protect; \
        struct { int outer; } R_protect_check; /* for error check */ \
        do {

#define BEGIN_PROTECT4(v1,v2,v3,v4) \
    do { \
        SEXP v1 = R_NilValue, v2 = R_NilValue, v3 = R_NilValue; \
        SEXP v4 = R_NilValue; \
        BEGIN_PROTECT_label: if (0) goto END_PROTECT_label; /* error check */ \
        const struct { \
            const struct R_local_protect *next; int cnt; SEXP *Protected[4]; \
        } R_local_protect = { R_local_protect_start, \
              4, { &v1, &v2, &v3, &v4 } }; \
        R_local_protect_start = (const struct R_local_protect *) \
                                  &R_local_protect; \
        struct { int outer; } R_protect_check; /* for error check */ \
        do {

#define BEGIN_PROTECT5(v1,v2,v3,v4,v5) \
    do { \
        SEXP v1 = R_NilValue, v2 = R_NilValue, v3 = R_NilValue; \
        SEXP v4 = R_NilValue, v5 = R_NilValue; \
        BEGIN_PROTECT_label: if (0) goto END_PROTECT_label; /* error check */ \
        const struct { \
            const struct R_local_protect *next; int cnt; SEXP *Protected[5]; \
        } R_local_protect = { R_local_protect_start, \
              5, { &v1, &v2, &v3, &v4, &v5 } }; \
        R_local_protect_start = (const struct R_local_protect *) \
                                  &R_local_protect; \
        struct { int outer; } R_protect_check; /* for error check */ \
        do {

#define BEGIN_PROTECT6(v1,v2,v3,v4,v5,v6) \
    do { \
        SEXP v1 = R_NilValue, v2 = R_NilValue, v3 = R_NilValue; \
        SEXP v4 = R_NilValue, v5 = R_NilValue, v6 = R_NilValue; \
        BEGIN_PROTECT_label: if (0) goto END_PROTECT_label; /* error check */ \
        const struct { \
            const struct R_local_protect *next; int cnt; SEXP *Protected[6]; \
        } R_local_protect = { R_local_protect_start, \
              6, { &v1, &v2, &v3, &v4, &v5, &v6 } }; \
        R_local_protect_start = (const struct R_local_protect *) \
                                  &R_local_protect; \
        struct { int outer; } R_protect_check; /* for error check */ \
        do {

#define BEGIN_PROTECT7(v1,v2,v3,v4,v5,v6,v7) \
    do { \
        SEXP v1 = R_NilValue, v2 = R_NilValue, v3 = R_NilValue; \
        SEXP v4 = R_NilValue, v5 = R_NilValue, v6 = R_NilValue; \
        SEXP v7 = R_NilValue; \
        BEGIN_PROTECT_label: if (0) goto END_PROTECT_label; /* error check */ \
        const struct { \
            const struct R_local_protect *next; int cnt; SEXP *Protected[7]; \
        } R_local_protect = { R_local_protect_start, \
              7, { &v1, &v2, &v3, &v4, &v5, &v6, &v7 } }; \
        R_local_protect_start = (const struct R_local_protect *) \
                                  &R_local_protect; \
        struct { int outer; } R_protect_check; /* for error check */ \
        do {

#define BEGIN_PROTECT8(v1,v2,v3,v4,v5,v6,v7,v8) \
    do { \
        SEXP v1 = R_NilValue, v2 = R_NilValue, v3 = R_NilValue; \
        SEXP v4 = R_NilValue, v5 = R_NilValue, v6 = R_NilValue; \
        SEXP v7 = R_NilValue, v8 = R_NilValue; \
        BEGIN_PROTECT_label: if (0) goto END_PROTECT_label; /* error check */ \
        const struct { \
            const struct R_local_protect *next; int cnt; SEXP *Protected[8]; \
        } R_local_protect = { R_local_protect_start, \
              8, { &v1, &v2, &v3, &v4, &v5, &v6, &v7, &v8 } }; \
        R_local_protect_start = (const struct R_local_protect *) \
                                  &R_local_protect; \
        struct { int outer; } R_protect_check; /* for error check */ \
        do {

#define BEGIN_PROTECT9(v1,v2,v3,v4,v5,v6,v7,v8,v9) \
    do { \
        SEXP v1 = R_NilValue, v2 = R_NilValue, v3 = R_NilValue; \
        SEXP v4 = R_NilValue, v5 = R_NilValue, v6 = R_NilValue; \
        SEXP v7 = R_NilValue, v8 = R_NilValue, v9 = R_NilValue; \
        BEGIN_PROTECT_label: if (0) goto END_PROTECT_label; /* error check */ \
        const struct { \
            const struct R_local_protect *next; int cnt; SEXP *Protected[9]; \
        } R_local_protect = { R_local_protect_start, \
              9, { &v1, &v2, &v3, &v4, &v5, &v6, &v7, &v8, &v9 } }; \
        R_local_protect_start = (const struct R_local_protect *) \
                                  &R_local_protect; \
        struct { int outer; } R_protect_check; /* for error check */ \
        do {

#define BEGIN_INNER_PROTECT0() \
    do { \
        if (0) R_local_protect . next /* error check */; \
        const struct { /* not actually put in local_protect list */ \
            const struct R_local_protect *next; \
        } R_local_protect_inner = { R_local_protect_start }; \
        int R_protect_check; /* for error check */ \
        do {

#define BEGIN_INNER_PROTECT1(v1) \
    do { \
        SEXP v1 = R_NilValue; \
        if (0) R_local_protect . next /* error check */; \
        const struct { \
            const struct R_local_protect *next; int cnt; SEXP *Protected[1]; \
        } R_local_protect_inner = { R_local_protect_start, \
              1, { &v1 } }; \
        R_local_protect_start = (const struct R_local_protect *) \
                                  &R_local_protect_inner; \
        int R_protect_check; /* for error check */ \
        do {

#define BEGIN_INNER_PROTECT2(v1,v2) \
    do { \
        SEXP v1 = R_NilValue, v2 = R_NilValue; \
        if (0) R_local_protect . next /* error check */; \
        const struct { \
            const struct R_local_protect *next; int cnt; SEXP *Protected[2]; \
        } R_local_protect_inner = { R_local_protect_start, \
              2, { &v1, &v2 } }; \
        R_local_protect_start = (const struct R_local_protect *) \
                                  &R_local_protect_inner; \
        int R_protect_check; /* for error check */ \
        do {

#define BEGIN_INNER_PROTECT3(v1,v2,v3) \
    do { \
        SEXP v1 = R_NilValue, v2 = R_NilValue, v3 = R_NilValue; \
        if (0) R_local_protect . next /* error check */; \
        const struct { \
            const struct R_local_protect *next; int cnt; SEXP *Protected[3]; \
        } R_local_protect_inner = { R_local_protect_start, \
              3, { &v1, &v2, &v3 } }; \
        R_local_protect_start = (const struct R_local_protect *) \
                                  &R_local_protect_inner; \
        int R_protect_check; /* for error check */ \
        do {

#define BEGIN_INNER_PROTECT4(v1,v2,v3,v4) \
    do { \
        SEXP v1 = R_NilValue, v2 = R_NilValue, v3 = R_NilValue; \
        SEXP v4 = R_NilValue; \
        if (0) R_local_protect . next /* error check */; \
        const struct { \
            const struct R_local_protect *next; int cnt; SEXP *Protected[4]; \
        } R_local_protect_inner = { R_local_protect_start, \
              4, { &v1, &v2, &v3, &v4 } }; \
        R_local_protect_start = (const struct R_local_protect *) \
                                  &R_local_protect_inner; \
        int R_protect_check; /* for error check */ \
        do {

#define BEGIN_INNER_PROTECT5(v1,v2,v3,v4,v5) \
    do { \
        SEXP v1 = R_NilValue, v2 = R_NilValue, v3 = R_NilValue; \
        SEXP v4 = R_NilValue, v5 = R_NilValue; \
        if (0) R_local_protect . next /* error check */; \
        const struct { \
            const struct R_local_protect *next; int cnt; SEXP *Protected[5]; \
        } R_local_protect_inner = { R_local_protect_start, \
              5, { &v1, &v2, &v3, &v4, &v5 } }; \
        R_local_protect_start = (const struct R_local_protect *) \
                                  &R_local_protect_inner; \
        int R_protect_check; /* for error check */ \
        do {

#define BEGIN_INNER_PROTECT6(v1,v2,v3,v4,v5,v6) \
    do { \
        SEXP v1 = R_NilValue, v2 = R_NilValue, v3 = R_NilValue; \
        SEXP v4 = R_NilValue, v5 = R_NilValue, v6 = R_NilValue; \
        if (0) R_local_protect . next /* error check */; \
        const struct { \
            const struct R_local_protect *next; int cnt; SEXP *Protected[6]; \
        } R_local_protect_inner = { R_local_protect_start, \
              6, { &v1, &v2, &v3, &v4, &v5, &v6 } }; \
        R_local_protect_start = (const struct R_local_protect *) \
                                  &R_local_protect_inner; \
        int R_protect_check; /* for error check */ \
        do {

#define BEGIN_INNER_PROTECT7(v1,v2,v3,v4,v5,v6,v7) \
    do { \
        SEXP v1 = R_NilValue, v2 = R_NilValue, v3 = R_NilValue; \
        SEXP v4 = R_NilValue, v5 = R_NilValue, v6 = R_NilValue; \
        SEXP v7 = R_NilValue; \
        if (0) R_local_protect . next /* error check */; \
        const struct { \
            const struct R_local_protect *next; int cnt; SEXP *Protected[7]; \
        } R_local_protect_inner = { R_local_protect_start, \
              7, { &v1, &v2, &v3, &v4, &v5, &v6, &v7 } }; \
        R_local_protect_start = (const struct R_local_protect *) \
                                  &R_local_protect_inner; \
        int R_protect_check; /* for error check */ \
        do {

#define BEGIN_INNER_PROTECT8(v1,v2,v3,v4,v5,v6,v7,v8) \
    do { \
        SEXP v1 = R_NilValue, v2 = R_NilValue, v3 = R_NilValue; \
        SEXP v4 = R_NilValue, v5 = R_NilValue, v6 = R_NilValue; \
        SEXP v7 = R_NilValue, v8 = R_NilValue; \
        if (0) R_local_protect . next /* error check */; \
        const struct { \
            const struct R_local_protect *next; int cnt; SEXP *Protected[8]; \
        } R_local_protect_inner = { R_local_protect_start, \
              8, { &v1, &v2, &v3, &v4, &v5, &v6, &v7, &v8 } }; \
        R_local_protect_start = (const struct R_local_protect *) \
                                  &R_local_protect_inner; \
        int R_protect_check; /* for error check */ \
        do {

#define BEGIN_INNER_PROTECT9(v1,v2,v3,v4,v5,v6,v7,v8,v9) \
    do { \
        SEXP v1 = R_NilValue, v2 = R_NilValue, v3 = R_NilValue; \
        SEXP v4 = R_NilValue, v5 = R_NilValue, v6 = R_NilValue; \
        SEXP v7 = R_NilValue, v8 = R_NilValue, v9 = R_NilValue; \
        if (0) R_local_protect . next /* error check */; \
        const struct { \
            const struct R_local_protect *next; int cnt; SEXP *Protected[9]; \
        } R_local_protect_inner = { R_local_protect_start, \
              9, { &v1, &v2, &v3, &v4, &v5, &v6, &v7, &v8, &v9 } }; \
        R_local_protect_start = (const struct R_local_protect *) \
                                  &R_local_protect_inner; \
        int R_protect_check; /* for error check */ \
        do {

#define ALSO_PROTECT1(v1) \
    if (0) R_local_protect . next /* error check */; \
    CHK_IS_SEXP(v1); \
    const struct { \
        const struct R_local_protect *next; int cnt; SEXP *Protected[1]; \
    } R_local_protect_also = { R_local_protect_start, \
          1, { &v1 } }; \
    R_local_protect_start = (const struct R_local_protect *) \
                              &R_local_protect_also;

#define ALSO_PROTECT2(v1,v2) \
    if (0) R_local_protect . next /* error check */; \
    CHK_IS_SEXP(v1); CHK_IS_SEXP(v2); \
    const struct { \
        const struct R_local_protect *next; int cnt; SEXP *Protected[2]; \
    } R_local_protect_also = { R_local_protect_start, \
          2, { &v1, &v2 } }; \
    R_local_protect_start = (const struct R_local_protect *) \
                              &R_local_protect_also;

#define ALSO_PROTECT3(v1,v2,v3) \
    if (0) R_local_protect . next /* error check */; \
    CHK_IS_SEXP(v1); CHK_IS_SEXP(v2); CHK_IS_SEXP(v3); \
    const struct { \
        const struct R_local_protect *next; int cnt; SEXP *Protected[3]; \
    } R_local_protect_also = { R_local_protect_start, \
          3, { &v1, &v2, &v3 } }; \
    R_local_protect_start = (const struct R_local_protect *) \
                              &R_local_protect_also;

#define ALSO_PROTECT4(v1,v2,v3,v4) \
    if (0) R_local_protect . next /* error check */; \
    CHK_IS_SEXP(v1); CHK_IS_SEXP(v2); CHK_IS_SEXP(v3); CHK_IS_SEXP(v4); \
    const struct { \
        const struct R_local_protect *next; int cnt; SEXP *Protected[4]; \
    } R_local_protect_also = { R_local_protect_start, \
          4, { &v1, &v2, &v3, &v4 } }; \
    R_local_protect_start = (const struct R_local_protect *) \
                              &R_local_protect_also;

#define ALSO_PROTECT5(v1,v2,v3,v4,v5) \
    if (0) R_local_protect . next /* error check */; \
    CHK_IS_SEXP(v1); CHK_IS_SEXP(v2); CHK_IS_SEXP(v3); CHK_IS_SEXP(v4); \
    CHK_IS_SEXP(v5); \
    const struct { \
        const struct R_local_protect *next; int cnt; SEXP *Protected[5]; \
    } R_local_protect_also = { R_local_protect_start, \
          5, { &v1, &v2, &v3, &v4, &v5 } }; \
    R_local_protect_start = (const struct R_local_protect *) \
                              &R_local_protect_also;

#define ALSO_PROTECT6(v1,v2,v3,v4,v5,v6) \
    if (0) R_local_protect . next /* error check */; \
    CHK_IS_SEXP(v1); CHK_IS_SEXP(v2); CHK_IS_SEXP(v3); CHK_IS_SEXP(v4); \
    CHK_IS_SEXP(v5); CHK_IS_SEXP(v6); \
    const struct { \
        const struct R_local_protect *next; int cnt; SEXP *Protected[6]; \
    } R_local_protect_also = { R_local_protect_start, \
          6, { &v1, &v2, &v3, &v4, &v5, &v6 } }; \
    R_local_protect_start = (const struct R_local_protect *) \
                              &R_local_protect_also;

#define ALSO_PROTECT7(v1,v2,v3,v4,v5,v6,v7) \
    if (0) R_local_protect . next /* error check */; \
    CHK_IS_SEXP(v1); CHK_IS_SEXP(v2); CHK_IS_SEXP(v3); CHK_IS_SEXP(v4); \
    CHK_IS_SEXP(v5); CHK_IS_SEXP(v6); CHK_IS_SEXP(v7); \
    const struct { \
        const struct R_local_protect *next; int cnt; SEXP *Protected[7]; \
    } R_local_protect_also = { R_local_protect_start, \
          7, { &v1, &v2, &v3, &v4, &v5, &v6, &v7 } }; \
    R_local_protect_start = (const struct R_local_protect *) \
                              &R_local_protect_also;

#define ALSO_PROTECT8(v1,v2,v3,v4,v5,v6,v7,v8) \
    if (0) R_local_protect . next /* error check */; \
    CHK_IS_SEXP(v1); CHK_IS_SEXP(v2); CHK_IS_SEXP(v3); CHK_IS_SEXP(v4); \
    CHK_IS_SEXP(v5); CHK_IS_SEXP(v6); CHK_IS_SEXP(v7); CHK_IS_SEXP(v8); \
    const struct { \
        const struct R_local_protect *next; int cnt; SEXP *Protected[8]; \
    } R_local_protect_also = { R_local_protect_start, \
          8, { &v1, &v2, &v3, &v4, &v5, &v6, &v7, &v8 } }; \
    R_local_protect_start = (const struct R_local_protect *) \
                              &R_local_protect_also;

#define ALSO_PROTECT9(v1,v2,v3,v4,v5,v6,v7,v8,v9) \
    if (0) R_local_protect . next /* error check */; \
    CHK_IS_SEXP(v1); CHK_IS_SEXP(v2); CHK_IS_SEXP(v3); CHK_IS_SEXP(v4); \
    CHK_IS_SEXP(v5); CHK_IS_SEXP(v6); CHK_IS_SEXP(v7); CHK_IS_SEXP(v8); \
    CHK_IS_SEXP(v9); \
    const struct { \
        const struct R_local_protect *next; int cnt; SEXP *Protected[9]; \
    } R_local_protect_also = { R_local_protect_start, \
          9, { &v1, &v2, &v3, &v4, &v5, &v6, &v7, &v8, &v9 } }; \
    R_local_protect_start = (const struct R_local_protect *) \
                              &R_local_protect_also;

#define END_PROTECT \
        } while (0); /* break/continue inside will go here */ \
        if (0) R_protect_check . outer; /* error check */ \
        R_local_protect_start = R_local_protect . next; \
        END_PROTECT_label: if (0) goto BEGIN_PROTECT_label; /* error check */ \
    } while (0)

#define END_INNER_PROTECT \
        } while (0); /* break/continue inside will go here */ \
        R_local_protect_start = R_local_protect_inner . next; \
    } while (0)

#define RETURN_SEXP_INSIDE_PROTECT(e) \
    do { \
        CHK_IS_SEXP(e); \
        SEXP R_value_about_to_be_returned = (e); \
        R_local_protect_start = R_local_protect . next; \
        return R_value_about_to_be_returned; \
    } while (0)

#define RETURN_OUTSIDE_PROTECT(e) \
    do { \
        R_local_protect_start = R_local_protect . next; \
        return (e); \
    } while (0)


/* Segment indexes for constant segments.  They start at 1, since 
   SGGC_NO_OBJECT_ZERO is defined. */

#define R_SGGC_NIL_INDEX 1
#define R_SGGC_ENV_INDEX 2
#define R_SGGC_SYM_INDEX 3
#define R_SGGC_INT_INDEX 4
#define R_SGGC_MISC_INDEX 5
#define R_SGGC_CHAR_INDEX 6     /* also uses 7, 8, and 9 */
#define R_SGGC_STRING_INDEX 10  /* also uses 11, 12, and 13 */
#define R_SGGC_LIST1_INDEX 14
#define R_SGGC_PADDING_INDEX 15 /* unused, here so next will be multiple of 4 */
#define R_SGGC_SCALAR_STACK_INDEX 16  /* and possibly subsequent segments,
                                         must be a multiple of 4 */

/* R_EmptyEnv - an empty environment at the root of the environment tree */

#if USE_COMPRESSED_POINTERS
#define R_EmptyEnv ((SEXP)SGGC_CPTR_VAL(R_SGGC_ENV_INDEX,0))
#else
ConstExtern R_CONST ENV_SEXPREC R_env_consts[1]; /* Defined in const-objs.c */
#define R_EmptyEnv ((SEXP) &R_env_consts[0])
#endif

LibExtern SEXP	R_GlobalEnv;        /* The "global" environment */
LibExtern SEXP	R_BaseEnv;          /* The base environment (formerly R_NilValue) */

LibExtern SEXP	R_BaseNamespace;    /* The (fake) namespace for base */
LibExtern SEXP	R_NamespaceRegistry;/* Registry for registered namespaces */

#define R_Srcref R_high_frequency_globals.Srcref

/* R_NilValue - the R NULL object */

#if USE_COMPRESSED_POINTERS
#define R_NilValue ((SEXP)SGGC_CPTR_VAL(R_SGGC_NIL_INDEX,0))
#else
ConstExtern R_CONST SEXPREC R_NilValue_const; /* defined in const-objs.c */
#define R_NilValue ((SEXP) &R_NilValue_const)
#endif

/* Symbol constants. */

#if !USE_COMPRESSED_POINTERS
ConstExtern SYM_SEXPREC R_sym_consts[3];         /* defined in const-objs.c */
#endif

/* R_UnboundValue - for symbol with no value. */

#if USE_COMPRESSED_POINTERS
#define R_UnboundValue ((SEXP)SGGC_CPTR_VAL(R_SGGC_SYM_INDEX,0*SGGC_SYM_CHUNKS))
#else
#define R_UnboundValue ((SEXP) &R_sym_consts[0]) /* for sym with no value */
#endif

/* Missing argument symbols.  MissingUnder form is for "_". */

#if USE_COMPRESSED_POINTERS
#define R_MissingArg ((SEXP)SGGC_CPTR_VAL(R_SGGC_SYM_INDEX,1*SGGC_SYM_CHUNKS))
#else
#define R_MissingArg ((SEXP) &R_sym_consts[1])
#endif

#if USE_COMPRESSED_POINTERS
#define R_MissingUnder ((SEXP)SGGC_CPTR_VAL(R_SGGC_SYM_INDEX,2*SGGC_SYM_CHUNKS))
#else
#define R_MissingUnder ((SEXP) &R_sym_consts[2])
#endif

/* Logical / Integer / Real Values.  Defined in const-objs.c, must keep
   in sync. */

#if USE_COMPRESSED_POINTERS
#define R_ScalarInteger0To31(v)   ((SEXP)SGGC_CPTR_VAL(R_SGGC_INT_INDEX,v))
#define R_ScalarLogicalFALSE      ((SEXP)SGGC_CPTR_VAL(R_SGGC_MISC_INDEX,0))
#define R_ScalarLogicalTRUE       ((SEXP)SGGC_CPTR_VAL(R_SGGC_MISC_INDEX,1))
#define R_ScalarLogicalNA         ((SEXP)SGGC_CPTR_VAL(R_SGGC_MISC_INDEX,2))
#define R_ScalarIntegerNA         ((SEXP)SGGC_CPTR_VAL(R_SGGC_MISC_INDEX,3))
#define R_ScalarRealZero          ((SEXP)SGGC_CPTR_VAL(R_SGGC_MISC_INDEX,4))
#define R_ScalarRealOne           ((SEXP)SGGC_CPTR_VAL(R_SGGC_MISC_INDEX,5))
#define R_ScalarRealTwo           ((SEXP)SGGC_CPTR_VAL(R_SGGC_MISC_INDEX,6))
#define R_ScalarRealHalf          ((SEXP)SGGC_CPTR_VAL(R_SGGC_MISC_INDEX,7))
#define R_ScalarRealNA            ((SEXP)SGGC_CPTR_VAL(R_SGGC_MISC_INDEX,8))
#else
ConstExtern R_CONST VECTOR_SEXPREC_C R_ScalarInteger_consts[32];
ConstExtern R_CONST VECTOR_SEXPREC_C R_ScalarMisc_consts[9];
#define R_ScalarInteger0To31(v) ((SEXP) &R_ScalarInteger_consts[v])
#define R_ScalarLogicalFALSE    ((SEXP) &R_ScalarMisc_consts[0])
#define R_ScalarLogicalTRUE     ((SEXP) &R_ScalarMisc_consts[1])
#define R_ScalarLogicalNA       ((SEXP) &R_ScalarMisc_consts[2])
#define R_ScalarIntegerNA       ((SEXP) &R_ScalarMisc_consts[3])
#define R_ScalarRealZero        ((SEXP) &R_ScalarMisc_consts[4])
#define R_ScalarRealOne         ((SEXP) &R_ScalarMisc_consts[5])
#define R_ScalarRealTwo         ((SEXP) &R_ScalarMisc_consts[6])
#define R_ScalarRealHalf        ((SEXP) &R_ScalarMisc_consts[7])
#define R_ScalarRealNA          ((SEXP) &R_ScalarMisc_consts[8])
#endif

/* CHARSXP and scalar STRSXP constants.  Defined in const-objs.c. */

#if USE_COMPRESSED_POINTERS
#define R_ASCII_CHAR(c) ((SEXP)SGGC_CPTR_VAL(R_SGGC_CHAR_INDEX+(c>>5),(c&0x1f)))
#define R_ASCII_SCALAR_STRING(c) \
                      ((SEXP)SGGC_CPTR_VAL(R_SGGC_STRING_INDEX+(c>>5),(c&0x1f)))
#else
ConstExtern /* R_CONST */ VECTOR_SEXPREC_C R_ASCII_consts[128];
ConstExtern R_CONST VECTOR_SEXPREC_C R_ScalarString_consts[128];
#define R_ASCII_CHAR(c) ((SEXP) &R_ASCII_consts[(int)c])
#define R_ASCII_SCALAR_STRING(c) ((SEXP) &R_ScalarString_consts[(int)c])
#endif

/* Start of scalar stack. */

#define SCALAR_STACK_SIZE 128  /* Number of values allowed on scalar stack */

ConstExtern VECTOR_SEXPREC_C R_scalar_stack_space[SCALAR_STACK_SIZE];

#if USE_COMPRESSED_POINTERS
#define R_scalar_stack_start ((SEXP) SGGC_CPTR_VAL(R_SGGC_SCALAR_STACK_INDEX,0))
#else
#define R_scalar_stack_start ((SEXP) &R_scalar_stack_space[0])
#endif

#if USE_COMPRESSED_POINTERS
#define R_NoObject SGGC_NO_OBJECT
#else
#define R_NoObject NULL
#endif

#ifdef __MAIN__
attribute_hidden
#else
extern
#endif
SEXP	R_RestartToken;     /* Marker for restarted function calls */

/* Symbol Table Shortcuts */

#define R_DotsSymbol R_high_frequency_globals.DotsSymbol
#define R_BraceSymbol R_high_frequency_globals.BraceSymbol

LibExtern SEXP	R_BracketSymbol;    /* "[" */
LibExtern SEXP	R_Bracket2Symbol;   /* "[[" */
LibExtern SEXP	R_DollarSymbol;	    /* "$" */
LibExtern SEXP	R_SubAssignSymbol;  /* "[<-" */
LibExtern SEXP	R_SubSubAssignSymbol; /* "[[<-" */
LibExtern SEXP	R_DollarAssignSymbol; /* "$<-" */
LibExtern SEXP	R_AssignSymbols[4]; /* 0, "<-", "<<-", "=" */
LibExtern SEXP	R_LocalAssignSymbol;   /* same as R_AssignSymbols[1] */
LibExtern SEXP	R_GlobalAssignSymbol;  /* same as R_AssignSymbols[2] */
LibExtern SEXP	R_EqAssignSymbol;      /* same as R_AssignSymbols[3] */
LibExtern SEXP	R_LocalRightAssignSymbol;  /* -> */
LibExtern SEXP	R_GlobalRightAssignSymbol; /* ->> */

LibExtern SEXP	R_ClassSymbol;	    /* "class" */
LibExtern SEXP	R_DeviceSymbol;     /* ".Device" */
LibExtern SEXP	R_DimNamesSymbol;   /* "dimnames" */
LibExtern SEXP	R_DimSymbol;	    /* "dim" */
LibExtern SEXP	R_DropSymbol;	    /* "drop" */
LibExtern SEXP	R_LastvalueSymbol;  /* ".Last.value" */
LibExtern SEXP	R_LevelsSymbol;	    /* "levels" */
LibExtern SEXP	R_ModeSymbol;	    /* "mode" */
LibExtern SEXP	R_NameSymbol;	    /* "name" */
LibExtern SEXP	R_NamesSymbol;	    /* "names" */
LibExtern SEXP	R_NaRmSymbol;	    /* "na.rm" */
LibExtern SEXP	R_xSymbol;          /* "x" */
LibExtern SEXP  R_PackageSymbol;    /* "package" */
LibExtern SEXP  R_QuoteSymbol;	    /* "quote" */
LibExtern SEXP	R_RowNamesSymbol;   /* "row.names" */
LibExtern SEXP	R_SeedsSymbol;	    /* ".Random.seed" */
LibExtern SEXP	R_SourceSymbol;     /* "source" */
LibExtern SEXP	R_SrcrefSymbol;     /* "srcref" */
LibExtern SEXP	R_TspSymbol;	    /* "tsp" */
LibExtern SEXP	R_LogarithmSymbol;  /* "logarithm" */
LibExtern SEXP	R_LogSymbol;        /* "log" */
LibExtern SEXP	R_ValueSymbol;	    /* "value" */
LibExtern SEXP	R_GradientSymbol;   /* "gradient" */
LibExtern SEXP	R_HessianSymbol;    /* "hessian" */

LibExtern SEXP  R_dot_defined;      /* ".defined" */
LibExtern SEXP  R_dot_Method;       /* ".Method" */
LibExtern SEXP  R_dot_target;       /* ".target" */

LibExtern SEXP R_NaokSymbol;	    /* "NAOK" */
LibExtern SEXP R_DupSymbol;	    /* "DUP" */
LibExtern SEXP R_PkgSymbol;	    /* "PACKAGE" */
LibExtern SEXP R_EncSymbol;	    /* "ENCODING" */
LibExtern SEXP R_HelperSymbol;	    /* "HELPER" */
LibExtern SEXP R_CSingSymbol;	    /* "Csingle" */

LibExtern SEXP R_NativeSymbolSymbol;            /* "native symbol" */
LibExtern SEXP R_RegisteredNativeSymbolSymbol;  /* "registered native symbol" */

LibExtern SEXP R_factor_CHARSXP;     /* some CHARSXP's that are useful */
LibExtern SEXP R_ordered_CHARSXP;    /*   for inherits_CHARSXP         */
LibExtern SEXP R_NativeSymbolInfo_CHARSXP;
LibExtern SEXP R_matrix_CHARSXP;     /* .. or for class dispatch       */
LibExtern SEXP R_array_CHARSXP;
LibExtern SEXP R_integer_CHARSXP;
LibExtern SEXP R_double_CHARSXP;
LibExtern SEXP R_numeric_CHARSXP;
LibExtern SEXP R_name_CHARSXP;
LibExtern SEXP R_function_CHARSXP;
LibExtern SEXP R_any_CHARSXP;
LibExtern SEXP R_modulus_CHARSXP;
LibExtern SEXP R_sign_CHARSXP;
LibExtern SEXP R_det_CHARSXP;
LibExtern SEXP R_TRUE_CHARSXP;
LibExtern SEXP R_FALSE_CHARSXP;

/* Missing Values - others from Arith.h */
#define NA_STRING	R_NaString
LibExtern SEXP	R_NaString;	    /* NA_STRING as a CHARSXP */
LibExtern SEXP	R_BlankString;	    /* "" as a CHARSXP */
LibExtern SEXP	R_BlankScalarString; /* "" as a STRSXP */
 
/* srcref related functions */
SEXP R_GetCurrentSrcref(int);
SEXP R_GetSrcFilename(SEXP);

/* Coercion warnings - will be OR'ed : */

#define WARN_NA	   1
#define WARN_INACC 2
#define WARN_IMAG  4
#define WARN_RAW  8


/*--- FUNCTIONS ------------------------------------------------------ */

/* Type Coercions of all kinds */

SEXP Rf_asChar(SEXP);
SEXP Rf_coerceVector(SEXP, SEXPTYPE);
SEXP Rf_PairToVectorList(SEXP x);
SEXP Rf_VectorToPairList(SEXP x);
SEXP Rf_asCharacterFactor(SEXP x);
int Rf_asLogical(SEXP x);
int Rf_asInteger(SEXP x);
double Rf_asReal(SEXP x);
Rcomplex Rf_asComplex(SEXP x);



/* Other Internally Used Functions, excluding those which are inline-able*/

char * Rf_acopy_string(const char *);
SEXP Rf_alloc3DArray(SEXPTYPE, int, int, int);
SEXP Rf_allocArray(SEXPTYPE, SEXP);
SEXP Rf_allocMatrix(SEXPTYPE, int, int);
SEXP Rf_allocList(int);
SEXP Rf_allocS4Object(void);
SEXP Rf_allocSExp(SEXPTYPE);
SEXP Rf_allocVector(SEXPTYPE, R_len_t);
SEXP Rf_allocVector1RAW(void);
SEXP Rf_allocVector1LGL(void);
SEXP Rf_allocVector1INT(void);
SEXP Rf_allocVector1REAL(void);
SEXP Rf_reallocVector(SEXP, R_len_t, int);
int  Rf_any_duplicated(SEXP x, Rboolean from_last);
int  Rf_any_duplicated3(SEXP x, SEXP incomp, Rboolean from_last);
SEXP Rf_applyClosure(SEXP, SEXP, SEXP, SEXP, SEXP *);
SEXP Rf_applyClosure_v(SEXP, SEXP, SEXP, SEXP, SEXP *, int);
SEXP Rf_arraySubscript(int, SEXP, SEXP, SEXP (*)(SEXP,SEXP),
                       SEXP (*)(SEXP, int), SEXP);
SEXP Rf_classgets(SEXP, SEXP);
SEXP Rf_cons_with_tag(SEXP, SEXP, SEXP);
SEXP Rf_cons(SEXP, SEXP);
void Rf_double_to_string(char *, double);
void Rf_integer_to_string(char *, int);
Rboolean Rf_copy_1_string(char *, int, const char *);
Rboolean Rf_copy_2_strings(char *, int, const char *, const char *);
Rboolean Rf_copy_3_strings(char *, int, const char *, const char *, const char *);
void Rf_copyMatrix(SEXP, SEXP, Rboolean);
void Rf_copyMostAttrib(SEXP, SEXP);
void Rf_copyVector(SEXP, SEXP);
void Rf_copy_elements(SEXP, int, int, SEXP, int, int, int);
int Rf_copy_elements_coerced(SEXP, int, int, SEXP, int, int, int);
void Rf_copy_elements_recycled(SEXP, int, SEXP, int);
void Rf_copy_transposed(SEXP, SEXP, int, int);
int Rf_countContexts(int, int);
SEXP Rf_CreateTag(SEXP);
void Rf_defineVar(SEXP, SEXP, SEXP);
SEXP Rf_dimgets(SEXP, SEXP);
SEXP Rf_dimnamesgets(SEXP, SEXP);
SEXP Rf_DropDims(SEXP);
SEXP Rf_DropDimsNotSuppressed(SEXP,int*);
SEXP Rf_duplicate(SEXP);
SEXP Rf_duplicated(SEXP, Rboolean);
int Rf_ep_match_exprs(SEXP, SEXP);
int Rf_ep_match_string_expr(const char *, SEXP);
SEXP Rf_eval(SEXP, SEXP);
SEXP Rf_evalv(SEXP, SEXP, int);
SEXP Rf_findFun(SEXP, SEXP);
SEXP Rf_findFunMethod(SEXP, SEXP);
SEXP Rf_findVar(SEXP, SEXP);
SEXP Rf_findVarPendingOK(SEXP, SEXP);
SEXP Rf_findVarInFrame(SEXP, SEXP);
SEXP Rf_findVarInFramePendingOK(SEXP, SEXP);
SEXP Rf_findVarInFrame3(SEXP, SEXP, int);
SEXP Rf_findVarInFrame3_nolast(SEXP, SEXP, int);
SEXP Rf_fixup_NaRm(SEXP);
SEXP Rf_getAttrib(SEXP, SEXP);
SEXP Rf_getNamesAttrib(SEXP);
SEXP Rf_GetArrayDimnames(SEXP);
SEXP Rf_GetColNames(SEXP);
void Rf_GetMatrixDimnames(SEXP, SEXP*, SEXP*, const char**, const char**);
SEXP Rf_GetOption(SEXP, SEXP); /* pre-2.13.0 compatibility */
SEXP Rf_GetOption1(SEXP);
int Rf_GetOptionDigits(void);
int Rf_GetOptionWidth(void);
SEXP Rf_GetRowNames(SEXP);
void Rf_gsetVar(SEXP, SEXP, SEXP);
SEXPTYPE Rf_higher_atomic_type (SEXPTYPE, SEXPTYPE);
SEXP Rf_install(const char *);
SEXP Rf_installChar(SEXP);
SEXP Rf_installed_already(const char *);
SEXP Rf_installed_already_with_hash(const char *, int);
Rboolean Rf_isFree(SEXP);
Rboolean Rf_isOrdered(SEXP);
Rboolean Rf_isUnordered(SEXP);
Rboolean Rf_isUnsorted(SEXP, Rboolean);
R_len_t Rf_length(SEXP);
SEXP Rf_lengthgets(SEXP, R_len_t);
SEXP Rf_xlengthgets(SEXP, R_xlen_t);
SEXP R_lsInternal(SEXP, Rboolean);
SEXP Rf_match(SEXP, SEXP, int);
SEXP Rf_matchE(SEXP, SEXP, int, SEXP);
SEXP Rf_namesgets(SEXP, SEXP);
SEXP Rf_mkChar(const char *);
SEXP Rf_mkCharLen(const char *, int);
int Rf_ncols(SEXP);
int Rf_nrows(SEXP);
SEXP Rf_nthcdr(SEXP, int);

Rboolean Rf_pmatch(SEXP, SEXP, Rboolean);
Rboolean Rf_psmatch(const char *, const char *, Rboolean);
void Rf_PrintValue(SEXP);
SEXP Rf_protect(SEXP);
void Rf_protect2(SEXP, SEXP);
void Rf_protect3(SEXP, SEXP, SEXP);
void Rf_protect4(SEXP, SEXP, SEXP, SEXP);
void Rf_rep_element (SEXP, int, SEXP, int, int);
SEXP Rf_ScalarComplexMaybeConst(Rcomplex);
SEXP Rf_ScalarIntegerMaybeConst(int);
SEXP Rf_ScalarRawMaybeConst(Rbyte);
SEXP Rf_ScalarRealMaybeConst(double);
SEXP Rf_ScalarStringMaybeConst(SEXP);
SEXP Rf_setAttrib(SEXP, SEXP, SEXP);
void Rf_setSVector(SEXP*, int, SEXP);
void Rf_set_symbits_in_env(SEXP);
void Rf_setVar(SEXP, SEXP, SEXP);
int Rf_set_var_in_frame(SEXP, SEXP, SEXP, int, int);
void Rf_set_var_nonlocal(SEXP, SEXP, SEXP, int);
SEXP Rf_MaybeConstList1(SEXP);
SEXPTYPE Rf_str2type(const char *);
Rboolean Rf_StringBlank(SEXP);
SEXP Rf_substitute(SEXP,SEXP);
int Rf_tag_index(SEXP,SEXP);
const char * Rf_translateChar0(SEXP);
const char * Rf_translateCharUTF8(SEXP);
const char * Rf_type2char(SEXPTYPE);
SEXP Rf_type2rstr(SEXPTYPE);
SEXP Rf_type2str(SEXPTYPE);
SEXP Rf_type2str_nowarn(SEXPTYPE);
SEXP Rf_type2str(SEXPTYPE);
void Rf_unprotect(int);
void Rf_unprotect_ptr(SEXP);
SEXP Rf_with_changed_nth(SEXP,int,SEXP);
SEXP Rf_with_no_nth(SEXP,int);
SEXP Rf_with_pairlist_appended(SEXP,SEXP);
SEXP Rf_copy_pairlist(SEXP);
R_xlen_t Rf_xlength(SEXP);

SEXP R_ProtectWithIndex(SEXP, PROTECT_INDEX *);
void R_Reprotect(SEXP, PROTECT_INDEX);
SEXP R_tryEval(SEXP, SEXP, int *);
SEXP R_tryEvalSilent(SEXP, SEXP, int *);
const char *R_curErrorBuf();

Rboolean Rf_isS4(SEXP);
SEXP Rf_asS4(SEXP, Rboolean, int);
SEXP Rf_S3Class(SEXP);
int Rf_isBasicClass(const char *);

typedef enum {
    CE_NATIVE = 0,
    CE_UTF8   = 1,
    CE_LATIN1 = 2,
    CE_BYTES  = 3,
    CE_SYMBOL = 5,
    CE_ANY    =99
} cetype_t;

cetype_t Rf_getCharCE(SEXP);
SEXP Rf_mkCharCE(const char *, cetype_t);
SEXP Rf_mkCharLenCE(const char *, int, cetype_t);
const char *Rf_reEnc(const char *x, cetype_t ce_in, cetype_t ce_out, int subst);

				/* return(.) NOT reached : for -Wall */
#define error_return(msg)	{ Rf_error(msg);	   return R_NilValue; }
#define errorcall_return(cl,msg){ Rf_errorcall(cl, msg);   return R_NilValue; }


/* Structure containing frequently-used globals, to ensure locality of
   reference, and perhaps allow the compiler to generate faster code
   for addressing these variables (from knowing they are adjacent). */

LibExtern struct {
    SEXP *PPStack;                /* Pointer to area for pointer protect stack*/
    int PPStackTop;               /* Top of the pointer protection stack */
    int PPStackSize;              /* Size of pointer protect stack (elements) */
    unsigned variant_result;      /* 0 or kind of variant result */
    int EvalDepth;                /* Evaluation recursion depth */
    int Expressions;              /* options(expressions) */
    short evalcount;              /* counts down to check user interrupt */
    short Q_Visible;              /* Value visibility flag.  Note: The symbol
                                          "Visible" is used in Windows Rtools */
    SEXP DotsSymbol;              /* Symbol ... */
    SEXP binding_cell;            /* Binding cell for variable found, or NULL */
    char *CStackThreshold;        /* Threshold for overflow detection */
    SEXP VStack;                  /* R_alloc stack pointer */
    SEXP scalar_stack;            /* Next unused position on scalar stack */
    const struct R_local_protect *local_protect_start;/*Start of protect chain*/
    SEXP Srcref;                  /* Current srcref, for debuggers */
    SEXP BraceSymbol;             /* Symbol { */
    short Profiling;              /* Whether performance profiling enabled */

    SEXP gradient;                /* Gradient of expression, if requested and
                                     provided */
} R_high_frequency_globals;

#define InitHighFrequencyGlobals() \
do \
{ \
    R_high_frequency_globals.EvalDepth    = 0; \
    R_high_frequency_globals.Expressions  = 5000; \
    R_high_frequency_globals.evalcount    = 0; \
    R_high_frequency_globals.VStack       = R_NoObject; \
    R_high_frequency_globals.scalar_stack = R_scalar_stack_start; \
    R_high_frequency_globals.PPStackSize  = R_PPSSIZE; \
    R_high_frequency_globals.Profiling    = 0; \
    R_high_frequency_globals.local_protect_start = NULL; \
    R_high_frequency_globals.gradient     = R_NoObject; \
} while (0)


#ifdef __MAIN__
#undef extern
#undef LibExtern
#endif

/* External pointer interface */
SEXP R_MakeExternalPtr(void *p, SEXP tag, SEXP prot);
void *R_ExternalPtrAddr(SEXP s);
SEXP R_ExternalPtrTag(SEXP s);
SEXP R_ExternalPtrProtected(SEXP s);
void R_ClearExternalPtr(SEXP s);
void R_SetExternalPtrAddr(SEXP s, void *p);
void R_SetExternalPtrTag(SEXP s, SEXP tag);
void R_SetExternalPtrProtected(SEXP s, SEXP p);

/* Finalization interface */
typedef void (*R_CFinalizer_t)(SEXP);
void R_RegisterFinalizer(SEXP s, SEXP fun);
void R_RegisterCFinalizer(SEXP s, R_CFinalizer_t fun);
void R_RegisterFinalizerEx(SEXP s, SEXP fun, Rboolean onexit);
void R_RegisterCFinalizerEx(SEXP s, R_CFinalizer_t fun, Rboolean onexit);

/* Weak reference interface */
SEXP R_MakeWeakRef(SEXP key, SEXP val, SEXP fin, Rboolean onexit);
SEXP R_MakeWeakRefC(SEXP key, SEXP val, R_CFinalizer_t fin, Rboolean onexit);
SEXP R_WeakRefKey(SEXP w);
SEXP R_WeakRefValue(SEXP w);
void R_RunWeakRefFinalizer(SEXP w);

SEXP R_PromiseExpr(SEXP);
SEXP R_ClosureExpr(SEXP);
void R_initialize_bcode(void);
SEXP R_bcEncode(SEXP);
SEXP R_bcDecode(SEXP);
#define PREXPR(e) R_PromiseExpr(e)
#define BODY_EXPR(e) R_ClosureExpr(e)

/* Protected evaluation */
Rboolean R_ToplevelExec(void (*fun)(void *), void *data);

/* Environment and Binding Features */
void R_RestoreHashCount(SEXP rho);
Rboolean R_IsPackageEnv(SEXP rho);
SEXP R_PackageEnvName(SEXP rho);
SEXP R_FindPackageEnv(SEXP info);
Rboolean R_IsNamespaceEnv(SEXP rho);
SEXP R_NamespaceEnvSpec(SEXP rho);
SEXP R_FindNamespace(SEXP info);
void R_LockEnvironment(SEXP env, Rboolean bindings);
Rboolean R_EnvironmentIsLocked(SEXP env);
void R_LockBinding(SEXP sym, SEXP env);
void R_unLockBinding(SEXP sym, SEXP env);
void R_MakeActiveBinding(SEXP sym, SEXP fun, SEXP env);
Rboolean R_BindingIsLocked(SEXP sym, SEXP env);
Rboolean R_BindingIsActive(SEXP sym, SEXP env);
Rboolean R_HasFancyBindings(SEXP rho);


/* ../main/errors.c : */
/* needed for R_load/savehistory handling in front ends */
R_NORETURN void Rf_errorcall(SEXP, const char *, ...);
void Rf_warningcall(SEXP, const char *, ...);
void Rf_warningcall_immediate(SEXP, const char *, ...);

typedef void *R_pstream_data_t;

typedef enum {
    R_pstream_any_format,
    R_pstream_ascii_format,
    R_pstream_binary_format,
    R_pstream_xdr_format
} R_pstream_format_t;

typedef struct R_outpstream_st *R_outpstream_t;
struct R_outpstream_st {
    R_pstream_data_t data;
    R_pstream_format_t type;
    int version;
    void (*OutChar)(R_outpstream_t, int);
    void (*OutBytes)(R_outpstream_t, void *, int);
    SEXP (*OutPersistHookFunc)(SEXP, SEXP);
    SEXP OutPersistHookData;
};

typedef struct R_inpstream_st *R_inpstream_t;
struct R_inpstream_st {
    R_pstream_data_t data;
    R_pstream_format_t type;
    int (*InChar)(R_inpstream_t);
    void (*InBytes)(R_inpstream_t, void *, int);
    SEXP (*InPersistHookFunc)(SEXP, SEXP);
    SEXP InPersistHookData;
};

void R_InitInPStream(R_inpstream_t stream, R_pstream_data_t data,
		     R_pstream_format_t type,
		     int (*inchar)(R_inpstream_t),
		     void (*inbytes)(R_inpstream_t, void *, int),
		     SEXP (*phook)(SEXP, SEXP), SEXP pdata);
void R_InitOutPStream(R_outpstream_t stream, R_pstream_data_t data,
		      R_pstream_format_t type, int version,
		      void (*outchar)(R_outpstream_t, int),
		      void (*outbytes)(R_outpstream_t, void *, int),
		      SEXP (*phook)(SEXP, SEXP), SEXP pdata);

void R_InitFileInPStream(R_inpstream_t stream, FILE *fp,
			 R_pstream_format_t type,
			 SEXP (*phook)(SEXP, SEXP), SEXP pdata);
void R_InitFileOutPStream(R_outpstream_t stream, FILE *fp,
			  R_pstream_format_t type, int version,
			  SEXP (*phook)(SEXP, SEXP), SEXP pdata);

#ifdef NEED_CONNECTION_PSTREAMS
/* The connection interface is not yet available to packages.  To
   allow limited use of connection pointers this defines the opaque
   pointer type. */
#ifndef HAVE_RCONNECTION_TYPEDEF
typedef struct Rconn  *Rconnection;
#define HAVE_RCONNECTION_TYPEDEF
#endif
void R_InitConnOutPStream(R_outpstream_t stream, Rconnection con,
			  R_pstream_format_t type, int version,
			  SEXP (*phook)(SEXP, SEXP), SEXP pdata);
void R_InitConnInPStream(R_inpstream_t stream,  Rconnection con,
			 R_pstream_format_t type,
			 SEXP (*phook)(SEXP, SEXP), SEXP pdata);
#endif

void R_Serialize(SEXP s, R_outpstream_t ops);
SEXP R_Unserialize(R_inpstream_t ips);

/* slot management (in attrib.c) */
SEXP R_do_slot(SEXP obj, SEXP name);
SEXP R_do_slot_assign(SEXP obj, SEXP name, SEXP value);
int R_has_slot(SEXP obj, SEXP name);

/* class definition, new objects (objects.c) */
SEXP R_do_MAKE_CLASS(const char *what);
SEXP R_getClassDef  (const char *what);
SEXP R_do_new_object(SEXP class_def);
/* supporting  a C-level version of  is(., .) : */
int R_check_class_and_super(SEXP x, const char **valid, SEXP rho);
int R_check_class_etc      (SEXP x, const char **valid);

/* preserve objects across GCs */
void R_PreserveObject(SEXP);
void R_ReleaseObject(SEXP);

/* Shutdown actions */
void R_dot_Last(void);		/* in main.c */
void R_RunExitFinalizers(void);	/* in memory.c */

/* Replacements for popen and system */
#ifdef HAVE_POPEN
FILE *R_popen(const char *, const char *);
#endif
int R_system(const char *);

/* R_compute_identical:  C version of identical() function
   The third arg to R_compute_identical() consists of bitmapped flags for non-default options:
   currently all default to TRUE, so the flag is set for FALSE values:
   1 = !NUM_EQ
   2 = !SINGLE_NA
   4 = !ATTR_AS_SET
   8 = !IGNORE_BYTECODE
*/
Rboolean R_compute_identical(SEXP, SEXP, int);

#ifndef R_NO_REMAP
#define acopy_string		Rf_acopy_string
#define alloc3DArray            Rf_alloc3DArray
#define allocArray		Rf_allocArray
#define allocList		Rf_allocList
#define allocMatrix		Rf_allocMatrix
#define allocS4Object		Rf_allocS4Object
#define allocSExp		Rf_allocSExp
#define allocVector		Rf_allocVector
#define allocVector1RAW		Rf_allocVector1RAW
#define allocVector1LGL		Rf_allocVector1LGL
#define allocVector1INT		Rf_allocVector1INT
#define allocVector1REAL	Rf_allocVector1REAL
#define reallocVector		Rf_reallocVector
#define any_duplicated		Rf_any_duplicated
#define any_duplicated3		Rf_any_duplicated3
#define applyClosure		Rf_applyClosure
#define applyClosure_v		Rf_applyClosure_v
#define arraySubscript		Rf_arraySubscript
#define asChar			Rf_asChar
#define asCharacterFactor	Rf_asCharacterFactor
#define asComplex		Rf_asComplex
#define asInteger		Rf_asInteger
#define asLogical		Rf_asLogical
#define asReal			Rf_asReal
#define asS4			Rf_asS4
#define C99_from_R_complex	Rf_C99_from_R_complex
#define classgets		Rf_classgets
#define coerceVector		Rf_coerceVector
#define conformable		Rf_conformable
#define cons_with_tag		Rf_cons_with_tag
#define cons			Rf_cons
#define copy_1_string		Rf_copy_1_string
#define copy_2_strings		Rf_copy_2_strings
#define copy_3_strings		Rf_copy_3_strings
#define copyMatrix		Rf_copyMatrix
#define copyMostAttrib		Rf_copyMostAttrib
#define copyVector		Rf_copyVector
#define copy_elements		Rf_copy_elements
#define copy_elements_coerced	Rf_copy_elements_coerced
#define copy_elements_recycled	Rf_copy_elements_recycled
#define copy_transposed		Rf_copy_transposed
#define countContexts		Rf_countContexts
#define CreateTag		Rf_CreateTag
#define defineVar		Rf_defineVar
#define dimgets			Rf_dimgets
#define dimnamesgets		Rf_dimnamesgets
#define double_to_string	Rf_double_to_string
#define DropDims                Rf_DropDims
#define DropDimsNotSuppressed   Rf_DropDimsNotSuppressed
#define duplicate		Rf_duplicate
#define duplicated		Rf_duplicated
#define elt			Rf_elt
#define ep_match_strings	Rf_ep_match_strings
#define ep_match_exprs		Rf_ep_match_exprs
#define ep_match_string_expr	Rf_ep_match_string_expr
#define errorcall		Rf_errorcall
#define eval			Rf_eval
#define evalv			Rf_evalv
#define findFun			Rf_findFun
#define findFunMethod		Rf_findFunMethod
#define findVar			Rf_findVar
#define findVarPendingOK	Rf_findVarPendingOK
#define findVarInFrame		Rf_findVarInFrame
#define findVarInFramePendingOK	Rf_findVarInFramePendingOK
#define findVarInFrame3		Rf_findVarInFrame3
#define findVarInFrame3_nolast	Rf_findVarInFrame3_nolast
#define fixup_NaRm		Rf_fixup_NaRm
#define GetArrayDimnames	Rf_GetArrayDimnames
#define getAttrib		Rf_getAttrib
#define getAttrib00		Rf_getAttrib00
#define getNamesAttrib		Rf_getNamesAttrib
#define getCharCE		Rf_getCharCE
#define GetColNames		Rf_GetColNames
#define GetMatrixDimnames	Rf_GetMatrixDimnames
#define GetOption1		Rf_GetOption1
#define GetOptionDigits		Rf_GetOptionDigits
#define GetOptionWidth		Rf_GetOptionWidth
#define GetOption		Rf_GetOption
#define GetRowNames		Rf_GetRowNames
#define gsetVar			Rf_gsetVar
#define inherits		Rf_inherits
#define inherits_CHAR		Rf_inherits_CHAR
#define integer_to_string	Rf_integer_to_string
#define install			Rf_install
#define installChar		Rf_installChar
#define installed_already	Rf_installed_already
#define installed_already_with_hash	Rf_installed_already_with_hash
#define isArray			Rf_isArray
#define isBasicClass            Rf_isBasicClass
#define isComplex		Rf_isComplex
#define isEnvironment		Rf_isEnvironment
#define isExpression		Rf_isExpression
#define isFactor		Rf_isFactor
#define isFrame			Rf_isFrame
#define isFree			Rf_isFree
#define isFunction		Rf_isFunction
#define isInteger		Rf_isInteger
#define isLanguage		Rf_isLanguage
#define isList			Rf_isList
#define isLogical		Rf_isLogical
#define isSymbol		Rf_isSymbol
#define isMatrix		Rf_isMatrix
#define isNewList		Rf_isNewList
#define isNull			Rf_isNull
#define isNumeric		Rf_isNumeric
#define isNumber		Rf_isNumber
#define isObject		Rf_isObject
#define isOrdered		Rf_isOrdered
#define isPairList		Rf_isPairList
#define isPrimitive		Rf_isPrimitive
#define isRaw			Rf_isRaw
#define isReal			Rf_isReal
#define isS4			Rf_isS4
#define isString		Rf_isString
#define isTs			Rf_isTs
#define isUnordered		Rf_isUnordered
#define isUnsorted		Rf_isUnsorted
#define isUserBinop		Rf_isUserBinop
#define isValidString		Rf_isValidString
#define isValidStringF		Rf_isValidStringF
#define isVector		Rf_isVector
#define isVectorAtomic		Rf_isVectorAtomic
#define isVectorizable		Rf_isVectorizable
#define isVectorList		Rf_isVectorList
#define isVectorNonpointer	Rf_isVectorNonpointer
#define lang1			Rf_lang1
#define lang2			Rf_lang2
#define lang3			Rf_lang3
#define lang4			Rf_lang4
#define lang5			Rf_lang5
#define lang6			Rf_lang6
#define lastElt			Rf_lastElt
#define lcons			Rf_lcons
#define length(x)		Rf_length(x)
#define lengthgets		Rf_lengthgets
#define list1			Rf_list1
#define list2			Rf_list2
#define list3			Rf_list3
#define list4			Rf_list4
#define list5			Rf_list5
#define listAppend		Rf_listAppend
#define match			Rf_match
#define matchE			Rf_matchE
#define mkChar			Rf_mkChar
#define mkCharCE		Rf_mkCharCE
#define mkCharLen		Rf_mkCharLen
#define mkCharLenCE		Rf_mkCharLenCE
#define mkNamed			Rf_mkNamed
#define mkString		Rf_mkString
#define namesgets		Rf_namesgets
#define ncols			Rf_ncols
#define nlevels			Rf_nlevels
#define nrows			Rf_nrows
#define nthcdr			Rf_nthcdr
#define PairToVectorList	Rf_PairToVectorList
#define pmatch			Rf_pmatch
#define psmatch			Rf_psmatch
#define PrintValue		Rf_PrintValue
#define protect			Rf_protect
#define R_from_C99_complex	Rf_R_from_C99_complex
#define reEnc			Rf_reEnc
#define rep_one_string_element	Rf_rep_one_string_element
#define rep_string_elements	Rf_rep_string_elements
#define rownamesgets		Rf_rownamesgets
#define S3Class                 Rf_S3Class
#define ScalarComplex		Rf_ScalarComplex
#define ScalarComplexMaybeConst	Rf_ScalarComplexMaybeConst
#define ScalarInteger		Rf_ScalarInteger
#define ScalarIntegerMaybeConst	Rf_ScalarIntegerMaybeConst
#define ScalarLogical		Rf_ScalarLogical
#define ScalarLogicalMaybeConst	Rf_ScalarLogicalMaybeConst
#define ScalarReal		Rf_ScalarReal
#define ScalarRealMaybeConst	Rf_ScalarRealMaybeConst
#define ScalarString		Rf_ScalarString
#define ScalarStringMaybeConst	Rf_ScalarStringMaybeConst
#define ScalarRaw		Rf_ScalarRaw
#define ScalarRawMaybeConst		Rf_ScalarRawMaybeConst
#define setAttrib		Rf_setAttrib
#define setSVector		Rf_setSVector
#define set_symbits_in_env	Rf_set_symbits_in_env
#define setVar			Rf_setVar
#define set_var_in_frame	Rf_set_var_in_frame
#define set_var_nonlocal	Rf_set_var_nonlocal
#define MaybeConstList1		Rf_MaybeConstList1
#define str2type		Rf_str2type
#define StringBlank		Rf_StringBlank
#define substitute		Rf_substitute
#define tag_index		Rf_tag_index
#define translateChar		Rf_translateChar
#define translateChar0		Rf_translateChar0
#define translateCharUTF8      	Rf_translateCharUTF8
#define type2char		Rf_type2char
#define type2rstr               Rf_type2rstr
#define type2str                Rf_type2str
#define type2str_nowarn         Rf_type2str_nowarn
#define unprotect		Rf_unprotect
#define unprotect_ptr		Rf_unprotect_ptr
#define VectorToPairList	Rf_VectorToPairList
#define warningcall		Rf_warningcall
#define warningcall_immediate	Rf_warningcall_immediate
#define with_changed_nth	Rf_with_changed_nth
#define with_no_nth		Rf_with_no_nth
#define copy_pairlist		Rf_copy_pairlist
#define with_pairlist_appended	Rf_with_pairlist_appended
#define xlength(x)		Rf_xlength(x)
#define xlengthgets		Rf_xlengthgets

#endif


/* Sets of SEXTYPES.  One tests using code like 

       if ((SET >> type) & 1) ...  

   Used in inlined functions, and elsewhere. */

#define CONS_TYPES ( \
  (1<<LISTSXP) + (1<<LANGSXP) + (1<<DOTSXP) \
)

#define PAIRLIST_TYPES ( \
  (1<<NILSXP) + (1<<LISTSXP) + (1<<LANGSXP) \
)

#define ATOMIC_VECTOR_TYPES ( \
  (1<<LGLSXP) + (1<<INTSXP) + (1<<REALSXP) + \
  (1<<RAWSXP) + (1<<STRSXP) + (1<<CPLXSXP) \
)

#define NONATOMIC_VECTOR_TYPES ( \
  (1<<VECSXP) + (1<<EXPRSXP) \
)

#define NONPOINTER_VECTOR_TYPES ( \
  (1<<LGLSXP) + (1<<INTSXP) + (1<<REALSXP) + \
  (1<<RAWSXP) + (1<<CPLXSXP) \
)

#define VECTOR_TYPES ( \
  ATOMIC_VECTOR_TYPES + NONATOMIC_VECTOR_TYPES \
)

#define VECTOR_OR_LIST_TYPES ( \
  VECTOR_TYPES + (1<<LISTSXP) \
)

#define VECTOR_OR_CHAR_TYPES ( \
  VECTOR_TYPES + (1<<CHARSXP) \
)

#define PRIMITIVE_FUN_TYPES ( \
  (1<<BUILTINSXP) + (1<<SPECIALSXP) \
)

#define FUNCTION_TYPES ( \
  PRIMITIVE_FUN_TYPES + (1<<CLOSXP) \
)

#define NUMERIC_TYPES ( \
  (1<<LGLSXP) + (1<<INTSXP) + (1<<REALSXP) \
)

#define NUMBER_TYPES ( \
  NUMERIC_TYPES + (1<<CPLXSXP) \
)

/* Types for which existence of an attribute is recorded with HAS_ATTRIB */
#define HAS_ATTRIB_TYPES ( \
  VECTOR_TYPES + (1<<LISTSXP) + (1<<PROMSXP) \
)

/* Bit flags that say whether each SEXP type evaluates to itself.  Used via
   SELF_EVAL(t), which says whether something of type t evaluates to itself. 
   Relies on the type field being 5 bits, so that the shifts below will not
   exceed the capacity of a 32-bit word.  (Also assumes, of course, that these
   shifts and adds will be done at compile time.) */

#define SELF_EVAL_TYPES ( \
  (1<<NILSXP) + \
  (1<<LISTSXP) + \
  (1<<LGLSXP) + \
  (1<<INTSXP) + \
  (1<<REALSXP) + \
  (1<<STRSXP) + \
  (1<<CPLXSXP) + \
  (1<<RAWSXP) + \
  (1<<S4SXP) + \
  (1<<SPECIALSXP) + \
  (1<<BUILTINSXP) + \
  (1<<ENVSXP) + \
  (1<<CLOSXP) + \
  (1<<VECSXP) + \
  (1<<EXTPTRSXP) + \
  (1<<WEAKREFSXP) + \
  (1<<EXPRSXP) )

#define SELF_EVAL(t) ((SELF_EVAL_TYPES>>(t))&1)


/* Sizes of vector type elements. */

#ifndef __MAIN__
extern
#endif
char R_vector_element_size[32]
#ifdef __MAIN__
= {
   /* NILSXP */      0,
   /* SYMSXP */      0,
   /* LISTSXP */     0,
   /* CLOSXP */      0,
   /* ENVSXP */      0,
   /* PROMSXP */     0,
   /* LANGSXP */     0,
   /* SPECIALSXP */  0,
   /* BUILTINSXP */  0,
   /* CHARSXP */     sizeof(char),
   /* LGLSXP */      sizeof(int),
   /* unused */      0,  
   /* unused */      0,  
   /* INTSXP */      sizeof(int),
   /* REALSXP */     sizeof(double),
   /* CPLXSXP */     sizeof(Rcomplex),
   /* STRSXP */      sizeof(SEXP),
   /* DOTSXP */      0,
   /* unused */      0,  
   /* VECSXP */      sizeof(SEXP),
   /* EXPRSXP */     sizeof(SEXP),
   /* BCODESXP */    0,
   /* EXTPTRSXP */   0,
   /* WEAKREFSXP */  0,
   /* RAWSXP */      sizeof(Rbyte),
   /* S4SXP */       0,
   /* unused */      0,  
   /* unused */      0,  
   /* unused */      0,  
   /* unused */      0,  
   /* unused */      0,  
   /* unused */      0  
}
#endif
;


#if defined(CALLED_FROM_DEFN_H) && !defined(__MAIN__)
#include "Rinlinedfuns.h"
#else
/* need remapped names here for use with R_NO_REMAP */

/*
   These are the inlinable functions that are provided in Rinlinedfuns.h
   It is *essential* that these do not appear in any other header file,
   with or without the Rf_ prefix.
*/
Rboolean Rf_conformable(SEXP, SEXP);
SEXP	 Rf_elt(SEXP, int);
Rboolean Rf_inherits(SEXP, const char *);
Rboolean Rf_inherits_CHAR(SEXP, SEXP);
Rboolean Rf_isArray(SEXP);
Rboolean Rf_isFactor(SEXP);
Rboolean Rf_isFrame(SEXP);
Rboolean Rf_isFunction(SEXP);
Rboolean Rf_isInteger(SEXP);
Rboolean Rf_isLanguage(SEXP);
Rboolean Rf_isList(SEXP);
Rboolean Rf_isMatrix(SEXP);
Rboolean Rf_isNewList(SEXP);
Rboolean Rf_isNumber(SEXP);
Rboolean Rf_isNumeric(SEXP);
Rboolean Rf_isPairList(SEXP);
Rboolean Rf_isPrimitive(SEXP);
Rboolean Rf_isTs(SEXP);
Rboolean Rf_isUserBinop(SEXP);
Rboolean Rf_isValidString(SEXP);
Rboolean Rf_isValidStringF(SEXP);
Rboolean Rf_isVector(SEXP);
Rboolean Rf_isVectorAtomic(SEXP);
Rboolean Rf_isVectorList(SEXP);
Rboolean Rf_isVectorNonpointer(SEXP);
Rboolean Rf_isVectorizable(SEXP);
SEXP	 Rf_lang1(SEXP);
SEXP	 Rf_lang2(SEXP, SEXP);
SEXP	 Rf_lang3(SEXP, SEXP, SEXP);
SEXP	 Rf_lang4(SEXP, SEXP, SEXP, SEXP);
SEXP	 Rf_lang5(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP	 Rf_lang6(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP	 Rf_lastElt(SEXP);
SEXP	 Rf_lcons(SEXP, SEXP);
SEXP	 Rf_list1(SEXP);
SEXP	 Rf_list2(SEXP, SEXP);
SEXP	 Rf_list3(SEXP, SEXP, SEXP);
SEXP	 Rf_list4(SEXP, SEXP, SEXP, SEXP);
SEXP	 Rf_list5(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP	 Rf_listAppend(SEXP, SEXP);
SEXP	 Rf_mkNamed(SEXPTYPE, const char **);
SEXP	 Rf_mkString(const char *);
int	 Rf_nlevels(SEXP);
SEXP	 Rf_ScalarRaw(Rbyte);
SEXP	 Rf_ScalarLogical(int);
SEXP	 Rf_ScalarInteger(int);
SEXP	 Rf_ScalarReal(double);
SEXP	 Rf_ScalarComplex(Rcomplex);
SEXP	 Rf_ScalarString(SEXP);
SEXP	 Rf_ScalarLogicalMaybeConst(int);
const char * Rf_translateChar(SEXP);

#ifdef complex  /* In C99, should be defined if complex.h included */
double complex Rf_C99_from_R_complex(Rcomplex *);
void Rf_R_from_C99_complex(Rcomplex *, double complex);
#endif

#endif

#ifdef USE_RINTERNALS

/* Test macros with function versions above */
#undef isNull
#define isNull(s)	((s) == R_NilValue)
#undef isRaw
#define isRaw(s)	(TYPEOF(s) == RAWSXP)
#undef isSymbol
#define isSymbol(s)	(TYPEOF(s) == SYMSXP)
#undef isLogical
#define isLogical(s)	(TYPEOF(s) == LGLSXP)
#undef isReal
#define isReal(s)	(TYPEOF(s) == REALSXP)
#undef isComplex
#define isComplex(s)	(TYPEOF(s) == CPLXSXP)
#undef isExpression
#define isExpression(s) (TYPEOF(s) == EXPRSXP)
#undef isEnvironment
#define isEnvironment(s) (TYPEOF(s) == ENVSXP)
#undef isString
#define isString(s)	(TYPEOF(s) == STRSXP)
#undef isObject
#define isObject(s)	(OBJECT(s) != 0)

#endif


#ifdef __cplusplus
}
#endif
 
#endif /* R_INTERNALS_H_ */
