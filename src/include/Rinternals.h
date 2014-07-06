/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2014 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2010   The R Development Core Team.
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

/* Variables that need to be declared as firstprivate in omp parallel
   constructs, since they're used in macros such as NA_REAL. */

#define R_OMP_FIRSTPRIVATE_VARS R_NaReal,R_NaInt,R_NaN_cast_to_int

typedef unsigned char Rbyte;

/* type for length of vectors etc */
typedef int R_len_t;  /* might later be unsigned or long */
#define R_LEN_T_MAX INT_MAX

/* Fundamental Data Types:  These are largely Lisp
 * influenced structures, with the exception of LGLSXP,
 * INTSXP, REALSXP, CPLXSXP and STRSXP which are the
 * element types for S-like data objects.

 * Note that the gap of 11 and 12 below is because of
 * the withdrawal of native "factor" and "ordered" types.
 *
 *			--> TypeTable[] in ../main/util.c for  typeof()
 */

/*  These exact numeric values are seldom used, but they are, e.g., in
 *  ../main/subassign.c
*/

typedef unsigned int SEXPTYPE;  /* used in serialize.c for things that aren't
                                   actual types */

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
#define NEWSXP      30    /* fresh node creaed in new page */
#define FREESXP     31    /* node released by GC */

#define FUNSXP      99    /* Closure or Builtin or Special */


/* Flags.  Order may be fiddled to try to improve performance.  Total
   size is 64 bits = 8 bytes. */

struct sxpinfo_struct {
    /* Type and namedcnt in first byte */
    unsigned int nmcnt : 3;   /* count of "names" referring to object */
    unsigned int type : 5;    /* ==> (FUNSXP == 99) %% 2^5 == 3 == CLOSXP
                               * -> warning: `type' is narrower than values
                               *              of its type
                               * when SEXPTYPE was an enum */
    /* Garbage collector stuff - keep in one byte to maybe speed up access */
    unsigned int gccls : 3;   /* node class for garbage collector */
    unsigned int gcgen : 1;   /* old generation number - may be best first */
    unsigned int mark : 1;    /* marks object as in use in garbage collector */
    /*unsigned int gcoton:1; */ /* 1 if already in old-to-new list - DISABLED */
    /* Object flag */
    unsigned int obj : 1;     /* set if this is an S3 or S4 object */
    /* Flags to synchronize with helper threads */
    unsigned int in_use: 1;   /* whether contents may be in use by a helper */
    unsigned int being_computed : 1;  /* whether helper may be computing this */
    /* "general purpose" field, used for miscellaneous purposes */
    unsigned int gp : 16;     /* The "general purpose" field */
    union {
      struct {                /* field below is for vectors only */
        R_len_t truelength;      /* for old stuff - may someday be defunct... */
      } vec;
      struct {                /* fields below are for non-vectors only */
        /* Debugging */
        unsigned int debug : 1;  /* function/environment is being debugged */
        unsigned int rstep : 1;  /* function is to be debugged, but only once */
        unsigned int trace : 1;  /* function is being traced */
        /* Symbol binding */
        unsigned int basec : 1;       /* sym has base binding in global cache */
        unsigned int spec_sym : 1;    /* this is a "special" symbol */
        unsigned int no_spec_sym : 1; /* environment has no special symbols */
        unsigned int unused : 2; /* not yet used */
        /* Primitive operations */
        unsigned char var1, var2;/* variants for evals of fast primitive args */
        unsigned char pending_ok;/* whether args can have computation pending */
      } nonvec;                  /*  - only one bit, but maybe faster as byte */
    } u;
};

/* Macros to access the vector or non-vector part of the sxpinfo structure,
   checking validity if CHECK_VEC_NONVEC is defined (it should not normally
   be defined because it significantly degrades performance). */

#ifdef CHECK_VEC_NONVEC
extern struct sxpinfo_struct *Rf_verify_vec (void *);
extern struct sxpinfo_struct *Rf_verify_nonvec (void *);
#define VEC_SXPINFO(x)    (Rf_verify_vec((void*)x)->u.vec)
#define NONVEC_SXPINFO(x) (Rf_verify_nonvec((void*)x)->u.nonvec)
#else
#define VEC_SXPINFO(x)    ((x)->sxpinfo.u.vec)
#define NONVEC_SXPINFO(x) ((x)->sxpinfo.u.nonvec)
#endif

struct vecsxp_struct {
    R_len_t length;
};

struct primsxp_struct {    /* table offset of this and other info is in gp  */
    /* The two function pointers below can't use SEXP, since not defined yet*/
    void *(*primsxp_cfun)();   /* c-code address for prim fun, from table   */
    void *(*primsxp_fast_cfun)(); /* c-code addr for fast interface, or NULL*/
    short primsxp_code;        /* operation code, from table                */
    signed char primsxp_arity; /* function arity (-1 for any), from table   */
    unsigned int primsxp_print:2;   /* print/invisible indicator, from table*/
    unsigned int primsxp_variant:1; /* pass variant to cfun, from table     */
    unsigned int primsxp_internal:1;/* call with .Internal flag, from table */
    unsigned int primsxp_foreign:1; /* primitive to call C/Fortran function */
    /* bits below only for when fast_cfun!=NULL (last 2 only when arity==2) */
    unsigned int primsxp_dsptch1:1; /* might dispatch on 1st argument       */
    unsigned int primsxp_dsptch2:1; /* might dispatch on 2nd argument       */
    unsigned int primsxp_uni_too:1; /* can be unary as well as binary       */
};

struct symsxp_struct {
    struct SEXPREC *pname;
    struct SEXPREC *value;
    struct SEXPREC *internal;
};

struct listsxp_struct {
    struct SEXPREC *carval;
    struct SEXPREC *cdrval;
    struct SEXPREC *tagval;
};

struct envsxp_struct {
    struct SEXPREC *frame;
    struct SEXPREC *enclos;
    struct SEXPREC *hashtab;
};

struct closxp_struct {
    struct SEXPREC *formals;
    struct SEXPREC *body;
    struct SEXPREC *env;
};

struct promsxp_struct {
    struct SEXPREC *value;
    struct SEXPREC *expr;
    struct SEXPREC *env;
};

/* Every node must have a set of sxpinfo flags and an attribute field,
   plus fields used to maintain the collector's linked list structures. */

#define SEXPREC_HEADER \
    struct sxpinfo_struct sxpinfo; \
    struct SEXPREC *attrib; \
    struct SEXPREC *gengc_next_node, *gengc_prev_node

/* The standard node structure consists of a header followed by the
   node data.  The size varies with the size of a pointer, as follows
   (assuming R_len_t is no bigger than a pointer):
   
       4-byte pointers:  32 bytes
       8-byte pointers:  56 bytes

   Note, however, that the actual amount allocated could be greater, as
   determined by the definitions of the node class sizes in memory.c.

   Standard nodes may be used to hold small vectors as well as cons cells
   and other fixed-size objects.
*/

typedef struct SEXPREC {
    SEXPREC_HEADER;
    union {
	struct primsxp_struct primsxp;
	struct symsxp_struct symsxp;
	struct listsxp_struct listsxp;
	struct envsxp_struct envsxp;
	struct closxp_struct closxp;
	struct promsxp_struct promsxp;
    } u;
} SEXPREC, *SEXP;

/* Reduced version of SEXPREC used as a header in vector nodes.  The 
   layout MUST be kept consistent with the SEXPREC definition.  The size
   varies with the size of a pointer, the size of R_len_t, and whether
   alignment to a multiple of 8 bytes is done, as follows:
   
       4-byte pointers, 4-byte R_len_t:  24 bytes
       8-byte pointers, 4-byte R_len_t:  36 bytes (40 bytes if aligned)
       8-byte pointers, 8-byte R_len_t:  40 bytes
*/

typedef struct VECTOR_SEXPREC {
    SEXPREC_HEADER;
    struct vecsxp_struct vecsxp;
} VECTOR_SEXPREC, *VECSEXP;

typedef union { VECTOR_SEXPREC s; double align; } SEXPREC_ALIGN;

/* Version of VECTOR_SEXPREC used for defining constants in const_obj.c */

typedef const struct VECTOR_SEXPREC_CONST {
    SEXPREC_HEADER;
    struct vecsxp_struct vecsxp;
    union { double d; int w[2]; int i; char c; } data;
} VECTOR_SEXPREC_CONST;


#ifdef USE_RINTERNALS
/* This is intended for use only within R itself.
 * It defines internal structures that are otherwise only accessible
 * via SEXP, and macros to replace many (but not all) of accessor functions
 * (which are always defined).
 *
 * Making R_NilValue a constant has necessitated exposing more of the
 * internals (some of what is above used to be after this ifdef).
 */


/* Macros for accessing and changing NAMEDCNT. */

#define MAX_NAMEDCNT 7	/* Must be either 2 or a power of 2 minus 1, limited 
                           by the number of bits in the nmcnt sxpinfo field */

/* Below is the obvious way of implementing these macros. */

/* When a variable may be being used by a helper, it is treated as temporarily
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

#ifdef HELPERS_DISABLED
#define helpers_wait_until_not_in_use(v) 0
#else
extern void helpers_wait_until_not_in_use(SEXP);
#endif

/* Below is for packages that (naughtily) define USE_RINTERNALS, but don't
   include Defn.h, where helpers_is_in_use is defined. */

#ifndef helpers_is_in_use  
#define helpers_is_in_use(x)               ((x)->sxpinfo.in_use)
#endif

#define NAMEDCNT(x) \
( helpers_is_in_use(x) && (x)->sxpinfo.nmcnt != MAX_NAMEDCNT \
     ? (helpers_wait_until_not_in_use(x), (x)->sxpinfo.nmcnt) \
     : (x)->sxpinfo.nmcnt )

#define NAMEDCNT_EQ_0(x) \
( (x)->sxpinfo.nmcnt != 0 ? 0 : !helpers_is_in_use(x) ? 1 \
    : (helpers_wait_until_not_in_use(x), 1) )

#define NAMEDCNT_GT_0(x) \
( (x)->sxpinfo.nmcnt != 0 ? 1 : !helpers_is_in_use(x) ? 0 \
    : (helpers_wait_until_not_in_use(x), 0) )

#define NAMEDCNT_GT_1(x) \
( (x)->sxpinfo.nmcnt > 1 ? 1 : !helpers_is_in_use(x) ? 0 \
    : (helpers_wait_until_not_in_use(x), 0) )

#define SET_NAMEDCNT_0(x)    ((x)->sxpinfo.nmcnt = 0)
#define SET_NAMEDCNT_1(x)    ((x)->sxpinfo.nmcnt = 1)

/* Be careful not to write to an object with NAMEDCNT equal to MAX_NAMEDCNT, 
   even if the new value is also MAX_NAMEDCNT, since it might be a constant 
   object in a read-only memory area. */

#define SET_NAMEDCNT(x,v) do { \
    SEXP _p_ = (x); int _v_ = v; \
    if (_p_->sxpinfo.nmcnt != _v_) \
        _p_->sxpinfo.nmcnt = _v_; \
  } while (0)

#define SET_NAMEDCNT_MAX(x) do { \
    SEXP _p_ = (x); \
    if (_p_->sxpinfo.nmcnt < MAX_NAMEDCNT) \
        _p_->sxpinfo.nmcnt = MAX_NAMEDCNT; \
  } while (0)

#define INC_NAMEDCNT(x) do { \
    SEXP _p_ = (x); \
    if (_p_->sxpinfo.nmcnt < MAX_NAMEDCNT) \
        _p_->sxpinfo.nmcnt += 1; \
  } while (0)

#define INC_NAMEDCNT_0_AS_1(x) do { \
    SEXP _p_ = (x); \
    if (_p_->sxpinfo.nmcnt == 0) \
        _p_->sxpinfo.nmcnt = 2; \
    else if (_p_->sxpinfo.nmcnt < MAX_NAMEDCNT) \
        _p_->sxpinfo.nmcnt += 1; \
  } while (0)

#define DEC_NAMEDCNT(x) do { \
    SEXP _p_ = (x); \
    if (_p_->sxpinfo.nmcnt < MAX_NAMEDCNT && _p_->sxpinfo.nmcnt != 0) \
        _p_->sxpinfo.nmcnt -= 1; \
  } while (0)

/* Changes for an optimized implemention of the above macros.  The optimization
   assumes MAX_NAMEDCNT is a power of 2 minus 1.  There's not much to gain with
   gcc 4.6.3 on Intel - gcc is sometimes too smart to need help, and other times
   too dumb to take advantage of hints. */

#if MAX_NAMEDCNT!=2 && 1     /* Change 1 to 0 to disable these optimizations */

#undef NAMEDCNT_GT_1
#define NAMEDCNT_GT_1(x) \
( ((x)->sxpinfo.nmcnt & (MAX_NAMEDCNT-1)) != 0 ? 1 \
    : !helpers_is_in_use(x) ? 0 \
    : (helpers_wait_until_not_in_use(x), 0) )

#endif

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
  ( (x)->sxpinfo.nmcnt > 1 && (x)->sxpinfo.nmcnt < MAX_NAMEDCNT \
      ? (((x)->sxpinfo.nmcnt = MAX_NAMEDCNT), 2) \
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

#define SET_VECTOR_ELEMENT_TO_VALUE(_dst_,_i_,_val_) do { \
    SEXP _v_ = _val_; \
    if (!DUPVE || NAMEDCNT_EQ_0(_v_)) { \
        SET_VECTOR_ELT (_dst_, _i_, _v_); \
        if (NAMEDCNT_GT_0(_v_)) \
            INC_NAMEDCNT(_v_); \
    } \
    else \
        SET_VECTOR_ELT (_dst_, _i_, duplicate(_v_)); \
} while (0)

#define SET_VECTOR_ELEMENT_FROM_VECTOR(_dst_,_i_,_src_,_j_) do { \
    SEXP _s_ = _src_; \
    SEXP _v_ = VECTOR_ELT(_s_,_j_); \
    if (!DUPVE || NAMEDCNT_EQ_0(_s_)) { \
        SET_VECTOR_ELT (_dst_, _i_, _v_); \
        if (NAMEDCNT_GT_0(_s_)) \
            INC_NAMEDCNT_0_AS_1(_v_); \
    } \
    else \
        SET_VECTOR_ELT (_dst_, _i_, duplicate(_v_)); \
} while (0)


/* General Cons Cell Attributes */
#define ATTRIB(x)	((x)->attrib)
#define OBJECT(x)	((x)->sxpinfo.obj)
#define MARK(x)		((x)->sxpinfo.mark)
#define TYPEOF(x)	((x)->sxpinfo.type)
#define RTRACE(x)	(NONVEC_SXPINFO(x).trace)
#define LEVELS(x)	((x)->sxpinfo.gp)
  /* For SET_OBJECT and SET_TYPE, don't set if new value is the current value,
     to avoid crashing on an innocuous write to a constant that may be stored
     in read-only memory. */
#define SET_OBJECT(x,v) do { \
    SEXP _x_ = (x); int _v_ = (v); \
    if (_x_->sxpinfo.obj!=_v_) _x_->sxpinfo.obj = _v_; \
  } while (0)
#define SET_TYPEOF(x,v) do { \
    SEXP _x_ = (x); int _v_ = (v); \
    if (_x_->sxpinfo.type!=_v_) _x_->sxpinfo.type = _v_; \
  } while (0)
#define SET_RTRACE(x,v)	(NONVEC_SXPINFO(x).trace=(v))
#define SETLEVELS(x,v)	((x)->sxpinfo.gp=(v))

/* The TRUELENGTH is seldom used, and usually has no connection with length. */
#define TRUELENGTH(x)	(VEC_SXPINFO(x).truelength)
#define SET_TRUELENGTH(x,v)  (VEC_SXPINFO(x).truelength = (v))

/* S4 object bit, set by R_do_new_object for all new() calls.  Avoid writes
   of what's already there, in case object is a constant in read-only memory. */
#define S4_OBJECT_MASK (1<<4)
#define IS_S4_OBJECT(x) (((x)->sxpinfo.gp & S4_OBJECT_MASK) != 0)
#define SET_S4_OBJECT(x) do { \
    SEXP _x_ = (x); \
    if (!IS_S4_OBJECT(_x_)) _x_->sxpinfo.gp |= S4_OBJECT_MASK; \
  } while (0)
#define UNSET_S4_OBJECT(x) do { \
    SEXP _x_ = (x); \
    if (IS_S4_OBJECT(_x_)) _x_->sxpinfo.gp &= ~S4_OBJECT_MASK; \
  } while (0)

/* Vector Access Macros */
#define LENGTH(x)	(((VECSEXP) (x))->vecsxp.length)
#define SETLENGTH(x,v)	((((VECSEXP) (x))->vecsxp.length)=(v)) /* DEPRECATED */

/* Under the generational allocator the data for vector nodes comes
   immediately after the node structure, so the data address is a
   known offset from the node SEXP. */
#define DATAPTR(x)	(((SEXPREC_ALIGN *) (x)) + 1)
#define CHAR(x)		((const char *) DATAPTR(x))
#define LOGICAL(x)	((int *) DATAPTR(x))
#define INTEGER(x)	((int *) DATAPTR(x))
#define RAW(x)		((Rbyte *) DATAPTR(x))
#define COMPLEX(x)	((Rcomplex *) DATAPTR(x))
#define REAL(x)		((double *) DATAPTR(x))
#define STRING_ELT(x,i)	((SEXP *) DATAPTR(x))[i]
#define VECTOR_ELT(x,i)	((SEXP *) DATAPTR(x))[i]
#define STRING_PTR(x)	((SEXP *) DATAPTR(x))
#define VECTOR_PTR(x)	((SEXP *) DATAPTR(x))

/* List Access Macros */
/* These also work for ... objects */
#define LISTVAL(x)	((x)->u.listsxp)
#define TAG(e)		((e)->u.listsxp.tagval)
#define CAR(e)		((e)->u.listsxp.carval)
#define CDR(e)		((e)->u.listsxp.cdrval)
#define CAAR(e)		CAR(CAR(e))
#define CDAR(e)		CDR(CAR(e))
#define CADR(e)		CAR(CDR(e))
#define CDDR(e)		CDR(CDR(e))
#define CADDR(e)	CAR(CDR(CDR(e)))
#define CADDDR(e)	CAR(CDR(CDR(CDR(e))))
#define CAD4R(e)	CAR(CDR(CDR(CDR(CDR(e)))))
#define MISSING_MASK	15 /* reserve 4 bits--only 2 uses now */
#define MISSING(x)	((x)->sxpinfo.gp & MISSING_MASK)/* for closure calls */
#define SET_MISSING(x,v) do { \
  SEXP __x__ = (x); \
  int __v__ = (v); \
  int __other_flags__ = __x__->sxpinfo.gp & ~MISSING_MASK; \
  __x__->sxpinfo.gp = __other_flags__ | __v__; \
} while (0)

/* Closure Access Macros */
#define FORMALS(x)	((x)->u.closxp.formals)
#define BODY(x)		((x)->u.closxp.body)
#define CLOENV(x)	((x)->u.closxp.env)
#define RDEBUG(x)	(NONVEC_SXPINFO(x).debug)
#define SET_RDEBUG(x,v)	(NONVEC_SXPINFO(x).debug=(v))
#define RSTEP(x)	(NONVEC_SXPINFO(x).rstep)
#define SET_RSTEP(x,v)	(NONVEC_SXPINFO(x).rstep=(v))

/* Symbol Access Macros */
#define PRINTNAME(x)	((x)->u.symsxp.pname)
#define SYMVALUE(x)	((x)->u.symsxp.value)
#define INTERNAL(x)	((x)->u.symsxp.internal)
#define DDVAL_MASK	1
#define DDVAL(x)	((x)->sxpinfo.gp & DDVAL_MASK) /* for ..1, ..2 etc */
#define SET_DDVAL_BIT(x) (((x)->sxpinfo.gp) |= DDVAL_MASK)
#define UNSET_DDVAL_BIT(x) (((x)->sxpinfo.gp) &= ~DDVAL_MASK)
#define SET_DDVAL(x,v) ((v) ? SET_DDVAL_BIT(x) : UNSET_DDVAL_BIT(x)) /* for ..1, ..2 etc */
#define BASE_CACHE(x)  (NONVEC_SXPINFO(x).basec) /* 1 = base binding in global cache*/
#define SET_BASE_CACHE(x,v) (NONVEC_SXPINFO(x).basec = (v))

/* Flag indicating whether a symbol is special. */
#define SPEC_SYM(x)	(NONVEC_SXPINFO(x).spec_sym)
#define SET_SPEC_SYM(x,v) (NONVEC_SXPINFO(x).spec_sym = (v)) 

/* Environment Access Macros */
#define FRAME(x)	((x)->u.envsxp.frame)
#define ENCLOS(x)	((x)->u.envsxp.enclos)
#define HASHTAB(x)	((x)->u.envsxp.hashtab)
#define ENVFLAGS(x)	((x)->sxpinfo.gp)	/* for environments */
#define SET_ENVFLAGS(x,v)	(((x)->sxpinfo.gp)=(v))
#define NO_SPEC_SYM(x)  (NONVEC_SXPINFO(x).no_spec_sym) /* 1 = env has no special symbol */
#define SET_NO_SPEC_SYM(x,v) (NONVEC_SXPINFO(x).no_spec_sym = (v))

#else /* not USE_RINTERNALS */

typedef struct SEXPREC *SEXP;

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

/* General Cons Cell Attributes */
SEXP (ATTRIB)(SEXP x);
int  (OBJECT)(SEXP x);
int  (MARK)(SEXP x);
int  (TYPEOF)(SEXP x);
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
int  (LENGTH)(SEXP x);
int  (TRUELENGTH)(SEXP x);
void (SETLENGTH)(SEXP x, int v);
void (SET_TRUELENGTH)(SEXP x, int v);
int  (LEVELS)(SEXP x);
int  (SETLEVELS)(SEXP x, int v);

int  *(LOGICAL)(SEXP x);
int  *(INTEGER)(SEXP x);
Rbyte *(RAW)(SEXP x);
double *(REAL)(SEXP x);
Rcomplex *(COMPLEX)(SEXP x);
SEXP (STRING_ELT)(SEXP x, int i);
SEXP (VECTOR_ELT)(SEXP x, int i);
void SET_STRING_ELT(SEXP x, int i, SEXP v);
void copy_string_elements(SEXP x, int i, SEXP v, int j, int n);
SEXP SET_VECTOR_ELT(SEXP x, int i, SEXP v);
void copy_vector_elements(SEXP x, int i, SEXP v, int j, int n);
SEXP *(STRING_PTR)(SEXP x);
SEXP *(VECTOR_PTR)(SEXP x);

/* List Access Functions */
/* These also work for ... objects */
#define CONS(a, b)	cons((a), (b))		/* data lists */
#define LCONS(a, b)	lcons((a), (b))		/* language lists */
SEXP (TAG)(SEXP e);
SEXP (CAR)(SEXP e);
SEXP (CDR)(SEXP e);
SEXP (CAAR)(SEXP e);
SEXP (CDAR)(SEXP e);
SEXP (CADR)(SEXP e);
SEXP (CDDR)(SEXP e);
SEXP (CADDR)(SEXP e);
SEXP (CADDDR)(SEXP e);
SEXP (CAD4R)(SEXP e);
int  (MISSING)(SEXP x);
void (SET_MISSING)(SEXP x, int v);
void SET_TAG(SEXP x, SEXP y);
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

/* Hashing Functions */
/* There are macro versions in Defn.h */
int  (HASHASH)(SEXP x);
int  (HASHVALUE)(SEXP x);
void (SET_HASHASH)(SEXP x, int v);
void (SET_HASHVALUE)(SEXP x, int v);


/* External pointer access macros */
#define EXTPTR_PTR(x)	CAR(x)
#define EXTPTR_PROT(x)	CDR(x)
#define EXTPTR_TAG(x)	TAG(x)

/* Bytecode access macros */
#define BCODE_CODE(x)	CAR(x)
#define BCODE_CONSTS(x) CDR(x)
#define BCODE_EXPR(x)	TAG(x)
#define isByteCode(x)	(TYPEOF(x)==BCODESXP)

/* Pointer Protection and Unprotection */
#define PROTECT(s)		Rf_protect(s)
#define PROTECT2(s1,s2)		Rf_protect2(s1,s2) /* BEWARE! All args eval'd */
#define PROTECT3(s1,s2,s3)	Rf_protect3(s1,s2,s3) /* before any protected */
#define UNPROTECT(n)		Rf_unprotect(n)
#define UNPROTECT_PTR(s)	Rf_unprotect_ptr(s)

/* We sometimes need to coerce a protected value and place the new
   coerced value under protection.  For these cases PROTECT_WITH_INDEX
   saves an index of the protection location that can be used to
   replace the protected value using REPROTECT. */
typedef int PROTECT_INDEX;
#define PROTECT_WITH_INDEX(x,i) R_ProtectWithIndex(x,i)
#define REPROTECT(x,i) R_Reprotect(x,i)

/* Evaluation Environment */
LibExtern SEXP	R_GlobalEnv;	    /* The "global" environment */

LibExtern SEXP  R_EmptyEnv;	    /* An empty environment at the root of the
				    	environment tree */
LibExtern SEXP  R_BaseEnv;	    /* The base environment; formerly R_NilValue */
LibExtern SEXP	R_BaseNamespace;    /* The (fake) namespace for base */
LibExtern SEXP	R_NamespaceRegistry;/* Registry for registered namespaces */

LibExtern SEXP	R_Srcref;           /* Current srcref, for debuggers */

/* R_NilValue is the R NULL object */
#define R_NilValue ((SEXP) &R_NilValue_const)
extern const SEXPREC R_NilValue_const; /* defined in const-objs.c */

/* Special Values */
LibExtern SEXP	R_UnboundValue;	    /* Unbound marker */
LibExtern SEXP	R_MissingArg;	    /* Missing argument marker */

/* Logical Values.  Defined in const-objs.c */

#define R_ScalarLogicalNA ((SEXP) &R_ScalarLogicalNA_const)
extern VECTOR_SEXPREC_CONST R_ScalarLogicalNA_const;

#define R_ScalarLogicalFALSE ((SEXP) &R_ScalarLogicalFALSE_const)
extern VECTOR_SEXPREC_CONST R_ScalarLogicalFALSE_const;

#define R_ScalarLogicalTRUE ((SEXP) &R_ScalarLogicalTRUE_const)
extern VECTOR_SEXPREC_CONST R_ScalarLogicalTRUE_const;

/* Integer Values.  Defined in const-objs.c */

#define R_ScalarIntegerNA ((SEXP) &R_ScalarIntegerNA_const)
extern VECTOR_SEXPREC_CONST R_ScalarIntegerNA_const;

#define R_ScalarInteger0To10(v) ((SEXP) &R_ScalarInteger0To10_const[v])
extern VECTOR_SEXPREC_CONST R_ScalarInteger0To10_const[11];

/* Real Values.  Defined in const-objs.c */

#define R_ScalarRealNA ((SEXP) &R_ScalarRealNA_const)
extern VECTOR_SEXPREC_CONST R_ScalarRealNA_const;

#define R_ScalarRealZero ((SEXP) &R_ScalarRealZero_const)
extern VECTOR_SEXPREC_CONST R_ScalarRealZero_const;

#define R_ScalarRealOne ((SEXP) &R_ScalarRealOne_const)
extern VECTOR_SEXPREC_CONST R_ScalarRealOne_const;


#ifdef __MAIN__
attribute_hidden
#else
extern
#endif
SEXP	R_RestartToken;     /* Marker for restarted function calls */

/* Symbol Table Shortcuts */
LibExtern SEXP	R_Bracket2Symbol;   /* "[[" */
LibExtern SEXP	R_BracketSymbol;    /* "[" */
LibExtern SEXP	R_BraceSymbol;      /* "{" */
LibExtern SEXP	R_ClassSymbol;	    /* "class" */
LibExtern SEXP	R_DeviceSymbol;     /* ".Device" */
LibExtern SEXP	R_DimNamesSymbol;   /* "dimnames" */
LibExtern SEXP	R_DimSymbol;	    /* "dim" */
LibExtern SEXP	R_DollarSymbol;	    /* "$" */
LibExtern SEXP	R_DotsSymbol;	    /* "..." */
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
LibExtern SEXP	R_TspSymbol;	    /* "tsp" */
LibExtern SEXP	R_ValueSymbol;	    /* "value" */
LibExtern SEXP	R_AssignSymbols[4]; /* 0, "<-", "<<-", "=" */
LibExtern SEXP	R_SubAssignSymbol;  /* "[<-" */
LibExtern SEXP	R_SubSubAssignSymbol; /* "[[<-" */
LibExtern SEXP	R_DollarAssignSymbol; /* "$<-" */

LibExtern SEXP  R_dot_defined;      /* ".defined" */
LibExtern SEXP  R_dot_Method;       /* ".Method" */
LibExtern SEXP  R_dot_target;       /* ".target" */

/* Missing Values - others from Arith.h */
#define NA_STRING	R_NaString
LibExtern SEXP	R_NaString;	    /* NA_STRING as a CHARSXP */
LibExtern SEXP	R_BlankString;	    /* "" as a CHARSXP */

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
int  Rf_any_duplicated(SEXP x, Rboolean from_last);
int  Rf_any_duplicated3(SEXP x, SEXP incomp, Rboolean from_last);
SEXP Rf_applyClosure(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP Rf_applyClosure_v(SEXP, SEXP, SEXP, SEXP, SEXP, int);
SEXP Rf_arraySubscript(int, SEXP, SEXP, SEXP (*)(SEXP,SEXP),
                       SEXP (*)(SEXP, int), SEXP);
SEXP Rf_classgets(SEXP, SEXP);
SEXP Rf_cons_with_tag(SEXP, SEXP, SEXP);
SEXP Rf_cons(SEXP, SEXP);
Rboolean Rf_copy_1_string(char *, int, const char *);
Rboolean Rf_copy_2_strings(char *, int, const char *, const char *);
Rboolean Rf_copy_3_strings(char *, int, const char *, const char *, const char *);
void Rf_copyMatrix(SEXP, SEXP, Rboolean);
void Rf_copyMostAttrib(SEXP, SEXP);
void Rf_copyVector(SEXP, SEXP);
void Rf_copy_elements(SEXP, int, int, SEXP, int, int, int);
int Rf_copy_elements_coerced(SEXP, int, int, SEXP, int, int, int);
int Rf_countContexts(int, int);
SEXP Rf_CreateTag(SEXP);
void Rf_defineVar(SEXP, SEXP, SEXP);
SEXP Rf_dimgets(SEXP, SEXP);
SEXP Rf_dimnamesgets(SEXP, SEXP);
SEXP Rf_DropDims(SEXP);
SEXP Rf_duplicate(SEXP);
SEXP Rf_duplicated(SEXP, Rboolean);
int Rf_ep_match_strings(const char *, const char *);
int Rf_ep_match_exprs(SEXP, SEXP);
int Rf_ep_match_string_expr(const char *, SEXP);
SEXP Rf_eval(SEXP, SEXP);
SEXP Rf_evalv(SEXP, SEXP, int);
SEXP Rf_findFun(SEXP, SEXP);
SEXP Rf_findVar(SEXP, SEXP);
SEXP Rf_findVarPendingOK(SEXP, SEXP);
SEXP Rf_findVarInFrame(SEXP, SEXP);
SEXP Rf_findVarInFrame3(SEXP, SEXP, int);
SEXP Rf_fixup_NaRm(SEXP);
SEXP Rf_getAttrib(SEXP, SEXP);
SEXP Rf_getAttrib00(SEXP, SEXP);
SEXP Rf_GetArrayDimnames(SEXP);
SEXP Rf_GetColNames(SEXP);
void Rf_GetMatrixDimnames(SEXP, SEXP*, SEXP*, const char**, const char**);
SEXP Rf_GetOption(SEXP, SEXP); /* pre-2.13.0 compatibility */
SEXP Rf_GetOption1(SEXP);
int Rf_GetOptionDigits(void);
int Rf_GetOptionWidth(void);
SEXP Rf_GetRowNames(SEXP);
void Rf_gsetVar(SEXP, SEXP, SEXP);
SEXP Rf_install(const char *);
Rboolean Rf_isFree(SEXP);
Rboolean Rf_isOrdered(SEXP);
Rboolean Rf_isUnordered(SEXP);
Rboolean Rf_isUnsorted(SEXP, Rboolean);
R_len_t Rf_length(SEXP);
SEXP Rf_lengthgets(SEXP, R_len_t);
SEXP R_lsInternal(SEXP, Rboolean);
SEXP Rf_match(SEXP, SEXP, int);
SEXP Rf_matchE(SEXP, SEXP, int, SEXP);
SEXP Rf_namesgets(SEXP, SEXP);
SEXP Rf_mkChar(const char *);
SEXP Rf_mkCharLen(const char *, int);
Rboolean Rf_NonNullStringMatch(SEXP, SEXP);
int Rf_ncols(SEXP);
int Rf_nrows(SEXP);
SEXP Rf_nthcdr(SEXP, int);

Rboolean Rf_pmatch(SEXP, SEXP, Rboolean);
Rboolean Rf_psmatch(const char *, const char *, Rboolean);
void Rf_PrintValue(SEXP);
SEXP Rf_protect(SEXP);
void Rf_protect2(SEXP, SEXP);
void Rf_protect3(SEXP, SEXP, SEXP);
SEXP Rf_ScalarComplex(Rcomplex);
SEXP Rf_ScalarComplexShared(Rcomplex);
SEXP Rf_ScalarInteger(int);
SEXP Rf_ScalarIntegerShared(int);
SEXP Rf_ScalarLogical(int);  /* shared version is inlined */
SEXP Rf_ScalarRaw(Rbyte);
SEXP Rf_ScalarRawShared(Rbyte);
SEXP Rf_ScalarReal(double);
SEXP Rf_ScalarRealShared(double);
SEXP Rf_ScalarString(SEXP);
SEXP Rf_ScalarStringShared(SEXP);
SEXP Rf_setAttrib(SEXP, SEXP, SEXP);
void Rf_setNoSpecSymFlag(SEXP);
void Rf_setSVector(SEXP*, int, SEXP);
void Rf_set_elements_to_NA_or_NULL(SEXP, int, int);
void Rf_setVar(SEXP, SEXP, SEXP);
int Rf_set_var_in_frame(SEXP, SEXP, SEXP, int, int);
void Rf_set_var_nonlocal(SEXP, SEXP, SEXP, int);
SEXPTYPE Rf_str2type(const char *);
Rboolean Rf_StringBlank(SEXP);
SEXP Rf_substitute(SEXP,SEXP);
int Rf_tag_index(SEXP,SEXP);
const char * Rf_translateChar(SEXP);
const char * Rf_translateChar0(SEXP);
const char * Rf_translateCharUTF8(SEXP);
const char * Rf_type2char(SEXPTYPE);
SEXP Rf_type2str(SEXPTYPE);
void Rf_unprotect(int);
void Rf_unprotect_ptr(SEXP);
SEXP Rf_with_changed_nth(SEXP,int,SEXP);
SEXP Rf_with_no_nth(SEXP,int);
SEXP Rf_with_pairlist_appended(SEXP,SEXP);

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
void Rf_errorcall(SEXP, const char *, ...);
void Rf_warningcall(SEXP, const char *, ...);
void Rf_warningcall_immediate(SEXP, const char *, ...);

/* Save/Load Interface */
#define R_XDR_DOUBLE_SIZE 8
#define R_XDR_INTEGER_SIZE 4

void R_XDREncodeDouble(double d, void *buf);
double R_XDRDecodeDouble(void *buf);
void R_XDREncodeInteger(int i, void *buf);
int R_XDRDecodeInteger(void *buf);

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
#define countContexts		Rf_countContexts
#define CreateTag		Rf_CreateTag
#define defineVar		Rf_defineVar
#define dimgets			Rf_dimgets
#define dimnamesgets		Rf_dimnamesgets
#define DropDims                Rf_DropDims
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
#define findVar			Rf_findVar
#define findVarPendingOK	Rf_findVarPendingOK
#define findVarInFrame		Rf_findVarInFrame
#define findVarInFrame3		Rf_findVarInFrame3
#define fixup_NaRm		Rf_fixup_NaRm
#define GetArrayDimnames	Rf_GetArrayDimnames
#define getAttrib		Rf_getAttrib
#define getAttrib00		Rf_getAttrib00
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
#define install			Rf_install
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
#define NonNullStringMatch	Rf_NonNullStringMatch
#define nrows			Rf_nrows
#define nthcdr			Rf_nthcdr
#define PairToVectorList	Rf_PairToVectorList
#define pmatch			Rf_pmatch
#define psmatch			Rf_psmatch
#define PrintValue		Rf_PrintValue
#define protect			Rf_protect
#define R_from_C99_complex	Rf_R_from_C99_complex
#define reEnc			Rf_reEnc
#define rownamesgets		Rf_rownamesgets
#define S3Class                 Rf_S3Class
#define ScalarComplex		Rf_ScalarComplex
#define ScalarComplexShared	Rf_ScalarComplexShared
#define ScalarInteger		Rf_ScalarInteger
#define ScalarIntegerShared	Rf_ScalarIntegerShared
#define ScalarLogical		Rf_ScalarLogical
#define ScalarLogicalShared	Rf_ScalarLogicalShared
#define ScalarReal		Rf_ScalarReal
#define ScalarRealShared	Rf_ScalarRealShared
#define ScalarString		Rf_ScalarString
#define ScalarStringShared	Rf_ScalarStringShared
#define ScalarRaw		Rf_ScalarRaw
#define ScalarRawShared		Rf_ScalarRawShared
#define setAttrib		Rf_setAttrib
#define setNoSpecSymFlag	Rf_setNoSpecSymFlag
#define setSVector		Rf_setSVector
#define set_elements_to_NA_or_NULL Rf_set_elements_to_NA_or_NULL
#define setVar			Rf_setVar
#define set_var_in_frame	Rf_set_var_in_frame
#define set_var_nonlocal	Rf_set_var_nonlocal
#define str2type		Rf_str2type
#define StringBlank		Rf_StringBlank
#define substitute		Rf_substitute
#define tag_index		Rf_tag_index
#define translateChar		Rf_translateChar
#define translateChar0		Rf_translateChar0
#define translateCharUTF8      	Rf_translateCharUTF8
#define type2char		Rf_type2char
#define type2str		Rf_type2str
#define unprotect		Rf_unprotect
#define unprotect_ptr		Rf_unprotect_ptr
#define VectorToPairList	Rf_VectorToPairList
#define warningcall		Rf_warningcall
#define warningcall_immediate	Rf_warningcall_immediate
#define with_changed_nth	Rf_with_changed_nth
#define with_no_nth		Rf_with_no_nth
#define with_pairlist_appended	Rf_with_pairlist_appended

#endif

/* Sets of SEXTYPES, for fast testing with if ((set >> type) & 1) ... 
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

#if defined(CALLED_FROM_DEFN_H) && !defined(__MAIN__) && (defined(COMPILING_R) || ( __GNUC__ && !defined(__INTEL_COMPILER) ))
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
SEXP	 Rf_ScalarLogicalShared(int);

#ifdef complex  /* In C99, should be defined if complex.h included */
double complex Rf_C99_from_R_complex(Rcomplex *);
void Rf_R_from_C99_complex(Rcomplex *, double complex);
#endif

#endif

#ifdef USE_RINTERNALS

/* Test macros with function versions above */
#undef isNull
#define isNull(s)	(TYPEOF(s) == NILSXP)
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
