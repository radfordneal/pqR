/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2014 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2011  The R Development Core Team.
 *
 *  The changes in pqR from R-2.15.0 distributed by the R Core Team are
 *  documented in the NEWS and MODS files in the top-level source directory.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/*
 *	This code implements a non-moving generational collector
 *      with two or three generations.
 *
 *	Memory allocated by R_alloc is maintained in a stack.  Code
 *	that R_allocs memory must use vmaxget/VMAXGET and vmaxset/VMAXSET 
 *	to obtain and reset the stack pointer.
 */

#define USE_RINTERNALS

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <R_ext/RS.h> /* for S4 allocation */

#define USE_FAST_PROTECT_MACROS   /* MUST be defined in this module! */
#define R_USE_SIGNALS 1
#include <Defn.h>
#include <R_ext/GraphicsEngine.h> /* GEDevDesc, GEgetDevice */
#include <R_ext/Rdynload.h>

#include <helpers/helpers-app.h>


/* CONFIGURATION OPTIONS.  

   Any valid settings for the options below should work, with different effects
   on performance.  However, some combinations may not have been tested 
   recently (or at all). */

#define EXPEL_OLD_TO_NEW 0 /* Immediately expel, rather than use OldToNew? */

#define FLAG_OLD_TO_NEW 1  /* Use "prev" field to flag nodes already moved
                              to OldToNew?  (Only when EXPEL_OLD_TO_NEW is 0) */

#define SORT_NODES 1  /* Sort free nodes in every page on each full GC? */

#define NODE_ALIGN 64 /* Address boundary for aligning nodes in pages, to try
                         to improve cache performance (must be a power of 2) */

#define PAGE_SIZE (2048+64) /* Number of bytes in a page of nodes. */

#define MAX_NODE_CLASSES 8  /* Max number of node classes, from bits in gccls */
#define NUM_NODE_CLASSES 8  /* At least 2, and no more than the max above */

#define STRHASHINITSIZE (1<<16) /* Initial number of slots in string hash table
                                   (must be a power of two) */
#define STRHASHMAXSIZE (1<<21)  /* Maximum slots in the string hash table */

#define ENABLE_SHARED_CONSTANTS 1  /* Normally 1, to enable use of shared
                                      constants 0.0, 0L, etc. But doesn't affect
                                      sharing of logicals FALSE, TRUE, and NA,
                                      which is done in Rinlinedfuns.h */

/* NodeClassSize gives the number of VECRECs in nodes of the small node classes.
   One of these will be identified (at run time) as SEXPREC_class, used for
   "cons" cells (so it's necessary that one be big enough for this).  Note 
   that the last node class is for larger vectors, and has no entry here.

   The values in the initialization below will usually be replaced by values 
   derived from NodeClassBytes32 or NodeClassBytes64, which are designed for 
   32-bit and 64-bit systems, unless USE_FALBACK_SIZES is set to 1.

   The initialization below is for the maximum number of small node classes.
   The number of classes used may be smaller, as determined by the setting of
   NUM_NODE_CLASSES above. 

   The first node class must be big enough to hold a length one vector of
   RAWSXP, LGLSXP, INTSXP, or REALSXP type (but not necessarily CPLXSXP).
*/

static int NodeClassSize[MAX_NODE_CLASSES-1]    /* Fallback sizes, in VECRECs */
        = {  1,  2,  4,  6,  8,  16,  32 };     /*  (typically 8-byte chunks) */

#define USE_FALLBACK_SIZES 0  /* Use above sizes even when sizes below apply? */

static int NodeClassBytes32[MAX_NODE_CLASSES-1] /* Sizes for 32-bit platforms */
        = { 32, 40, 48, 64, 80,  96, 128 };     /*  (bytes, including header) */
/* VECRECs:  1,  2,  3,  5,  7,   9,  13     (assuming 24-byte SEXPREC_ALIGN) */

static int NodeClassBytes64[MAX_NODE_CLASSES-1] /* Sizes for 64-bit platforms */
        = { 48, 56, 64, 80, 96, 128, 192 };     /*  (bytes, including header) */
/* VECRECs:  1,  2,  3,  5,  7,  11,  19     (assuming 40-byte SEXPREC_ALIGN) */


/* DEBUGGING OPTIONS.

   The 'testvalgrind' function invoked with .Internal is always present.

   Options set externally:

   VALGRIND_LEVEL  

       Set by --with-valgrind-instrumentation=n configure option, where
       n (default 0) controls VALGRIND instrumentation.  Currently, any
       non-zero value enables all the extra instrumentation.

   NVALGRIND

       It may be necessary to define NVALGRIND for a non-gcc
       compiler on a supported architecture if it has different
       syntax for inline assembly language from gcc.

   PROTECT_CHECK / TESTING_WRITE_BARRIER

       If defined, tries to detect unprotected SEXPs.  See below.

   Other debug options are set by the definitions below. */

#define TOLERATE_NULL 1 /* If non-zero, node forwarding & aging ignores zero
                           pointers (which shouldn't exist), to avoid crashing.
                           Some packages incorrectly create objects with zero
                           pointers, so this option should probably be set to 1,
                           but it could be set to 0 for debugging purposes. */

#define DEBUG_GC 0  /* How much debug info to write about node lists in a GC  */
                    /* 0: none, 1: summary only, 2: do checks, 3: print counts*/

#define DEBUG_ADJUST_HEAP 0 /* Whether to write debug info on heap adjustments*/

#define DEBUG_RELEASE 0  /* Whether to write debug info on page releases */

#define SNAP_CHECK 0  /* 1 to check validity of list when snapping/unsnapping */

#define PAGE_PAD 0      /* Number of extra bytes to allocate at the end of each
                           page as padding, to help detect/alleviate overruns */

#define LARGE_VEC_PAD 0 /* Number of extra bytes to allocate at the end of each
                           large vector as padding, for overrun detection... */

#define DEBUG_GLOBAL_STRING_HASH 0

#define DEBUG_SHOW_CHARSXP_CACHE 0

#define PRINT_TYPE_STATS 0 /* Set to 1 to print stats on # of objects of each 
                              type after garbage collection initiated by gc() */

#define ABORT_ON_BAD_SEXPTYPE 1 /* Set to 1 to make bad SEXPTYPE result in a
                                   call of abort() rather than a report */


/* VALGRIND declarations.

   For Win32, Valgrind is useful only if running under Wine. */

#ifdef Win32
# ifndef USE_VALGRIND_FOR_WINE
# define NVALGRIND 1
#endif
#endif

#ifndef NVALGRIND
# include "memcheck.h"
#endif

#ifndef VALGRIND_LEVEL
#define VALGRIND_LEVEL 0
#endif


static void GetNewPage(int node_class);

#if defined(Win32) && defined(LEA_MALLOC)
/*#include <stddef.h> */
extern void *Rm_malloc(size_t n);
extern void *Rm_calloc(size_t n_elements, size_t element_size);
extern void Rm_free(void * p);
extern void *Rm_realloc(void * p, size_t n);
#define calloc Rm_calloc
#define malloc Rm_malloc
#define realloc Rm_realloc
#define free Rm_free
#endif

/* malloc uses size_t.  We are assuming here that size_t is at least
   as large as unsigned long.  Changed from int at 1.6.0 to (i) allow
   2-4Gb objects on 32-bit system and (ii) objects limited only by
   length on a 64-bit system.
*/

static int gc_reporting = 0;
static int gc_count = 0;


#ifdef TESTING_WRITE_BARRIER
# define PROTECTCHECK
#endif

#ifdef PROTECTCHECK
/* This is used to help detect unprotected SEXP values.  It is most
   useful if the strict barrier is enabled as well. The strategy is:

       All GCs are full GCs

       New nodes are marked as NEWSXP

       After a GC all free nodes that are not of type NEWSXP are
       marked as type FREESXP

       Most calls to accessor functions check their SEXP inputs and
       SEXP outputs with CHK() to see if a reachable node is a
       FREESXP and signal an error if a FREESXP is found.

   Combined with GC torture this can help locate where an unprotected
   SEXP is being used.

   This approach will miss cases where an unprotected node has been
   re-allocated.  For these cases it is possible to set
   gc_inhibit_release to TRUE.  FREESXP nodes will not be reallocated,
   or large ones released, until gc_inhibit_release is set to FALSE
   again.  This will of course result in memory growth and should be
   used with care and typically in combination with OS mechanisms to
   limit process memory usage.  LT */

/* Before a node is marked as a FREESXP by the collector the previous
   type is recorded.  For now using the LEVELS field seems
   reasonable.  */
#define OLDTYPE(s) LEVELS(s)
#define SETOLDTYPE(s, t) SETLEVELS(s, t)

static const char *sexptype2char(SEXPTYPE type);

static R_INLINE SEXP CHK(SEXP x)
{
    /* **** NULL check because of R_CurrentExpr */
    if (x != NULL && TYPEOF(x) == FREESXP)
	error("unprotected object (%p) encountered (was %s)",
	      x, sexptype2char(OLDTYPE(x)));
    return x;
}
#else
#define CHK(x) (x)
#endif

/* The following three variables definitions are used to record the
   address and type of the first bad type seen during a collection,
   and for FREESXP nodes they record the old type as well. */
static SEXPTYPE bad_sexp_type_seen = 0;
static SEXP bad_sexp_type_sexp = NULL;
#ifdef PROTECTCHECK
static SEXPTYPE bad_sexp_type_old_type = 0;
#endif
static int bad_sexp_type_line = 0;

#if ABORT_ON_BAD_SEXPTYPE
#define register_bad_sexp_type(s,line) abort()
#else
static void register_bad_sexp_type(SEXP s, int line)
{
    if (bad_sexp_type_seen == 0) {
	bad_sexp_type_seen = TYPEOF(s);
	bad_sexp_type_sexp = s;
	bad_sexp_type_line = line;
#ifdef PROTECTCHECK
	if (TYPEOF(s) == FREESXP)
	    bad_sexp_type_old_type = OLDTYPE(s);
#endif
    }
}
#endif


/* slight modification of typename() from install.c -- should probably merge */
static const char *sexptype2char(SEXPTYPE type) {
    switch (type) {
    case NILSXP:	return "NILSXP";
    case SYMSXP:	return "SYMSXP";
    case LISTSXP:	return "LISTSXP";
    case CLOSXP:	return "CLOSXP";
    case ENVSXP:	return "ENVSXP";
    case PROMSXP:	return "PROMSXP";
    case LANGSXP:	return "LANGSXP";
    case SPECIALSXP:	return "SPECIALSXP";
    case BUILTINSXP:	return "BUILTINSXP";
    case CHARSXP:	return "CHARSXP";
    case LGLSXP:	return "LGLSXP";
    case INTSXP:	return "INTSXP";
    case REALSXP:	return "REALSXP";
    case CPLXSXP:	return "CPLXSXP";
    case STRSXP:	return "STRSXP";
    case DOTSXP:	return "DOTSXP";
    case ANYSXP:	return "ANYSXP";
    case VECSXP:	return "VECSXP";
    case EXPRSXP:	return "EXPRSXP";
    case BCODESXP:	return "BCODESXP";
    case EXTPTRSXP:	return "EXTPTRSXP";
    case WEAKREFSXP:	return "WEAKREFSXP";
    case S4SXP:		return "S4SXP";
    case RAWSXP:	return "RAWSXP";
    case NEWSXP:	return "NEWSXP"; /* should never happen */
    case FREESXP:	return "FREESXP";
    default:	 	return "<unknown>";
    }
}


/* Declarations relating to GC torture

   **** if the user specified a wait before starting to force
   **** collecitons it might make sense to also wait before starting
   **** to inhibit releases */

static int gc_force_wait = 0;
static int gc_force_gap = 0;
static Rboolean gc_inhibit_release = FALSE;
#define FORCE_GC (gc_force_wait > 0 ? \
  (--gc_force_wait > 0 ? 0 : (gc_force_wait = gc_force_gap, 1)) : 0)

#define GC_PROT(X) do { \
    int __wait__ = gc_force_wait; \
    int __gap__ = gc_force_gap;			   \
    Rboolean __release__ = gc_inhibit_release;	   \
    X;						   \
    gc_force_wait = __wait__;			   \
    gc_force_gap = __gap__;			   \
    gc_inhibit_release = __release__;		   \
}  while(0)


/* Declarations relating to Rprofmem */

static int R_IsMemReporting;
static int R_MemReportingToTerminal;
static int R_MemPagesReporting;
static int R_MemStackReporting;
static int R_MemDetailsReporting;
static FILE *R_MemReportingOutfile;
static R_size_t R_MemReportingThreshold;
static R_len_t R_MemReportingNElem;
static void R_ReportAllocation (R_size_t, SEXPTYPE, R_len_t);
static void R_ReportNewPage();

extern SEXP framenames;  /* in model.c */

static void R_gc_internal(R_size_t size_needed);
static void R_gc_full(R_size_t size_needed);

static SEXPREC UnmarkedNodeTemplate; /* initialized to zeros, since static */

static SEXP R_StringHash;   /* Global hash of CHARSXPs */

#define NODE_IS_MARKED(s) (MARK(s))
#define MARK_NODE(s) (MARK(s)=1)  /* Should only be called if !NODE_IS_MARKED 
                                     since s could be a read-only constant */
#define UNMARK_NODE(s) (MARK(s)=0)

/* Tuning Constants. Most of these could be made settable from R,
   within some reasonable constraints at least.  Since there are quite
   a lot of constants it would probably make sense to put together
   several "packages" representing different space/speed tradeoffs
   (e.g. very aggressive freeing and small increments to conserve
   memory; much less frequent releasing and larger increments to
   increase speed). */

/* There are three levels of collections.  Level 0 collects only the
   youngest generation, level 1 collects the two youngest generations,
   and level 2 collects all generations.  Higher level collections
   occur at least after specified numbers of lower level ones.  After
   LEVEL_0_FREQ level zero collections a level 1 collection is done;
   after every LEVEL_1_FREQ level 1 collections a level 2 collection
   occurs.  Thus, roughly, every LEVEL_0_FREQ-th collection is a level
   1 collection and every (LEVEL_0_FREQ * LEVEL_1_FREQ)-th collection
   is a level 2 collection.  */
#define LEVEL_0_FREQ 20
#define LEVEL_1_FREQ 5
static int collect_counts_max[] = { LEVEL_0_FREQ, LEVEL_1_FREQ };

/* When a level N collection fails to produce at least MinFreeFrac *
   R_NSize free nodes and MinFreeFrac * R_VSize free vector space, the
   next collection will be a level N + 1 collection.

   This constant is also used in heap size adjustment as a minimal
   fraction of the minimal heap size levels that should be available
   for allocation. */
static double R_MinFreeFrac = 0.2;

/* When pages are released, a number of free nodes equal to
   R_MaxKeepFrac times the number of allocated nodes for each class is
   retained.  Pages not needed to meet this requirement are released.
   An attempt to release pages is made every level 1 or level 2 collection. */
static double R_MaxKeepFrac = 0.5;

/* The heap size constants R_NSize and R_VSize are used for triggering
   collections.  The initial values set by defaults or command line
   arguments are used as minimal values.  After full collections these
   levels are adjusted up or down, though not below the minimal values
   or above the maximum values, towards maintain heap occupancy within
   a specified range.  When the number of nodes in use reaches
   R_NGrowFrac * R_NSize, the value of R_NSize is incremented by
   R_NGrowIncrMin + R_NGrowIncrFrac * R_NSize.  When the number of
   nodes in use falls below R_NShrinkFrac, R_NSize is decremented by
   R_NShrinkIncrMin + R_NShrinkFrac * R_NSize.  Analogous adjustments
   are made to R_VSize.

   This mechanism for adjusting the heap size constants is very
   primitive but hopefully adequate for now.  Some modeling and
   experimentation would be useful.  We want the heap sizes to get set
   at levels adequate for the current computations.  The present
   mechanism uses only the size of the current live heap to provide
   information about the current needs; since the current live heap
   size can be very volatile, the adjustment mechanism only makes
   gradual adjustments.  A more sophisticated strategy would use more
   of the live heap history. */
static double R_NGrowFrac = 0.70;
static double R_NShrinkFrac = 0.30;

static double R_VGrowFrac = 0.70;
static double R_VShrinkFrac = 0.30;

#ifdef SMALL_MEMORY
/* On machines with only 32M of memory (or on a classic Mac OS port)
   it might be a good idea to use settings like these that are more
   aggressive at keeping memory usage down. */
static double R_NGrowIncrFrac = 0.0, R_NShrinkIncrFrac = 0.2;
static int R_NGrowIncrMin = 50000, R_NShrinkIncrMin = 0;
static double R_VGrowIncrFrac = 0.0, R_VShrinkIncrFrac = 0.2;
static int R_VGrowIncrMin = 100000, R_VShrinkIncrMin = 0;

#else /* SMALL_MEMORY not defined */
static double R_NGrowIncrFrac = 0.05, R_NShrinkIncrFrac = 0.2;
static int R_NGrowIncrMin = 40000, R_NShrinkIncrMin = 0;
static double R_VGrowIncrFrac = 0.05, R_VShrinkIncrFrac = 0.2;
static int R_VGrowIncrMin = 80000, R_VShrinkIncrMin = 0;
#endif

/* Maximal Heap Limits.  These variables contain upper limits on the
   heap sizes.  They could be made adjustable from the R level,
   perhaps by a handler for a recoverable error.

   Access to these values is provided with reader and writer
   functions; the writer function insures that the maximal values are
   never set below the current ones. */
static R_size_t R_MaxVSize = R_SIZE_T_MAX;
static R_size_t R_MaxNSize = R_SIZE_T_MAX;
static int vsfac = 1; /* current units for vsize: changes at initialization */

R_size_t attribute_hidden R_GetMaxVSize(void)
{
    if (R_MaxVSize == R_SIZE_T_MAX) return R_SIZE_T_MAX;
    return R_MaxVSize*vsfac;
}

void attribute_hidden R_SetMaxVSize(R_size_t size)
{
    if (size == R_SIZE_T_MAX) return;
    if (size / vsfac >= R_VSize) R_MaxVSize = (size+1)/vsfac;
}

R_size_t attribute_hidden R_GetMaxNSize(void)
{
    return R_MaxNSize;
}

void attribute_hidden R_SetMaxNSize(R_size_t size)
{
    if (size >= R_NSize) R_MaxNSize = size;
}

void R_SetPPSize(R_size_t size)
{
    R_PPStackSize = size;
}

/* Miscellaneous Globals. */

static SEXP R_PreciousList;             /* List of Persistent Objects */
static R_size_t R_LargeVallocSize = 0;
static R_size_t R_SmallNallocSize = 0;
static R_size_t orig_R_NSize;
static R_size_t orig_R_VSize;

static R_size_t R_N_maxused=0;		/* Records of maximum used (but can */
static R_size_t R_V_maxused=0;		/*   be reset by gc(reset=TRUE)     */
static double R_NMega_max=0.0;


/* Node Classes.  Smallish vectors and "cons" cells are in classes 
   1, ..., NUM_SMALL_NODE_CLASSES.  Large vector nodes are in class 
   LARGE_NODE_CLASS.  For vector nodes the node header is followed 
   in memory by the vector data, offset from the header by SEXPREC_ALIGN. */

#define LARGE_NODE_CLASS (NUM_NODE_CLASSES - 1)
#define NUM_SMALL_NODE_CLASSES (NUM_NODE_CLASSES - 1)

#define NODE_CLASS(s) ((s)->sxpinfo.gccls)
#define SET_NODE_CLASS(s,v) (((s)->sxpinfo.gccls) = (v))

static int SEXPREC_class;    /* Small node class used for "cons" cells */
static int sizeof_SEXPREC;   /* Size of SEXPREC_class nodes */


/* Node Generations. */

#define NUM_OLD_GENERATIONS 2

/* sxpinfo allocates one bit for the old generation count, so only 1
   or 2 is allowed */
#if NUM_OLD_GENERATIONS > 2 || NUM_OLD_GENERATIONS < 1
# error number of old generations must be 1 or 2
#endif

#define NODE_GENERATION(s) ((s)->sxpinfo.gcgen)
#define SET_NODE_GENERATION(s,g) ((s)->sxpinfo.gcgen=(g))

#define NODE_GEN_IS_YOUNGEST(x) (!NODE_IS_MARKED(x))
#define NODE_GEN_IS_YOUNGER(s,g) \
  (! NODE_IS_MARKED(s) || NODE_GENERATION(s) < (g))

static int num_old_gens_to_collect = 0;
static int gen_gc_counts[NUM_OLD_GENERATIONS + 1];
static int collect_counts[NUM_OLD_GENERATIONS];


/* Node Pages.  Nodes other than large vector nodes are allocated
   from fixed size pages.  The pages for each node class are kept in a
   linked list. */

typedef union PAGE_HEADER {
  union PAGE_HEADER *next;
  double align;
} PAGE_HEADER;

#define NODE_SIZE(c) (sizeof(SEXPREC_ALIGN) + NodeClassSize[c] * sizeof(VECREC))

#define PAGE_SKIP(p) \
    ( (((uintptr_t)(p+1)+(NODE_ALIGN-1)) & ~(NODE_ALIGN-1)) - (uintptr_t)(p+1) )
#define PAGE_DATA(p) \
    ( (void *) ((uintptr_t)(p+1) + PAGE_SKIP(p)) )
#define PAGE_COUNT(p,nsize) \
    ( (PAGE_SIZE - sizeof(PAGE_HEADER) - PAGE_SKIP(p)) / nsize )

#define VHEAP_FREE() (R_VSize - R_LargeVallocSize /* - R_SmallVallocSize */)


/* The Heap Structure.  Nodes for each class/generation combination
   are arranged in circular doubly-linked lists.  The double linking
   allows nodes to be removed in constant time; this is used by the
   collector to move reachable nodes out of free space and into the
   appropriate generation.  The circularity eliminates the need for
   end checks.  In addition, each link is anchored at an artificial
   node, the Peg SEXPREC's in the structure below, which simplifies
   pointer maintenance.  The circular doubly-linked arrangement is
   taken from Baker's in-place incremental collector design; see
   ftp://ftp.netcom.com/pub/hb/hbaker/NoMotionGC.html or the Jones and
   Lins GC book.  The linked lists are implemented by adding two
   pointer fields to the SEXPREC structure, which increases its size
   from 5 to 7 words. Other approaches are possible but don't seem
   worth pursuing for R.

   There are two options for dealing with old-to-new pointers.  The
   first option is to make sure they never occur by transferring all
   referenced younger objects to the generation of the referrer when a
   reference to a newer object is assigned to an older one.  This is
   enabled by setting EXPEL_OLD_TO_NEW to 1.  The second alternative,
   used when EXPEL_OLD_TO_NEW is 0, is to keep track of all nodes that 
   may contain references to newer nodes and to "age" the nodes they 
   refer to at the beginning of each collection.  The first option is 
   simpler in some ways, but will create more floating garbage and add 
   a bit to the execution time, though the difference is probably marginal 
   on both counts.*/

static struct gen_heap {

    SEXP Old[NUM_OLD_GENERATIONS];      /* Marked nodes of old generations */
    SEXP New;                           /* Unmarked nodes, free or newly used */
    SEXP Free;                          /* Free nodes, points inside New. 
                                           Not used for LARGE_NODE_CLASS */
#if !EXPEL_OLD_TO_NEW
    SEXP OldToNew[NUM_OLD_GENERATIONS]; /* Old nodes pointing to younger ones */
#endif

    SEXPREC OldPeg[NUM_OLD_GENERATIONS]; /* Actual space for the heads of the */
    SEXPREC NewPeg;                      /*   lists in Old, New, and OldToNew */
#if !EXPEL_OLD_TO_NEW
    SEXPREC OldToNewPeg[NUM_OLD_GENERATIONS];
#endif

    int OldCount[NUM_OLD_GENERATIONS];  /* # of nodes in Old and OldToNew */
    int AllocCount;                     /* # of nodes allocated, used or not */
    int PageCount;                      /* # of pages alloc'd */
    PAGE_HEADER *pages;                 /* Ptr to most recently alloc'd page */

} R_GenHeap[NUM_NODE_CLASSES];

static R_size_t R_NodesInUse = 0;

#define NEXT_NODE(s) (s)->gengc_next_node
#define PREV_NODE(s) (s)->gengc_prev_node
#define SET_NEXT_NODE(s,t) (NEXT_NODE(s) = (t))
#define SET_PREV_NODE(s,t) (PREV_NODE(s) = (t))
#define MAKE_EMPTY(s) (NEXT_NODE(s) = PREV_NODE(s) = (s))


/* Node List Manipulation */

/* unsnap node s from its list */
#define UNSNAP_NODE(s) do { \
  SEXP un__n__ = (s); \
  SEXP next = NEXT_NODE(un__n__); \
  SEXP prev = PREV_NODE(un__n__); \
  if (SNAP_CHECK && \
        (!SORT_NODES || num_old_gens_to_collect!=NUM_OLD_GENERATIONS)) { \
      if (PREV_NODE(next) != un__n__ || NEXT_NODE(prev) != un__n__) abort(); \
  } \
  SET_NEXT_NODE(prev, next); \
  SET_PREV_NODE(next, prev); \
} while(0)

/* snap in node s before node t */
#define SNAP_NODE(s,t) do { \
  SEXP sn__n__ = (s); \
  SEXP next = (t); \
  SEXP prev = PREV_NODE(next); \
  if (SNAP_CHECK && \
        (!SORT_NODES || num_old_gens_to_collect!=NUM_OLD_GENERATIONS)) { \
      if (NEXT_NODE(prev) != next) abort(); \
  } \
  SET_NEXT_NODE(sn__n__, next); \
  SET_PREV_NODE(next, sn__n__); \
  SET_NEXT_NODE(prev, sn__n__); \
  SET_PREV_NODE(sn__n__, prev); \
} while (0)

/* move all nodes on from_peg to to_peg */
#define BULK_MOVE(from_peg,to_peg) do { \
  SEXP __from__ = (from_peg); \
  SEXP __to__ = (to_peg); \
  SEXP first_old = NEXT_NODE(__from__); \
  SEXP last_old = PREV_NODE(__from__); \
  SEXP first_new = NEXT_NODE(__to__); \
  SET_PREV_NODE(first_old, __to__); \
  SET_NEXT_NODE(__to__, first_old); \
  SET_PREV_NODE(first_new, last_old); \
  SET_NEXT_NODE(last_old, first_new); \
  SET_NEXT_NODE(__from__, __from__); \
  SET_PREV_NODE(__from__, __from__); \
} while (0);


/* Processing Node Children */

#ifdef USE_ATTRIB_FIELD_FOR_CHARSXP_CACHE_CHAINS
/* When the CHARSXP hash chains are maintained through the ATTRIB
   field it is important that we NOT trace those fields otherwise too
   many CHARSXPs will be kept alive artificially. As a safety we don't
   ignore all non-NULL ATTRIB values for CHARSXPs but only those that
   are themselves CHARSXPs, which is what they will be if they are
   part of a hash chain.  Theoretically, for CHARSXPs the ATTRIB field
   should always be either R_NilValue or a CHARSXP. */
# ifdef PROTECTCHECK
#  define HAS_GENUINE_ATTRIB(x) \
    (TYPEOF(x) != FREESXP && ATTRIB(x) != R_NilValue && \
     (TYPEOF(x) != CHARSXP || TYPEOF(ATTRIB(x)) != CHARSXP))
# else
#  define HAS_GENUINE_ATTRIB(x) \
    (ATTRIB(x) != R_NilValue && \
     (TYPEOF(x) != CHARSXP || TYPEOF(ATTRIB(x)) != CHARSXP))
# endif
#else
# ifdef PROTECTCHECK
#  define HAS_GENUINE_ATTRIB(x) \
    (TYPEOF(x) != FREESXP && ATTRIB(x) != R_NilValue)
# else
#  define HAS_GENUINE_ATTRIB(x) (ATTRIB(x) != R_NilValue)
# endif
#endif
#ifdef PROTECTCHECK
#define FREE_FORWARD_CASE case FREESXP: if (gc_inhibit_release) break;
#define FREE_FORWARD_ELSE_IF \
            else if (typ_ == FREESXP && gc_inhibit_release) break;
#else
#define FREE_FORWARD_CASE
#define FREE_FORWARD_ELSE_IF
#endif

/* This macro calls dc_action_ for each child of n_, passing
   dc_extra_ as a second argument for each call.

   It combines handling of all "no action" types, and of all 
   "three-pointer" types, and of all "vector of pointers" types,
   relying on the layouts of these being parallel. */

#define no_action_types \
( (1 << NILSXP) + \
  (1 << BUILTINSXP) + \
  (1 << SPECIALSXP) + \
  (1 << CHARSXP) + \
  (1 << LGLSXP) + \
  (1 << INTSXP) + \
  (1 << REALSXP) + \
  (1 << CPLXSXP) + \
  (1 << WEAKREFSXP) + \
  (1 << RAWSXP) + \
  (1 << S4SXP) )

#define three_pointer_types \
( (1 << ENVSXP) + \
  (1 << CLOSXP) + \
  (1 << PROMSXP) + \
  (1 << LISTSXP) + \
  (1 << LANGSXP) + \
  (1 << DOTSXP) + \
  (1 << SYMSXP) + \
  (1 << BCODESXP) )

#define vector_of_pointers_types \
( (1 << VECSXP) + \
  (1 << EXPRSXP) + \
  (1 << STRSXP) )

#define DO_CHILDREN(n_,dc_action_,dc_extra_) do { \
    if (HAS_GENUINE_ATTRIB(n_)) { \
        dc_action_ (ATTRIB(n_), dc_extra_); \
    } \
    int typ_ = TYPEOF(n_); \
    if (! ((no_action_types >> typ_) & 1)) { \
        SEXP *strt_; R_len_t cnt_; \
        if ((three_pointer_types >> typ_) & 1) { \
            strt_ = &CAR(n_); cnt_ = 3; \
        } \
        else if ((vector_of_pointers_types >> typ_) & 1) { \
            cnt_ = LENGTH(n_); \
            if (cnt_ == 0) break; \
            strt_ = &STRING_ELT(n_,0); \
        } \
        else if (typ_ == EXTPTRSXP) { \
            strt_ = &CDR(n_); cnt_ = 2; \
        } \
        FREE_FORWARD_ELSE_IF \
        else \
            register_bad_sexp_type(n_, __LINE__); \
        do { \
            dc_action_ (*strt_++, dc_extra_); \
            cnt_ -= 1; \
        } while (cnt_ > 0); \
    } \
} while(0)

#define DO_CHILDREN_DEBUG(__n__,dc__action__,extra1,extra2) do { \
  int _r_, _c_ = 0; \
  if (HAS_GENUINE_ATTRIB(__n__)) { \
    _c_ += _r_ = dc__action__(ATTRIB(__n__), __n__,extra1,extra2); \
    if (_r_) REprintf("  -- %s attrib\n",type2char(TYPEOF(__n__))); \
  } \
  switch (TYPEOF(__n__)) { \
  case NILSXP: \
  case BUILTINSXP: \
  case SPECIALSXP: \
  case CHARSXP: \
  case LGLSXP: \
  case INTSXP: \
  case REALSXP: \
  case CPLXSXP: \
  case WEAKREFSXP: \
  case RAWSXP: \
  case S4SXP: \
    break; \
  case STRSXP: \
  case EXPRSXP: \
  case VECSXP: \
    { \
      int i; \
      for (i = 0; i < LENGTH(__n__); i++) { \
	_c_ += _r_ = dc__action__(STRING_ELT(__n__, i), __n__,extra1,extra2); \
        if (_r_) REprintf(" -- %s %d\n",type2char(TYPEOF(__n__)),i); \
      } \
    } \
    break; \
  case ENVSXP: \
    _c_ += _r_ = dc__action__(FRAME(__n__), __n__,extra1,extra2); \
    if (_r_) REprintf(" -- %s frame\n",type2char(TYPEOF(__n__))); \
    _c_ += _r_ = dc__action__(ENCLOS(__n__), __n__,extra1,extra2); \
    if (_r_) REprintf(" -- %s enclos\n",type2char(TYPEOF(__n__))); \
    _c_ += _r_ = dc__action__(HASHTAB(__n__), __n__,extra1,extra2); \
    if (_r_) REprintf(" -- %s hashtab\n",type2char(TYPEOF(__n__))); \
    break; \
  case CLOSXP: \
  case PROMSXP: \
  case LISTSXP: \
  case LANGSXP: \
  case DOTSXP: \
  case SYMSXP: \
  case BCODESXP: \
    _c_ += _r_ = dc__action__(TAG(__n__), __n__,extra1,extra2); \
    if (_r_) REprintf(" -- %s tag\n",type2char(TYPEOF(__n__))); \
    _c_ += _r_ = dc__action__(CAR(__n__), __n__,extra1,extra2); \
    if (_r_) REprintf(" -- %s car\n",type2char(TYPEOF(__n__))); \
    _c_ += _r_ = dc__action__(CDR(__n__), __n__,extra1,extra2); \
    if (_r_) REprintf(" -- %s cdr\n",type2char(TYPEOF(__n__))); \
    break; \
  case EXTPTRSXP: \
    _c_ += _r_ = dc__action__(EXTPTR_PROT(__n__), __n__,extra1,extra2); \
    if (_r_) REprintf(" -- %s prot\n",type2char(TYPEOF(__n__))); \
    _c_ += _r_ = dc__action__(EXTPTR_TAG(__n__), __n__,extra1,extra2); \
    if (_r_) REprintf(" -- %s tag\n",type2char(TYPEOF(__n__))); \
    break; \
  FREE_FORWARD_CASE \
  default: \
    register_bad_sexp_type(__n__, __LINE__);		\
  } \
  if (_c_ && getenv("ABORT")) abort(); \
} while(0)


/* Forwarding Nodes.  These macros mark nodes or children of nodes and
   place them on the forwarding list.  The forwarding list is assumed
   to be in a local variable of the caller named "forwarded_nodes". 

   Forwarding a zero pointer is tolerated if TOLERATE_NULL is set to 1, 
   although such zero pointers are not supposed to be there. */

#define FORWARD_NODE(s) do { \
  SEXP fn__n__ = (s); \
  if ((!TOLERATE_NULL || fn__n__) && !NODE_IS_MARKED(fn__n__)) { \
    CHECK_FOR_FREE_NODE(fn__n__) \
    MARK_NODE(fn__n__); \
    UNSNAP_NODE(fn__n__); \
    SET_NEXT_NODE(fn__n__, forwarded_nodes); \
    forwarded_nodes = fn__n__; \
  } \
} while (0)

#define FC_FORWARD_NODE(__n__,__dummy__) FORWARD_NODE(__n__)
#define FORWARD_CHILDREN(__n__) DO_CHILDREN(__n__,FC_FORWARD_NODE, 0)


/* This macro should help localize where a FREESXP node is encountered
   in the GC */
#ifdef PROTECTCHECK
#define CHECK_FOR_FREE_NODE(s) { \
    SEXP cf__n__ = (s); \
    if (TYPEOF(cf__n__) == FREESXP && ! gc_inhibit_release) \
	register_bad_sexp_type(cf__n__, __LINE__); \
}
#else
#define CHECK_FOR_FREE_NODE(s)
#endif


static void mem_err_heap(R_size_t size)
{
    errorcall(R_NilValue, _("vector memory exhausted (limit reached?)"));
}

static void mem_err_cons(void)
{
    errorcall(R_NilValue, _("cons memory exhausted (limit reached?)"));
}

static void mem_err_malloc(R_size_t size)
{
    errorcall(R_NilValue, _("memory exhausted (limit reached?)"));
}


/* Node Allocation - get_free_node.  Initializes sxpinfo to zeros (except 
   node class is set as passed), and ATTRIB to R_NilValue.  Other fields
   are not initialized. 

   The _gc version does a garbage collection first, and reports an error
   if it fails to recover enough for a free node. The _gc1, _gc2, and _gc3
   versions protect 1, 2, or 3 SEXP arguments before the garbage collection. */

static R_INLINE SEXP get_free_node (int c)
{
    /* if (c < 0 || c >= NUM_SMALL_NODE_CLASSES) abort(); */

    SEXP n = R_GenHeap[c].Free;
    if (n == R_GenHeap[c].New) {
        GetNewPage(c); /* guaranteed to provide a free node (or gives error) */
        n = R_GenHeap[c].Free;
    }
    R_GenHeap[c].Free = NEXT_NODE(n);
    R_NodesInUse++;

#if VALGRIND_LEVEL>0
    VALGRIND_MAKE_MEM_UNDEFINED (n, NODE_SIZE(c));
    VALGRIND_MAKE_MEM_DEFINED (&n->gengc_next_node, sizeof(SEXP));
    VALGRIND_MAKE_MEM_DEFINED (&n->gengc_prev_node, sizeof(SEXP));
#endif

    n->sxpinfo = UnmarkedNodeTemplate.sxpinfo;
    SET_NODE_CLASS(n,c);
    ATTRIB(n) = R_NilValue;
    R_SmallNallocSize += NodeClassSize[c];
    return n;
}

#define NO_FREE_NODES() (R_NodesInUse >= R_NSize)

static SEXP get_free_node_gc (int c)
{ 
    R_gc_internal(0);
    if (NO_FREE_NODES()) mem_err_cons(); 
    return get_free_node(c);
}

static SEXP get_free_node_gc1 (int c, SEXP p1)
{ 
    PROTECT(p1);
    R_gc_internal(0);
    if (NO_FREE_NODES()) mem_err_cons(); 
    UNPROTECT(1);
    return get_free_node(c);
}

static SEXP get_free_node_gc2 (int c, SEXP p1, SEXP p2)
{ 
    PROTECT2(p1,p2);
    R_gc_internal(0);
    if (NO_FREE_NODES()) mem_err_cons(); 
    UNPROTECT(2);
    return get_free_node(c);
}

static SEXP get_free_node_gc3 (int c, SEXP p1, SEXP p2, SEXP p3)
{ 
    PROTECT3(p1,p2,p3);
    R_gc_internal(0);
    if (NO_FREE_NODES()) mem_err_cons(); 
    UNPROTECT(3);
    return get_free_node(c);
}


/* Debugging Routines. */

#if DEBUG_GC>1

int CheckStuff(SEXP x, SEXP n, int w, int g)
{
    int err = 0;
    if (x == NULL) {
        REprintf("NULL pointer when looking at children (%lx %d %d)!\n",n,w,g);
        err = 1;
    }
    if (w == 1 && x != NULL && NODE_GENERATION(x) < g && n != R_StringHash) {
	REprintf(
          "untraced old-to-new reference (%lx %lx %d %d"
#if FLAG_OLD_TO_NEW
           "%d %d"
#endif
          "\n", n, x, NODE_GENERATION(x), g 
#if FLAG_OLD_TO_NEW
          , n->gengc_prev_node!=0, x->gengc_prev_node!=0
#endif
        );
        err = 1;
    }
    return err;
}

void DEBUG_CHECK_NODE_COUNTS(char *where)
{
    int i, OldCount, NewCount, OldToNewCount, gen;
    SEXP s;

    if (DEBUG_GC>2) {
        REprintf("%s:\n", where);
        REprintf("Class      New      Old OldToNew    Total\n");
    }
    for (i = 0; i < NUM_NODE_CLASSES; i++) {
	for (s = NEXT_NODE(R_GenHeap[i].New), NewCount = 0;
	     s != R_GenHeap[i].New;
	     s = NEXT_NODE(s)) {
	    NewCount++;
            if (s != NEXT_NODE(PREV_NODE(s)) || s != PREV_NODE(NEXT_NODE(s))) {
                REprintf("Garbled links in New list!\n");
                REprintf(" -- %d %d\n",i,NewCount);
            }
	    if (i != NODE_CLASS(s)) {
		REprintf("Inconsistent class assignment for node (1)!\n");
                REprintf(" -- %s %d %d\n",type2char(TYPEOF(s)),i,NODE_CLASS(s));
            }
            if (NODE_IS_MARKED(s)) {
                REprintf("Node in New list is marked!\n");
                REprintf(" -- %s %d\n",type2char(TYPEOF(s)),i);
            }
#if FLAG_OLD_TO_NEW
            if (s->gengc_prev_node == 0) {
                REprintf("Node in New has zero prev!\n");
                REprintf(" -- %s %d %d\n", type2char(TYPEOF(s)),
                         i, NODE_CLASS(s));
            }
#endif
	}
	for (gen = 0, OldCount = 0, OldToNewCount = 0;
	     gen < NUM_OLD_GENERATIONS;
	     gen++) {
	    for (s = NEXT_NODE(R_GenHeap[i].Old[gen]);
		 s != R_GenHeap[i].Old[gen];
		 s = NEXT_NODE(s)) {
		OldCount++;
                if (s!=NEXT_NODE(PREV_NODE(s)) || s!=PREV_NODE(NEXT_NODE(s))) {
                    REprintf("Garbled links in Old list!\n");
                    REprintf(" -- %d %d %d\n",i,gen,OldCount);
                }
#if FLAG_OLD_TO_NEW
                if (s->gengc_prev_node == 0) {
		    REprintf("Node in Old has zero prev!\n");
                    REprintf(" -- %s %d %d\n", type2char(TYPEOF(s)),
                             i, NODE_CLASS(s));
                }
#endif
		if (i != NODE_CLASS(s)) {
		    REprintf("Inconsistent class assignment for node (2)!\n");
                    REprintf(" -- %s %d %d\n", type2char(TYPEOF(s)),
                             i, NODE_CLASS(s));
                }
		if (gen != NODE_GENERATION(s)) {
		    REprintf("Inconsistent node generation (1)!\n");
                    REprintf(" -- %s %d %d\n", type2char(TYPEOF(s)),
                             gen, NODE_GENERATION(s));
                }
                if (!NODE_IS_MARKED(s)) {
                    REprintf("Node in Old list is unmarked!\n");
                    REprintf(" -- %s %d %d\n",type2char(TYPEOF(s)),i,gen);
                }
		DO_CHILDREN_DEBUG(s, CheckStuff, 1, gen);
	    }
	    for (s = NEXT_NODE(R_GenHeap[i].OldToNew[gen]);
		 s != R_GenHeap[i].OldToNew[gen];
		 s = NEXT_NODE(s)) {
		OldToNewCount++;
                if (s!=NEXT_NODE(PREV_NODE(s)) || s!=PREV_NODE(NEXT_NODE(s))) {
                    REprintf("Garbled links in OldToNew list!\n");
                    REprintf(" -- %d %d %d\n",i,gen,OldToNewCount);
                }
#if FLAG_OLD_TO_NEW
                if (s->gengc_prev_node != 0) {
		    REprintf("Node in OldToNew does not have zero prev!\n");
                    REprintf(" -- %s %d %d\n", type2char(TYPEOF(s)),
                             i, NODE_CLASS(s));
                }
#endif
		if (i != NODE_CLASS(s)) {
		    REprintf("Inconsistent class assignment for node (3)!\n");
                    REprintf(" -- %s %d %d\n", type2char(TYPEOF(s)),
                             i, NODE_CLASS(s));
                }
		if (gen != NODE_GENERATION(s)) {
		    REprintf("Inconsistent node generation (2)!\n");
                    REprintf(" -- %s %d %d\n", type2char(TYPEOF(s)),
                             gen, NODE_GENERATION(s));
                }
                if (!NODE_IS_MARKED(s)) {
                    REprintf("Node in OldToNew list is unmarked!\n");
                    REprintf(" -- %s %d %d\n",type2char(TYPEOF(s)),i,gen);
                }
		DO_CHILDREN_DEBUG(s, CheckStuff, 2, gen);
	    }
	}
        if (DEBUG_GC>2) {
            REprintf ("  %1d   %8d %8d %8d %8d\n",
                       i, NewCount, OldCount, OldToNewCount,
                       NewCount + OldCount + OldToNewCount);
        }
    }
}
#else
#define DEBUG_CHECK_NODE_COUNTS(s)
#endif /* DEBUG_GC>1 */

#if DEBUG_GC>0

#define DEBUG_TABLE 1  /* 1 to get a full table of counts in gc info printed */

static unsigned int old_to_new_count = 0;

static void DEBUG_GC_SUMMARY(int gclev)
{
    int i, gen, OldCount, total;
#if DEBUG_TABLE
    int count[NUM_NODE_CLASSES][NUM_OLD_GENERATIONS][32];
    int t;
#endif
    REprintf("\nGC at level %d, VSize = %lu + %lu = %lu\nClass counts -", 
             gclev, 0 /* R_SmallVallocSize */, R_LargeVallocSize,
	     /* R_SmallVallocSize + */ R_LargeVallocSize);
    total = 0;
    for (i = 0; i < NUM_NODE_CLASSES; i++) {
	for (gen = 0, OldCount = 0; gen < NUM_OLD_GENERATIONS; gen++) {
	    OldCount += R_GenHeap[i].OldCount[gen];
#if DEBUG_TABLE
            for (t = 0; t < 32; t++) count[i][gen][t] = 0;
            for (SEXP s = NEXT_NODE(R_GenHeap[i].Old[gen]);
                 s != R_GenHeap[i].Old[gen]; s = NEXT_NODE(s)) 
                count[i][gen][TYPEOF(s)] += 1;
#endif
        }
	REprintf(" %d:%d", i, OldCount);
        total += OldCount;
    }
    REprintf(" (total %lu)\n", total);
    if (R_NodesInUse != total) 
        REprintf("Total != R_NodesInUse (%lu) !!\n",R_NodesInUse);
#if DEBUG_TABLE
    REprintf("          ");
    for (i = 0; i < NUM_NODE_CLASSES; i++) REprintf("  class%d",i);
    REprintf("\n");
    for (t = 0; t < 32; t++) {
        REprintf("%10s",sexptype2char(t));
        for (i = 0; i < NUM_NODE_CLASSES; i++) REprintf("%8d",count[i][0][t]);
        REprintf("\n");
        for (gen = 1; gen < NUM_OLD_GENERATIONS; gen++) {
            REprintf("          ");
            for (i = 0; i < NUM_NODE_CLASSES; i++) REprintf("%8d",count[i][gen][t]);
            REprintf("\n");
        }
    }
#endif
    total = 0;
    for (SEXP s = R_PreciousList; s != R_NilValue; s = CDR(s))
        total += 1;
    REprintf("Number of objects on precious list: %d\n",total);
    REprintf("Old-to-new since last report: %u\n",old_to_new_count);
    old_to_new_count = 0;
}
#else
#define DEBUG_GC_SUMMARY(x)
#endif /* DEBUG_GC>0 */

#if DEBUG_ADJUST_HEAP
static void DEBUG_ADJUST_HEAP_PRINT(double node_occup, double vect_occup)
{
    int i;
    R_size_t alloc;
    REprintf("Node occupancy: %.0f%%\nVector occupancy: %.0f%%\n",
	     100.0 * node_occup, 100.0 * vect_occup);
    alloc = R_LargeVallocSize +
	sizeof(SEXPREC_ALIGN) * R_GenHeap[LARGE_NODE_CLASS].AllocCount;
    for (i = 0; i < NUM_SMALL_NODE_CLASSES; i++)
	alloc += PAGE_SIZE * R_GenHeap[i].PageCount;
    REprintf("Total allocation: %lu\n", alloc);
    REprintf("Ncells %lu\nVcells %lu\n", R_NSize, R_VSize);
}
#else
#define DEBUG_ADJUST_HEAP_PRINT(node_occup, vect_occup)
#endif /* DEBUG_ADJUST_HEAP */

#if DEBUG_RELEASE
static void DEBUG_RELEASE_PRINT(int rel_pages, int rel_count, int maxrel, int i)
{
    if (maxrel > 0) {
	int gen, n;
	REprintf(
         "Class: %d, pages = %d, maxrel = %d, pages release = %d, nodes released = %d\n",
		 i, R_GenHeap[i].PageCount, maxrel, rel_pages, rel_count);
	for (gen = 0, n = 0; gen < NUM_OLD_GENERATIONS; gen++)
	    n += R_GenHeap[i].OldCount[gen];
	REprintf("Allocated = %d, in use = %d\n", R_GenHeap[i].AllocCount, n);
    }
}
#else
#define DEBUG_RELEASE_PRINT(rel_pages, rel_count, maxrel, i)
#endif /* DEBUG_RELEASE */


/* Page Allocation and Release. */

static void GetNewPage(int node_class)
{
    SEXP s;
    char *data;
    PAGE_HEADER *page;
    int node_size, page_count, i;

    page = malloc(PAGE_SIZE+PAGE_PAD);
    if (page == NULL) {
	R_gc_full(0);
	page = malloc(PAGE_SIZE+PAGE_PAD);
	if (page == NULL)
	    mem_err_malloc((R_size_t) PAGE_SIZE);
    }

    node_size = NODE_SIZE(node_class);
    page_count = PAGE_COUNT(page,node_size);

    if (R_IsMemReporting) R_ReportNewPage();
    page->next = R_GenHeap[node_class].pages;
    R_GenHeap[node_class].pages = page;
    R_GenHeap[node_class].PageCount++;

    /* Put nodes in page into "New", used by "Free".  Make sure they will be 
       allocated in increasing order of address, for cache efficiency. */

    SEXP base = R_GenHeap[node_class].New;
    for (i = 0, data = PAGE_DATA(page); i<page_count; i++, data += node_size) {
	s = (SEXP) data;
        s->sxpinfo = UnmarkedNodeTemplate.sxpinfo;
	SET_NODE_CLASS(s, node_class);
#ifdef PROTECTCHECK
	TYPEOF(s) = NEWSXP;
#endif
	SNAP_NODE(s, base);
        if (i == 0) R_GenHeap[node_class].Free = s;
    }

    if ((uintptr_t)data > (uintptr_t)page + PAGE_SIZE) abort();

    R_GenHeap[node_class].AllocCount += page_count;

#if VALGRIND_LEVEL>0
    VALGRIND_MAKE_MEM_NOACCESS (page, PAGE_SIZE+PAGE_PAD);
    VALGRIND_MAKE_MEM_DEFINED (&page->next, sizeof page->next);
    for (i = 0, data = PAGE_DATA(page); i<page_count; i++, data += node_size) {
	s = (SEXP) data;
        VALGRIND_MAKE_MEM_DEFINED (&s->gengc_next_node, sizeof(SEXP));
        VALGRIND_MAKE_MEM_DEFINED (&s->gengc_prev_node, sizeof(SEXP));
        VALGRIND_MAKE_MEM_DEFINED (&s->sxpinfo, sizeof s->sxpinfo);
    }
#endif

    if (R_GenHeap[node_class].Free == R_GenHeap[node_class].New)
        abort(); /* Shouldn't be possible, since page_count should be > 0 */
}

/* Scan pages, releasing (some) pages with no nodes in use, and (maybe)
   sorting nodes in increasing order by address (in an attempt to improve 
   locality of reference), putting unused nodes in the New list, and nodes
   in use in the Old list for their generation.  Releasing and sorting in 
   one scan gives better cache / virtual memory performance. */

static void release_or_sort_pages (int sort)
{
    for (int i = 0; i < NUM_SMALL_NODE_CLASSES; i++) {
        int node_size = NODE_SIZE(i);
        R_size_t maxrel, rel_count;
        PAGE_HEADER *page, *last;
        SEXP *old, new;
        int rel_pages;

        maxrel = R_GenHeap[i].AllocCount;
        for (int gen = 0; gen < NUM_OLD_GENERATIONS; gen++)
            maxrel -= (1.0 + R_MaxKeepFrac) * R_GenHeap[i].OldCount[gen];

        /* all nodes in New space should be both free and unmarked */
        rel_count = 0;
        rel_pages = 0;
        last = NULL;
        page = R_GenHeap[i].pages;

        if (sort) {
            old = R_GenHeap[i].Old;
            for (int g = 0; g < NUM_OLD_GENERATIONS; g++) 
                MAKE_EMPTY(old[g]);
            new = R_GenHeap[i].New;
            MAKE_EMPTY(new);
        }

        while (page != NULL && (sort || rel_count < maxrel)) {
            int page_count = PAGE_COUNT(page,node_size);
            PAGE_HEADER *next = page->next;
            char *data;
            int j;
            if (rel_count < maxrel) { /* want to release more */
                for (j = 0, data = PAGE_DATA(page);
                     j < page_count && !NODE_IS_MARKED((SEXP)data); 
                     j++, data += node_size) ;
                if (j == page_count) { /* no marked nodes - free this page */
                    if (!sort) {
                        for (j = 0, data = PAGE_DATA(page); 
                             j < page_count; 
                             j++, data += node_size) 
                            UNSNAP_NODE((SEXP)data);
                    }
                    R_GenHeap[i].AllocCount -= page_count;
                    R_GenHeap[i].PageCount--;
                    rel_pages += 1;
                    rel_count += page_count;
                    free(page);
                    page = next;
                    if (last == NULL)
                        R_GenHeap[i].pages = page;
                    else
                        last->next = page;
                    continue;
                }
            }
            if (sort) {
                for (j = 0, data = PAGE_DATA(page);
                     j < page_count; 
                     j++, data += node_size) {
                    if (NODE_IS_MARKED((SEXP)data)) 
                        SNAP_NODE((SEXP)data, old[NODE_GENERATION((SEXP)data)]);
                    else
                        SNAP_NODE((SEXP)data, new);
                }
            }
            last = page;
            page = next;
        }

        DEBUG_RELEASE_PRINT(rel_pages, rel_count, maxrel, i);
    }
}

/* compute size in VEC units so result will fit in LENGTH field for FREESXPs */
static R_INLINE R_size_t getVecSizeInVEC(SEXP s)
{
    R_size_t size;
    switch (TYPEOF(s)) {	/* get size in bytes */
    case CHARSXP:
	size = LENGTH(s) + 1;
	break;
    case RAWSXP:
	size = LENGTH(s);
	break;
    case LGLSXP:
    case INTSXP:
	size = LENGTH(s) * sizeof(int);
	break;
    case REALSXP:
	size = LENGTH(s) * sizeof(double);
	break;
    case CPLXSXP:
	size = LENGTH(s) * sizeof(Rcomplex);
	break;
    case STRSXP:
    case EXPRSXP:
    case VECSXP:
	size = LENGTH(s) * sizeof(SEXP);
	break;
    default:
	register_bad_sexp_type(s, __LINE__);
	size = 0;
    }
    return BYTE2VEC(size);
}

static void ReleaseLargeFreeVectors(void)
{
    SEXP s = NEXT_NODE(R_GenHeap[LARGE_NODE_CLASS].New);

    while (s != R_GenHeap[LARGE_NODE_CLASS].New) {
	SEXP next = NEXT_NODE(s);
        R_size_t size;
#ifdef PROTECTCHECK
        if (TYPEOF(s) == FREESXP)
            size = LENGTH(s);
        else
            /* should not get here -- arrange for a warning/error? */
            size = getVecSizeInVEC(s);
#else
        size = getVecSizeInVEC(s);
#endif
        R_LargeVallocSize -= size;
        R_GenHeap[LARGE_NODE_CLASS].AllocCount--;
        free(s);
        s = next;
    }

    MAKE_EMPTY (R_GenHeap[LARGE_NODE_CLASS].New);
}


/* Heap Size Adjustment. */

static void AdjustHeapSize(R_size_t size_needed)
{
    R_size_t R_MinNFree = orig_R_NSize * R_MinFreeFrac;
    R_size_t R_MinVFree = orig_R_VSize * R_MinFreeFrac;
    R_size_t NNeeded = R_NodesInUse + R_MinNFree;
    R_size_t VNeeded = /* R_SmallVallocSize + */ R_LargeVallocSize
	+ size_needed + R_MinVFree;
    double node_occup = ((double) NNeeded) / R_NSize;
    double vect_occup =	((double) VNeeded) / R_VSize;

    if (node_occup > R_NGrowFrac) {
	R_size_t change = R_NGrowIncrMin + R_NGrowIncrFrac * R_NSize;
	if (R_MaxNSize >= R_NSize + change)
	    R_NSize += change;
    }
    else if (node_occup < R_NShrinkFrac) {
	R_NSize -= (R_NShrinkIncrMin + R_NShrinkIncrFrac * R_NSize);
	if (R_NSize < NNeeded)
	    R_NSize = (NNeeded < R_MaxNSize) ? NNeeded: R_MaxNSize;
	if (R_NSize < orig_R_NSize)
	    R_NSize = orig_R_NSize;
    }

    if (vect_occup > 1.0 && VNeeded < R_MaxVSize)
	R_VSize = VNeeded;
    if (vect_occup > R_VGrowFrac) {
	R_size_t change = R_VGrowIncrMin + R_VGrowIncrFrac * R_VSize;
	if (R_MaxVSize - R_VSize >= change)
	    R_VSize += change;
    }
    else if (vect_occup < R_VShrinkFrac) {
	R_VSize -= R_VShrinkIncrMin + R_VShrinkIncrFrac * R_VSize;
	if (R_VSize < VNeeded)
	    R_VSize = VNeeded;
	if (R_VSize < orig_R_VSize)
	    R_VSize = orig_R_VSize;
    }

    DEBUG_ADJUST_HEAP_PRINT(node_occup, vect_occup);
}


/* Managing Old-to-New References.  Tolerates s being zero (incorrectly) if
   TOLERATE_NULL is set to 1. */

#define AGE_NODE(s,g) do { \
  SEXP an__n__ = (s); \
  int an__g__ = (g); \
  if ((!TOLERATE_NULL || an__n__) && NODE_GEN_IS_YOUNGER(an__n__, an__g__)) { \
    if (NODE_IS_MARKED(an__n__)) \
       R_GenHeap[NODE_CLASS(an__n__)].OldCount[NODE_GENERATION(an__n__)]--; \
    else \
      MARK_NODE(an__n__); \
    SET_NODE_GENERATION(an__n__, an__g__); \
    UNSNAP_NODE(an__n__); \
    SET_NEXT_NODE(an__n__, forwarded_nodes); \
    forwarded_nodes = an__n__; \
  } \
} while (0)

static void AgeNodeAndChildren(SEXP s, int gen)
{
    SEXP forwarded_nodes = NULL;
    AGE_NODE(s, gen);
    while (forwarded_nodes != NULL) {
	s = forwarded_nodes;
	forwarded_nodes = NEXT_NODE(forwarded_nodes);
	if (NODE_GENERATION(s) != gen)
	    REprintf("****snapping into wrong generation\n");
	SNAP_NODE(s, R_GenHeap[NODE_CLASS(s)].Old[gen]);
	R_GenHeap[NODE_CLASS(s)].OldCount[gen]++;
	DO_CHILDREN(s, AGE_NODE, gen);
    }
}

static void old_to_new(SEXP x, SEXP y)
{
#if EXPEL_OLD_TO_NEW
    AgeNodeAndChildren(y, NODE_GENERATION(x));
#else
    UNSNAP_NODE(x);
#if FLAG_OLD_TO_NEW
    SEXP h = R_GenHeap[NODE_CLASS(x)].OldToNew[NODE_GENERATION(x)];
    SET_NEXT_NODE(x,NEXT_NODE(h));
    SET_NEXT_NODE(h,x);
    x->gengc_prev_node = 0;
#else
    SNAP_NODE(x, R_GenHeap[NODE_CLASS(x)].OldToNew[NODE_GENERATION(x)]);
#endif
#endif

#if DEBUG_GC>0
    old_to_new_count += 1;
#endif
}

#if EXPEL_OLD_TO_NEW
#define CHECK_OLD_TO_NEW(x,y) do { \
  if (NODE_IS_MARKED(CHK(x)) \
       && (!NODE_IS_MARKED(y) || NODE_GENERATION(x) > NODE_GENERATION(y))) \
      old_to_new(x,y); \
  } while (0)
#else
#if FLAG_OLD_TO_NEW
#define CHECK_OLD_TO_NEW(x,y) do { \
  if (NODE_IS_MARKED(CHK(x)) \
   && (x)->gengc_prev_node != 0 \
   && (!NODE_IS_MARKED(CHK(y)) || NODE_GENERATION(x) > NODE_GENERATION(y))) \
      old_to_new(x,y); \
  } while (0)
#else
#define CHECK_OLD_TO_NEW(x,y) do { \
  if (NODE_IS_MARKED(CHK(x)) \
   && (!NODE_IS_MARKED(CHK(y)) || NODE_GENERATION(x) > NODE_GENERATION(y))) \
      old_to_new(x,y); \
  } while (0)
#endif
#endif


/* Finalization and Weak References */

/* The design of this mechanism is very close to the one described in
   "Stretching the storage manager: weak pointers and stable names in
   Haskell" by Peyton Jones, Marlow, and Elliott (at
   www.research.microsoft.com/Users/simonpj/papers/weak.ps.gz). --LT */

static SEXP R_weak_refs = NULL;

#define READY_TO_FINALIZE_MASK 1

#define SET_READY_TO_FINALIZE(s) ((s)->sxpinfo.gp |= READY_TO_FINALIZE_MASK)
#define CLEAR_READY_TO_FINALIZE(s) ((s)->sxpinfo.gp &= ~READY_TO_FINALIZE_MASK)
#define IS_READY_TO_FINALIZE(s) ((s)->sxpinfo.gp & READY_TO_FINALIZE_MASK)

#define FINALIZE_ON_EXIT_MASK 2

#define SET_FINALIZE_ON_EXIT(s) ((s)->sxpinfo.gp |= FINALIZE_ON_EXIT_MASK)
#define CLEAR_FINALIZE_ON_EXIT(s) ((s)->sxpinfo.gp &= ~FINALIZE_ON_EXIT_MASK)
#define FINALIZE_ON_EXIT(s) ((s)->sxpinfo.gp & FINALIZE_ON_EXIT_MASK)

#define WEAKREF_SIZE 4
#define WEAKREF_KEY(w) VECTOR_ELT(w, 0)
#define SET_WEAKREF_KEY(w, k) SET_VECTOR_ELT(w, 0, k)
#define WEAKREF_VALUE(w) VECTOR_ELT(w, 1)
#define SET_WEAKREF_VALUE(w, v) SET_VECTOR_ELT(w, 1, v)
#define WEAKREF_FINALIZER(w) VECTOR_ELT(w, 2)
#define SET_WEAKREF_FINALIZER(w, f) SET_VECTOR_ELT(w, 2, f)
#define WEAKREF_NEXT(w) VECTOR_ELT(w, 3)
#define SET_WEAKREF_NEXT(w, n) SET_VECTOR_ELT(w, 3, n)

static SEXP MakeCFinalizer(R_CFinalizer_t cfun);

static SEXP NewWeakRef(SEXP key, SEXP val, SEXP fin, Rboolean onexit)
{
    SEXP w;

    switch (TYPEOF(key)) {
    case NILSXP:
    case ENVSXP:
    case EXTPTRSXP:
	break;
    default: error(_("can only weakly reference/finalize reference objects"));
    }

    PROTECT2 (key, fin);
    PROTECT (val = NAMEDCNT_GT_0(val) ? duplicate(val) : val);

    w = allocVector(VECSXP, WEAKREF_SIZE);
    SET_TYPEOF(w, WEAKREFSXP);
    if (key != R_NilValue) {
	/* If the key is R_NilValue we don't register the weak reference.
	   This is used in loading saved images. */
	SET_WEAKREF_KEY(w, key);
	SET_WEAKREF_VALUE(w, val);
	SET_WEAKREF_FINALIZER(w, fin);
	SET_WEAKREF_NEXT(w, R_weak_refs);
	CLEAR_READY_TO_FINALIZE(w);
	if (onexit)
	    SET_FINALIZE_ON_EXIT(w);
	else
	    CLEAR_FINALIZE_ON_EXIT(w);
	R_weak_refs = w;
    }
    UNPROTECT(3);
    return w;
}

SEXP R_MakeWeakRef(SEXP key, SEXP val, SEXP fin, Rboolean onexit)
{
    switch (TYPEOF(fin)) {
    case NILSXP:
    case CLOSXP:
    case BUILTINSXP:
    case SPECIALSXP:
	break;
    default: error(_("finalizer must be a function or NULL"));
    }
    return NewWeakRef(key, val, fin, onexit);
}

SEXP R_MakeWeakRefC(SEXP key, SEXP val, R_CFinalizer_t fin, Rboolean onexit)
{
    SEXP w;
    PROTECT2 (key, val);
    w = NewWeakRef(key, val, MakeCFinalizer(fin), onexit);
    UNPROTECT(2);
    return w;
}

static void CheckFinalizers(void)
{
    SEXP s;
    for (s = R_weak_refs; s != R_NilValue; s = WEAKREF_NEXT(s))
	if (! NODE_IS_MARKED(WEAKREF_KEY(s)) && ! IS_READY_TO_FINALIZE(s))
	    SET_READY_TO_FINALIZE(s);
}

/* C finalizers are stored in a CHARSXP.  It would be nice if we could
   use EXTPTRSXP's but these only hold a void *, and function pointers
   are not guaranteed to be compatible with a void *.  There should be
   a cleaner way of doing this, but this will do for now. --LT */
/* Changed to RAWSXP in 2.8.0 */
static Rboolean isCFinalizer(SEXP fun)
{
    return TYPEOF(fun) == RAWSXP;
    /*return TYPEOF(fun) == EXTPTRSXP;*/
}

static SEXP MakeCFinalizer(R_CFinalizer_t cfun)
{
    SEXP s = allocVector(RAWSXP, sizeof(R_CFinalizer_t));
    *((R_CFinalizer_t *) RAW(s)) = cfun;
    return s;
    /*return R_MakeExternalPtr((void *) cfun, R_NilValue, R_NilValue);*/
}

static R_CFinalizer_t GetCFinalizer(SEXP fun)
{
    return *((R_CFinalizer_t *) RAW(fun));
    /*return (R_CFinalizer_t) R_ExternalPtrAddr(fun);*/
}

SEXP R_WeakRefKey(SEXP w)
{
    if (TYPEOF(w) != WEAKREFSXP)
	error(_("not a weak reference"));
    return WEAKREF_KEY(w);
}

SEXP R_WeakRefValue(SEXP w)
{
    SEXP v;
    if (TYPEOF(w) != WEAKREFSXP)
	error(_("not a weak reference"));
    v = WEAKREF_VALUE(w);
    if (v!=R_NilValue) 
        SET_NAMEDCNT_MAX(v);
    return v;
}

void R_RunWeakRefFinalizer(SEXP w)
{
    SEXP key, fun, e;
    if (TYPEOF(w) != WEAKREFSXP)
	error(_("not a weak reference"));
    key = WEAKREF_KEY(w);
    fun = WEAKREF_FINALIZER(w);
    SET_WEAKREF_KEY(w, R_NilValue);
    SET_WEAKREF_VALUE(w, R_NilValue);
    SET_WEAKREF_FINALIZER(w, R_NilValue);
    if (! IS_READY_TO_FINALIZE(w))
	SET_READY_TO_FINALIZE(w); /* insures removal from list on next gc */
    PROTECT2 (key, fun);
    if (isCFinalizer(fun)) {
	/* Must be a C finalizer. */
	R_CFinalizer_t cfun = GetCFinalizer(fun);
	cfun(key);
    }
    else if (fun != R_NilValue) {
	/* An R finalizer. */
	PROTECT(e = LCONS(fun, LCONS(key, R_NilValue)));
	eval(e, R_GlobalEnv);
	UNPROTECT(1);
    }
    UNPROTECT(2);
}

static Rboolean RunFinalizers(void)
{
    volatile SEXP s, last;
    volatile Rboolean finalizer_run = FALSE;

    for (s = R_weak_refs, last = R_NilValue; s != R_NilValue;) {
	SEXP next = WEAKREF_NEXT(s);
	if (IS_READY_TO_FINALIZE(s)) {
	    RCNTXT thiscontext;
	    RCNTXT * volatile saveToplevelContext;
	    volatile int savestack;
	    volatile SEXP topExp;

	    finalizer_run = TRUE;

	    /* A top level context is established for the finalizer to
	       insure that any errors that might occur do not spill
	       into the call that triggered the collection. */
	    begincontext(&thiscontext, CTXT_TOPLEVEL, R_NilValue, R_GlobalEnv,
			 R_BaseEnv, R_NilValue, R_NilValue);
	    saveToplevelContext = R_ToplevelContext;
	    PROTECT(topExp = R_CurrentExpr);
	    savestack = R_PPStackTop;
	    if (! SETJMP(thiscontext.cjmpbuf)) {
		R_GlobalContext = R_ToplevelContext = &thiscontext;

		/* The entry in the weak reference list is removed
		   before running the finalizer.  This insures that a
		   finalizer is run only once, even if running it
		   raises an error. */
		if (last == R_NilValue)
		    R_weak_refs = next;
		else
		    SET_WEAKREF_NEXT(last, next);
		/* The value of 'next' is protected to make is safe
		   for thsis routine to be called recursively from a
		   gc triggered by a finalizer. */
		PROTECT(next);
		R_RunWeakRefFinalizer(s);
		UNPROTECT(1);
	    }
	    endcontext(&thiscontext);
	    R_ToplevelContext = saveToplevelContext;
	    R_PPStackTop = savestack;
	    R_CurrentExpr = topExp;
	    UNPROTECT(1);
	}
	else last = s;
	s = next;
    }
    return finalizer_run;
}

void R_RunExitFinalizers(void)
{
    SEXP s;

    for (s = R_weak_refs; s != R_NilValue; s = WEAKREF_NEXT(s))
	if (FINALIZE_ON_EXIT(s))
	    SET_READY_TO_FINALIZE(s);
    RunFinalizers();
}

void R_RegisterFinalizerEx(SEXP s, SEXP fun, Rboolean onexit)
{
    R_MakeWeakRef(s, R_NilValue, fun, onexit);
}

void R_RegisterFinalizer(SEXP s, SEXP fun)
{
    R_RegisterFinalizerEx(s, fun, FALSE);
}

void R_RegisterCFinalizerEx(SEXP s, R_CFinalizer_t fun, Rboolean onexit)
{
    R_MakeWeakRefC(s, R_NilValue, fun, onexit);
}

void R_RegisterCFinalizer(SEXP s, R_CFinalizer_t fun)
{
    R_RegisterCFinalizerEx(s, fun, FALSE);
}

/* R interface function */

static SEXP do_regFinaliz(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int onexit;

    checkArity(op, args);

    if (TYPEOF(CAR(args)) != ENVSXP && TYPEOF(CAR(args)) != EXTPTRSXP)
	error(_("first argument must be environment or external pointer"));
    if (TYPEOF(CADR(args)) != CLOSXP)
	error(_("second argument must be a function"));

    onexit = asLogical(CADDR(args));
    if(onexit == NA_LOGICAL)
	error(_("third argument must be 'TRUE' or 'FALSE'"));

    R_RegisterFinalizerEx(CAR(args), CADR(args), onexit);
    return R_NilValue;
}


/* THE GENERATIONAL GARBAGE COLLECTOR. */

/* Take nodes from forwarded list, forward their children, and snap
   into the Old list for their class - except snapping isn't done (apart
   from large nodes) if no_snap is 1 (which is set when small nodes will 
   later be scanned sequentially as part of a full collection). */

static void process_nodes (SEXP forwarded_nodes, int no_snap)
{
    if (forwarded_nodes == NULL) return;

    SEXP s = forwarded_nodes;
    int cl = NODE_CLASS(s);

    for (;;) {
        struct gen_heap *h = &R_GenHeap[cl];
        do {
            int gn = NODE_GENERATION(s);
            h->OldCount[gn] += 1;
            forwarded_nodes = NEXT_NODE(s);
            if (!no_snap || cl == LARGE_NODE_CLASS) 
                SNAP_NODE(s, h->Old[gn]);
            FORWARD_CHILDREN(s);
            if (forwarded_nodes == NULL)
                return;
            s = forwarded_nodes;
        } while (NODE_CLASS(s) == cl);
        cl = NODE_CLASS(s);
    }
}

#if !EXPEL_OLD_TO_NEW
/* Eliminate old-to-new references in generations to collect by
   transferring referenced nodes to referring generation.  The outer
   loop must be over generations, in increasing order - otherwise
   unsnapping a child might unsnap it from an OldToNew list before
   its children are looked at. */

static void transfer_old_to_new (int num_old_gens_to_collect)
{   int gen, i;
    SEXP s;
    for (gen = 0; gen < num_old_gens_to_collect; gen++) {
	for (i = 0; i < NUM_NODE_CLASSES; i++) {
	    s = NEXT_NODE(R_GenHeap[i].OldToNew[gen]);
	    while (s != R_GenHeap[i].OldToNew[gen]) {
		SEXP next = NEXT_NODE(s);
		DO_CHILDREN(s, AgeNodeAndChildren, gen);
		if (NODE_GENERATION(s) != gen)
		    REprintf("****snapping into wrong generation\n");
		SNAP_NODE(s, R_GenHeap[i].Old[gen]);
		s = next;
	    }
	    MAKE_EMPTY(s);
	}
    }
}
#endif

/* Unmark all marked nodes in old generations to be collected and move to 
   New space */

void unmark_and_move_to_new (int num_old_gens_to_collect)
{   int gen, i;
    for (gen = 0; gen < num_old_gens_to_collect; gen++) {
        for (i = 0; i < NUM_NODE_CLASSES; i++) {
            SEXP peg = R_GenHeap[i].Old[gen];
            SEXP s = NEXT_NODE(peg);
            R_GenHeap[i].OldCount[gen] = 0;
            if (s == peg) continue;
            if (gen != NUM_OLD_GENERATIONS - 1) {
                do {
                    SET_NODE_GENERATION (s, gen+1);
                    UNMARK_NODE(s);
                    s = NEXT_NODE(s);
                } while (s != peg);
            }
            else {
                do {
                    UNMARK_NODE(s);
                    s = NEXT_NODE(s);
                } while (s != peg);
            }
            BULK_MOVE(peg, R_GenHeap[i].New);
        }
    }
}

static void RunGenCollect(R_size_t size_needed)
{
    int i, gen, gens_collected;
    RCNTXT *ctxt;
    SEXP s;
    SEXP forwarded_nodes;

    bad_sexp_type_seen = 0;

    /* determine number of generations to collect */
    while (num_old_gens_to_collect < NUM_OLD_GENERATIONS) {
	if (collect_counts[num_old_gens_to_collect]-- <= 0) {
	    collect_counts[num_old_gens_to_collect] =
		collect_counts_max[num_old_gens_to_collect];
	    num_old_gens_to_collect++;
	}
	else break;
    }

#ifdef PROTECTCHECK
    num_old_gens_to_collect = NUM_OLD_GENERATIONS;
#endif

 again:
    gens_collected = num_old_gens_to_collect;

    int no_snap =
      SORT_NODES && num_old_gens_to_collect == NUM_OLD_GENERATIONS;

#if DEBUG_GC>1
    char mess[20];
    sprintf(mess,"START OF GC (%d)",num_old_gens_to_collect);
    DEBUG_CHECK_NODE_COUNTS(mess);
#endif

#if !EXPEL_OLD_TO_NEW
    transfer_old_to_new (num_old_gens_to_collect);
    DEBUG_CHECK_NODE_COUNTS("after transfering old to new");
#endif

    unmark_and_move_to_new (num_old_gens_to_collect);

    DEBUG_CHECK_NODE_COUNTS("after unmarking and moving to new");

    forwarded_nodes = NULL;

#if !EXPEL_OLD_TO_NEW
    /* scan nodes in uncollected old generations with old-to-new pointers */
    for (gen = num_old_gens_to_collect; gen < NUM_OLD_GENERATIONS; gen++)
	for (i = 0; i < NUM_NODE_CLASSES; i++)
	    for (s = NEXT_NODE(R_GenHeap[i].OldToNew[gen]);
		 s != R_GenHeap[i].OldToNew[gen];
		 s = NEXT_NODE(s))
		FORWARD_CHILDREN(s);
    DEBUG_CHECK_NODE_COUNTS("after scanning uncollected old generations");
#endif

    /* forward all roots */

    static SEXP *root_vars[] = { 
        &NA_STRING,	          /* Builtin constants */
        &R_BlankString,
        &R_RestartToken,
        &R_MissingArg,

        &R_GlobalEnv,	          /* Global environment */
        &R_BaseEnv,
        &R_Warnings,	          /* Warnings, if any */

        &R_HandlerStack,          /* Condition handler stack */
        &R_RestartStack,          /* Available restarts stack */

        &R_PreciousList,
        0
    };

    for (i = 0; root_vars[i] != 0; i++)
        FORWARD_NODE(*root_vars[i]);

    if (R_VStack != NULL)
        FORWARD_NODE(R_VStack);

    if (R_SymbolTable != NULL) /* Symbol table, could be NULL during startup */
        for (i = 0; i < HSIZE; i++)
            FORWARD_NODE(R_SymbolTable[i]);

    if (R_CurrentExpr != NULL)	           /* Current expression */
	FORWARD_NODE(R_CurrentExpr);

    for (i = 0; i < R_MaxDevices; i++) {   /* Device display lists */
	pGEDevDesc gdd = GEgetDevice(i);
	if (gdd) {
	    if (gdd->displayList != NULL)
                FORWARD_NODE(gdd->displayList);
	    if (gdd->savedSnapshot != NULL)
                FORWARD_NODE(gdd->savedSnapshot);
	    if (gdd->dev != NULL && gdd->dev->eventEnv != NULL)
	    	FORWARD_NODE(gdd->dev->eventEnv);
	}
    }

    for (ctxt = R_GlobalContext ; ctxt != NULL ; ctxt = ctxt->nextcontext) {
        SEXP *cntxt_ptrs[] = { /* using a run-time initialized table might be
                                  slower, but is certainly more compact */
	    &ctxt->conexit,       /* on.exit expressions */
	    &ctxt->promargs,	  /* promises supplied to closure */
	    &ctxt->callfun,       /* the closure called */
	    &ctxt->sysparent,     /* calling environment */
	    &ctxt->call,          /* the call */
	    &ctxt->cloenv,        /* the closure environment */
	    &ctxt->handlerstack,  /* the condition handler stack */
	    &ctxt->restartstack,  /* the available restarts stack */
	    &ctxt->srcref,	  /* the current source reference */
            0
        };
        for (i = 0; cntxt_ptrs[i] != 0; i++)
            FORWARD_NODE(*cntxt_ptrs[i]);
    }

    if (framenames != NULL)		   /* used for interprocedure    */
        FORWARD_NODE(framenames);	   /*   communication in model.c */

    for (i = 0; i < R_PPStackTop; i++)	   /* Protected pointers */
        if (R_PPStack[i] != NULL) 
            FORWARD_NODE(R_PPStack[i]);

    for (SEXP *sp = R_BCNodeStackBase; sp<R_BCNodeStackTop; sp++) /* Byte code stack */
        FORWARD_NODE(*sp);


    /* MAIN PROCESSING STEP.  Marks most nodes that are in use. */

    process_nodes (forwarded_nodes, no_snap); 
    forwarded_nodes = NULL;

    DEBUG_CHECK_NODE_COUNTS("after processing forwarded list (1)");


    /* HANDLE INPUTS AND OUTPUTS OF TASKS. */

    /* Wait for all tasks whose output variable is no longer referenced
       (ie, not marked above) and is not in use by another task, to ensure
       they don't stay around for a long time.  (Such unreferenced outputs
       should rarely arise in real programs.) */

    for (SEXP *var_list = helpers_var_list(1); *var_list; var_list++) {
        SEXP v = *var_list;
        if (!NODE_IS_MARKED(v) && !helpers_is_in_use(v))
            helpers_wait_until_not_being_computed(v);
    }

    /* For a full collection, wait for tasks that have large variables
       as inputs or outputs that haven't already been marked above, so
       that we can then collect these variables. */

    if (num_old_gens_to_collect == NUM_OLD_GENERATIONS) {
        for (SEXP *var_list = helpers_var_list(0); *var_list; var_list++) {
            SEXP v = *var_list;
            if (!NODE_IS_MARKED(v) && NODE_CLASS(v) == LARGE_NODE_CLASS) {
                if (helpers_is_being_computed(v))
                    helpers_wait_until_not_being_computed(v);
                if (helpers_is_in_use(v))
                    helpers_wait_until_not_in_use(v);
            }
        }
    }

    /* Forward and then process all inputs and outputs of scheduled tasks. */

    for (SEXP *var_list = helpers_var_list(0); *var_list; var_list++)
        FORWARD_NODE(*var_list);

    process_nodes (forwarded_nodes, no_snap); 
    forwarded_nodes = NULL;

    DEBUG_CHECK_NODE_COUNTS("after processing forwarded list (2)");


    /* IDENTIFY WEAKLY REACHABLE NODES */
    {
	Rboolean recheck_weak_refs;
	do {
	    recheck_weak_refs = FALSE;
	    for (s = R_weak_refs; s != R_NilValue; s = WEAKREF_NEXT(s)) {
		if (NODE_IS_MARKED(WEAKREF_KEY(s))) {
		    if (! NODE_IS_MARKED(WEAKREF_VALUE(s))) {
			recheck_weak_refs = TRUE;
			FORWARD_NODE(WEAKREF_VALUE(s));
		    }
		    if (! NODE_IS_MARKED(WEAKREF_FINALIZER(s))) {
			recheck_weak_refs = TRUE;
			FORWARD_NODE(WEAKREF_FINALIZER(s));
		    }
		}
	    }
	    process_nodes (forwarded_nodes, no_snap); 
            forwarded_nodes = NULL;
	} while (recheck_weak_refs);
    }

    /* mark nodes ready for finalizing */
    CheckFinalizers();

    /* process the weak reference chain */
    for (s = R_weak_refs; s != R_NilValue; s = WEAKREF_NEXT(s)) {
	FORWARD_NODE(s);
	FORWARD_NODE(WEAKREF_KEY(s));
	FORWARD_NODE(WEAKREF_VALUE(s));
	FORWARD_NODE(WEAKREF_FINALIZER(s));
    }
    process_nodes (forwarded_nodes, no_snap); 
    forwarded_nodes = NULL;

    DEBUG_CHECK_NODE_COUNTS("after processing forwarded list (3)");

    /* process CHARSXP cache */
    if (R_StringHash != NULL) /* in case of GC during initialization */
    {
        /* At this point, the hash table itself will not have been scanned.
           Some of the CHARSXP entries will be marked, either from being in 
           an older generation not being collected, or from a reference from
           a scanned node.  We need to remove unmarked entries here. */

	SEXP t;
	int nc = 0;
	for (i = 0; i < LENGTH(R_StringHash); i++) {
	    s = VECTOR_ELT(R_StringHash, i);
	    t = R_NilValue;
	    while (s != R_NilValue) {
#if DEBUG_GLOBAL_STRING_HASH
                if (TYPEOF(s)!=CHARSXP)
                   REprintf(
                     "R_StringHash table contains a non-CHARSXP (%d, gc)!\n",
                      TYPEOF(s));
#endif
		if (! NODE_IS_MARKED(CXHEAD(s))) { 
                    /* remove unused CHARSXP, and associated cons cell (if 
                       not linking by attribute field) */
		    if (t == R_NilValue) /* head of list */
                        /* Do NOT use SET_VECTOR_ELT - no old-to-new tracking */
			VECTOR_ELT(R_StringHash, i) = CXTAIL(s);
		    else
			CXTAIL(t) = CXTAIL(s);
		    s = CXTAIL(s);
		    continue;
		}
		t = s;
		s = CXTAIL(s);
	    }
	    if(VECTOR_ELT(R_StringHash, i) != R_NilValue) nc++;
	}
	SET_HASHSLOTSUSED (R_StringHash, nc);
        FORWARD_NODE(R_StringHash);
    }
    process_nodes (forwarded_nodes, no_snap); 
    forwarded_nodes = NULL;

    DEBUG_CHECK_NODE_COUNTS("after processing forwarded list (4)");

#ifdef PROTECTCHECK
    for(i=0; i< NUM_SMALL_NODE_CLASSES;i++){
	s = NEXT_NODE(R_GenHeap[i].New);
	while (s != R_GenHeap[i].New) {
	    SEXP next = NEXT_NODE(s);
	    if (TYPEOF(s) != NEWSXP) {
		if (TYPEOF(s) != FREESXP) {
		    SETOLDTYPE(s, TYPEOF(s));
		    TYPEOF(s) = FREESXP;
		}
		if (gc_inhibit_release)
		    FORWARD_NODE(s);
	    }
	    s = next;
	}
    }
    s = NEXT_NODE(R_GenHeap[LARGE_NODE_CLASS].New);
    while (s != R_GenHeap[LARGE_NODE_CLASS].New) {
	SEXP next = NEXT_NODE(s);
	if (TYPEOF(s) != NEWSXP) {
	    if (TYPEOF(s) != FREESXP) {
		/**** could also leave this alone and restore the old
		      node type in ReleaseLargeFreeVectors before
		      calculating size */
		R_size_t size = getVecSizeInVEC(s);
		LENGTH(s) = size;
		SETOLDTYPE(s, TYPEOF(s));
		TYPEOF(s) = FREESXP;
	    }
	    if (gc_inhibit_release)
		FORWARD_NODE(s);
	}
	s = next;
    }
    if (gc_inhibit_release) {
	process_nodes (forwarded_nodes, no_snap); 
        forwarded_nodes = NULL;
    }
#endif

    /* release large vector allocations */
    ReleaseLargeFreeVectors();

    DEBUG_CHECK_NODE_COUNTS("after releasing large allocated nodes");

    /* update heap statistics */
    R_Collected = R_NSize;
    R_SmallNallocSize = 0;
    for (gen = 0; gen < NUM_OLD_GENERATIONS; gen++) {
	for (i = 0; i < NUM_SMALL_NODE_CLASSES; i++)
	    R_SmallNallocSize += R_GenHeap[i].OldCount[gen] * NodeClassSize[i];
	for (i = 0; i < NUM_NODE_CLASSES; i++)
	    R_Collected -= R_GenHeap[i].OldCount[gen];
    }
    R_NodesInUse = R_NSize - R_Collected;

    if (num_old_gens_to_collect < NUM_OLD_GENERATIONS) {
	if (R_Collected < R_MinFreeFrac * R_NSize ||
	    VHEAP_FREE() < size_needed + R_MinFreeFrac * R_VSize) {
	    num_old_gens_to_collect++;
	    if (R_Collected <= 0 || VHEAP_FREE() < size_needed)
		goto again;
	}
	else num_old_gens_to_collect = 0;
    }
    else num_old_gens_to_collect = 0;

    if (gens_collected == NUM_OLD_GENERATIONS) {
	/**** do some adjustment for intermediate collections? */
	AdjustHeapSize(size_needed);
	release_or_sort_pages(SORT_NODES);
	DEBUG_CHECK_NODE_COUNTS("after heap adjustment");
    }
    else if (gens_collected > 0) {
	release_or_sort_pages(0);
	DEBUG_CHECK_NODE_COUNTS("after heap adjustment");
    }

    /* reset Free pointers */
    for (i = 0; i < NUM_SMALL_NODE_CLASSES; i++)
	R_GenHeap[i].Free = NEXT_NODE(R_GenHeap[i].New);

#if VALGRIND_LEVEL>0
    /* Tell Valgrind that free nodes are not accessible */
    for (i = 0; i < NUM_SMALL_NODE_CLASSES; i++) {
	for (s = R_GenHeap[i].Free; s != R_GenHeap[i].New; s = NEXT_NODE(s)) {
            VALGRIND_MAKE_MEM_NOACCESS(s, NODE_SIZE(i));
            VALGRIND_MAKE_MEM_DEFINED (&s->gengc_next_node, sizeof(SEXP));
            VALGRIND_MAKE_MEM_DEFINED (&s->gengc_prev_node, sizeof(SEXP));
            VALGRIND_MAKE_MEM_DEFINED (&s->sxpinfo, sizeof s->sxpinfo);
	}
    }
#endif

    gen_gc_counts[gens_collected]++;

    if (gc_reporting) {
	REprintf("Garbage collection %d = %d", gc_count, gen_gc_counts[0]);
	for (i = 0; i < NUM_OLD_GENERATIONS; i++)
	    REprintf("+%d", gen_gc_counts[i + 1]);
	REprintf(" (level %d) ... ", gens_collected);
	DEBUG_GC_SUMMARY(gens_collected);
    }
}

/* public interface for controlling GC torture settings */
void R_gc_torture(int gap, int wait, Rboolean inhibit)
{
    if (gap != NA_INTEGER && gap >= 0)
	gc_force_wait = gc_force_gap = gap;
    if (gap > 0) {
	if (wait != NA_INTEGER && wait > 0)
	    gc_force_wait = wait;
    }
#ifdef PROTECTCHECK
    if (gap > 0) {
	if (inhibit != NA_LOGICAL)
	    gc_inhibit_release = inhibit;
    }
    else gc_inhibit_release = FALSE;
#endif
}

static SEXP do_gctorture(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int gap;
    SEXP old = ScalarLogical(gc_force_wait > 0);

    checkArity(op, args);

    if (isLogical(CAR(args))) {
	Rboolean on = asLogical(CAR(args));
	if (on == NA_LOGICAL) gap = NA_INTEGER;
	else if (on) gap = 1;
	else gap = 0;
    }
    else gap = asInteger(CAR(args));

    R_gc_torture(gap, 0, FALSE);

    return old;
}

static SEXP do_gctorture2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int gap, wait;
    Rboolean inhibit;
    SEXP old = ScalarInteger(gc_force_gap);

    checkArity(op, args);
    gap = asInteger(CAR(args));
    wait = asInteger(CADR(args));
    inhibit = asLogical(CADDR(args));
    R_gc_torture(gap, wait, inhibit);

    return old;
}

/* initialize gctorture settings from environment variables */
static void init_gctorture(void)
{
    char *arg = getenv("R_GCTORTURE");
    if (arg != NULL) {
	int gap = atoi(arg);
	if (gap > 0) {
	    gc_force_wait = gc_force_gap = gap;
	    arg = getenv("R_GCTORTURE_WAIT");
	    if (arg != NULL) {
		int wait = atoi(arg);
		if (wait > 0)
		    gc_force_wait = wait;
	    }
#ifdef PROTECTCHECK
	    arg = getenv("R_GCTORTURE_INHIBIT_RELEASE");
	    if (arg != NULL) {
		int inhibit = atoi(arg);
		if (inhibit > 0) gc_inhibit_release = TRUE;
		else gc_inhibit_release = FALSE;
	    }
#endif
	}
    }
}

static SEXP do_gcinfo(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int i;
    SEXP old = ScalarLogical(gc_reporting);
    checkArity(op, args);
    i = asLogical(CAR(args));
    if (i != NA_LOGICAL)
	gc_reporting = i;
    return old;
}

/* reports memory use to profiler in eval.c */

void attribute_hidden get_current_mem(unsigned long *smallvsize,
				      unsigned long *largevsize,
				      unsigned long *nodes)
{
    *smallvsize = 0 /* R_SmallVallocSize */;
    *largevsize = R_LargeVallocSize;
    *nodes = R_NodesInUse * sizeof(SEXPREC_ALIGN) 
           + R_SmallNallocSize * sizeof(VECREC);
    return;
}

static SEXP do_gc(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP value;
    int ogc, reset_max;
    double R_NMega;
    R_size_t onsize = R_NSize /* can change during collection */;

    checkArity(op, args);
    ogc = gc_reporting;
    gc_reporting = asLogical(CAR(args));
    reset_max = asLogical(CADR(args));
    num_old_gens_to_collect = NUM_OLD_GENERATIONS;

    R_gc();

#   if PRINT_TYPE_STATS
        int count[32], count0[32], count00[32], count01[32];
        for (int i = 0; i<32; i++) 
            count[i] = count0[i] = count00[i] = count01[i] = 0;
        REprintf (
         "\n# OF OBJECTS OF EACH TYPE (PLUS COUNT IN CLASS 0, LENGTH 0/1)\n");
        REprintf ("\n%11s%10s %10s %10s %10s\n", 
                  "Type", "Total", "Class 0", "Length 0", "Length 1");
        for (int i = 0; i < NUM_NODE_CLASSES; i++) {
            for (int g = 0; g < NUM_OLD_GENERATIONS; g++) {
                for (SEXP s = NEXT_NODE(R_GenHeap[i].Old[g]);
                     s != R_GenHeap[i].Old[g]; 
                     s = NEXT_NODE(s)) {
                    count[TYPEOF(s)] += 1;
                    if (i == 0) {
                        count0[TYPEOF(s)] += 1;
                        if (LENGTH(s) == 0) count00[TYPEOF(s)] += 1;
                        if (LENGTH(s) == 1) count01[TYPEOF(s)] += 1;
                    }
                }
            }
        }
        for (int i = 0; i<32; i++)
            printf ("%11s%10d %10d %10d %10d\n", sexptype2char(i),
                    count[i], count0[i], count00[i], count01[i]);
        REprintf("\n");
#   endif

    gc_reporting = ogc;
    /*- now return the [used , gc trigger size] for cells and heap */
    PROTECT(value = allocVector(REALSXP, 14));
    REAL(value)[0] = onsize - R_Collected;
    REAL(value)[1] = R_VSize - VHEAP_FREE();
    REAL(value)[4] = R_NSize;
    REAL(value)[5] = R_VSize;
    /* next four are in 0.1Mb, rounded up */
    R_NMega = (onsize-R_Collected)/Mega * sizeof(SEXPREC_ALIGN) 
               + R_SmallNallocSize/Mega * vsfac;
    REAL(value)[2] = 0.1*ceil(10. * R_NMega);
    REAL(value)[3] = 0.1*ceil(10. * (R_VSize - VHEAP_FREE())/Mega * vsfac);
    REAL(value)[6] = 0.1*ceil(10. * R_NSize/Mega * sizeof_SEXPREC);
    REAL(value)[7] = 0.1*ceil(10. * R_VSize/Mega * vsfac);
    REAL(value)[8] = (R_MaxNSize < R_SIZE_T_MAX) ?
	0.1*ceil(10. * R_MaxNSize/Mega * sizeof_SEXPREC) : NA_REAL;
    REAL(value)[9] = (R_MaxVSize < R_SIZE_T_MAX) ?
	0.1*ceil(10. * R_MaxVSize/Mega * vsfac) : NA_REAL;
    if (reset_max){
	    R_N_maxused = onsize - R_Collected;
	    R_NMega_max = R_NMega;
	    R_V_maxused = R_VSize - VHEAP_FREE();
    }
    REAL(value)[10] = R_N_maxused;
    REAL(value)[11] = R_V_maxused;
    REAL(value)[12] = 0.1*ceil(10. * R_NMega_max);
    REAL(value)[13] = 0.1*ceil(10. * R_V_maxused/Mega*vsfac);
    UNPROTECT(1);
    return value;
}


/* InitMemory : Initialise the memory to be used in R. */
/* This includes: stack space, node space and vector space */

#define PP_REDZONE_SIZE 1000L
static R_size_t R_StandardPPStackSize, R_RealPPStackSize;

void attribute_hidden InitMemory()
{
    int gen, i;

#if VALGRIND_TEST
    valgrind_test();
#endif

    /* Set node class sizes.  Values for usual 32-bit and 64-bit architectures
       are designed to promote cache alignment. */

    if (!USE_FALLBACK_SIZES) {
        if (sizeof(SEXPREC_ALIGN)==24 && sizeof(VECREC)==8) {
            for (i = 0; i < NUM_SMALL_NODE_CLASSES; i++)
                NodeClassSize[i] 
                  = (NodeClassBytes32[i]-sizeof(SEXPREC_ALIGN))/sizeof(VECREC);
        }
        if (sizeof(SEXPREC_ALIGN)==40 && sizeof(VECREC)==8) {
            for (i = 0; i < NUM_SMALL_NODE_CLASSES; i++)
                NodeClassSize[i] 
                  = (NodeClassBytes64[i]-sizeof(SEXPREC_ALIGN))/sizeof(VECREC);
        }
    }

    /* Find the class to use for "cons" cells. */

    for (i = 0; i < NUM_SMALL_NODE_CLASSES; i++) {
        int size = sizeof(SEXPREC_ALIGN) + NodeClassSize[i]*sizeof(VECREC);
        if (sizeof(SEXPREC) <= size) {
            SEXPREC_class = i;
            sizeof_SEXPREC = size;
            break;
        }
    }
    if (i == NUM_SMALL_NODE_CLASSES)
        R_Suicide("none of the small node classes is big enough for a SEXPREC");

    init_gctorture();

    gc_reporting = R_Verbose;
    R_StandardPPStackSize = R_PPStackSize;
    R_RealPPStackSize = R_PPStackSize + PP_REDZONE_SIZE;
    if (!(R_PPStack = (SEXP *) malloc(R_RealPPStackSize * sizeof(SEXP))))
	R_Suicide("couldn't allocate memory for pointer stack");
    R_PPStackTop = 0;
#if VALGRIND_LEVEL>0
    VALGRIND_MAKE_MEM_NOACCESS(R_PPStack+R_PPStackSize, PP_REDZONE_SIZE);
#endif
    vsfac = sizeof(VECREC);
    R_VSize = (R_VSize + 1)/vsfac;
    if (R_MaxVSize < R_SIZE_T_MAX) R_MaxVSize = (R_MaxVSize + 1)/vsfac;

    UNMARK_NODE(&UnmarkedNodeTemplate);

    for (i = 0; i < NUM_NODE_CLASSES; i++) {
        for (gen = 0; gen < NUM_OLD_GENERATIONS; gen++) {
            R_GenHeap[i].Old[gen] = &R_GenHeap[i].OldPeg[gen];
            MAKE_EMPTY (R_GenHeap[i].Old[gen]);
#if VALGRIND_LEVEL>0
            VALGRIND_MAKE_MEM_NOACCESS(&R_GenHeap[i].OldPeg[gen], 
                                       sizeof(SEXPREC));
            VALGRIND_MAKE_MEM_DEFINED(&R_GenHeap[i].OldPeg[gen].gengc_next_node,
                                      sizeof(SEXP));
            VALGRIND_MAKE_MEM_DEFINED(&R_GenHeap[i].OldPeg[gen].gengc_prev_node,
                                      sizeof(SEXP));
#endif
#if !EXPEL_OLD_TO_NEW
            R_GenHeap[i].OldToNew[gen] = &R_GenHeap[i].OldToNewPeg[gen];
            MAKE_EMPTY (R_GenHeap[i].OldToNew[gen]);
#if VALGRIND_LEVEL>0
            VALGRIND_MAKE_MEM_NOACCESS(&R_GenHeap[i].OldToNewPeg[gen],
                                       sizeof(SEXPREC));
            VALGRIND_MAKE_MEM_DEFINED (&R_GenHeap[i].OldToNewPeg[gen].gengc_next_node,
                                       sizeof(SEXP));
            VALGRIND_MAKE_MEM_DEFINED (&R_GenHeap[i].OldToNewPeg[gen].gengc_prev_node,
                                       sizeof(SEXP));
#endif
#endif
            R_GenHeap[i].OldCount[gen] = 0;
        }
        R_GenHeap[i].New = &R_GenHeap[i].NewPeg;
        MAKE_EMPTY (R_GenHeap[i].New);
#if VALGRIND_LEVEL>0
        VALGRIND_MAKE_MEM_NOACCESS(&R_GenHeap[i].NewPeg, 
                                   sizeof(SEXPREC));
        VALGRIND_MAKE_MEM_DEFINED (&R_GenHeap[i].NewPeg.gengc_next_node,
                                   sizeof(SEXP));
        VALGRIND_MAKE_MEM_DEFINED (&R_GenHeap[i].NewPeg.gengc_prev_node,
                                   sizeof(SEXP));
#endif
    }

    for (i = 0; i < NUM_SMALL_NODE_CLASSES; i++)
        R_GenHeap[i].Free = NEXT_NODE(R_GenHeap[i].New);
#if VALGRIND_LEVEL>0
    VALGRIND_MAKE_MEM_NOACCESS(&R_GenHeap[LARGE_NODE_CLASS].Free, sizeof(SEXP));
#endif

    SET_NODE_CLASS(&UnmarkedNodeTemplate, 0);
    orig_R_NSize = R_NSize;
    orig_R_VSize = R_VSize;

    R_BCNodeStackBase = (SEXP *) malloc(R_BCNODESTACKSIZE * sizeof(SEXP));
    if (R_BCNodeStackBase == NULL)
	R_Suicide("couldn't allocate node stack");
#ifdef BC_INT_STACK
    R_BCIntStackBase =
      (IStackval *) malloc(R_BCINTSTACKSIZE * sizeof(IStackval));
    if (R_BCIntStackBase == NULL)
	R_Suicide("couldn't allocate integer stack");
#endif
    R_BCNodeStackTop = R_BCNodeStackBase;
    R_BCNodeStackEnd = R_BCNodeStackBase + R_BCNODESTACKSIZE;
#ifdef BC_INT_STACK
    R_BCIntStackTop = R_BCIntStackBase;
    R_BCIntStackEnd = R_BCIntStackBase + R_BCINTSTACKSIZE;
#endif

    R_weak_refs = R_NilValue;

    R_HandlerStack = R_RestartStack = R_NilValue;

    /*  Unbound values which are to be preserved through GCs */
    R_PreciousList = R_NilValue;
    
    /*  The current source line */
    R_Srcref = R_NilValue;
}


/* Since memory allocated from the heap is non-moving, R_alloc just
   allocates off the heap as RAWSXP/REALSXP and maintains the stack of
   allocations through the ATTRIB pointer.  The stack pointer R_VStack
   is traced by the collector.  Defined using the fast macros in Defn.h */
void *vmaxget(void)
{
    return VMAXGET();
}

void vmaxset(const void *ovmax)
{
    VMAXSET(ovmax);
}

char *R_alloc(size_t nelem, int eltsize)
{
    R_size_t size = nelem * eltsize;
    double dsize = (double)nelem * eltsize;
    if (dsize > 0) { /* precaution against integer overflow */
	SEXP s;
#if SIZEOF_SIZE_T > 4
	/* In this case by allocating larger units we can get up to
	   size(double) * (2^31 - 1) bytes, approx 16Gb */
	if(dsize < R_LEN_T_MAX)
	    s = allocVector(RAWSXP, size + 1);
	else if(dsize < sizeof(double) * (R_LEN_T_MAX - 1))
	    s = allocVector(REALSXP, (int)(0.99+dsize/sizeof(double)));
	else {
	    error(_("cannot allocate memory block of size %0.1f Gb"),
		  dsize/1024.0/1024.0/1024.0);
	    s = R_NilValue; /* -Wall */
	}
#else
	if(dsize > R_LEN_T_MAX) /* must be in the Gb range */
	    error(_("cannot allocate memory block of size %0.1f Gb"),
		  dsize/1024.0/1024.0/1024.0);
	s = allocVector(RAWSXP, size + 1);
#endif
	ATTRIB(s) = R_VStack == NULL ? R_NilValue : R_VStack;
	R_VStack = s;
	return (char *)DATAPTR(s);
    }
    else return NULL;
}



/* S COMPATIBILITY */

char *S_alloc(long nelem, int eltsize)
{
    R_size_t size  = nelem * eltsize;
    char *p = R_alloc(nelem, eltsize);

    memset(p, 0, size);
    return p;
}


char *S_realloc(char *p, long new, long old, int size)
{
    size_t nold;
    char *q;
    /* shrinking is a no-op */
    if(new <= old) return p;
    q = R_alloc((size_t)new, size);
    nold = (size_t)old * size;
    memcpy(q, p, nold);
    memset(q + nold, 0, (size_t)new*size - nold);
    return q;
}

/* "allocSExp" allocate a SEXPREC */
/* call gc if necessary */

SEXP allocSExp(SEXPTYPE t)
{
    SEXP s;
    if (FORCE_GC || NO_FREE_NODES())
	s = get_free_node_gc(SEXPREC_class);
    else
        s = get_free_node(SEXPREC_class);
    TYPEOF(s) = t;
    CAR(s) = R_NilValue;
    CDR(s) = R_NilValue;
    TAG(s) = R_NilValue;
    return s;
}

static SEXP allocSExpNonCons(SEXPTYPE t)
{
    SEXP s;
    if (FORCE_GC || NO_FREE_NODES())
	s = get_free_node_gc(SEXPREC_class);
    else
        s = get_free_node(SEXPREC_class);
    TYPEOF(s) = t;
    return s;
}

/* cons is defined directly to avoid the need to protect its arguments
   unless a GC will actually occur. */
SEXP cons(SEXP car, SEXP cdr)
{
    SEXP s;
    if (FORCE_GC || NO_FREE_NODES())
	s = get_free_node_gc2(SEXPREC_class,car,cdr);
    else
        s = get_free_node(SEXPREC_class);
    TYPEOF(s) = LISTSXP;
    CAR(s) = CHK(car);
    CDR(s) = CHK(cdr);
    TAG(s) = R_NilValue;
    return s;
}

/* version of cons that sets TAG too.  Caller needn't protect arguments. */
SEXP cons_with_tag(SEXP car, SEXP cdr, SEXP tag)
{
    SEXP s;
    if (FORCE_GC || NO_FREE_NODES())
	s = get_free_node_gc3(SEXPREC_class,car,cdr,tag);
    else
        s = get_free_node(SEXPREC_class);
    SET_TYPEOF(s,LISTSXP);
    CAR(s) = CHK(car);
    CDR(s) = CHK(cdr);
    TAG(s) = CHK(tag);
    return s;
}

/*----------------------------------------------------------------------

  NewEnvironment

  Create an environment by extending "rho" with a frame obtained by
  pairing the variable names given by the tags on "namelist" with
  the values given by the elements of "valuelist".  Note that "namelist" 
  can be shorter than "valuelist" if the rest of "valuelist" already 
  has tags. (In particular, "namelist" can be R_NilValue if all of
  "valuelist" already has tags.)

  NewEnvironment is defined directly to avoid the need to protect its
  arguments unless a GC will actually occur.  This definition allows
  the namelist argument to be shorter than the valuelist; in this
  case the remaining values must be named already.  (This is useful
  in cases where the entire valuelist is already named--namelist can
  then be R_NilValue.)

  The valuelist is destructively modified and used as the
  environment's frame.
*/
SEXP NewEnvironment(SEXP namelist, SEXP valuelist, SEXP rho)
{
    SEXP v, n, newrho;

    if (FORCE_GC || NO_FREE_NODES())
	newrho = get_free_node_gc3(SEXPREC_class,namelist,valuelist,rho);
    else
        newrho = get_free_node(SEXPREC_class);

    TYPEOF(newrho) = ENVSXP;
    FRAME(newrho) = valuelist;
    ENCLOS(newrho) = CHK(rho);
    HASHTAB(newrho) = R_NilValue;

    v = CHK(valuelist);
    n = CHK(namelist);
    while (v != R_NilValue && n != R_NilValue) {
	SET_TAG(v, TAG(n));
	v = CDR(v);
	n = CDR(n);
    }
    return (newrho);
}

/* mkPROMISE is defined directly do avoid the need to protect its arguments
   unless a GC will actually occur. 

   NAMEDCNT for the new promise is set to 1, and 'expr' has its NAMEDCNT
   set to the maximum. */

SEXP attribute_hidden mkPROMISE(SEXP expr, SEXP rho)
{
    SEXP s;
    if (FORCE_GC || NO_FREE_NODES())
	s = get_free_node_gc2(SEXPREC_class,expr,rho);
    else
        s = get_free_node(SEXPREC_class);

    SET_NAMEDCNT_MAX(expr);
    /* SET_NAMEDCNT_1(s); */

    TYPEOF(s) = PROMSXP;
    s->u.promsxp.value = R_UnboundValue;
    PRCODE(s) = CHK(expr);
    PRENV(s) = CHK(rho);
    PRSEEN(s) = 0;
    return s;
}


/* Fast, specialize allocVector for vectors of length 1.  The type 
   passed must be RAWSXP, LGLSXP, INTSXP, or REALSXP, so a that a vector
   of length 1 is guaranteed to fit in the first node class, and
   so that there's no need to initialize a pointer in the data part. 

   The version with an argument is static.  Versions for each allowed
   type are defined below for use elsewhere in the interpreter, in which
   we hope the compiler will optimize the tail call to a simple jump. 
   (This avoids any need for an error check on "type" to guard against 
   mis-use.) */

static SEXP allocVector1 (SEXPTYPE type)
{
#if VALGRIND_LEVEL==0
    SEXP s;
    if (FORCE_GC || NO_FREE_NODES())
	s = get_free_node_gc(0);
    else
        s = get_free_node(0);
    TYPEOF(s) = type;
    LENGTH(s) = 1;
    if (R_IsMemReporting && !R_MemPagesReporting)
        R_ReportAllocation (
          sizeof(SEXPREC_ALIGN) + NodeClassSize[0] * sizeof(VECREC), type, 1);
    return s;
#else
    return allocVector (type, 1);
#endif
}

SEXP allocVector1RAW(void)  { return allocVector1(RAWSXP); }
SEXP allocVector1LGL(void)  { return allocVector1(LGLSXP); }
SEXP allocVector1INT(void)  { return allocVector1(INTSXP); }
SEXP allocVector1REAL(void) { return allocVector1(REALSXP); }


/* Versions of functions for allocation of scalars that may return a 
   shared object.  ScalarLogicalMaybeConst is in Rinlinedfuns.h. */

SEXP ScalarIntegerMaybeConst(int x)
{
    if (ENABLE_SHARED_CONSTANTS) {
        if (x >=0 && x <= 10)
            return R_ScalarInteger0To10(x);
        if (x == NA_INTEGER)
            return R_ScalarIntegerNA;
    }

    return ScalarInteger(x);
}

SEXP ScalarRealMaybeConst(double x)
{
    if (ENABLE_SHARED_CONSTANTS) {

        /* Compare to pre-allocated values as 8-byte unsigned integers, not 
           as doubles, since double comparison doesn't work for NA or when 
           comparing -0 and +0 (which should be distinct). */

        if (*(uint64_t*) &x == *(uint64_t*) &REAL(R_ScalarRealZero)[0]) 
            return R_ScalarRealZero;
        if (*(uint64_t*) &x == *(uint64_t*) &REAL(R_ScalarRealOne)[0]) 
            return R_ScalarRealOne;
        if (*(uint64_t*) &x == *(uint64_t*) &REAL(R_ScalarRealNA)[0]) 
            return R_ScalarRealNA;
    }

    return ScalarReal(x);
}

SEXP ScalarComplexMaybeConst(Rcomplex x)
{
    return ScalarComplex(x);
}

SEXP ScalarStringMaybeConst(SEXP x)
{
    return ScalarString(x);
}

SEXP ScalarRawMaybeConst(Rbyte x)
{
    return ScalarRaw(x);
}

/* Allocate a vector object (and also list-like objects).
   This ensures only validity of list-like (LISTSXP, VECSXP, EXPRSXP),
   STRSXP and CHARSXP types;  e.g., atomic types remain un-initialized
   and must be initialized upstream, e.g., in do_makevector().
*/

#define FAST_ALLOC_TYPES ( \
    (1<<LGLSXP) + (1<<INTSXP) + (1<<REALSXP) + (1<<RAWSXP) )

SEXP allocVector(SEXPTYPE type, R_len_t length)
{
    SEXP s;

    if (length < 0 )
        errorcall(R_GlobalContext->call,
                  _("negative length vectors are not allowed"));

    /* Do numeric (not complex) vectors of length 1 and 0 specially, for speed.
       These are guaranteed to fit in the first node class.  Don't do this with
       VALGRIND_LEVEL>0, since that needs actual_size, etc. */

#if VALGRIND_LEVEL==0
    if (length <= 1 && ((FAST_ALLOC_TYPES>>type) & 1)) {
        if (FORCE_GC || NO_FREE_NODES())
            s = get_free_node_gc(0);
        else
            s = get_free_node(0);
        TYPEOF(s) = type;
        LENGTH(s) = length;
        if (R_IsMemReporting && !R_MemPagesReporting)
            R_ReportAllocation (
                sizeof(SEXPREC_ALIGN) + NodeClassSize[0] * sizeof(VECREC),
                type, length);
        return s;
    }
#endif

    /* Find the number of bytes in the data part of the vector that are
       actually used.  Also, create and return lists, which aren't actually
       vectors, but are nevertheless allowed types for allocVector. */

    R_size_t actual_size = 0;

    switch (type) {
    case RAWSXP:
        actual_size = length;
        break;
    case CHARSXP:
        actual_size = length + 1;
        break;
    case LGLSXP:
    case INTSXP:
        if (length > R_SIZE_T_MAX / sizeof(int))
            errorcall (R_GlobalContext->call,
                       _("cannot allocate vector of length %d"), length);
        actual_size = length * sizeof(int);
        break;
    case REALSXP:
        if (length > R_SIZE_T_MAX / sizeof(double))
            errorcall (R_GlobalContext->call,
                       _("cannot allocate vector of length %d"), length);
        actual_size = length * sizeof(double);
        break;
    case CPLXSXP:
        if (length > R_SIZE_T_MAX / sizeof(Rcomplex))
            errorcall (R_GlobalContext->call,
                       _("cannot allocate vector of length %d"), length);
        actual_size = length * sizeof(Rcomplex);
        break;
    case STRSXP:
    case EXPRSXP:
    case VECSXP:
        if (length > R_SIZE_T_MAX / sizeof(SEXP))
            errorcall (R_GlobalContext->call,
                      _("cannot allocate vector of length %d"), length);
        actual_size = length * sizeof(SEXP);
        break;

    case NILSXP:
        return R_NilValue;
    case LANGSXP:
        if(length == 0) return R_NilValue;
        s = allocList(length);
        TYPEOF(s) = LANGSXP;
        return s;
    case LISTSXP:
        return allocList(length);

    default:
        error(_("invalid type/length (%s/%d) in vector allocation"),
              type2char(type), length);
    }

    /* Compute number of notional "vector cells" to allocate. */

    R_size_t size = actual_size == 0 ? 0 : (actual_size-1) / sizeof(VECREC) + 1;

    /* Find the node class to use. */

    int node_class = LARGE_NODE_CLASS;
    R_size_t alloc_size = size;
    R_len_t i;

    for (i = 0; i < NUM_SMALL_NODE_CLASSES; i++) {
        if (size <= NodeClassSize[i]) {
            node_class = i;
            alloc_size = NodeClassSize[i];
            break;
        }
    }


    if (node_class < NUM_SMALL_NODE_CLASSES) {
        if (FORCE_GC || NO_FREE_NODES())
            s = get_free_node_gc(node_class);
        else
            s = get_free_node(node_class);
#if VALGRIND_LEVEL>0
        VALGRIND_MAKE_MEM_NOACCESS (DATAPTR(s), NODE_SIZE(NODE_CLASS(s))
                                                 - sizeof(SEXPREC_ALIGN));
#endif
    }
    else {
        /* save current R_VSize to roll back adjustment if malloc fails */
        R_size_t old_R_VSize = R_VSize;
        R_size_t size_to_malloc;
        if (FORCE_GC || NO_FREE_NODES() || VHEAP_FREE() < alloc_size) {
            R_gc_internal(alloc_size);
	    if (NO_FREE_NODES())
	        mem_err_cons();
	    if (VHEAP_FREE() < alloc_size)
	        mem_err_heap(size);
        }
        s = NULL; 
        if (size < (R_SIZE_T_MAX / sizeof(VECREC)) - sizeof(SEXPREC_ALIGN)) {
            size_to_malloc = sizeof(SEXPREC_ALIGN) + size * sizeof(VECREC)
                              + LARGE_VEC_PAD;
            s = malloc(size_to_malloc);
            if (s == NULL) {
                /* If we are near the address space limit, we
                   might be short of address space.  So return
                   all unused objects to malloc and try again. */
                R_gc_full(alloc_size);
                s = malloc(size_to_malloc);
            }
        }
        if (s == NULL) {
            double dsize = (double)size * sizeof(VECREC)/1024.0;
            /* reset the vector heap limit */
            R_VSize = old_R_VSize;
            if(dsize > 1024.0*1024.0)
                errorcall(R_NilValue,
                          _("cannot allocate vector of size %0.1f Gb"),
                          dsize/1024.0/1024.0);
            else if(dsize > 1024.0)
                errorcall(R_NilValue,
                          _("cannot allocate vector of size %0.1f Mb"),
                          dsize/1024.0);
            else
                errorcall(R_NilValue,
                          _("cannot allocate vector of size %0.f Kb"),
                          dsize);
        }
        s->sxpinfo = UnmarkedNodeTemplate.sxpinfo;
        SET_NODE_CLASS(s, LARGE_NODE_CLASS);
        ATTRIB(s) = R_NilValue;
        R_LargeVallocSize += size;
        R_GenHeap[LARGE_NODE_CLASS].AllocCount++;
        R_NodesInUse++;
        SNAP_NODE(s, R_GenHeap[LARGE_NODE_CLASS].New);
#if VALGRIND_LEVEL>0
        VALGRIND_MAKE_MEM_NOACCESS (DATAPTR(s), size_to_malloc
                                                 - sizeof(SEXPREC_ALIGN));
#endif
    }

    TYPEOF(s) = type;
    LENGTH(s) = length;

    if (R_IsMemReporting) {
        if (!R_MemPagesReporting || node_class >= NUM_SMALL_NODE_CLASSES)
            R_ReportAllocation (
                sizeof(SEXPREC_ALIGN) + alloc_size * sizeof(VECREC),
                type, length);
    }

#if VALGRIND_LEVEL>0
    VALGRIND_MAKE_MEM_UNDEFINED(DATAPTR(s), actual_size);
#endif

    /* For EXPRSXP, VECSXP, and STRSXP, prevent disaster in the case */
    /* that an uninitialised list vector or string vector is marked */
    /* Direct assignment is OK since the node was just allocated and */
    /* so is at least as new as R_NilValue and R_BlankString.  Strings
    /* are initialized to R_BlankString. */

    if (type == EXPRSXP || type == VECSXP) {
	SEXP *data = STRING_PTR(s);
	for (i = 0; i < length; i++)
	    data[i] = R_NilValue;
    }
    else if(type == STRSXP) {
	SEXP *data = STRING_PTR(s);
	for (i = 0; i < length; i++)
	    data[i] = R_BlankString;
    }
    else if (type == CHARSXP) {
	CHAR_RW(s)[length] = 0;
    }

    return s;
}

/* For future hiding of allocVector(CHARSXP) */
SEXP attribute_hidden allocCharsxp(R_len_t len)
{
    return allocVector(CHARSXP, len);
}


SEXP allocList(int n)
{
    int i;
    SEXP result;
    result = R_NilValue;
    for (i = 0; i < n; i++)
	result = CONS(R_NilValue, result);
    return result;
}

SEXP allocS4Object(void)
{
   SEXP s;
   GC_PROT(s = allocSExpNonCons(S4SXP));
   SET_S4_OBJECT(s);
   TAG(s) = R_NilValue;
   return s;
}


/* "gc" a mark-sweep or in-place generational garbage collector */

void R_gc(void)
{
    R_gc_internal(0);
}

static void R_gc_full(R_size_t size_needed)
{
    num_old_gens_to_collect = NUM_OLD_GENERATIONS;
    R_gc_internal(size_needed);
}

extern double R_getClockIncrement(void);
extern void R_getProcTime(double *data);

static double gctimes[5], gcstarttimes[5];
static Rboolean gctime_enabled = FALSE;

/* this is primitive */
static SEXP do_gctime(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;

    if (args == R_NilValue)
	gctime_enabled = TRUE;
    else {
	check1arg(args, call, "on");
	gctime_enabled = asLogical(CAR(args));
    }
    ans = allocVector(REALSXP, 5);
    REAL(ans)[0] = gctimes[0];
    REAL(ans)[1] = gctimes[1];
    REAL(ans)[2] = gctimes[2];
    REAL(ans)[3] = gctimes[3];
    REAL(ans)[4] = gctimes[4];
    return ans;
}

static void gc_start_timing(void)
{
    if (gctime_enabled)
	R_getProcTime(gcstarttimes);
}

static void gc_end_timing(void)
{
    if (gctime_enabled) {
	double times[5];
	R_getProcTime(times);

#if 0   /* DISABLED:  Seems to make no sense (regardless of real resolution) */
	double delta = R_getClockIncrement();
	/* add delta to compensate for timer resolution:
	   NB: as all current Unix-alike systems use getrusage, 
	   this may over-compensate.
	 */
	gctimes[0] += times[0] - gcstarttimes[0] + delta;
	gctimes[1] += times[1] - gcstarttimes[1] + delta;
#else
	gctimes[0] += times[0] - gcstarttimes[0];
	gctimes[1] += times[1] - gcstarttimes[1];
#endif
	gctimes[2] += times[2] - gcstarttimes[2];
	gctimes[3] += times[3] - gcstarttimes[3];
	gctimes[4] += times[4] - gcstarttimes[4];
    }
}

#define R_MAX(a,b) ( (a) < (b) ? (b) : (a) )

static void R_gc_internal(R_size_t size_needed)
{
    R_size_t onsize = R_NSize /* can change during collection */;
    double ncells, vcells, vfrac, nfrac;
    Rboolean first = TRUE;
    SEXPTYPE first_bad_sexp_type = 0;
#ifdef PROTECTCHECK
    SEXPTYPE first_bad_sexp_type_old_type = 0;
#endif
    SEXP first_bad_sexp_type_sexp = NULL;
    int first_bad_sexp_type_line = 0;

 again:

    gc_count++;

    R_N_maxused = R_MAX(R_N_maxused, R_NodesInUse);
    R_NMega_max = R_MAX(R_NMega_max, R_NodesInUse/Mega * sizeof(SEXPREC_ALIGN)
                                      + R_SmallNallocSize/Mega * vsfac);
    R_V_maxused = R_MAX(R_V_maxused, R_VSize - VHEAP_FREE());

    BEGIN_SUSPEND_INTERRUPTS {
	gc_start_timing();
	RunGenCollect(size_needed);
	gc_end_timing();
    } END_SUSPEND_INTERRUPTS;

    if (bad_sexp_type_seen != 0 && first_bad_sexp_type == 0) {
	first_bad_sexp_type = bad_sexp_type_seen;
#ifdef PROTECTCHECK
	first_bad_sexp_type_old_type = bad_sexp_type_old_type;
#endif
	first_bad_sexp_type_sexp = bad_sexp_type_sexp;
	first_bad_sexp_type_line = bad_sexp_type_line;
    }

    if (gc_reporting) {
	ncells = onsize - R_Collected;
	nfrac = (100.0 * ncells) / R_NSize;
	/* We try to make this consistent with the results returned by gc */
	ncells = 0.1*ceil(10.0 * (ncells * sizeof_SEXPREC/Mega
                                   + R_SmallNallocSize/Mega * vsfac));
	REprintf("\n%.1f Mbytes of cons cells used (%d%%)\n",
		 ncells, (int) (nfrac + 0.5));
	vcells = R_VSize - VHEAP_FREE();
	vfrac = (100.0 * vcells) / R_VSize;
	vcells = 0.1*ceil(10.0*vcells/Mega * vsfac);
	REprintf("%.1f Mbytes of vectors used (%d%%)\n",
		 vcells, (int) (vfrac + 0.5));
    }

    if (first) {
	first = FALSE;
	/* Run any eligible finalizers.  The return result of
	   RunFinalizers is TRUE if any finalizers are actually run.
	   There is a small chance that running finalizers here may
	   chew up enough memory to make another immediate collection
	   necessary.  If so, we jump back to the beginning and run
	   the collection, but on this second pass we do not run
	   finalizers. */
	if (RunFinalizers() &&
	    (NO_FREE_NODES() || size_needed > VHEAP_FREE()))
	    goto again;
    }

    if (first_bad_sexp_type != 0) {
#ifdef PROTECTCHECK
	if (first_bad_sexp_type == FREESXP)
	    error("GC encountered a node (%p) with type FREESXP (was %s)"
		  " at memory.c:%d",
		  first_bad_sexp_type_sexp,
		  sexptype2char(first_bad_sexp_type_old_type),
		  first_bad_sexp_type_line);
	else
	    error("GC encountered a node (%p) with an unknown SEXP type: %s"
		  " at memory.c:%d",
		  first_bad_sexp_type_sexp,
		  sexptype2char(first_bad_sexp_type),
		  first_bad_sexp_type_line);
#else
	error("GC encountered a node (%p) with an unknown SEXP type: %s"
	      " at memory.c:%d",
	      first_bad_sexp_type_sexp,
	      sexptype2char(first_bad_sexp_type),
	      first_bad_sexp_type_line);
#endif
    }
}

static SEXP do_memlimits(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;
    double nsize, vsize;
    R_size_t tmp;

    checkArity(op, args);
    nsize = asReal(CAR(args));
    vsize = asReal(CADR(args));

    if (ISNAN(nsize) || nsize <= 0) ;
    else if (nsize >= R_SIZE_T_MAX) R_MaxNSize = R_SIZE_T_MAX;
    else if (R_FINITE(nsize)) R_SetMaxNSize((R_size_t) nsize);

    if (ISNAN(vsize) || vsize <= 0) ;
    else if (vsize >= R_SIZE_T_MAX) R_MaxVSize = R_SIZE_T_MAX;
    else if (R_FINITE(vsize)) R_SetMaxVSize((R_size_t) vsize);

    PROTECT(ans = allocVector(REALSXP, 2));
    tmp = R_GetMaxNSize();
    REAL(ans)[0] = (tmp < R_SIZE_T_MAX) ? tmp : NA_REAL;
    tmp = R_GetMaxVSize();
    REAL(ans)[1] = (tmp < R_SIZE_T_MAX) ? tmp : NA_REAL;
    UNPROTECT(1);
    return ans;
}

static SEXP do_memoryprofile(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, nms;
    int i, tmp;

    PROTECT(ans = allocVector(INTSXP, 24));
    PROTECT(nms = allocVector(STRSXP, 24));
    for (i = 0; i < 24; i++) {
	INTEGER(ans)[i] = 0;
	SET_STRING_ELT(nms, i, type2str(i > LGLSXP? i+2 : i));
    }
    setAttrib(ans, R_NamesSymbol, nms);

    BEGIN_SUSPEND_INTERRUPTS {
      int gen;

      /* run a full GC to make sure that all stuff in use is in Old space */
      num_old_gens_to_collect = NUM_OLD_GENERATIONS;
      R_gc();
      for (gen = 0; gen < NUM_OLD_GENERATIONS; gen++) {
	for (i = 0; i < NUM_NODE_CLASSES; i++) {
	  SEXP s;
	  for (s = NEXT_NODE(R_GenHeap[i].Old[gen]);
	       s != R_GenHeap[i].Old[gen];
	       s = NEXT_NODE(s)) {
	      tmp = TYPEOF(s);
	      if(tmp > LGLSXP) tmp -= 2;
	      INTEGER(ans)[tmp]++;
	  }
	}
      }
    } END_SUSPEND_INTERRUPTS;
    UNPROTECT(2);
    return ans;
}

/* "protect" push a single argument onto R_PPStack.

   In handling a stack overflow we have to be careful not to use
   PROTECT. error("protect(): stack overflow") would call deparse1,
   which uses PROTECT and segfaults.

   However, the traceback creation in the normal error handler also
   does a PROTECT, as does the jumping code, at least if there are
   cleanup expressions to handle on the way out.  So for the moment
   we'll allocate a slightly larger PP stack and only enable the added
   red zone during handling of a stack overflow error.  LT 

   The PROTECT, UNPROTECT, PROTECT_WITH_INDEX, and REPROTECT macros at 
   the end of Defn.h do these things without procedure call overhead, and 
   are used here to define these functions, to keep the code in sync. 
*/

static void reset_pp_stack(void *data)
{
    R_size_t *poldpps = data;
    R_PPStackSize =  *poldpps;
}

void attribute_hidden Rf_protect_error (void)
{
    RCNTXT cntxt;
    R_size_t oldpps = R_PPStackSize;

    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
             R_NilValue, R_NilValue);
    cntxt.cend = &reset_pp_stack;
    cntxt.cenddata = &oldpps;

    if (R_PPStackSize < R_RealPPStackSize)
        R_PPStackSize = R_RealPPStackSize;
    errorcall(R_NilValue, _("protect(): protection stack overflow"));

    /* endcontext(&cntxt); */ /* not reached */
}

SEXP protect(SEXP s)
{
    return PROTECT (CHK(s));
}


/* Push 2 or 3 arguments onto protect stack.  BEWARE! All arguments will
   be evaluated (in the C sense) before any are protected. */

void Rf_protect2 (SEXP s1, SEXP s2)
{
    PROTECT2 (CHK(s1), CHK(s2));
}

void Rf_protect3 (SEXP s1, SEXP s2, SEXP s3)
{
    PROTECT3 (CHK(s1), CHK(s2), CHK(s3));
}


/* "unprotect" pop argument list from top of R_PPStack */

void attribute_hidden Rf_unprotect_error (void)
{
    error(_("unprotect(): only %d protected items"), R_PPStackTop);
}

void unprotect(int l)
{
    UNPROTECT(l);
}


/* "unprotect_ptr" remove pointer from somewhere in R_PPStack.  Don't
   try to combine use of this with use of ProtectWithIndex! */

void unprotect_ptr(SEXP s)
{
    int i = R_PPStackTop;

    /* go look for  s  in  R_PPStack */
    /* (should be among the top few items) */
    do {
	if (i == 0)
	    error(_("unprotect_ptr: pointer not found"));
    } while ( R_PPStack[--i] != s );

    /* OK, got it, and  i  is indexing its location */
    /* Now drop stack above it, if any */

    while (++i < R_PPStackTop) R_PPStack[i - 1] = R_PPStack[i];

    R_PPStackTop--;
}

SEXP R_ProtectWithIndex(SEXP s, PROTECT_INDEX *pi)
{
    return PROTECT_WITH_INDEX(CHK(s),pi);
}

void R_Reprotect(SEXP s, PROTECT_INDEX i)
{
    REPROTECT(CHK(s),i);
}

/* remove all objects from the protection stack from index i upwards
   and return them in a vector. The order in the vector is from new
   to old. */
SEXP R_CollectFromIndex(PROTECT_INDEX i)
{
    SEXP res;
    R_size_t top = R_PPStackTop, j = 0;
    if (i > top) i = top;
    res = protect(allocVector(VECSXP, top - i));
    while (i < top)
	SET_VECTOR_ELT(res, j++, R_PPStack[--top]);
    R_PPStackTop = top; /* this includes the protect we used above */
    return res;
}

/* "initStack" initialize environment stack */
void initStack(void)
{
    R_PPStackTop = 0;
}


/* S-like wrappers for calloc, realloc and free that check for error
   conditions */

void *R_chk_calloc(size_t nelem, size_t elsize)
{
    void *p;
#ifndef HAVE_WORKING_CALLOC
    if(nelem == 0)
	return(NULL);
#endif
    p = calloc(nelem, elsize);
    if(!p) /* problem here is that we don't have a format for size_t. */
	error(_("Calloc could not allocate memory (%.0f of %u bytes)"),
	      (double) nelem, elsize);
    return(p);
}

void *R_chk_realloc(void *ptr, size_t size)
{
    void *p;
    /* Protect against broken realloc */
    if(ptr) p = realloc(ptr, size); else p = malloc(size);
    if(!p)
	error(_("Realloc could not re-allocate memory (%.0f bytes)"), 
	      (double) size);
    return(p);
}

void R_chk_free(void *ptr)
{
    /* S-PLUS warns here, but there seems no reason to do so */
    /* if(!ptr) warning("attempt to free NULL pointer by Free"); */
    if(ptr) free(ptr); /* ANSI C says free has no effect on NULL, but
			  better to be safe here */
}

/* This code keeps a list of objects which are not assigned to variables
   but which are required to persist across garbage collections.  The
   objects are registered with R_PreserveObject and deregistered with
   R_ReleaseObject. Preserving/Releasing NULL is ignored. */

void R_PreserveObject(SEXP object)
{
    if (object != NULL)
        R_PreciousList = CONS(object, R_PreciousList);
}

static SEXP RecursiveRelease(SEXP object, SEXP list)
{
    if (!isNull(list)) {
	if (object == CAR(list))
	    return CDR(list);
	else
	    CDR(list) = RecursiveRelease(object, CDR(list));
    }
    return list;
}

void R_ReleaseObject(SEXP object)
{
    if (object != NULL)
        R_PreciousList = RecursiveRelease(object, R_PreciousList);
}


/* External Pointer Objects */
SEXP R_MakeExternalPtr(void *p, SEXP tag, SEXP prot)
{
    SEXP s = allocSExp(EXTPTRSXP);
    EXTPTR_PTR(s) = p;
    EXTPTR_PROT(s) = CHK(prot);
    EXTPTR_TAG(s) = CHK(tag);
    return s;
}

void *R_ExternalPtrAddr(SEXP s)
{
    return EXTPTR_PTR(CHK(s));
}

SEXP R_ExternalPtrTag(SEXP s)
{
    return CHK(EXTPTR_TAG(CHK(s)));
}

SEXP R_ExternalPtrProtected(SEXP s)
{
    return CHK(EXTPTR_PROT(CHK(s)));
}

void R_ClearExternalPtr(SEXP s)
{
    EXTPTR_PTR(s) = NULL;
}

void R_SetExternalPtrAddr(SEXP s, void *p)
{
    EXTPTR_PTR(s) = p;
}

void R_SetExternalPtrTag(SEXP s, SEXP tag)
{
    CHECK_OLD_TO_NEW(s, tag);
    EXTPTR_TAG(s) = tag;
}

void R_SetExternalPtrProtected(SEXP s, SEXP p)
{
    CHECK_OLD_TO_NEW(s, p);
    EXTPTR_PROT(s) = p;
}

/* Work around casting issues: works where it is needed */
typedef union {void *p; DL_FUNC fn;} fn_ptr;

/* used in package methods */
SEXP R_MakeExternalPtrFn(DL_FUNC p, SEXP tag, SEXP prot)
{
    fn_ptr tmp;
    SEXP s = allocSExp(EXTPTRSXP);
    tmp.fn = p;
    EXTPTR_PTR(s) = tmp.p;
    EXTPTR_PROT(s) = CHK(prot);
    EXTPTR_TAG(s) = CHK(tag);
    return s;
}

attribute_hidden
DL_FUNC R_ExternalPtrAddrFn(SEXP s)
{
    fn_ptr tmp;
    tmp.p =  EXTPTR_PTR(CHK(s));
    return tmp.fn;
}



/* The following functions are replacements for the accessor macros.
   They are used by code that does not have direct access to the
   internal representation of objects.  The replacement functions
   implement the write barrier. */

/* General Cons Cell Attributes */
SEXP (ATTRIB)(SEXP x) { return CHK(ATTRIB(CHK(x))); }
int (OBJECT)(SEXP x) { return OBJECT(CHK(x)); }
int (MARK)(SEXP x) { return MARK(CHK(x)); }
int (TYPEOF)(SEXP x) { return TYPEOF(CHK(x)); }
int (NAMED)(SEXP x) { return NAMED(CHK(x)); }
int (RTRACE)(SEXP x) { return RTRACE(CHK(x)); }
int (LEVELS)(SEXP x) { return LEVELS(CHK(x)); }

void (SET_ATTRIB)(SEXP x, SEXP v) {
    if(TYPEOF(v) != LISTSXP && TYPEOF(v) != NILSXP)
	error("value of 'SET_ATTRIB' must be a pairlist or NULL, not a '%s'",
	      type2char(TYPEOF(x)));
    if (ATTRIB(x) != v) {
        CHECK_OLD_TO_NEW(x, v);
        ATTRIB(x) = v;
    }
}

void SET_ATTRIB_TO_ANYTHING(SEXP x, SEXP v) {
    if (ATTRIB(x) != v) {
        CHECK_OLD_TO_NEW(x, v);
        ATTRIB(x) = v;
    }
}

void (SET_OBJECT)(SEXP x, int v) { SET_OBJECT(CHK(x), v); }
void (SET_TYPEOF)(SEXP x, int v) { SET_TYPEOF(CHK(x), v); }
void (SET_NAMED)(SEXP x, int v) { SET_NAMED(CHK(x), v); }
void (SET_RTRACE)(SEXP x, int v) { SET_RTRACE(CHK(x), v); }
int (SETLEVELS)(SEXP x, int v) { return SETLEVELS(CHK(x), v); }
void DUPLICATE_ATTRIB(SEXP to, SEXP from) {
    SET_ATTRIB(CHK(to), duplicate(CHK(ATTRIB(CHK(from)))));
    SET_OBJECT(CHK(to), OBJECT(from));
    if (IS_S4_OBJECT(from)) SET_S4_OBJECT(to); else UNSET_S4_OBJECT(to);
}

/* S4 object testing */
int (IS_S4_OBJECT)(SEXP x){ return IS_S4_OBJECT(CHK(x)); }
void (SET_S4_OBJECT)(SEXP x){ SET_S4_OBJECT(CHK(x)); }
void (UNSET_S4_OBJECT)(SEXP x){ UNSET_S4_OBJECT(CHK(x)); }

/* Vector Accessors */
int (LENGTH)(SEXP x) { return LENGTH(CHK(x)); }
int (TRUELENGTH)(SEXP x) { return TRUELENGTH(CHK(x)); }
void (SETLENGTH)(SEXP x, int v) { SETLENGTH(CHK(x), v); }
void (SET_TRUELENGTH)(SEXP x, int v) { SET_TRUELENGTH(CHK(x), v); }

const char *(R_CHAR)(SEXP x) {
    if(TYPEOF(x) != CHARSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "CHAR", "CHARSXP", type2char(TYPEOF(x)));
    return (const char *)CHAR(x);
}

SEXP (STRING_ELT)(SEXP x, int i) {
    if(TYPEOF(x) != STRSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "STRING_ELT", "character vector", type2char(TYPEOF(x)));
    return CHK(STRING_ELT(x, i));
}

SEXP (VECTOR_ELT)(SEXP x, int i) {
    /* We need to allow vector-like types here */
    if(TYPEOF(x) != VECSXP &&
       TYPEOF(x) != EXPRSXP &&
       TYPEOF(x) != WEAKREFSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "VECTOR_ELT", "list", type2char(TYPEOF(x)));
    return CHK(VECTOR_ELT(x, i));
}

int *(LOGICAL)(SEXP x) {
    if(TYPEOF(x) != LGLSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "LOGICAL",  "logical", type2char(TYPEOF(x)));
  return LOGICAL(x);
}

/* Maybe this should exclude logicals, but it is widely used */
int *(INTEGER)(SEXP x) {
    if(TYPEOF(x) != INTSXP && TYPEOF(x) != LGLSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "INTEGER", "integer", type2char(TYPEOF(x)));
    return INTEGER(x);
}

Rbyte *(RAW)(SEXP x) {
    if(TYPEOF(x) != RAWSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "RAW", "raw", type2char(TYPEOF(x)));
    return RAW(x);
}

double *(REAL)(SEXP x) {
    if(TYPEOF(x) != REALSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "REAL", "numeric", type2char(TYPEOF(x)));
    return REAL(x);
}

Rcomplex *(COMPLEX)(SEXP x) {
    if(TYPEOF(x) != CPLXSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "COMPLEX", "complex", type2char(TYPEOF(x)));
    return COMPLEX(x);
}

SEXP *(STRING_PTR)(SEXP x) { return STRING_PTR(CHK(x)); }

SEXP *(VECTOR_PTR)(SEXP x)
{
  error(_("not safe to return vector pointer"));
  return NULL;
}

void (SET_STRING_ELT)(SEXP x, int i, SEXP v) {
    if(TYPEOF(x) != STRSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "SET_STRING_ELT", "character vector", type2char(TYPEOF(x)));
    if(TYPEOF(v) != CHARSXP)
       error("Value of SET_STRING_ELT() must be a 'CHARSXP' not a '%s'",
	     type2char(TYPEOF(v)));
    CHECK_OLD_TO_NEW(x, v);
    STRING_ELT(x, i) = v;
}

/* Copy n string elements from v (starting at j) to x (starting at i). */
void copy_string_elements(SEXP x, int i, SEXP v, int j, int n) 
{
    SEXP e;
    int k;

    if(TYPEOF(x) != STRSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
         "copy_string_elements", "character vector", type2char(TYPEOF(x)));

    if (NODE_GEN_IS_YOUNGEST(x)) {
        /* x can't be older than anything */
        for (k = 0; k<n; k++) {
            e = STRING_ELT(v,j+k);
            STRING_ELT(x,i+k) = e;
        }
    }
    else {  
        /* need to check each time if x is older */
        for (k = 0; k<n; k++) {
            e = STRING_ELT(v,j+k);
            CHECK_OLD_TO_NEW(x, e);
            STRING_ELT(x,i+k) = e;
        }
    }
}

SEXP (SET_VECTOR_ELT)(SEXP x, int i, SEXP v) {
    /*  we need to allow vector-like types here */
    if(TYPEOF(x) != VECSXP &&
       TYPEOF(x) != EXPRSXP &&
       TYPEOF(x) != WEAKREFSXP) {
	error("%s() can only be applied to a '%s', not a '%s'",
	      "SET_VECTOR_ELT", "list", type2char(TYPEOF(x)));
    }
    CHECK_OLD_TO_NEW(x, v);
    return VECTOR_ELT(x, i) = v;
}

/* Copy n vector elements from v (starting at j) to x (starting at i). */
void copy_vector_elements(SEXP x, int i, SEXP v, int j, int n) 
{
    SEXP e;
    int k;

    /*  we need to allow vector-like types here */
    if(TYPEOF(x) != VECSXP &&
       TYPEOF(x) != EXPRSXP &&
       TYPEOF(x) != WEAKREFSXP) {
	error("%s() can only be applied to a '%s', not a '%s'",
	      "copy_vector_elements", "list", type2char(TYPEOF(x)));
    }

    if (NODE_GEN_IS_YOUNGEST(x)) {
        /* x can't be older than anything */
        for (k = 0; k<n; k++) {
            e = VECTOR_ELT(v,j+k);
            VECTOR_ELT(x,i+k) = e;
        }
    }
    else {  
        /* need to check each time if x is older */
        for (k = 0; k<n; k++) {
            e = VECTOR_ELT(v,j+k);
            CHECK_OLD_TO_NEW(x, e);
            VECTOR_ELT(x,i+k) = e;
        }
    }
}


/* List Accessors */
SEXP (TAG)(SEXP e) { return CHK(TAG(CHK(e))); }
SEXP (CAR)(SEXP e) { return CHK(CAR(CHK(e))); }
SEXP (CDR)(SEXP e) { return CHK(CDR(CHK(e))); }
SEXP (CAAR)(SEXP e) { return CHK(CAAR(CHK(e))); }
SEXP (CDAR)(SEXP e) { return CHK(CDAR(CHK(e))); }
SEXP (CADR)(SEXP e) { return CHK(CADR(CHK(e))); }
SEXP (CDDR)(SEXP e) { return CHK(CDDR(CHK(e))); }
SEXP (CADDR)(SEXP e) { return CHK(CADDR(CHK(e))); }
SEXP (CADDDR)(SEXP e) { return CHK(CADDDR(CHK(e))); }
SEXP (CAD4R)(SEXP e) { return CHK(CAD4R(CHK(e))); }
int (MISSING)(SEXP x) { return MISSING(CHK(x)); }

void (SET_TAG)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); TAG(x) = v; }

SEXP (SETCAR)(SEXP x, SEXP y)
{
    CHECK_OLD_TO_NEW(x, y);
    CAR(x) = y;
    return y;
}

SEXP (SETCDR)(SEXP x, SEXP y)
{
    CHECK_OLD_TO_NEW(x, y);
    CDR(x) = y;
    return y;
}

SEXP (SETCADR)(SEXP x, SEXP y)
{
    SEXP cell;
    cell = CDR(x);
    CHECK_OLD_TO_NEW(cell, y);
    CAR(cell) = y;
    return y;
}

SEXP (SETCADDR)(SEXP x, SEXP y)
{
    SEXP cell;
    cell = CDDR(x);
    CHECK_OLD_TO_NEW(cell, y);
    CAR(cell) = y;
    return y;
}

#define CDDDR(x) CDR(CDR(CDR(x)))

SEXP (SETCADDDR)(SEXP x, SEXP y)
{
    SEXP cell;
    cell = CDDDR(x);
    CHECK_OLD_TO_NEW(cell, y);
    CAR(cell) = y;
    return y;
}

#define CD4R(x) CDR(CDR(CDR(CDR(x))))

SEXP (SETCAD4R)(SEXP x, SEXP y)
{
    SEXP cell;
    cell = CD4R(x);
    CHECK_OLD_TO_NEW(cell, y);
    CAR(cell) = y;
    return y;
}

void (SET_MISSING)(SEXP x, int v) { SET_MISSING(CHK(x), v); }

/* Closure Accessors */
SEXP (FORMALS)(SEXP x) { return CHK(FORMALS(CHK(x))); }
SEXP (BODY)(SEXP x) { return CHK(BODY(CHK(x))); }
SEXP (CLOENV)(SEXP x) { return CHK(CLOENV(CHK(x))); }
int (RDEBUG)(SEXP x) { return RDEBUG(CHK(x)); }
int (RSTEP)(SEXP x) { return RSTEP(CHK(x)); }

void (SET_FORMALS)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); FORMALS(x) = v; }
void (SET_BODY)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); BODY(x) = v; }
void (SET_CLOENV)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); CLOENV(x) = v; }
void (SET_RDEBUG)(SEXP x, int v) { SET_RDEBUG(CHK(x), v); }
void (SET_RSTEP)(SEXP x, int v) { SET_RSTEP(CHK(x), v); }

/* Primitive Accessors */
attribute_hidden int (PRIMOFFSET)(SEXP x) { return PRIMOFFSET(x); }
attribute_hidden void (SET_PRIMOFFSET)(SEXP x, int v) { SET_PRIMOFFSET(x, v); }

/* Symbol Accessors */
SEXP (PRINTNAME)(SEXP x) { return CHK(PRINTNAME(CHK(x))); }
SEXP (SYMVALUE)(SEXP x) { return CHK(SYMVALUE(CHK(x))); }
SEXP (INTERNAL)(SEXP x) { return CHK(INTERNAL(CHK(x))); }
int (DDVAL)(SEXP x) { return DDVAL(CHK(x)); }

void (SET_PRINTNAME)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); PRINTNAME(x) = v; }
void (SET_SYMVALUE)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); SYMVALUE(x) = v; }
void (SET_INTERNAL)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); INTERNAL(x) = v; }
void (SET_DDVAL)(SEXP x, int v) { SET_DDVAL(CHK(x), v); }

/* Environment Accessors */
SEXP (FRAME)(SEXP x) { return CHK(FRAME(CHK(x))); }
SEXP (ENCLOS)(SEXP x) { return CHK(ENCLOS(CHK(x))); }
SEXP (HASHTAB)(SEXP x) { return CHK(HASHTAB(CHK(x))); }
int (ENVFLAGS)(SEXP x) { return ENVFLAGS(CHK(x)); }

void (SET_FRAME)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); FRAME(x) = v; }
void (SET_ENCLOS)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); ENCLOS(x) = v; }
void (SET_HASHTAB)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); HASHTAB(x) = v; }
void (SET_ENVFLAGS)(SEXP x, int v) { SET_ENVFLAGS(x, v); }

/* Promise Accessors */
SEXP (PRCODE)(SEXP x) { return CHK(PRCODE(CHK(x))); }
SEXP (PRENV)(SEXP x) { return CHK(PRENV(CHK(x))); }
SEXP (PRVALUE)(SEXP x) { return CHK(PRVALUE(CHK(x))); }
int (PRSEEN)(SEXP x) { return PRSEEN(CHK(x)); }

void (SET_PRENV)(SEXP x, SEXP v){ CHECK_OLD_TO_NEW(x, v); PRENV(x) = v; }
void (SET_PRVALUE)(SEXP x, SEXP v) 
  { CHECK_OLD_TO_NEW(x, v); x->u.promsxp.value = v; }
void (SET_PRCODE)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); PRCODE(x) = v; }
void (SET_PRSEEN)(SEXP x, int v) { SET_PRSEEN(CHK(x), v); }

/* Hashing Accessors */
int (HASHASH)(SEXP x) { return HASHASH(CHK(x)); }
int (HASHVALUE)(SEXP x) { return HASHVALUE(CHK(x)); }

void (SET_HASHASH)(SEXP x, int v) { SET_HASHASH(CHK(x), v); }
void (SET_HASHVALUE)(SEXP x, int v) { SET_HASHVALUE(CHK(x), v); }

/* Test functions */
Rboolean Rf_isNull(SEXP s) { return isNull(s); }
Rboolean Rf_isRaw(SEXP s) { return isRaw(s); }
Rboolean Rf_isSymbol(SEXP s) { return isSymbol(s); }
Rboolean Rf_isLogical(SEXP s) { return isLogical(s); }
Rboolean Rf_isReal(SEXP s) { return isReal(s); }
Rboolean Rf_isComplex(SEXP s) { return isComplex(s); }
Rboolean Rf_isExpression(SEXP s) { return isExpression(s); }
Rboolean Rf_isEnvironment(SEXP s) { return isEnvironment(s); }
Rboolean Rf_isString(SEXP s) { return isString(s); }
Rboolean Rf_isObject(SEXP s) { return isObject(s); }

/* Bindings accessors */
Rboolean attribute_hidden
(IS_ACTIVE_BINDING)(SEXP b) {return IS_ACTIVE_BINDING(b);}
Rboolean attribute_hidden
(BINDING_IS_LOCKED)(SEXP b) {return BINDING_IS_LOCKED(b);}
void attribute_hidden
(SET_ACTIVE_BINDING_BIT)(SEXP b) {SET_ACTIVE_BINDING_BIT(b);}
void attribute_hidden (LOCK_BINDING)(SEXP b) {LOCK_BINDING(b);}
void attribute_hidden (UNLOCK_BINDING)(SEXP b) {UNLOCK_BINDING(b);}

/* R_FunTab accessors */
int (PRIMVAL)(SEXP x) { return PRIMVAL(x); }
CCODE (PRIMFUN)(SEXP x) { return PRIMFUN(x); }
void (SET_PRIMFUN)(SEXP x, CCODE f) { SET_PRIMFUN(x,f); }

/* for use when testing the write barrier */
int  attribute_hidden (IS_BYTES)(SEXP x) { return IS_BYTES(x); }
int  attribute_hidden (IS_LATIN1)(SEXP x) { return IS_LATIN1(x); }
int  attribute_hidden (IS_ASCII)(SEXP x) { return IS_ASCII(x); }
int  attribute_hidden (IS_UTF8)(SEXP x) { return IS_UTF8(x); }
void attribute_hidden (SET_BYTES)(SEXP x) { SET_BYTES(x); }
void attribute_hidden (SET_LATIN1)(SEXP x) { SET_LATIN1(x); }
void attribute_hidden (SET_UTF8)(SEXP x) { SET_UTF8(x); }
void attribute_hidden (SET_ASCII)(SEXP x) { SET_ASCII(x); }
int  attribute_hidden (ENC_KNOWN)(SEXP x) { return ENC_KNOWN(x); }
void attribute_hidden (SET_CACHED)(SEXP x) { SET_CACHED(x); }
int  attribute_hidden (IS_CACHED)(SEXP x) { return IS_CACHED(x); }


/* ------------------------------------------------------------------------
   Function (plus global variables) not intended to ever be called, and 
   normally not defined, whose code can be examined to see how the compiler
   is implementing facilities such as the NAMEDCNT macros. 
*/

#if 0

SEXP R_tobj_a, R_tobj_b, R_tobj_c, R_tobj_d, R_tobj_e, R_tobj_f, R_tobj_g,
     R_tobj_h, R_tobj_i, R_tobj_j, R_tobj_k, R_tobj_l, R_tobj_m, R_tobj_n,
     R_tobj_o, R_tobj_p, R_tobj_q, R_tobj_r, R_tobj_s, R_tobj_t, R_tobj_u,
     R_tobj_v, R_tobj_w, R_tobj_x, R_tobj_y, R_tobj_z;

int  R_tint_a, R_tint_b, R_tint_c, R_tint_d, R_tint_e, R_tint_f, R_tint_g,
     R_tint_h, R_tint_i, R_tint_j, R_tint_k, R_tint_l, R_tint_m, R_tint_n,
     R_tint_o, R_tint_p, R_tint_q, R_tint_r, R_tint_s, R_tint_t, R_tint_u,
     R_tint_v, R_tint_w, R_tint_x, R_tint_y, R_tint_z;

void Rf_code_gen_test_func (void)
{
  R_tint_a = NAMEDCNT(R_tobj_a);
  R_tint_b = NAMED(R_tobj_b);

  if (NAMEDCNT_EQ_0(R_tobj_c)) REprintf("xx\n");
  if (NAMEDCNT_GT_0(R_tobj_d)) REprintf("xx\n");
  if (NAMEDCNT_GT_1(R_tobj_e)) REprintf("xx\n");

  SET_NAMEDCNT(R_tobj_f,R_tint_f);
  SET_NAMEDCNT_0(R_tobj_g);
  SET_NAMEDCNT_1(R_tobj_h);
  SET_NAMEDCNT_MAX(R_tobj_i);

  SET_NAMED(R_tobj_j,R_tint_j);

  INC_NAMEDCNT(R_tobj_k);
  DEC_NAMEDCNT(R_tobj_l);

  R_tint_m = LEVELS(R_tobj_m);
  R_tint_n = TRUELENGTH(R_tobj_n);

  SETLEVELS(R_tobj_o,R_tint_o);
  SET_TRUELENGTH(R_tobj_p,R_tint_p);

  R_tint_q = TYPEOF(R_tobj_q);
  R_tint_r = NODE_CLASS(R_tobj_r);

  SET_NODE_CLASS(R_tobj_s,R_tint_s);
}

#endif


/* ------------------------------------------------------------------------ */

static SEXP do_pnamedcnt(SEXP call, SEXP op, SEXP args, SEXP rho)
{   SEXP a;
    int j;

    if (args == R_NilValue)
        error(_("too few arguments"));

    check1arg_x (args, call);

    for (a = CDR(args); a != R_NilValue; a = CDR(a))
        if (!isString(CAR(a)))
            error(_("invalid argument"));

    /* access nmcnt directly, so won't delay for possible task syncronization */
    Rprintf ("PNAMEDCNT:  %d  %x  %s", CAR(args)->sxpinfo.nmcnt, CAR(args),
                                       type2char(TYPEOF(CAR(args))));

    for (a = CDR(args); a != R_NilValue; a = CDR(a)) {
        Rprintf(" :");
        for (j = 0; j < LENGTH(CAR(a)); j++)
            Rprintf(" %s", CHAR(STRING_ELT(CAR(a),j)));
    }

    Rprintf("\n");

    return CAR(args);
}


/*******************************************/
/* Non-sampling memory use profiler reports vector allocations and/or
   calls to GetNewPage */
/*******************************************/

static void R_OutputStackTrace (void)
{
    RCNTXT *cptr;
    int newline;

    if (!R_MemStackReporting) goto print_newline;

    newline = R_MemReportingToTerminal | R_MemDetailsReporting;

    if (R_MemReportingOutfile != NULL) 
        fprintf (R_MemReportingOutfile, ":");
    if (R_MemReportingToTerminal) 
        REprintf (":");

    for (cptr = R_GlobalContext; cptr; cptr = cptr->nextcontext) {
	if ((cptr->callflag & (CTXT_FUNCTION | CTXT_BUILTIN))
	    && TYPEOF(cptr->call) == LANGSXP) {
	    SEXP fun = CAR(cptr->call);
	    if (!newline) newline = 1;
	    if (R_MemReportingOutfile != NULL)
                fprintf (R_MemReportingOutfile, "\"%s\" ",
		         TYPEOF(fun) == SYMSXP ? CHAR(PRINTNAME(fun)) :
		         "<Anonymous>");
	    if (R_MemReportingToTerminal)
                REprintf ("\"%s\" ",
		          TYPEOF(fun) == SYMSXP ? CHAR(PRINTNAME(fun)) :
		          "<Anonymous>");
	}
    }

    if (!newline) return;

print_newline:
    if (R_MemReportingOutfile != NULL) 
        fprintf (R_MemReportingOutfile, "\n");
    if (R_MemReportingToTerminal) 
        REprintf ("\n");
}

static void R_ReportAllocation (R_size_t size, SEXPTYPE type, R_len_t length)
{
    if (size > R_MemReportingThreshold && length >= R_MemReportingNElem) {
        if (R_MemReportingOutfile != NULL) {
            if (R_MemDetailsReporting)
                fprintf (R_MemReportingOutfile, "%lu (%s %lu)",
                  (unsigned long) size, type2char(type), (unsigned long)length);
            else 
                fprintf (R_MemReportingOutfile, "%lu ",
                  (unsigned long) size);
        }
        if (R_MemReportingToTerminal) {
            if (R_MemDetailsReporting)
                REprintf ("RPROFMEM: %lu (%s %lu)",
                  (unsigned long) size, type2char(type), (unsigned long)length);
            else
                REprintf ("RPROFMEM: %lu ", 
                  (unsigned long) size);
        }
        R_OutputStackTrace();
    }
}

static void R_ReportNewPage(void)
{
    if (R_MemPagesReporting) {
        if (R_MemReportingOutfile != NULL)
            fprintf (R_MemReportingOutfile, "new page");
        if (R_MemReportingToTerminal)
            REprintf ("RPROFMEM: new page");
	R_OutputStackTrace();
    }
}

static void R_EndMemReporting(void)
{
    if(R_MemReportingOutfile != NULL) {
	fclose (R_MemReportingOutfile);
	R_MemReportingOutfile=NULL;
    }
    R_IsMemReporting = 0;
}

static void R_InitMemReporting(SEXP filename, int append)
{
    if (R_IsMemReporting)
        R_EndMemReporting();

    if (strlen(CHAR(filename)) > 0) {
        R_MemReportingOutfile = RC_fopen (filename, append ? "a" : "w", TRUE);
        if (R_MemReportingOutfile == NULL)
            error(_("Rprofmem: cannot open output file '%s'"), filename);
    }
    else
        R_MemReportingOutfile = NULL;

    R_IsMemReporting = 1;

    return;
}

static SEXP do_Rprofmem(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP filename, ap;
    int append_mode;

    checkArity(op, args);

    ap = args;
    if (!isString(CAR(ap)) || (LENGTH(CAR(ap))) != 1)
	error(_("invalid '%s' argument"), "filename");
    filename = STRING_ELT(CAR(ap), 0);

    ap = CDR(ap);
    append_mode = asLogical(CAR(ap));

    ap = CDR(ap);
    if (!isReal(CAR(ap)) || (LENGTH(CAR(ap))) != 1)
	error(_("invalid '%s' argument"), "threshold");
    R_MemReportingThreshold = REAL(CAR(ap))[0];

    ap = CDR(ap);
    if (!isReal(CAR(ap)) || (LENGTH(CAR(ap))) != 1)
	error(_("invalid '%s' argument"), "nelem");
    R_MemReportingNElem = REAL(CAR(ap))[0];

    ap = CDR(ap);
    R_MemStackReporting = asLogical(CAR(ap));

    ap = CDR(ap);
    R_MemReportingToTerminal = asLogical(CAR(ap));

    ap = CDR(ap);
    R_MemPagesReporting = asLogical(CAR(ap));

    ap = CDR(ap);
    R_MemDetailsReporting = asLogical(CAR(ap));

    if (R_MemReportingToTerminal || strlen(CHAR(filename)) > 0)
	R_InitMemReporting(filename, append_mode);
    else
	R_EndMemReporting();

    return R_NilValue;
}


/* String cache routines, including string hashing - formerly in envir.c */

SEXP mkCharCE(const char *name, cetype_t enc)
{
    return mkCharLenCE(name, strlen(name), enc);
}

/* no longer used in R but docuented in 2.7.x */
SEXP mkCharLen(const char *name, int len)
{
    return mkCharLenCE(name, len, CE_NATIVE);
}

SEXP mkChar(const char *name)
{
    return mkCharLenCE(name, strlen(name), CE_NATIVE);
}

/* CHARSXP hashing follows the hash structure from envir.c, but need separate
   code for get/set of values since our keys are char* and not SEXP symbol types
   and the string hash table is treated specially in garbage collection.

   Experience has shown that it is better to use a different hash function,
   and a power of 2 for the hash size.
*/

/* char_hash_size MUST be a power of 2 and char_hash_mask == char_hash_size - 1
   in order for x & char_hash_mask to be equivalent to x % char_hash_size. */

static unsigned int char_hash_size = STRHASHINITSIZE;
static unsigned int char_hash_mask = STRHASHINITSIZE-1;

static unsigned int char_hash(const char *s, int len)
{
    /* djb2 as from http://www.cse.yorku.ca/~oz/hash.html */
    char *p;
    int i;
    unsigned int h = 5381;
    for (p = (char *) s, i = 0; i < len; p++, i++)
	h = ((h << 5) + h) + (*p);
    return h;
}

void attribute_hidden InitStringHash()
{
    R_StringHash = allocVector (VECSXP, char_hash_size);
    HASHSIZE(R_StringHash) = char_hash_size;
    HASHSLOTSUSED(R_StringHash) = 0;
}

/* Resize the global R_StringHash CHARSXP cache */
static void R_StringHash_resize(unsigned int newsize)
{
    SEXP old_table = R_StringHash;
    SEXP new_table, chain, new_chain, val, next;
    unsigned int counter, new_hashcode, newmask;
#if DEBUG_GLOBAL_STRING_HASH
    unsigned int oldsize = HASHSIZE(R_StringHash);
    unsigned int oldslotsused = HASHSLOTSUSED(R_StringHash);
#endif

    /* Allocate the new hash table.  Chain moving is destructive and 
       does not involve allocation, so this is the only point where
       GC can occur.  The allocation could fail - ideally we would recover 
       from that and carry on with the original table, but we don't now. */

    new_table = allocVector (VECSXP, newsize);
    SET_HASHSLOTSUSED (new_table, 0);
    newmask = newsize - 1;

    /* transfer chains from old table to new table */
    for (counter = 0; counter < LENGTH(old_table); counter++) {
	chain = VECTOR_ELT(old_table, counter);
	while (chain != R_NilValue) {
	    val = CXHEAD(chain);
	    next = CXTAIL(chain);
#if DEBUG_GLOBAL_STRING_HASH
            if (TYPEOF(val)!=CHARSXP)
               REprintf("R_StringHash table contains a non-CHARSXP (%d, rs)!\n",
                        TYPEOF(val));
#endif
	    new_hashcode = char_hash (CHAR(val), LENGTH(val)) & newmask;
	    new_chain = VECTOR_ELT(new_table, new_hashcode);
	    /* If using a previously-unused slot then increase HASHSLOTSUSED */
	    if (new_chain == R_NilValue)
		SET_HASHSLOTSUSED(new_table, HASHSLOTSUSED(new_table) + 1);
	    /* Move the current chain link to the new chain.  This is a 
               destrictive modification, which does NOT do the old-to-new
               check, since table entries aren't supposed to be marked
               in the initial pass of the GC. */
	    CXTAIL(val) = new_chain;
	    VECTOR_ELT(new_table, new_hashcode) = val;
	    chain = next;
	}
    }
    R_StringHash = new_table;
    char_hash_size = newsize;
    char_hash_mask = newmask;
#if DEBUG_GLOBAL_STRING_HASH
    Rprintf ("Resized:  size %d => %d,  slotsused %d => %d\n",
      oldsize, HASHSIZE(new_table), oldslotsused, HASHSLOTSUSED(new_table));
#endif
}

/* mkCharCE - make a character (CHARSXP) variable and set its
   encoding bit.  If a CHARSXP with the same string already exists in
   the global CHARSXP cache, R_StringHash, it is returned.  Otherwise,
   a new CHARSXP is created, added to the cache and then returned. */


/* Because allocCharsxp allocates len+1 bytes and zeros the last,
   this will always zero-terminate */
SEXP mkCharLenCE(const char *name, int len, cetype_t enc)
{
    SEXP cval, chain;
    unsigned int hashcode;
    int need_enc;
    Rboolean embedNul = FALSE, is_ascii = TRUE;

    switch(enc){
    case CE_NATIVE:
    case CE_UTF8:
    case CE_LATIN1:
    case CE_BYTES:
    case CE_SYMBOL:
    case CE_ANY:
	break;
    default:
	error(_("unknown encoding: %d"), enc);
    }
    for (int slen = 0; slen < len; slen++) {
	if ((unsigned int) name[slen] > 127) is_ascii = FALSE;
	if (!name[slen]) embedNul = TRUE;
    }
    if (embedNul) {
	SEXP c;
	/* This is tricky: we want to make a reasonable job of
	   representing this string, and EncodeString() is the most
	   comprehensive */
	c = allocCharsxp(len);
	memcpy(CHAR_RW(c), name, len);
	switch(enc) {
	case CE_UTF8: SET_UTF8(c); break;
	case CE_LATIN1: SET_LATIN1(c); break;
	case CE_BYTES: SET_BYTES(c); break;
	default: break;
	}
	if (is_ascii) SET_ASCII(c);
	error(_("embedded nul in string: '%s'"),
	      EncodeString(c, 0, 0, Rprt_adj_none));
    }

    if (enc && is_ascii) enc = CE_NATIVE;
    switch(enc) {
    case CE_UTF8: need_enc = UTF8_MASK; break;
    case CE_LATIN1: need_enc = LATIN1_MASK; break;
    case CE_BYTES: need_enc = BYTES_MASK; break;
    default: need_enc = 0;
    }

    hashcode = char_hash(name, len) & char_hash_mask;

    /* Search for a cached value */
    cval = R_NilValue;
    for (chain = VECTOR_ELT(R_StringHash, hashcode); 
         chain != R_NilValue; 
         chain = CXTAIL(chain)) {
	SEXP val = CXHEAD(chain);
	if (need_enc == (ENC_KNOWN(val) | IS_BYTES(val))) {
            if (LENGTH(val) == len) {
                if (len == 0 || *CHAR(val) == *name /* quick pretest */
                                   && memcmp(CHAR(val), name, len) == 0) {
                    cval = val;
                    break;
                }
            }
	}
    }
    if (cval == R_NilValue) {
	/* no cached value; need to allocate one and add to the cache */
	cval = allocCharsxp(len);
	memcpy(CHAR_RW(cval), name, len);
	switch(enc) {
	case 0:
	    break;          /* don't set encoding */
	case CE_UTF8:
	    SET_UTF8(cval);
	    break;
	case CE_LATIN1:
	    SET_LATIN1(cval);
	    break;
	case CE_BYTES:
	    SET_BYTES(cval);
	    break;
	default:
	    error("unknown encoding mask: %d", enc);
	}
	if (is_ascii) SET_ASCII(cval);
	SET_CACHED(cval);  /* Mark it */
	/* add the new value to the cache */
	chain = VECTOR_ELT(R_StringHash, hashcode);
	if (chain == R_NilValue)
	    SET_HASHSLOTSUSED(R_StringHash, HASHSLOTSUSED(R_StringHash) + 1);

        /* The modifications below should NOT do the old-to-new check, since
           the table should not be looked at in the initial GC scan. */
#ifdef USE_ATTRIB_FIELD_FOR_CHARSXP_CACHE_CHAINS
	CXTAIL(cval) = chain;
        chain = cval;
#else
        chain = CONS(cval,chain);
#endif
	VECTOR_ELT(R_StringHash, hashcode) = chain;

	/* Resize the hash table if desirable and possible. */
	if (HASHSLOTSUSED(R_StringHash) > 0.85 * HASHSIZE(R_StringHash)
             && 2*char_hash_size <= STRHASHMAXSIZE) {
            /* NOTE!  Must protect cval here, since it is NOT protected by
               its presence in the hash table. */
            PROTECT(cval);
	    R_StringHash_resize (2*char_hash_size);
            UNPROTECT(1);
        }
    }

    return cval;
}


#if DEBUG_SHOW_CHARSXP_CACHE
/* Call this from gdb with

       call do_show_cache(10)

   for the first 10 cache chains in use. */
void do_show_cache(int n)
{
    int i, j;
    Rprintf("Cache size: %d\n", LENGTH(R_StringHash));
    Rprintf("Cache slots used:  %d\n", HASHSLOTSUSED(R_StringHash));
    for (i = 0, j = 0; j < n && i < LENGTH(R_StringHash); i++) {
	SEXP chain = VECTOR_ELT(R_StringHash, i);
	if (chain != R_NilValue) {
	    Rprintf("Line %d: ", i);
	    do {
		if (IS_UTF8(CXHEAD(chain)))
		    Rprintf("U");
		else if (IS_LATIN1(CXHEAD(chain)))
		    Rprintf("L");
		else if (IS_BYTES(CXHEAD(chain)))
		    Rprintf("B");
		Rprintf("|%s| ", CHAR(CXHEAD(chain)));
		chain = CXTAIL(chain);
	    } while (chain != R_NilValue);
	    Rprintf("\n");
	    j++;
	}
    }
}

void do_write_cache()
{
    int i;
    FILE *f = fopen("/tmp/CACHE", "w");
    if (f != NULL) {
	fprintf(f, "Cache size: %d\n", LENGTH(R_StringHash));
	fprintf(f, "Cache slots used:  %d\n", HASHSLOTSUSED(R_StringHash));
	for (i = 0; i < LENGTH(R_StringHash); i++) {
	    SEXP chain = VECTOR_ELT(R_StringHash, i);
	    if (chain != R_NilValue) {
		fprintf(f, "Line %d: ", i);
		do {
		    if (IS_UTF8(CXHEAD(chain)))
			fprintf(f, "U");
		    else if (IS_LATIN1(CXHEAD(chain)))
			fprintf(f, "L");
		    else if (IS_BYTES(CXHEAD(chain)))
			fprintf(f, "B");
		    fprintf(f, "|%s| ", CHAR(CXHEAD(chain)));
		    chain = CXTAIL(chain);
		} while (chain != R_NilValue);
		fprintf(f, "\n");
	    }
	}
	fclose(f);
    }
}
#endif /* DEBUG_SHOW_CHARSXP_CACHE */


/* RBufferUtils, moved from deparse.c */

#include "RBufferUtils.h"

/* Allocate at least blen+1 bytes, enough to hold a string of length blen. */

attribute_hidden void *R_AllocStringBuffer(size_t blen, R_StringBuffer *buf)
{
    size_t blen1, bsize = buf->defaultSize;

    /* for backwards compatibility, probably no longer needed */
    if(blen == (size_t)-1) {
	warning("R_AllocStringBuffer(-1) used: please report");
	R_FreeStringBufferL(buf);
	return NULL;
    }

    if (blen < buf->bufsize) return buf->data;
    blen1 = blen = (blen + 1);
    blen = (blen / bsize) * bsize;
    if(blen < blen1) blen += bsize;

    if(buf->data == NULL) {
	buf->data = (char *) malloc(blen);
        if (buf->data) buf->data[0] = 0;
    }
    else
	buf->data = (char *) realloc(buf->data, blen);

    if (!buf->data) {
	buf->bufsize = 0;
	/* don't translate internal error message */
	error("could not allocate memory (%u Mb) in C function 'R_AllocStringBuffer'",
	      (unsigned int) blen/1024/1024);
    }

    buf->bufsize = blen;
    return buf->data;
}

void attribute_hidden
R_FreeStringBuffer(R_StringBuffer *buf)
{
    if (buf->data != NULL) {
	free(buf->data);
	buf->bufsize = 0;
	buf->data = NULL;
    }
}

void attribute_hidden
R_FreeStringBufferL(R_StringBuffer *buf)
{
    if (buf->bufsize > buf->defaultSize) {
	free(buf->data);
	buf->bufsize = 0;
	buf->data = NULL;
    }
}

/* See if space for an operand can be used for the result too.  Assumes
   independent element-by-element computation.  Returns the operand that
   can be reused, or R_NilValue if neither can be reused. */

SEXP attribute_hidden can_save_alloc (SEXP s1, SEXP s2, SEXPTYPE typ)
{
    int n1 = LENGTH(s1);
    int n2 = LENGTH(s2);

    if (n1==0 || n2==0)  
        return R_NilValue;  /* since result may not have length max(n1,n2) */

    /* Try to use space for 2nd arg if both same length, so 1st argument's
       attributes will then take precedence when copied. */

    if (n2>=n1) {
        if (TYPEOF(s2)==typ && NAMEDCNT_EQ_0(s2)) {
            /* Must remove any "names" attribute of s2 to match action of
               copyMostAttrib.  Any "dim" and "dimnames" attributes are allowed
               to stay, since they will be overwritten anyway. */
            if (ATTRIB(s2)!=R_NilValue) 
                setAttrib (s2, R_NamesSymbol, R_NilValue);
            return s2;
        }
        else {
            /* Can use 1st arg's space only if 2nd arg has no attributes, else
               we may not get attributes of result right. */
            if (n1==n2 && TYPEOF(s1)==typ && NAMEDCNT_EQ_0(s1)
                       && ATTRIB(s2)==R_NilValue)
                return s1;
        }
    } else {
        if (TYPEOF(s1)==typ && NAMEDCNT_EQ_0(s1))
            return s1;
    }

    return R_NilValue;
}


/* ======== These need direct access to gp field for efficiency ======== */

/* FIXME: consider inlining here */
/* this has NA_STRING = NA_STRING */
int Seql(SEXP a, SEXP b)
{
    /* The only case where pointer comparisons do not suffice is where
      we have two strings in different encodings (which must be
      non-ASCII strings). Note that one of the strings could be marked
      as unknown. */
    if (a == b) return 1;
    /* Leave this to compiler to optimize */
    if (IS_CACHED(a) && IS_CACHED(b) && ENC_KNOWN(a) == ENC_KNOWN(b))
	return 0;
    else {
    	SEXP vmax = R_VStack;
    	int result = !strcmp(translateCharUTF8(a), translateCharUTF8(b));
    	R_VStack = vmax; /* discard any memory used by translateCharUTF8 */
    	return result;
    }
}


/* A count of the memory used by an object.

   This is called from user-level, so only some types of objects are important.
   
   An object gets charged for all the space allocated on the heap and
   all the nodes specifically due to it (including padding to the size
   of its node class), but not for the space for its name, nor for
   .Internals it references, nor for unused padding in pages of nodes. 

   Sharing of CHARSXPs withing a string (eg, in c("abc","abc")) is accounted
   for, but not other types of sharing (eg, in list("abc","abc")).

   Constant objects (in const-objs.c) are counted as being of zero size.
*/


SEXP csduplicated(SEXP x);  /* from unique.c */

static R_size_t objectsize(SEXP s)
{
    int i;
    SEXP tmp, dup;
    Rboolean isVec = FALSE;
    R_size_t cnt = 0;

    if (IS_CONSTNAT(s)) 
       return 0;

    switch (TYPEOF(s)) {
    case NILSXP:
	return(0);
	break;
    case SYMSXP:
	break;
    case LISTSXP:
    case LANGSXP:
    case BCODESXP:
	cnt += objectsize(TAG(s));
	cnt += objectsize(CAR(s));
	cnt += objectsize(CDR(s));
	break;
    case CLOSXP:
	cnt += objectsize(FORMALS(s));
	cnt += objectsize(BODY(s));
	/* no charge for the environment */
	break;
    case ENVSXP:
    case PROMSXP:
    case SPECIALSXP:
    case BUILTINSXP:
	break;
    case RAWSXP:
    case CHARSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
	isVec = TRUE;
	break;
    case STRSXP:
	dup = csduplicated(s);
	for (i = 0; i < LENGTH(s); i++) {
	    tmp = STRING_ELT(s, i);
	    if(tmp != NA_STRING && !LOGICAL(dup)[i])
		cnt += objectsize(tmp);
	}
	isVec = TRUE;
	break;
    case DOTSXP:
    case ANYSXP:
	/* we don't know about these */
	break;
    case VECSXP:
    case EXPRSXP:
    case WEAKREFSXP:
	/* Generic Vector Objects */
	for (i = 0; i < length(s); i++)
	    cnt += objectsize(VECTOR_ELT(s, i));
	isVec = TRUE;
	break;
    case EXTPTRSXP:
#if 0  /* disabled - seems to make no sense */
	cnt += sizeof(void *);  /* the actual pointer */
#endif
	cnt += objectsize(EXTPTR_PROT(s));
	cnt += objectsize(EXTPTR_TAG(s));
	break;
    case S4SXP:
	/* Has TAG and ATTRIB but no CAR nor CDR */
	cnt += objectsize(TAG(s));
	break;
    default:
	UNIMPLEMENTED_TYPE("object.size", s);
    }

    if (!isVec)
        cnt += sizeof_SEXPREC;
    else
        cnt += sizeof(SEXPREC_ALIGN) + sizeof(VECREC) *
                ( NODE_CLASS(s) == LARGE_NODE_CLASS 
                    ? getVecSizeInVEC(s)
                    : NodeClassSize[NODE_CLASS(s)] );

    /* add in attributes, except for CHARSXP, where they are actually
       the links for the CHARSXP cache. */
    if(TYPEOF(s) != CHARSXP) cnt += objectsize(ATTRIB(s));

    return(cnt);
}


static SEXP do_objectsize(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    return ScalarReal( (double) objectsize(CAR(args)) );
}


/* .Internal function for debugging the valgrind instrumentation code... */

volatile int R_valgrind_test_int;   /* places to store/access data */
volatile int R_valgrind_test_real;
volatile int R_valgrind_test_real2;

static SEXP do_testvalgrind(SEXP call, SEXP op, SEXP args, SEXP env)
{
    R_len_t sizel = asInteger(CAR(args));

    if (sizel == NA_INTEGER) {
        REprintf(
          "Using malloc'd memory for testvalgrind (level %d)\n", 
           VALGRIND_LEVEL);
        int *p = malloc(2*sizeof(int));
        REprintf("Undefined read for 'if'\n");
        if (*p==0) R_valgrind_test_real = 987; else R_valgrind_test_real += 33;
        REprintf("OK write\n");
        *p = 7+R_valgrind_test_real2;
        REprintf("OK read for 'if'\n");
        if (*p==0) R_valgrind_test_real = 9876; else R_valgrind_test_real += 37;
        REprintf("OK write\n");
        *(p+1) = 8+R_valgrind_test_real2;
#if VALGRIND_LEVEL>0
        VALGRIND_MAKE_MEM_NOACCESS(p,2*sizeof(int));
#endif
        REprintf("Not OK write\n");
        *p = 9+R_valgrind_test_real2;
        REprintf("Not OK read\n");
        R_valgrind_test_real = *(p+1);
#if VALGRIND_LEVEL>0
        VALGRIND_MAKE_MEM_DEFINED(p+1,sizeof(int));
#endif
        REprintf("Not OK read\n");
        R_valgrind_test_real = *p;
        REprintf("OK read\n");
        R_valgrind_test_real = *(p+1);
        /* Note: p not freed */
    }
    else if (sizel<0) {
        sizel = -sizel;
        REprintf(
          "Allocating integer vector of size %d for testvalgrind (level %d)\n",
           sizel, VALGRIND_LEVEL);
        SEXP vec = allocVector(INTSXP,sizel);

        REprintf("Invalid read before start of object\n");
        R_valgrind_test_int = ((int*)vec)[-1];
        REprintf("Invalid read after end of object\n");
        R_valgrind_test_int = INTEGER(vec)[sizel];

        REprintf("Invalid read used for 'if' from beginning of vector\n");
        if (INTEGER(vec)[0]>1) R_valgrind_test_int = 123; 
        else R_valgrind_test_int += 456;
        REprintf("Invalid read used for 'if' from end of vector\n");
        if (INTEGER(vec)[sizel-1]>1) R_valgrind_test_int = 987; 
        else R_valgrind_test_int += 654;

        REprintf("Store at beginning of vector\n");
        INTEGER(vec)[0] = 1234;
        REprintf("Store at end of vector\n");
        INTEGER(vec)[sizel-1] = 5678;

        REprintf("Valid read used for 'if' from beginning of vector\n");
        if (INTEGER(vec)[0]>1) R_valgrind_test_int = 123; 
        else R_valgrind_test_int += 456;
        REprintf("Valid read used for 'if' from end of vector\n");
        if (INTEGER(vec)[sizel-1]>1) R_valgrind_test_int = 987; 
        else R_valgrind_test_int += 654;

        REprintf("Do a garbage collection\n");
        R_gc();

        REprintf("Invalid read at beginning of no-longer-existing vector\n");
        R_valgrind_test_int = INTEGER(vec)[0];
        REprintf("Invalid read at end of no-longer-existing vector\n");
        R_valgrind_test_int = INTEGER(vec)[sizel-1];
        REprintf("Done testvalgrind\n");
    }
    else {
        REprintf(
          "Allocating real vector of size %d for testvalgrind (level %d)\n",
           sizel, VALGRIND_LEVEL);
        SEXP vec = allocVector(REALSXP,sizel);

        REprintf("Invalid read before start of object\n");
        R_valgrind_test_real = ((double*)vec)[-1];
        REprintf("Invalid read after end of object\n");
        R_valgrind_test_real = REAL(vec)[sizel];

        REprintf("Invalid read used for 'if' from beginning of vector\n");
        if (REAL(vec)[0]>1) R_valgrind_test_real = 123; 
        else R_valgrind_test_real += 456;
        REprintf("Invalid read used for 'if' from end of vector\n");
        if (REAL(vec)[sizel-1]>1) R_valgrind_test_real = 987; 
        else R_valgrind_test_real += 654;

        REprintf("Store at beginning of vector\n");
        REAL(vec)[0] = 1234;
        REprintf("Store at end of vector\n");
        REAL(vec)[sizel-1] = 5678;

        REprintf("Valid read used for 'if' from beginning of vector\n");
        if (REAL(vec)[0]>1) R_valgrind_test_real = 123; 
        else R_valgrind_test_real += 456;
        REprintf("Valid read used for 'if' from end of vector\n");
        if (REAL(vec)[sizel-1]>1) R_valgrind_test_real = 987; 
        else R_valgrind_test_real += 654;

        REprintf("Do a garbage collection\n");
        R_gc();

        REprintf("Invalid read at beginning of no-longer-existing vector\n");
        R_valgrind_test_real = REAL(vec)[0];
        REprintf("Invalid read at end of no-longer-existing vector\n");
        R_valgrind_test_real = REAL(vec)[sizel-1];
        REprintf("Done testvalgrind\n");
    }

    return R_NilValue;
}

/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_memory[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

{"reg.finalizer",do_regFinaliz,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"gctorture",	do_gctorture,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"gctorture2",	do_gctorture2,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"gcinfo",	do_gcinfo,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"gc",		do_gc,		0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"gc.time",	do_gctime,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"mem.limits",	do_memlimits,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"memory.profile",do_memoryprofile, 0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"pnamedcnt",	do_pnamedcnt,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"Rprofmem",	do_Rprofmem,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"object.size",	do_objectsize,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"testvalgrind",do_testvalgrind,0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};
