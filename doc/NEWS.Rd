\newcommand{\PR}{\Sexpr[results=rd]{tools:::Rd_expr_PR(#1)}}
\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{NEWS}
\title{ pqR News}
\encoding{UTF-8}


\section{CHANGES IN VERSION RELEASED 2014-06-06}{

  \subsection{INTRODUCTION}{

  \itemize{ 

\item This is a maintenance release, with bug fixes, documentation
      improvements (including provision of previously missing
      documentation), and changes for compatibility with R Core releases.
      There are some new features in this release that help
      with testing pqR and packages. There are no significant changes 
      in performance.

\item See the sections below on earlier releases for general
      information on pqR.
  }}

  \subsection{NEW FEATURES FOR TESTING}{
    \itemize{
\item The setting of the \code{R_SEED} environment variable now specifies what
      random number seed to use when \code{set.seed} is not called.  When
      \code{R_SEED} is not set, the seed will be set from the time and process
      ID as before.  It is recommended that \code{R_SEED} be set before running
      tests on pqR or packages, so that the results will be reproducible.
      For example, some packages report an error if a hypothesis test on
      simulated data results in a p-value less than some threshold.  If 
      \code{R_SEED} is not set, these packages will fail their tests now 
      and then at random, whereas setting \code{R_SEED} will result either
      in consistent success or (less likely) consistent failure.
\item The comparison of test output with saved output using \code{Rdiff} now
      ignores any output from \code{valgrind}, so spurious errors will not be
      triggered by using it.  When using \code{valgrind}, the
      output files should be checked manually for \code{valgrind} messages 
      that are of possible interest.
\item The test script in \file{tests/internet.R} no longer looks at CRAN's html
      code, which is subject to change.  It instead looks at a special test file
      at \url{pqR-project.org}.
  }}

  \subsection{CHANGE FOR COMPATIBILITY}{
    \itemize{
\item The result of \code{diag(list(1,3,5))} is now a matrix of type
      double.  In R-2.15.0, this expression did not produce a sensible
      result.  A previous fix in pqR made this expression produce a matrix of 
      type list.  A later change by R Core also fixed this, but so it
      produced a double matrix, coercing the list to a numeric vector
      (to the extent possible); pqR now does the same.
  }}

  \subsection{DOCUMENTATION UPDATES}{
    \itemize{
\item The documentation for \code{c} now says how the names for the 
      result are determined, including previously missing information
      on the \code{use.names} argument, and on the role of the names of
      arguments in the call of \code{c}.  This documentation is missing
      in R-2.15.0 and R-3.1.0.
\item The documentaton for \code{diag} now documents that a diagonal matrix 
      is always created with type double or complex, and that the
      names of an extracted diagonal vector are taken from a \code{names}
      attribute (if present), if not from the row and column names.  This
      information is absent in the documentation in R-2.15.1 and R-3.1.0.
\item Incorrect information regarding the pointer protection stack
      was removed from \code{help(Memory)}.  This incorrect information
      is present in R-2.15.0 and R-3.1.0 as well.
\item There is now information in \code{help(Arithmetic)} regarding what
      happens when the operands of an arithmetic operation are \code{NA}
      or \code{NaN}, including the arbitrary nature of the result when
      one operand is \code{NA} and the other is \code{NaN}.  There is
      no discussion of this issue in the documentation for R-2.15.0 and R-3.1.0.
\item The \code{R_HELPERS} and \code{R_HELPERS_TRACE} environment variables
      are now documented in \code{help("environment variables")}.  The
      documentation in \code{help(helpers)} has also been clarified.
  }}

  \subsection{BUG FIXES}{
    \itemize{
\item Fixed a lack of protection bug that arose when \code{log} produced a
      warning.
\item Fixed a stack imbalance bug that showed up when an assignment was
      made to an array of three or more dimensions using a zero-length
      subscript.
\item Fixed a problem with \code{news()} that was due to pqR's version
      numbers being dates (pqR issue #1).
\item Fixed a bug in the foreign package that showed up on systems where
      the C \code{char} type is unsigned, such as a Rasberry Pi running
      Rasbian.  I reported this to R Core, and it is also fixed in R-3.1.0.
\item Fixed out-of-bound memory accesses in \code{R_chull} and \code{scanFrame}
      that valgrind reports (but which are likely to be innocuous).
  }}

  \subsection{BUG FIXES CORRESPONDING TO THOSE IN LATER R CORE RELEASES}{
    \itemize{
\item From R-2.15.1: The string "infinity" now converts correctly to \code{Inf}
      (PR#14933).
\item From R-2.15.1: The generic for backsolve is now correct (PR#14883).
\item From R-2.15.1: A bug in \code{get_all_vars} was fixed (PR#14847).
\item From R-3.1.0-patched: Fixed a problem with \code{mcmapply} not 
      parallelizing when the number of jobs was less than number of cores.
      (However, unlike R-3.1.0-patched, this fix doesn't try to 
      parallelize when there is only one core.)
  }}
}

\section{CHANGES IN VERSION RELEASED 2014-02-23}{

  \subsection{INTRODUCTION}{

  \itemize{ 

\item This is a maintenance release, with bug fixes, changes for
      compatibility with packages, additional correctness tests, and 
      documentation improvements.  There are no new features in this 
      release, and no significant changes in performance.

\item See the sections below on earlier releases for general
      information on pqR.
  }}

  \subsection{INSTALLATION AND TESTING}{
    \itemize{
\item The information in the file "INSTALL" in the main source directory 
      has been re-written.  It now contains all the information expected
      to be needed for most installations, without the user needing to
      refer to R-admin, including information on the configuration
      options that have been added for pqR.  It also has information on
      how to build pqR from a development version downloaded from github.

\item Additional tests regarding subsetting operations, maintenance of
      NAMEDCNT, and operation of helper threads have been written.
      They are run with \code{make check} or \code{make check-all}.

\item A "create-configure" shell script is now included, which allows
      for creation of the "configure" shell script when it is non-functional
      or not present (as when building from a development version of pqR).
      It is not needed for typical installs of pqR releases.

\item Some problems with installation on Microsoft Windows (identified
      by Yu Gong) have hopefully been fixed.  (But trying to install
      pqR on Windows is still recommended only for adventurous users.)

\item A problem with installing pqR as a shared library when multithreading
      is disabled has been fixed.

\item Note that any packages (except those written only in R, plus 
      C or Fortran routines called by \code{.C} or \code{.Fortran}) that
      were compiled and installed under R Core versions of R must be 
      re-installed for use with pqR, as is generally the case with new versions
      of R (although it so happens that it is not necessary to re-install
      packages installed with pqR-2013-07-22 or pqR-2013-12-29 with this 
      release, because the formats of the crucial internal data structures 
      happen not to have changed).
  }}

  \subsection{DOCUMENTATION UPDATES}{
    \itemize{
\item The instructions in "INSTALL" have been re-written, as noted above.
\item The manual on "Writing R Extensions" now has additional information 
      (in the section on "Named objects and copying") on paying proper attention
      to NAMED for objects found in lists.
\item More instructions on how to create a release branch of pqR from a 
      development branch have been added to mods/README (or MODS).
  }}

  \subsection{CHANGES REGARDING PACKAGE COMPATIBILITY AND CHECKING}{
    \itemize{
\item Changed the behaviour of \code{$} when dispatching so that the unevaluated
      element name arrives as a string, as in R-2.15.0.  This behaviour is
      needed for the "dyn" package.  The issue is illustrated by the
      following code:
\preformatted{    a <- list(p=3,q=4)
    class(a) <- "fred"
    `$.fred` <-
      function (x,n) { print(list(n,substitute(n))); x[[n]] }
    print(a$q)
}
      In R-2.15.0, both elements of the list printed are strings, but in
      pqR-2013-12-29, the element from "substitute" is a symbol.  Changed
      \code{help("$")} to document this behaviour, and the corresponding 
      behaviour of \code{"$<-"}.  Added a test with \code{make check} for it.
\item Redefined "fork" to "Rf_fork" so that helper threads can be disabled
      in the child when "fork" is used in packages like "multicore". 
      (Special mods for this had previously been made to the "parallel" 
      package, but this is a more universal scheme.)
\item Added an option (currently set) for pqR to ignore incorrect zero
      pointers encountered by the garbage collector (as R-2.15.0 does).
      This avoids crashes with some packages (eg, "birch") that incorrectly
      set up objects with zero pointers.
\item Changed a C procedure name in the "matprod" routines to reduce the
      chance of a name conflict with C code in packages.
\item Made \code{NA_LOGICAL} and \code{NA_INTEGER} appear as variables
      (rather than constants) in packages, as needed for package
      "RcppEigen".
\item Made \code{R_CStackStart} and \code{R_CStackLimit} visible to 
      packages, as needed for package "vimcom".
\item Fixed problem with using \code{NAMED} in a package that defines
      \code{USE_RINTERNALS}, such as "igraph".
\item Calls of external routines with .Call and .External are now
      followed by checks that the routine didn't incorrectly change 
      the constant objects sometimes used internally in pqR for TRUE, 
      FALSE, and NA.  (Previously, such checks were made only after calls 
      of .C and .Fortran.)
  }}

  \subsection{BUG FIXES}{
    \itemize{
\item Fixed the following bug (also present in R-2.15.0 and R-3.0.2):
\preformatted{    x <- t(5)
    print (x \%*\% c(3,4))
    print (crossprod(5,c(3,4)))
}
     The call of \code{crossprod} produced an error, whereas the corresponding
     use of \code{\%*\%} does not.

     In pqR-2013-12-29, this bug also affected the expression 
     \code{t(5) \%*\% c(3,4)}, since it is converted to the equivalent of 
     \code{crossprod(5,c(3,4))}.

\item Fixed a problem in R_AllocStringBuffer that could result in
      a crash due to an invalid memory access.  (This bug is also
      present in R-2.15.0 and R-3.0.2.)
\item Fixed a bug in a "matprod" routine sometimes affecting 
      \code{tcrossprod} (or an equivalent use of \code{\%*\%}) with 
      helper threads.
\item Fixed a bug illustrated by the following:
\preformatted{    f <- function (a)
    { x <- a
      function () { b <- a; b[2]<-1000; a+b  }
    }
    g <- f(c(7,8,9))
    save.image("tmpimage")
    load("tmpimage")
    print(g())
}
      where the result printed was 14 2000 18 rather than 14 1008 18.
\item Fixed a bug in \code{prod} with an integer vector containing \code{NA}, 
      such as, \code{prod(NA)}.
\item Fixed a lack-of-protection bug in mkCharLenCE that showed up
      in checks for packages "cmrutils".
\item Fixed a problem with xtfrm demonstrated by the following:
\preformatted{    f<-function(...) xtfrm(...); f(c(1,3,2))
}
      which produced an error saying '...' was used in an incorrect context.
      This affected package "lsr". 
\item Fixed a bug in maintaining NAMEDCNT when assigning to a variable in
      an environment using \code{$}, which showed up in package "plus".
\item Fixed a bug that causes the code below to create a circular data 
      structure:
\preformatted{    { a <- list(1); a[[1]] <- a; a }
}
\item Fixed bugs such as that illustrated below:
\preformatted{    a <- list(list(list(1)))
    b <- a
    a[[1]][[1]][[1]]<-2
    print(b)
}
      in which the assignment to \code{a} changes \code{b}, and added tests
      for such bugs.
\item Fixed a bug where unary minus might improperly reuse its operand for
      the result even when it was logical (eg, in \code{-c(F,T,T,F)}).
\item Fixed a bug in pairlist element deletion, and added tests in subset.R
      for such cases.
\item The ISNAN trick (if enabled) is now used only in the interpreter itself,
      not in packages, since the macro implementing it evaluates its argument
      twice, which doesn't work if it has side effects (as happens in the 
      "ff" package).
\item Fixed a bug that sometimes resulted in task merging being disabled
      when it shouldn't have been.
}}
}

\section{CHANGES IN VERSION RELEASED 2013-12-29}{

  \subsection{INTRODUCTION}{

  \itemize{ 

\item This is the first publicized release of pqR after pqR-2013-07-22.  
      A verson dated 2013-11-28 was released for testing; it differs
      from this release only in bug and documentation fixes, which
      are not separately detailed in this NEWS file.

\item pqR is based on R-2.15.0, distributed by the R Core Team, but
improves on it in many ways, mostly ways that speed it up, but also by
implementing some new features and fixing some bugs.  See the notes
below on earlier pqR releases for general discussion of pqR, and for
information that has not changed from previous releases of pqR.

\item The most notable change in this release is that ``task merging''
      is now implemented.  This can speed up sequences
      of vector operations by merging several operations into one, which 
      reduces time spent writing and later reading data in memory. 
      See \code{help(merging)} and the item below for more details.

\item This release also includes other performance improvements, bug fixes,
      and code cleanups, as detailed below.
  }}

  \subsection{INSTALLATION AND TESTING}{
    \itemize{

\item Additional configuration options are now present to allow
      enabling and disabling of task merging, and more generally, of the
      deferred evaluation framework needed for both task merging and
      use of helper threads.  By default, these facilities are enabled.
      The \code{--disable-task-merging} option to \code{./configure}
      disables task merging, \code{--disable-helper-threads} disables
      support for helper threads (as before), and 
      \code{--disable-deferred-evaluation} disables both of these
      features, along with the whole deferred evaluation framework.
      See the \code{R-admin} manual for more details.

\item See the pqR wiki at \code{https://github.com/radfordneal/pqR/wiki}
      for the latest news regarding systems and packages that do or do not
      work with pqR.

\item Note that any packages (except those written only in R, plus 
      C or Fortran routines called by \code{.C} or \code{.Fortran}) that
      were compiled and installed under R Core versions of R must be 
      re-installed for use with pqR, as is generally the case with new versions
      of R (although it so happens that it is not necessary to re-install
      packages installed with pqR-2013-07-22 with this release, because the 
      formats of the crucial internal data structures happen not to have
      changed).

\item Additional tests of matrix multiplication (\code{\%*\%}, \code{crossprod},
      and \code{tcrossprod}) have been written.  They are run with
      \code{make check} or \code{make check-all}.

  }}

  \subsection{INTERNAL STRUCTURES AND APPLICATION PROGRAM INTERFACE}{
    \itemize{

\item The table of built-in function names, C functions implementing them, and
      operation flags, which was previously found in \code{src/main/names.c},
      has been split into multiple tables, located in the source files that 
      define such built-in functions (with only a few entries still in 
      \code{names.c}).  This puts the descriptions of these built-in
      functions next to their definitions, improving maintainability, and
      also reduces the number of global functions.  This change should have 
      no effects visible to users.

\item The initialization for fast dispatch to some primitive functions
      is now done in \code{names.c}, using tables in other source files
      analogous to those described in the point just above.  This is 
      cleaner, and eliminates an anomaly in the previous versions of
      pqR that a primitive function could be slower the first time it was
      used than when used later.
  }}

  \subsection{PERFORMANCE IMPROVEMENTS}{
    \itemize{
\item Some sequences of vector operations can now be merged into a single
      operation, which can speed them up by eliminating memory operations
      to store and fetch intermediate results.  For example, when \code{v} is 
      a long vector, the expression 
      \code{exp(v+1)} can be merged into one task, which will compute 
      \code{exp(v[i]+1)} for each element, \code{i}, of \code{v} in a 
      single loop.  

      Currently, such ``task merging'' is done only for (some)
      operations in which only one operand is a vector. When there are
      helper threads (which might be able to do some operations even
      faster, in parallel) merging is done only when one of the
      operations merged is a simple addition, subtraction, or
      multiplication (with one vector operand and one scalar operand).

      See \code{help(merging)} for more details.

\item During all garbage collections, any tasks whose outputs are not
      referenced are now waited for, to allow memory used by their outputs to be
      recovered.  (Such unreferenced outputs should be rare in real 
      programs.)  In a full garbage collection, tasks with large inputs
      or outputs that are referenced only as task inputs
      are also waited for, so that the memory they occupy can be recovered.

\item The built-in C matrix multiplication routines and those in the supplied 
      BLAS have both been sped up, especially those used by \code{crossprod}
      and \code{tcrossprod}.  This will of course have no effect if a different
      BLAS is used and the \code{mat_mult_with_BLAS} option is set to
      \code{TRUE}.

\item Matrix multiplications in which one operand can be recognized as the
      result of a transpose operation are now done without actually creating
      the transpose as an intermediate result, thereby reducing both 
      computation time and memory usage.  Effectively, these uses of the
      \code{\%*\%} operator are converted to uses of \code{crossprod} or
      \code{tcrossprod}.  See \code{help("\%*\%")} for details.

\item Speed of \code{ifelse} has been improved (though it's now slower when the
      condition is scalar due to the bug fix mentioned below).

\item Inputs to the mod operator can now be piped. (Previously, this was 
      inadvertently prevented in some cases.)

\item The speed of the quick check for NA/NaN that can be enabled with 
      \code{-DENABLE_ISNAN_TRICK} in CFLAGS has been improved.
  }}

  \subsection{BUG FIXES}{
    \itemize{
\item Fixed a bug in \code{ifelse} with scalar condition but other
      operands with length greater than one.  (Pointed out by Luke Tierney.)

\item Fixed a bug stemming from re-use of operand storage for a result
      (pointed out by Luke Tierney) illustrated by the following:
\preformatted{   A <- array(c(1), dim = c(1,1), dimnames = list("a", 1))
   x <- c(a=1)
   A/(pi*x)
}

\item The \code{--disable-mat-mult-with-BLAS-in-helpers} configuration
      setting is now respected for complex matrix multiplication
      (previously it had only disabled use of the BLAS in helper
      threads for real matrix multiplication).

\item The documentation for \code{aperm} now says that the default
      method does not copy attributes (other than dimensions and
      dimnames).  Previously, it incorrecty said it did (as is the
      case also in R-2.15.0 and R-3.0.2).

\item Changed \code{apply} from previous versions of pqR to replicate
      the behaviour seen in R-2.15.0 (and later R Core version) when the matrix 
      or array has a class attribute.  Documented this behaviour (which is
      somewhat dubious and convoluted) in the help entry for \code{apply}.
      This change fixes a problem seen in package TSA (and probably others).

\item Changed \code{rank} from prevous versions of pqR to replicate
      the behaviour when it is applied to data frames that is seen in R-2.15.0 
      (and later R Core versions).  Documented this (somewhat dubious) 
      behaviour in the help entry for \code{rank}.  This change fixes a
      problem in the \code{coin} package.

\item Fixed a bug in keeping track of references when assigning 
      repeated elements into a list array.

\item Fixed the following bug (also present in R-2.15.0 and R-3.0.2):
\preformatted{   v <- c(1,2)
   m <- matrix(c(3,4),1,2)
   print(t(m)\%*\%v)
   print(crossprod(m,v))
}
in which \code{crossprod} gave an error rather than produce the answer
for the corresponding use of \code{\%*\%}.

\item Bypassed a problem with the Xcode gcc compiler for the Mac that 
      led to it falsely saying that using -DENABLE_ISNAN_TRICK in CFLAGS
      doesn't work.
  }}
}


\section{CHANGES IN VERSION RELEASED 2013-07-22}{

  \subsection{INTRODUCTION}{

  \itemize{ 

\item pqR is based on R-2.15.0, distributed by the R Core Team, but
improves on it in many ways, mostly ways that speed it up, but also by
implementing some new features and fixing some bugs.  See the notes
below, on the release of 2013-06-28, for general discussion of pqR,
and for information on pqR that has not changed since that release.

\item This updated release of pqR provides some performance
enhancements and bug fixes, including some from R Core releases after
R-2.15.0.  More work is still needed to incorporate improvements in
R-2.15.1 and later R Core releases into pqR.

\item This release is the same as the briefly-released version of
2013-17-19, except that it fixes one bug and one reversion of an
optimization that were introduced in that release, and tweaks the
Windows Makefiles (which are not yet fully tested).

  }}

  \subsection{FEATURE AND DOCUMENTATION CHANGES}{
    \itemize{
      \item Detailed information on what operations can be done in helper
            threads is now provided by help(helpers).
      \item Assignment to parts of a vector via code such as 
            \code{v[[i]]<-value} and \code{v[ix]<-values} now automatically 
            converts raw values to the appropriate type
            for assignment into numeric or string vectors, and assignment
            of numeric or string values into a raw vector now results in the
            raw vector being first converted to the corresponding type.  This
            is consistent with the existing behaviour with other types.
      \item The allowed values for assignment to an element of an "expression" 
            list has been expanded to match the allowed values for ordinary
            lists.  These values (such as function closures) could previously 
            occur in expression lists as a result of other operations (such
            as creation with the \code{expression} primitive).
      \item Operations such as
            \code{v <- pairlist(1,2,3); v[[-2]] <- NULL} now raise an error.
            These operations were previously documented as being illegal, and
            they are illegal for ordinary lists.  The proper way to do
            this deletion is \code{v <- pairlist(1,2,3); v[-2] <- NULL}.
      \item Raising \code{-Inf} to a large value (eg, \code{(-Inf)^(1e16)})
            no longer produces an incomprehensible warning.  As before, the 
            value returned is \code{Inf}, because (due to their 
            limited-precision floating-point representation) all such large 
            numbers are even integers.
  }}

  \subsection{FEATURE CHANGES CORRESPONDING TO THOSE IN LATER R CORE RELEASES}{
    \itemize{
\item From R-2.15.1: On Windows, there are two new environment variables which
      control the defaults for command-line options.

      If \env{R_WIN_INTERNET2} is set to a non-empty value, it is as if
      \option{--internet2} was used.

      If \env{R_MAX_MEM_SIZE} is set, it gives the default memory limit
      if \option{--max-mem-size} is not specified: invalid values being
      ignored.

\item From R-2.15.1: The NA warning messages from e.g. \code{pchisq()} now 
      report the call to the closure and not that of the \code{.Internal}.

\item The following included software has been updated to new versions:
      zlib to 1.2.8, LZMA to 5.0.4, and PCRE to 8.33.
  }}

  \subsection{INSTALLATION AND TESTING}{
    \itemize{

\item See the pqR wiki at \code{https://github.com/radfordneal/pqR/wiki}
      for the latest news regarding systems and packages that do or do not
      work with pqR.

\item Note that any previosly-installed packages must be re-installed for 
      use with pqR (as is generally the case with new versions of R), except
      for those written purely in R.

\item It is now known that pqR can be successfully installed under Mac
      OS X for use via the command line (at least with some versions
      of OS X).  The gcc 4.2
      compiler supplied by Apple with Xcode works when helper threads
      are disabled, but does not have the full OpenMP support required for
      helper threads.   For helper threads to work, a C compiler that fully
      supports OpenMP is needed, such as gcc 4.7.3 (available via 
      macports.org).

      The Apple BLAS and LAPACK routines can be used by giving the
      \code{--with-blas='-framework vecLib'} and \code{--withlapack}
      options to \code{configure}.  This speeds up some operations
      but slows down others.

      The R Mac GUI would need to be recompiled for use with pqR.
      There are problems doing this unless helper threads
      are disabled (see pqR issue #17 for discussion).

      Compiled binary versions of pqR for Mac OS X are not yet being supplied.
      Installation on a Mac is recommended only for users experienced
      in installation of R from source code.

\item Success has also been reported in installing pqR on a Windows
      system, including with helper threads, but various tweaks were
      required.  Some of these tweaks are incorporated in this release,
      but they are probably not sufficient for installation "out of the box".
      Attempting to install pqR on Windows is recommended only for
      users who are both experienced and adventurous.

\item Compilation using the \code{-O3} option for gcc is not recommended.
      It speeds up some operations, but slows down others.  With gcc 4.7.3
      on a 32-bit Intel system running Ubuntu 13.04, compiling with 
      \code{-O3} causes compiled functions to crash. (This is not a
      pqR issue, since the same thing happens when R-2.15.0 is compiled 
      with \code{-O3}).
  }}

  \subsection{INTERNAL STRUCTURES AND APPLICATION PROGRAM INTERFACE}{
    \itemize{

\item The R internals manual now documents (in Section 1.8) a
      preliminary set of conventions that pqR follows (not yet
      perfectly) regarding when objects may be modified, and how
      NAMEDCNT should be maintained.  R-2.15.0 did not follow any
      clear conventions.

\item The documentation in the R internals manual on how helper
      threads are implemented in pqR now has the correct title.  (It
      would previously have been rather hard to notice.)

  }}

  \subsection{PERFORMANCE IMPROVEMENTS}{
    \itemize{
\item Some unnecessary duplication of objects has been eliminated.  Here
      are three examples:  
      Creation of lists no longer duplicates all the elements put in the
      list, but instead increments \code{NAMEDCNT} for these elements, so
      that
\preformatted{   a <- numeric(10000)
   k <- list(1,a)
}
no longer duplicates \code{a} when \code{k} is created (though a duplication
will be needed later if either \code{a} or \code{k[[2]]} is modified).
      Furthermore, the assignment below to \code{b$x}, no longer
      causes duplication of the 10000 elements of \code{y}:
\preformatted{   a <- list (x=1, y=seq(0,1,length=10000))
   b <- a
   b$x <- 2
}
Instead, a single vector of 10000 elements is shared between \code{a$y} and
\code{b$y}, and will be duplicated later only if necessary.  Unnecessary
duplication of a 10000-element vector is also avoided when \code{b[1]} is 
assigned to in the code below:
\preformatted{   a <- list (x=1, y=seq(0,1,length=10000))
   b <- a$y
   a$y <- 0
   b[1] <- 1
}
The assignment to \code{a$y} now reduces \code{NAMEDCNT} for the vector
bound to \code{b}, allowing it to be changed without duplication.

\item Assignment to part of a vector using code such as \code{v[101:200]<-0}
      will now not actually create a vector of 100 indexes, but will instead
      simply change the elements with indexes 101 to 200 without creating
      an index vector.  This optimization has not yet been implemented for
      matrix or array indexing.

\item Assignments to parts of vectors, matrices, and arrays using "[" has been
      sped up by detailed code improvements, quite substantially in some
      cases.

\item Subsetting of arrays of three or more dimensions using "[" has
      been sped up by detailed code improvements.

\item Pending summations of one-argument mathematical functions are now
      passed on by \code{sum}.  So, for example, in 
      \code{sum(exp(a)) + sum(exp(b))}, the two
      summations of exponentials can now potentially be done in parallel.

\item A full garbage collection now does not wait for all tasks being
      done by helpers to complete.  Instead, only tasks that are using
      or computing variables that are not otherwise referenced are 
      waited for (so that this storage can be reclaimed).
  }}

  \subsection{BUG FIXES}{
    \itemize{
\item A bug that could have affected the result of \code{sum(abs(v))} when
      it is done by a helper thread has been fixed.
\item A bug that could have allowed \code{as.vector}, \code{as.integer}, etc.
      to pass on an object still being computed to a caller not expecting
      such a pending object has been fixed.
\item Some bugs in which production of warnings at inopportune times could 
      have caused serious problems have been fixed.
\item The bug illustrated below (pqR issue #13) has been fixed:
\preformatted{   > l = list(list(list(1)))
   > l1 = l[[1]]
   > l[[c(1,1,1)]] <- 2
   > l1
   [[1]]
   [[1]][[1]]
   [1] 2
}
\item Fixed a bug (also present in R-2.15.0 and R-3.0.1) illustrated by the
following code:
\preformatted{   > a <- list(x=c(1,2),y=c(3,4))
   > b <- as.pairlist(a)
   > b$x[1] <- 9
   > print(a)
   $x
   [1] 9 2
   
   $y
   [1] 3 4
}
The value printed for a has a$x[1] changed to 9, when it should still be 1.
See pqR issue #14.
\item Fixed a bug (also present in R-2.15.0 and R-3.0.1) in which extending
      an "expression" by assigning to a new element changes it to an ordinary
      list.  See pqR issue #15.
\item Fixed several bugs (also present in R-2.15.0 and R-3.0.1) illustrated
by the code below (see pqR issue #16):
\preformatted{   v <- c(10,20,30)
   v[[2]] <- NULL        # wrong error message
   
   x <- pairlist(list(1,2))
   x[[c(1,2)]] <- NULL   # wrongly gives an error, referring to misuse
                         # of the internal SET_VECTOR_ELT procedure
   
   v<-list(1)
   v[[quote(abc)]] <- 2  # internal error, this time for SET_STRING_ELT
   
   a <- pairlist(10,20,30,40,50,60)
   dim(a) <- c(2,3)
   dimnames(a) <- list(c("a","b"),c("x","y","z"))
   print(a)              # doesn't print names
   
   a[["a","x"]] <- 0     # crashes with a segmentation fault
}
  }}

  \subsection{BUG FIXES CORRESPONDING TO THOSE IN LATER R CORE RELEASES}{
    \itemize{
\item From R-2.15.1: \code{formatC()} uses the C entry point \code{str_signif}
      which could write beyond the length allocated for the output string.

\item From R-2.15.1: \code{plogis(x, lower = FALSE, log.p = TRUE)} no longer
      underflows early for large x (e.g. 800).

\item From R-2.15.1: \code{?Arithmetic}'s \dQuote{\code{1 ^ y} and \code{y ^ 0}
	are \code{1}, \emph{always}} now also applies for \code{integer}
      vectors \code{y}.

\item From R-2.15.1: X11-based pixmap devices like \code{png(type = "Xlib")} 
      were trying to set the cursor style, which triggered some warnings and
      hangs.

\item From R-3.0.1 patched: Fixed comment-out bug in BLAS, as per PR 14964.
  }}
}


\section{CHANGES IN VERSION RELEASED 2013-06-28}{

\subsection{INTRODUCTION}{

\itemize{ 
\item This release of pqR is based on R-2.15.0, distributed by the R
Core Team, but improves on it in many ways, mostly ways that speed it
up, but also by implementing some new features and fixing some bugs.
One notable speed improvement in pqR is that for systems with multiple
processors or processor cores, pqR is able to do some numeric
computations in parallel with other operations of the interpreter, and
with other numeric computations.

\item This is the second publicised release of pqR (the first was on
2013-06-20, and there were earlier unpublicised releases). It fixes one
significant pqR bug (that could cause two empty strings to not compare
as equal, reported by Jon Clayden), fixes a bug reported to R Core (PR
15363) that also existed in pqR (see below), fixes a bug in deciding
when matrix multiplies are best done in a helper thread, and fixes some
issues preventing pqR from being built in some situations (including
some partial fixes for Windows suggested by "armgong").  Since the
rest of the news is almost unchanged from the previous release, I have
not made a separate news section for this release. (New sections will
be created once new releases have significant differences.)

\item This section documents changes in pqR from R-2.15.0 that are of
direct interest to users.  For changes from earlier version of R to
R-2.15.0, see the ONEWS, OONEWS, and OOONEWS files.  Changes of little
interest to users, such as code cleanups and internal details on
performance improvements, are documented in the file MODS, which
relates these changes to branches in the code repository at
github.com/radfordneal/pqR.

\item Note that for compatibility with R's version system, pqR presently
uses the same version number, 2.15.0, as the version of R on which it
is based. This allows checks for feature availability to continue to
work.  This scheme will likely change in the future.  Releases of pqR
with the same version number are distinguished by release date.

\item See radfordneal.github.io/pqR for current information on pqR, including
announcements of new releases, a link to the page for making and viewing
reports of bugs and other issues, and a link to the wiki page containing
information such as systems on which pqR has been tested.

}}

  \subsection{FEATURE CHANGES}{
    \itemize{
      \item A new primitive function \code{get_rm} has been added,
            which removes a variable while returning the value it
            had when removed.  See \code{help(get_rm)} for details,
            and how this can sometimes improve efficiency of R functions.

      \item An enhanced version of the \code{Rprofmem} function for profiling
            allocation of vectors has been implemented, that can
            display more information, and can output to the terminal,
            allowing the source of allocations to more easily be
            determined.  Also, \code{Rprofmem} is now always accessible
            (not requiring the \code{--enable-memory-profiling} configuration
            option).  Its overhead when not in use is negligible.
 
            The new version allows records of memory allocation to be
            output to the terminal, where their position relative to
            other output can be informative (this is the default for the
            new \code{Rprofmemt} variant).  More identifying
            information, including type, number of elements, and
            hexadecimal address, can also be output.  For more details on
            these and other changes, see \code{help(Rprofmem)}.

      \item A new primitive function, pnamedcnt, has been added, that
            prints the NAMEDCNT/NAMED count for an R object, which is helpful
            in tracking when objects will have to be duplicated.  For
            details, see help(pnamedcnt).

      \item The \code{tracemem} function is defunct.  What exactly it was
            supposed to do in R-2.15.0 was unclear, and optimizations 
            in pqR make it even less clear what it should do.  The bit
            in object headers that was used to implement it has been
            put to a better use in pqR.  The \code{--enable-memory-profiling}
            configuration option used to enable it no longer exists.

            The \code{retracemem} function remains for compatibility
            (doing nothing).  The \code{Rprofmemt} and \code{pnamedcnt}
            functions described above provide alternative ways of gaining
            insight into memory allocation behaviour.

      \item Some options that can be set by arguments to the R command can
            now also be set with environment variables, specifically, the
            values of R_DEBUGGER, R_DEBUGGER_ARGS, and R_HELPERS give the
            default when \code{--debugger}, \code{--debugger-args}, and 
            \code{--helpers} are not specified on the command line.  This 
            feature is useful when using a shell file or Makefile that contains 
            R commands that one would rather not have to modify.
    }
  }

  \subsection{INSTALLATION AND TESTING}{
    \itemize{

      \item The procedure for compiling and installing from source is largely 
            unchanged from R-2.15.0.  In particular, the final result is a 
            program called "R", not "pqR", though of course you can provide a 
            link to it called "pqR".  Note that (as for R-2.15.0) it is not 
            necessary to do an "install" after "make" --- one can just
            run bin/R in the directory where you did "make".  This may be 
            convenient if you wish to try out pqR along with your current 
            version of R.

      \item Testing of pqR has so far been done only on Linux/Unix
            systems, not on Windows or Mac systems.  There is no specific
            reason to believe that it will not work on Windows or Mac
            systems, but until tests have been done, trying to use it 
            on these systems is not recommended.  (However, some users
            have reported that pqR can be built on Mac systems, as long
            as a C compiler fully supporting OpenMP is used, or the
            \code{--disable-helper-threads} configuration option is used.)

      \item This release contains the versions of the standard and recommended
            packages that were released with R-2.15.0.  Newer versions may
            or may not be compatible (same as for R-2.15.0).

      \item It is intended that this release will be fully compatible with
            R-2.15.0, but you will need to recompile any packages (other
            that those with only R code) that you had installed for R-2.15.0, 
            and any other C code you use with R, since the format of internal 
            data structures has changed (see below).

      \item New configuration options relating to helper threads and
            to matrix multiplication now exist.  For details, see 
            doc/R-admin.html (or R-admin.pdf), or run \code{./configure --help}.

            In particular, the \code{--disable-helper-threads} option
            to configure will remove support for helper threads.  Use of
            this option is advised if you know that multiple processors
            or processor cores will not be available, or if you know that
            the C compiler used does not support OpenMP 3.0 or 3.1 (which 
            is used in the implementation of the helpers package).

      \item Including \code{-DENABLE_ISNAN_TRICK} in CFLAGS will speed up 
            checks for NA and NaN on machines on which it works.  It works
            on Intel processors (verified both empirically and by consulting
            Intel documentation).  It does not work on SPARC machines.

      \item The \code{--enable-memory-profiling} option to configure
            no longer exists.  In pqR, the \code{Rprofmem} function is always
            enabled, and the \code{tracemem} function is defunct.  (See
            discussion above.)

      \item When installing from source, the output of configure 
            now displays whether standard and recommended packages will
            be byte compiled.

      \item The tests of random number generation run with \code{make check-all}
            now set the random number seed explicitly.  Previously, the random
            number seed was set from the time and process ID, with the result
            that these tests would occasionally fail non-deterministically,
            when by chance one of the p-values obtained was below the threshold
            used.  (Any such failure should now occur consistently, rather
            than appearing to be due to a non-deterministic bug.)

      \item Note that (as in R-2.15.0) the output of \code{make check-all} for 
            the boot package includes many warning messages regarding a 
            non-integer argument, and when byte compilation is enabled, these 
            messages identify the wrong function call as the source.  This 
            appears to have no wider implications, and can be ignored.

      \item Testing of the "xz" compression method is now done with \code{try},
            so that failure will be tolerated on machines that don't have enough
            memory for these tests.

      \item The details of how valgrind is used have changed. See the source
            file \file{memory.c}.
    }
  }

  \subsection{INTERNAL STRUCTURES AND APPLICATION PROGRAM INTERFACE}{
    \itemize{
      \item The internal structure of an object has changed, in ways that 
            should be compatible with R-2.15.0, but which do require 
            re-compilation.  The flags in the object header for \code{DEBUG},
            \code{RSTEP}, and \code{TRACE} now exist only for non-vector 
            objects, which is sufficient for their present use (now that 
            \code{tracemem} is defunct).

      \item The sizes of objects have changed in some cases (though not most).
            For a 32-bit configuration, the size of a cons cell increases
            from 28 bytes to 32 bytes; for a 64-bit configuration, the
            size of a cons cell remains at 56 bytes.  For a 32-bit 
            configuration, the size of a vector of one double remains
            at 32 bytes; for a 64-bit configuration (with 8-byte alignment), 
            the size of a vector of one double remains at 48 bytes.

      \item Note that the actual amount of memory occupied by an object
            depends on the set of node classes defined (which may be tuned).
            There is no longer a separate node class for cons cells and
            zero-length vectors (as in R-2.15.0) --- instead, cons cells
            share a node class with whatever vectors also fit in that
            node class.

      \item The old two-bit NAMED field of an object is now a three-bit
            NAMEDCNT field, to allow for a better attempt at reference
            counting.  Versions of the the NAMED and SET_NAMED macros
            are still defined for compatibility.  See the R-ints manual
            for details.

      \item Setting the length of a vector to something less than its
            allocated length using SETLENGTH is deprecated.  The LENGTH
            field is used for memory allocation tracking by the garbage
            collector (as is also the case in R-2.15.0), so setting it 
            to the wrong value may cause problems.  (Setting the length
            to more than the allocated length is of course even worse.)
    }
  }

  \subsection{PERFORMANCE IMPROVEMENTS}{
    \itemize{
      \item Many detailed improvements have been made that reduce
            general interpretive overhead and speed up particular 
            functions.  Only some of these improvements are noted
            below.

      \item Numerical computations can now be performed in parallel with
            each other and with interpretation of R code, by using 
            ``helper threads'', on machines
            with multiple processors or multiple processor cores.  When
            the output of one such computation is used as the input to
            another computation, these computations can often be done
            in parallel, with the output of one task being ``pipelined''
            to the other task.  Note that these 
            parallel execution facilities do not require any changes to user 
            code --- only that helper threads be enabled with the 
            \code{--helpers} option to the command starting pqR. See 
            \code{help(helpers)} for details.

            However, helper threads are not used for operations that are 
            done within the interpreter for byte-compiled code or that are 
            done in primitive functions invoked by the byte-code interpreter.

            This facility is still undergoing rapid development.  Additional 
            documentation on which operations may be done in parallel will be 
            forthcoming.

      \item A better attempt at counting how many "names" an object has
            is now made, which reduces how often objects are duplicated
            unnecessarily.  This change is ongoing, with further improvements
            and documentation forthcoming.

      \item Several primitive functions that can generate integer sequences
            --- ":", seq.int, seq_len, and seq_along --- will now sometimes
            not generate an actual sequence, but rather just a description
            of its start and end points.  This is not visible to users,
            but is used to speed up several operations.

            In particular, "for" loops such as \code{for (i in 1:1000000) ...}
            are now done without actually allocating a vector to hold
            the sequence.  This saves both space and time.  Also,
            a subscript such as \code{101:200} for a vector or as the first 
            subscript for a matrix is now (often) handled without actually 
            creating a vector of indexes, saving both time and space.  

            However, the above performance improvements 
            are not effective in compiled code.

      \item Matrix multiplications with the \code{\%*\%} operator are now
            much faster when the operation is a vector dot product, a
            vector-matrix product, a matrix-vector product, or more generally
            when the sum of the numbers of rows and columns in the result
            is not much less than their product.  This improvement results
            from the elimination of a costly check for NA/NaN elements in the 
            operands before doing the multiply.  There is no need for this check
            if the supplied BLAS is used.  If a BLAS that does not properly
            handle NaN is supplied, the \code{\%*\%} operator will still
            handle NaN properly if the new library of matrix multiply
            routines is used for \code{\%*\%} instead of the BLAS.  See the
            next two items for more relevant details.

      \item A new library of matrix multiply routines is provided, which
            is guaranteed to handle NA/NaN correctly, and which supports
            pipelined computation with helper threads.  Whether this
            library or the BLAS routines are used for \code{\%*\%} is
            controlled by the \code{mat_mult_with_BLAS} option. The default
            is to not use the BLAS, but the 
            \code{--enable-mat-mult-with-BLAS-by-default} configuration option
            will change this.  See \code{help("\%*\%")} for details.

      \item The BLAS routines supplied with R were modified to improve the 
            performance of the routines DGEMM (matrix-matrix multiply) and 
            DGEMV (matrix-vector multiply).  Also, proper propagation of NaN, 
            Inf, etc. is now always done in these routines.  This speeds
            up the \code{\%*\%} operator in R, when the supplied BLAS is used
            for matrix multiplications, and speeds up other matrix operations
            that call these BLAS routines, if the BLAS used is the one supplied.

      \item The low-level routines for generation of uniform random
            numbers have been improved.  (These routines are also used for
            higher-level functions such as \code{rnorm}.)

            The previous code copied the seed back and forth to
            .Random.seed for every call of a random number generation
            function, which is rather time consuming given that for
            the default generator \code{.Random.seed} is 625 integers long.
            It also allocated new space for \code{.Random.seed} every time.
            Now, \code{.Random.seed} is used without copying, except when the 
            generator is user-supplied.  

            The previous code had imposed an unnecessary limit on the
            length of a seed for a user-supplied random number
            generator, which has now been removed.

      \item The \code{any} and \code{all} primitives have been substantially
            sped up for large vectors.  

            Also, expressions such as
            \code{all(v>0)} and \code{any(is.na(v))}, where \code{v} is a
            real vector, avoid computing and storing a logical vector,
            instead computing the result of \code{any} or \code{all}
            without this intermediate, looking at only as much of \code{v}
            as is needed to determine the result.
            However, this improvement is not effective in compiled code.

      \item When \code{sum} is applied to many mathematical functions
            of one vector argument, for example \code{sum(log(v))}, the
            sum is performed as the function is computed, without a
            vector being allocated to hold the function values.  
            However, this improvement is not effective in compiled code.

      \item The handling of power operations has been improved (primarily 
            for powers of reals, but slightly affecting powers of integers too).
            In particular, scalar powers of 2, 1, 0, and -1, are handled 
            specially to avoid general power operations in these cases.

      \item Extending lists and character vectors by assigning to an
            index past the end, and deleting list items by assigning NULL
            have been sped up substantially.

      \item The speed of the transpose (\code{t}) function has been
            improved, when applied to real, integer, and logical
            matrices.

      \item The \code{cbind} and \code{rbind} functions have been greatly
            sped up for large objects.

      \item The \code{c} and \code{unlist} functions have been sped up 
            by a bit in simple cases, and by a lot in some situations 
            involving names.

      \item The \code{matrix} function has been greatly sped up, in
            many cases.

      \item Extraction of subsets of vectors or matrices (eg, \code{v[100:200]}
            or \code{M[1:100,101:110]}) has been sped up substantially.

      \item Logical operations and relational operators have been sped up
            in simple cases. Relational
            operators have also been substantially sped up for long vectors.

      \item Access via the $ operator to lists, pairlists, and environments 
            has been sped up. 

      \item Existing code for handling special cases of "[" in which there is
            only one scalar index was replaced by cleaner code that handles 
            more cases.   The old code handled only integer and real vectors, 
            and only positive indexes.  The new code handles all atomic 
            vectors (logical, integer, real, complex, raw, and string), and 
            positive or negative indexes that are not out of bounds.

      \item Many unary and binary primitive functions are now usually
            called using a faster internal interface that does not allocate
            nodes for a pairlist of evaluated arguments. This change
            substantially speeds up some programs.

      \item Lookup of some builtin/special function symbols (eg, "+" and "if")
            has been sped up by allowing fast bypass of non-global environments
            that do not contain (and have never contained) one of these 
            symbols.

      \item Some binary and unary arithmetic operations have been sped
            up by, when possible, using the space holding one of the
            operands to hold the result, rather than allocating new
            space.  Though primarily a speed improvement, for very
            long vectors avoiding this allocation could avoid running
            out of space.

      \item Some speedup has been obtained by using new internal C functions 
            for performing exact or partial string matches in the interpreter.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item The "debug" facility has been fixed.  Its behaviour for if,
            while, repeat, and for statements when the inner statement
            was or was not one with curly brackets had made no sense.
            The fixed behaviour is now documented in help(debug). 
            (I reported this bug and how
            to fix it to the R Core Team in July 2012, but the bug is 
            still present in R-3.0.1, released May 2013.)

      \item Fixed a bug in \code{sum}, where overflow is allowed (and not 
            detected) where overflow can actually be avoided.  For example:
\preformatted{   > v<-c(3L,1000000000L:1010000000L,-(1000000000L:1010000000L))
   > sum(v)
   [1] 4629
}
            Also fixed a related bug in \code{mean}, applied to an integer
            vector, which would arise only on a system where a long double 
            is no bigger than a double.

      \item Fixed \code{diag} so that it returns a matrix when passed
            a list of elements to put on the diagonal.

      \item Fixed a bug that could lead to mis-identification of the 
            direction of stack growth on a non-Windows system, causing
            stack overflow to not be detected, and a segmentation fault 
            to occur.  (I also reported this bug and how to fix it to the 
            R Core Team, who included a fix in R-2.15.2.)

      \item Fixed a bug where, for example, \code{log(base=4)} returned 
            the natural log of 4, rather than signalling an error. 

      \item The documentation on what \code{MARGIN} arguments are allowed for
            \code{apply} has been clarified, and checks for validity added.
            The call 
\preformatted{   > apply(array(1:24,c(2,3,4)),-3,sum)
}
            now produces correct results (the same as when \code{MARGIN}
            is \code{1:2}).

      \item Fixed a bug in which \code{Im(matrix(complex(0),3,4))} returned
            a matrix of zero elements rather than a matrix of NA elements.

      \item Fixed a bug where more than six warning messages at startup
            would overwrite random memory, causing garbage output 
            and perhaps arbitrarily bizarre behaviour.

      \item Fixed a bug where LC_PAPER was not correctly set at startup.

      \item Fixed gc.time, which was producing grossly incorrect values
            for user and system time.

      \item Now check for bad arguments for .rowSums, .colSums, .rowMeans,
            and .rowMeans (would previously segfault if n*p too big).

      \item Fixed a bug where excess warning messages may be produced
            on conversion to RAW.  For instance:
\preformatted{   > as.raw(1e40)
   [1] 00
   Warning messages:
   1: NAs introduced by coercion 
   2: out-of-range values treated as 0 in coercion to raw 
}
            Now, only the second warning message is produced.

      \item A bug has been fixed in which rbind would not handle 
            non-vector objects such as function closures, whereas
            cbind did handle them, and both were documented to do so.

      \item Fixed a bug in numeric_deriv in stats/src/nls.c, where it
            was not duplicating when it should, as illustrated below:
\preformatted{   > x <- 5; y <- 2; f <- function (y) x
   > numericDeriv(f(y),"y")
    [1] 5
    attr(,"gradient")
         [,1]
    [1,]    0
    > x
    [1] 5
    attr(,"gradient")
         [,1]
    [1,]    0
}

      \item Fixed a bug in vapply illustrated by the following:
\preformatted{   X<-list(456)
   f<-function(a)X
   A<-list(1,2)  
   B<-vapply(A,f,list(0))
   print(B)
   X[[1]][1]<-444
   print(B)
}
            After the fix, the values in \code{B} are no long changed by the 
            assignment to \code{X}. Similar bugs in mapply, eapply, and rapply 
            have also been fixed.  I reported these bugs to r-devel, and
            (different) fixes are in R-3.0.0 and later versions.

      \item Fixed a but in rep.int illustrated by the following:
\preformatted{   a<-list(1,2)
   b<-rep.int(a,c(2,2))
   b[[1]][1]<-9
   print(a[[1]])
}

      \item Fixed a bug in mget, illustrated by the following code:
\preformatted{   a <- numeric(1)
   x <- mget("a",as.environment(1))
   print(x)
   a[1] <- 9
   print(x)
}

      \item Fixed bugs that the R Core Team fixed (differently) for R-2.15.3,
            illustrated by the following:
\preformatted{   a <- list(c(1,2),c(3,4))
   b <- list(1,2,3)
   b[2:3] <- a
   b[[2]][2] <- 99
   print(a[[1]][2])

   a <- list(1+1,1+1)
   b <- list(1,1,1,1)
   b[1:4] <- a
   b[[1]][1] <- 1
   print(b[2:4])
}

      \item Fixed a bug illustrated by the following:
\preformatted{   > library(compiler)
   > foo <- function(x,y) UseMethod("foo")
   > foo.numeric <- function(x,y) "numeric"
   > foo.default <- function(x,y) "default"
   > testi <- function () foo(x=NULL,2)
   > testc <- cmpfun (function () foo(x=NULL,2))
   > testi() 
   [1] "default"
   > testc()
   [1] "numeric"
}

      \item Fixed several bugs that produced wrong results 
            such as the following:
\preformatted{   a<-list(c(1,2),c(3,4),c(5,6))
   b<-a[2:3]
   a[[2]][2]<-9
   print(b[[1]][2])
}
      I reported this to r-devel, and a (different) fix is in R-3.0.0 and 
      later versions.

      \item Fixed bugs reported on r-devel by Justin Talbot, Jan 2013 (also
            fixed, differently, in R-2.15.3), illustrated by the following:
\preformatted{   a <- list(1)
   b <- (a[[1]] <- a)
   print(b)
   a <- list(x=1)
   b <- (a$x <- a)
   print(b)
}

      \item Fixed \code{svd} so that it will not return a list with
            \code{NULL} elements.  This matches the behaviour of \code{La.svd}.

      \item Fixed (by a kludge, not a proper fix) a bug in the "tre"
            package for regular expression matching (eg, in \code{sub}),
            which shows up when \code{WCHAR_MAX} doesn't fit in an
            "int".  The kludge reduces \code{WCHAR_MAX} to fit, but really
            the "int" variables ought to be bigger.  (This problem
            showed up on a Raspberry Pi running Raspbian.)

      \item Fixed a minor error-reporting bug with
            \code{(1:2):integer(0)} and similar expressions.

      \item Fixed a small error-reporting bug with "$",
            illustrated by the following output:
\preformatted{    > options(warnPartialMatchDollar=TRUE)
    > pl <- pairlist(abc=1,def=2)
    > pl$ab
    [1] 1
    Warning message:
    In pl$ab : partial match of 'ab' to ''
}

      \item Fixed documentation error in R-admin regarding the
            \code{--disable-byte-compiled-packages} configuration option, 
            and changed the DESCRIPTION file for the recommended mgcv 
            package to respect this option.

      \item Fixed a bug reported to R Core (PR 15363, 2013-006-26) that
            also existed in pqR-2013-06-20.  This bug sometimes caused memory
            expansion when many complex assignments or removals were done
            in the global environment.
    }
  }
}
