\newcommand{\PR}{\Sexpr[results=rd]{tools:::Rd_expr_PR(#1)}}
\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{NEWS}
\title{ pqR News}
\encoding{UTF-8}

\section{CHANGES IN VERSION RELEASED 2013-05-31}{

\subsection{INTRODUCTION}{

\itemize{
\item This is the initial public release of pqR.  It is based on R-2.15.0,
distributed by the R Core Team, but improves on it in many ways,
mostly ways that speed it up, but also by implementing some new
features and fixing some bugs.  

\item One notable improvement is that for systems with multiple
processors or processor cores, pqR is able to do some numeric
computations in parallel with other operations of the interpreter, and
with other numeric computations.

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

}}

  \subsection{NEW FEATURES}{
    \itemize{
      \item A new primitive function \code{get_rm} has been added,
            which removes a variable while returning the value it
            had when removed.  See \code{help(get_rm)} for details,
            and how this can sometimes improve efficiency of R functions.

      \item An enhanced version of the Rprofmem function for profiling
            allocation of vectors has been implemented, that can
            display more information, and can output to the terminal,
            allowing the source of allocations to more easily be
            determined.  Also, Rprofmem has now been made accessible
            even when the \code{--enable-memory-profiling} configuration
            option is not used, since the overhead of making it
            accessible is negligible.
 
            The new version allows records of memory allocation to be
            output to the terminal, where their position relative to
            other output can be informative.  More identifying
            information, including type, number of elements, and
            hexadecimal address, can also be output.  For more details on
            these and other changes, see \code{help(Rprofmem)}.

      \item A new primitive function, pnamedcnt, has been added, that
            prints the NAMEDCNT/NAMED count for an R object, which is helpful
            in tracking when objects will have to be duplicated.  For
            details, see help(pnamedcnt).

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

      \item Testing of pqR has so far been done only on Linux/Unix
            systems, not on Windows or Mac systems.  There is no specific
            reason to believe that it will not work on Windows or Mac
            systems, but until tests have been done, trying to use it 
            on these systems is not recommended.

      \item This release contains the versions of the standard and recommended
            packages that were released with R-2.15.0.  Newer versions may
            or may not be compatible (same as for R-2.15.0).

      \item It is intended that this release will be fully compatible with
            R-2.15.0, but you will need to recompile any packages (other
            that those with only R code) that you had installed for R-2.15.0, 
            and any other C code you use with R, since the format of internal 
            data structures has changed.

      \item New configuration options relating to helper threads and
            to matrix multiplication now exist.  See doc/R-admin.html 
            (or R-admin.pdf) for details.

      \item Including \code{-DENABLE_ISNAN_TRICK} in CFLAGS will speed up 
            checks for NA and NaN on machines on which it works.  It works
            on Intel processors (verified both empirically and by consulting
            Intel documentation).  It does not work on SPARC machines.

      \item When installing from source, the output of ./configure 
            now displays whether base and recommended packages will
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

      \item The details of how valgrind is used have changed. See the source
            file \file{memory.c}.
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
    }
  }
}
