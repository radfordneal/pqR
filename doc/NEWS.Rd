\newcommand{\PR}{\Sexpr[results=rd]{tools:::Rd_expr_PR(#1)}}
\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{NEWS}
\title{ pqR News}
\encoding{UTF-8}


\section{CHANGES IN VERSION 2.15.0}{

  \subsection{INTRODUCTION}{
    \itemize{

      \item This file documents changes in versions of pqR, starting
            with changes from R-2.15.0, distributed by the R Core
            Team, on which pqR was based.  For changes from earlier
            version of R to R-2.15.0, see the ONEWS, OONEWS, and
            OOONEWS files.  Some changes of little interest to users,
            such as code cleanups, are documented in the file MODS.

      \item Note that for compatibility with R's version system, pqR
            presently uses the same version number, 2.15.0, as the
            version of R on which it is based. This allows 
            checks for feature availability to continue to work.
            This scheme will likely change in the future.
    } 
  }

  \subsection{NEW FEATURES}{
    \itemize{
      \item An enhanced version of the Rprofmem function for profiling
            allocation of vectors has been implemented, that can
            display more information, and can output to the terminal,
            allowing the source of allocations to more easily be
            determined.  Also, Rprofmem has now been made accessible
            even when the --enable-memory-profiling configuration
            option is not used, since the overhead of making it
            accessible is negligible.
 
            The new version allows records of memory allocation to be
            output to the terminal, where their position relative to
            other output can be informative.  More identifying
            information, including type, number of elements, and
            hexadecimal address, can also be output.  For more details on
            these and other changes, see help(Rprofmem).
      \item **2
      \item A new primitive function, pnamedcnt, has been added, that
            prints the NAMED count for an R object, which is helpful
            in tracking when objects will have to be duplicated.  For
            details, see help(pnamedcnt).
      \item **4
      \item When installing from source, the output of ./configure 
            now displays whether base and recommended packages will
            be byte compiled.
      \item **6
      \item The tests of random number generation run with \code{make check-all}
            now set the random number seed explicity.  Previously, the random
            number seed was set from the time and process ID, with the result
            that these tests would occassionally fail non-deterministically,
            when by chance one of the p-values obtained was below the threshold
            used.  (Any such failure should now occur consistently, rather
            than appearing to be due to a non-deterministic bug.)

      \item Testing of the "xz" compression method is now done with \code{try},
            so that failure will be tolerated on machines that don't have enough
            memory for these tests.
      \item **8
      \item **9
      \item **10
      \item A new primitive function \code{get_rm} has been added,
            which removes a variable while returning the value it
            had when removed.  See \code{help(get_rm)} for details,
            and how this can sometimes improve efficiency of R functions.
      \item **12
      \item **13
      \item **14
      \item The BLAS routines supplied with R were modified to improve the 
            performance of the routines DGEMM (matrix-matrix multiply) and 
            DGEMV (matrix-vector multiply).  Also, proper propagation of NaN, 
            Inf, etc. is done now.
      \item **16
      \item The handling of power operations has been improved (primarily 
            for powers of reals, but slightly affecting powers of integers too).

            I previously proposed a simple patch speeding up squaring,
            which the R core team did not adopt.  Instead, in R-2.12.0
            and continuing in R-2.13.0, they introduced an inline
            function R_POW, that checks specially for a power of 2,
            and does it as a multiply, otherwise calling the R_pow
            function.  R_pow proceeds to check again for a power of 2
            at the beginning, and then check again for a power of 2
            just before calling the C pow function.  The R_pow
            function also contains a check for a power of 0.5, but it
            is disabled for recent versions of gcc, to bypass a bug
            that according to a comment existed at one time.  Note
            that the inline R_POW function will not necessarily be
            actually inlined by the compiler, and that in any case the
            check for a power of 2 is done over again for every
            element of a vector being raised to that power, even if
            the power is a scalar.

            In this new patch, if the power is a scalar, I check for
            it being 2, 1, 0, or -1, and if so handle it specially.
            Otherwise, I call R_pow, which I changed to not bother
            checking for powers of 2, and to actually check for a
            power of 0.5.  (Since the relevant code has changed, any
            buggy compilers still extant probably will compile the new
            code OK; if not, they probably compile lots of stuff
            incorrectly, since there is nothing unusual in the new
            code.)

            For non-scalar powers, I use R_POW, but change it to a
            macro, so that it will definitely be inlined, and have it
            check for powers of 2 and 1.  (If one is going to do this
            check when the power is a vector, it makes sense to tailor
            it to something other than a vector of powers that are all
            the same, since this doesn't seem like a common case.
            Some powers of 1 and some of 2 seems plausible in some
            statistical applications.)  The macro also allows for the
            power to be an integer, slightly speeding up some
            integer^integer operations.

            The speed improvement from this patch depends a lot on the
            machine architecture and the compiler.  On machines where
            memory is much slower than the processor, checking for a
            power of 2 every time may mostly overlap with the a memory
            fetch or store operation, but one would not expect this to
            always be the case.  The check for a power of -1 will
            greatly speed up code where the programmer had a stylistic
            preference for (...)^(-1) over 1/(...).
      \item **18
      \item **19
      \item **20
      \item **21
      \item **22
      \item Extending lists and character vectors by assigning to an
            index past the end, deleting list items by assigning NULL,
            and concatenation of character vectors with "c" have all
            been speeded up by factors of up to two or three.
      \item **24
      \item **25
      \item **26
      \item **27
      \item **28
      \item **29
      \item **30
      \item Existing code for handling special cases of "[" in which there is
            only one scalar index was replaced by cleaner code that handles 
            more cases.   The old code handled only integer and real vectors, 
            and only positive indexes.  The new code handles all atomic 
            vectors (logical, integer, real, complex, raw, and string), and 
            positive or negative indexes that are not out of bounds.
      \item Extraction of subsets of vectors or matrices (eg, v[10:20]
            or M[1:10,101:110]) has been sped up substantially.
      \item **33
      \item **34
      \item **35
      \item The low-level routines for generation of uniform random
            numbers have been improved.

            The previous code copied the seed back and forth to
            .Random.seed for every call of a random number generation
            function, which is rather time consuming given that for
            the default generator .Random.seed is 625 integers long.
            It also allocated new space for .Random.seed every time.
            I changed the code to use the seed in .Random.seed without
            copying, except when the generator is user-supplied.  

            The previous code had imposed an unnecessary limit on the
            length of a seed for a user-supplied random number
            generator, which I removed.
      \item **37
      \item **38
      \item **39
      \item **40
      \item **41
      \item **42
      \item **43
      \item **44
      \item **45
      \item The \code{any} and \code{all} primitives have been substantially
            speeded up for large vectors.
      \item **47
      \item **48
      \item **49
      \item **50
    }
  }

  \subsection{PERFORMANCE IMPROVEMENTS}{
    \itemize{
      \item Some speedup has been obtained by using new internal C functions 
            for performing exact or partial string matches in the interpreter.
      \item **2
      \item The speed of the transpose (t) function has been
            improved, when applied to real, integer, and logical
            matrices.
      \item **4
      \item Code for the sum and prod functions has been changed to
            move some checks outside the inner summation loops.  The effect
            depends on the extent to which the unnecessary checks overlap 
            memory fetch operations, but one would expect signficant 
            speed-ups with some machines/compilers.

            I had previously proposed this modificaton to sum and prod 
            before R-2.12.0.  That patch was not adopted by the R core team, 
            though they did swap the order of checking for NA/NaN and 
            checking the na.rm option, which avoids the worst inefficiency 
            of the previous code in R-2.11.1.  
      \item **6
      \item Various inlined procedures where changed to be more efficient, 
            and "length" was un-inlined, since it's fairly long.  Numerous
            occurences of for (i = 0; i < length(...); i++) were replaced
            by code that doesn't call length numerous times.
      \item **8
      \item Global constants R_ScalarLogicalNA, R_ScalarLogicalTRUE, and
            R_ScalarLogicalFALSE have been created, and the interpreter's
            ScalarLogical function now returns one of these rather than
            allocate new space for every logical value. 

            To avoid problems with an external C or Fortran routine
            changing one of these values (with an incorrect
            specification of DUP=FALSE even though it modifies the
            argument), the values of these constants are checked after
            the return of an external function called with .C or
            .Fortran, and if they have changed, their values are reset
            and an error is signalled.

            The bytecode interpreter sets up a similar set of logical
            constants.  That facility should be merged with this one
            (perhaps by just calling the ScalarLogical function in the
            bytecode interpreter).

            Various places in coerce.c were changed to use ScalarLogical
            rather than allocate logical values themselve.  This is both
            cleaner and now more efficient given the change above.
      \item **10
      \item Logical operations and relational operators have been sped up
            in simple cases, and use the new facility for producing a scalar
            logical result without allocating new storage.  Relational
            operators have also been substantially speeded up for long
            vectors.
      \item **12
      \item A call of R_isMissing was removed in the interpeter's evalList 
            function.  This check, done for every argument to a builtin 
            primitive that is a symbol, is slow, and appears to serve only 
            to produce an error message that is slightly different (and 
            sometimes less informative) than simply letting the symbol be 
            evaluated.
      \item **14
      \item **15
      \item **16
      \item **17
      \item **18
      \item The creation of argument lists for closures was sped up 
            by avoiding an unnecessary allocation of a CONS cell, in the 
            same way as was done in my Sep 2010 patch for evalList, which 
            was incorporated into 2.12.0 and later versions of R.
      \item **20
      \item Access via the $ operator to lists, pairlists, and environments 
            has been sped up.  The speedup comes mainly from (a) avoiding the
            overhead of calling DispatchOrEval if there are no complexities,
            (b) passing on the field to extract as a symbol, or a name, or 
            both, as available, and then converting only as necessary, 
            (c) using the new ep_match functions instead of the previous local
            pstrmatch procedure, and (d) not translating a string multiple 
            times.
      \item **22
      \item **23
      \item **24
      \item Lookup of symbols defined in the base environment has been
            sped up by flagging symbols that have a base environment
            definition recorded in the global cache. This allows the
            definition to be retrieved quickly without looking in the
            hash table.  In particular, this speeds up basic
            operations such as "+", "<-", "if", and "length".
      \item **26
      \item Lookup of some builtin/special function symbols (eg, '+' and 'if')
            has been sped up by allowing fast bypass of non-global environments
            that do not contain (and have never contained) one of these 
            symbols.
      \item **28

      \item Some binary and unary arithmetic operations have been sped
            up by, when possible, using the space holding one of the
            operands to hold the result, rather than allocating new
            space.  Though primarily a speed improvement, for very
            long vectors avoiding this allocation could avoid running
            out of space.
      \item **30
      \item **31
      \item **32
      \item **33
      \item PUT IN ITEM HERE ABOUT MATRIX MULTIPLY MODS.
      \item **35
      \item **36
      \item **37
      \item Several primitive functions that can generate integer sequences
            --- ":", seq.int, seq_len, and seq_along --- will now sometimes
            not generate an actual sequence, but rather just a description
            of its start and end points.  This is not visible to users
            (except in time and space savings), but is used to speed up 
            other primitive operations, specifically "for" loops such
            as \code{for (i in 1:1000000) ...}, and indexing of vectors
            such as \code{v[1000:2000]}.
      \item **39
      \item **40
      \item **41
      \item **42
      \item **43
      \item **44
      \item **45
      \item **46
      \item **47
      \item **48
      \item **49
      \item **50
      \item **51
      \item **52
      \item **53
      \item **54
      \item **55
      \item **56
      \item **57
      \item **58
      \item **59
      \item **60
      \item **61
      \item **62
      \item **63
      \item **64
      \item **65
      \item **66
      \item **67
      \item **68
      \item **69
      \item **70
      \item **71
      \item **72
      \item **73
      \item **74
      \item **75
      \item **76
      \item **77
      \item **78
      \item **79
      \item **80
      \item **81
      \item **82
      \item **83
      \item **84
      \item **85
      \item **86
      \item **87
      \item **88
      \item **89
      \item **90
      \item **91
      \item **92
      \item **93
      \item **94
      \item **95
      \item **96
      \item **97
      \item **98
      \item **99
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item The "debug" facility has been fixed.  Its behaviour for if,
            while, repeat, and for statements when the inner statement
            was or was not one with curly brackets had made no sense,
            and it had also displayed inappropriate expressions, and
            displayed things at the wrong time.  The new behaviour is
            now documented in help(debug).
      \item **2
      \item Fixed a small error-reporting bug with "$",
            illustrated by the following output:
\preformatted{
    > options(warnPartialMatchDollar=TRUE)
    > pl <- pairlist(abc=1,def=2)
    > pl$ab
    [1] 1
    Warning message:
    In pl$ab : partial match of 'ab' to ''
}
      \item **4
      \item Fixed documentation error in R-admin regarding the
            --disable-byte-compiled-packages configuration option, 
            and changed the DESCRIPTION file for the recommended mgcv 
            package to respect this option.
      \item **6
      \item Fixed a bug in numeric_deriv in stats/src/nls.c, where it
            was not duplicating when it should, as illustrated below:
\preformatted{
  > x <- 5; y <- 2; f <- function (y) x
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
      \item **8
      \item Fixed \code{diag} so that it returns a matrix when passed
            a list of elements to put on the diagonal.
      \item Fixed \code{svd} so that it will not return a list with
            \code{NULL} elements.  This matches the behaviour of \code{La.svd}.
      \item **11
      \item **12
      \item **13
      \item **14
      \item **15
      \item **16
      \item **17
      \item Fixed a bug where more than six warning messages at startup
            would overwrite random memory, causing garbage output 
            and perhaps arbitrarily bizarre behaviour.
      \item Fixed a bug where LC_PAPER was not correctly set at startup.
      \item **20
      \item **21
      \item **22
      \item **23
      \item **24
      \item Fixed a bug where excess warning messages may be produced
            on conversion to RAW.  For instance:
\preformatted{
  > as.raw(1e40)
  [1] 00
  Warning messages:
  1: NAs introduced by coercion 
  2: out-of-range values treated as 0 in coercion to raw 
}
            Now, only the second warning message is produced.
      \item **26
      \item **27
      \item **28
      \item **29
      \item **30
      \item **31
      \item **32
      \item Fixed a bug in which \code{Im(matrix(complex(0),3,4))} returned
            a matrix of zero elements rather than a matrix of NA elements.
      \item **34
      \item **35
      \item **36
      \item **37
      \item **38
      \item **39
      \item **40
      \item **41
      \item **42
      \item **43
      \item **44
      \item **45
      \item **46
      \item **47
      \item **48
      \item **49
      \item **50
    }
  }
}
