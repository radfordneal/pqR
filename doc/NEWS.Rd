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
      \item **8
      \item **9
      \item **10
      \item **11
      \item **12
      \item **13
      \item **14
      \item **15
      \item **16
      \item **17
      \item **18
      \item **19
      \item **20
      \item **21
      \item **22
      \item **23
      \item **24
      \item **25
      \item **26
      \item **27
      \item **28
      \item **29
      \item **30
      \item **31
      \item **32
      \item **33
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

  \subsection{PERFORMANCE IMPROVEMENTS}{
    \itemize{
      \item Some speedup has been obtained by using new internal C functions 
            for performing exact or partial string matches in the interpreter.
      \item **2
      \item **3
      \item **4
      \item **5
      \item **6
      \item Various inlined procedures where changed to be more efficient, 
            and "length" was un-inlined, since it's fairly long.  Numerous
            occurences of for (i = 0; i < length(...); i++) were replaced
            by code that doesn't call length numerous times.
      \item **8
      \item **9
      \item **10
      \item **11
      \item **12
      \item **13
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
      \item **21
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
      \item **29
      \item **30
      \item **31
      \item **32
      \item **33
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
      \item **1
      \item **2
      \item **3
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
      \item **9
      \item **10
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
      \item **25
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
