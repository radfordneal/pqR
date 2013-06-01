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
      \item **1
      \item **2
      \item **3
      \item **4
      \item **5
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
      \item **1
      \item **2
      \item **3
      \item **4
      \item **5
      \item **6
      \item **7
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
      \item **5
      \item **6
      \item **7
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
}
