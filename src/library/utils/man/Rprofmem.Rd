% File src/library/utils/man/Rprofmem.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Modifications for pqR Copyright (c) 2013 Radford M. Neal.
% Distributed under GPL 2 or later

\name{Rprofmem}
\alias{Rprofmem}
\alias{Rprofmemt}
\title{Enable Profiling of R's Memory Use}
\description{
 Enable or disable reporting of memory allocation in R.
}
\usage{
Rprofmem(filename = "Rprofmem.out", append = FALSE, 
         threshold = 0, nelem = 0, stack = TRUE,
         terminal = FALSE, pages = TRUE, details = FALSE)

Rprofmemt(filename = "", append = FALSE, 
         threshold = 0, nelem = 0, stack = TRUE,
         terminal = TRUE, pages = FALSE, details = TRUE)
}
\arguments{
  \item{filename}{The file to which reports of memory allocations are written, 
    or \code{NULL} or \code{""} if reports should not go to a file. }
  \item{append}{logical: should the file be appended to rather than 
                overwritten?}
  \item{threshold}{numeric: only allocations of vectors 
    with size larger than this number of bytes will be reported.}
  \item{nelem}{numeric: only allocations of vectors 
    with at least this many elements will be reported.}
  \item{stack}{logical: should a stack trace be included in reports?}
  \item{terminal}{logical: should reports be printed on the terminal (as
    well as possibly written to \code{filename})?}
  \item{pages}{logical: should allocation of pages for small vectors 
    be reported, and reporting of individual small vector allocations 
    suppressed?}
  \item{details}{logical: should details of allocation be reported,
    rather than only the total number of bytes?}
}
\details{
  The profiler tracks memory allocations, some of which will be to previously
  used memory and will not increase the total memory use of R.  
  \code{Rprofmem} and \code{Rprofmemt} are the same except for default values
  of the arguemnts, which for \code{Rprofmem} are set for backwards 
  compatibility, and for \code{Rprofmemt} are set for convenient use with 
  output to the terminal.

  Calling \code{Rprofmem} with either \code{terminal=TRUE} or with
  \code{filename} something other than \code{NULL} or \code{""} (or both)
  will enable profiling, with allocation reports going to one or both
  places.  
  Enabling profiling automatically disables any 
  existing profiling to another or the same file or to the terminal.
  Calling \code{Rprofmem} with \code{terminal=FALSE} and
  \code{filename} either \code{NULL} or \code{""} will disable profiling.
  Profiling will also be disabled by \code{Rprofmemt(NULL)}.

  Reports to the terminal are preceded by "RPROFMEM:".  These reports
  are not diverted to a file when \code{sink} is used.

  If \code{pages=TRUE} allocations of individual
  vectors will be reported only if they are "large", and allocations of 
  pages to hold small vectors will be reported.
  The size of a page of memory and the size over which a vector is "large"  
  (and hence for which \code{malloc} is used) are compile-time constants, 
  by default 2000 and 128 bytes respectively.

  If \code{pages=FALSE}, allocations
  of all vectors with size over \code{threshold} and number of elements 
  at least \code{nelem} are reported, and page allocations are not reported.

  A report of an allocation of a vector (to \code{filename} and/or the 
  terminal) will contain the number of bytes allocated, which
  will include space for a header as well as possible padding to the size of
  a conveniently-available block.  If \code{stack=TRUE}, the names of 
  functions in the call stack will also be printed, and if
  \code{details=TRUE}, the type and length of the vector allocated will be 
  displayed in parentheses (before the call stack).  

  An allocation of a page for small vectors (when \code{pages=TRUE}) will
  result in a report consisting of "new page:" followed by the call stack.

  When \code{terminal=TRUE} or \code{details=TRUE}, a newline is always 
  written after each allocation report.  For backward compatibility, this 
  is otherwise not the case when the call stack is empty.
}
\note{
 The memory profiler can be used at the same time as other \R and C profilers.
  }
\value{
  None  
}

\seealso{
  The R sampling profiler, \code{\link{Rprof}} also collects
  memory information.

  \code{\link{tracemem}} traces duplications of specific objects.

  The "Writing R Extensions" manual section on "Tidying and profiling R code"
}
\examples{
# Reports printed to the terminal, with details, for all vectors of 
# at least 10 elements.
Rprofmemt(nelem=10)
v <- numeric(10)
v[3] <- 1
u <- v
v[3] <- 2
Rprofmemt(NULL)

\dontrun{
# Reports go to a file.
Rprofmem("Rprofmem.out", threshold=1000)
example(glm)
Rprofmem(NULL)
noquote(readLines("Rprofmem.out", n=5))
}}
\keyword{utilities}
