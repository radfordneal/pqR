% File src/library/base/man/sort.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Modifications for pqR Copyright (c) 2017 Radford M. Neal.
% Distributed under GPL 2 or later

\name{sort}
\alias{sort}
\alias{sort.default}
\alias{sort.POSIXlt}
\alias{sort.int}
\title{Sorting or Ordering Vectors}
\description{
  Sort (or \emph{order}) a vector or factor (partially) into
  ascending or descending order.  For ordering along more than one
  variable, e.g., for sorting data frames, see \code{\link{order}}.
}
\usage{
sort(x, decreasing = FALSE, \dots)

\method{sort}{default}(x, decreasing = FALSE, na.last = NA, \dots)

sort.int(x, partial = NULL, na.last = NA, decreasing = FALSE,
         method = c("auto", "merge", "shell", "quick", "radix"),
         index.return = FALSE)
}
\arguments{
  \item{x}{for \code{sort} an \R object with a class or a numeric,
    complex, character or logical vector.  For \code{sort.int}, a
    numeric, complex, character or logical vector, or a factor.}
  \item{decreasing}{logical.  Should the sort be increasing or decreasing?
    For the \code{"radix"} method, this can be a vector of
    length equal to the number of arguments in \code{\dots}. For the
    other methods, it must be length one. Not available for partial sorting.}
  \item{\dots}{arguments to be passed to or from methods or (for the
    default methods and objects without a class) to \code{sort.int}.}
  \item{na.last}{for controlling the treatment of \code{NA}s.
    If \code{TRUE}, missing values in the data are put last; if
    \code{FALSE}, they are put first; if \code{NA}, they are removed.}
  \item{partial}{\code{NULL} or a  vector of indices for partial sorting.}
  \item{method}{character string specifying the algorithm used.  
    Can be abbreviated.}
  \item{index.return}{logical indicating if the ordering index vector should
    be returned as well. Supported by \code{method == "radix"} for any
    \code{na.last} mode and data type, and the other methods when
    \code{na.last = NA} (the default) and fully sorting non-factors.}
}
\details{
  \code{sort} is a generic function for which methods can be written,
  and \code{sort.int} is the internal method which is compatible
  with S if only the first three arguments are used.

  The default \code{sort} method makes use of \code{\link{order}} for
  classed objects, which in turn makes use of the generic function
  \code{\link{xtfrm}} (and can be slow unless a \code{xtfrm} method has
  been defined or \code{\link{is.numeric}(x)} is true).

  Complex values are sorted first by the real part, then the imaginary
  part.

  The \code{"auto"} method selects \code{"radix"} for numeric vectors,
  integer vectors, logical vectors, and factors, except that \code{"shell"}
  is used for vectors with length less than 500.  In other cases, method
  \code{"merge"} is used.
  
  All methods except \code{"quick"} are stable, leaving the order unchanged
  for elements that are equal.  For numeric vectors, this is observable only
  when the vector has names, or \code{index.return} is used.

  Except for method \code{"radix"}, the sort order for character
  vectors will depend on the collating sequence of the locale in use:
  see \code{\link{Comparison}}.  The sort order for factors is the
  order of their levels (which is particularly appropriate for ordered
  factors).

  Method \code{"merge"} uses a merge sort, which is an \eqn{O(n log
  n)} algorithm.  It requires auxiliary space proportional to the
  number of items, but this is likely to be an issue only on 32-bit
  platforms.

  Method \code{"shell"} uses Shellsort (an \eqn{O(n^{4/3})} variant from
  Sedgewick (1986)), which requires no auxiliary space, but is generally
  slower than merge sort for long vectors.

  Method \code{"quick"} uses Singleton (1969)'s implementation of
  Hoare's Quicksort method and is only available when \code{x} is
  numeric (double or integer) and \code{partial} is \code{NULL}.  (For
  other types of \code{x} merge sort is used, silently.)  It requires
  no auxiliary storage, and is normally somewhat faster than Shellsort
  (perhaps 50\% faster on vectors of length a million and twice as
  fast at a billion) but has poor performance in the rare worst case.
  (Peto's modification using a pseudo-random midpoint is used to make
  the worst case rarer.)  This is not a stable sort, so ties may be
  reordered.
  
  Method \code{"radix"} relies on simple hashing to scale time linearly
  with the input size, i.e., its asymptotic time complexity is O(n). The
  specific variant and its implementation originated from the data.table
  package and are due to Matt Dowle and Arun Srinivasan.  For small
  inputs (< 200), the implementation uses an insertion sort (O(n^2))
  that operates in-place to avoid the allocation overhead of the radix
  sort. For integer vectors of range less than 100,000, it switches to a
  simpler and faster linear time counting sort.

  The \code{"radix"} method generally outperforms the other methods,
  especially for long vectors of small integers or character strings.
  However, if \code{x} is a character vector, all elements must share
  the same encoding, only UTF-8 (including ASCII) and Latin-1
  encodings are supported, and collation always follows the "C" locale.

  If \code{partial} is not \code{NULL}, it is taken to contain indices
  of elements of the result which are to be placed in their correct
  positions in the sorted array by partial sorting.  For each of the
  result values in a specified position, any values smaller than that
  one are guaranteed to have a smaller index in the sorted array and any
  values which are greater are guaranteed to have a bigger index in the
  sorted array.  (This is included for efficiency, and many of the
  options are not available for partial sorting.  It is only
  substantially more efficient if \code{partial} has a handful of
  elements, and a full sort is done (a Quicksort if possible, otherwise
  the specified method, which must be \code{"merge"} or \code{"shell"})
  if there are more than 10.)  Names are discarded for partial sorting.
}

\value{
  For \code{sort}, the result depends on the S3 method which is
  dispatched.  If \code{x} does not have a class \code{sort.int} is used
  and it description applies.  For classed objects which do not have a
  specific method the default method will be used and is equivalent to
  \code{x[order(x, ...)]}: this depends on the class having a suitable
  method for \code{[} (and also that \code{\link{order}} will work,
  which requires a \code{\link{xtfrm}} method).

  For \code{sort.int} the value is the sorted vector unless
  \code{index.return} is true, when the result is a list with components
  named \code{x} and \code{ix} containing the sorted numbers and the
  ordering index vector.  In the latter case, if \code{method ==
  "quick"} ties may be reversed in the ordering (unlike
  \code{sort.list}) as quicksort is not stable.  For \code{method ==
  "radix"}, \code{index.return} is supported for all \code{na.last}
  modes. The other methods only support \code{index.return}
  when \code{na.last} is \code{NA}. The index vector
  refers to element numbers \emph{after removal of \code{NA}s}: see
  \code{\link{order}} if you want the original element numbers.

  All attributes are removed from the return value (see Becker \emph{et
  al}, 1988, p.146) except names, which are sorted.  (If
  \code{partial} is specified even the names are removed.)  Note that
  this means that the returned value has no class, except for factors
  and ordered factors (which are treated specially and whose result is
  transformed back to the original class).
}

\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Knuth, D. E. (1998)
  \emph{The Art of Computer Programming, Volume 3: Sorting and
    Searching.} 2nd ed. Addison-Wesley.
  
  Sedgewick, R. (1986)
  A new upper bound for Shell sort.
  \emph{J. Algorithms} \bold{7}, 159--173.

  Singleton, R. C. (1969)  An efficient algorithm for sorting with
  minimal storage: Algorithm 347.
  \emph{Communications of the ACM} \bold{12}, 185--187.
}
\seealso{
  \sQuote{\link{Comparison}} for how character strings are collated.

  \code{\link{order}} for sorting on or reordering multiple variables.

  \code{\link{is.unsorted}}. \code{\link{rank}}.
}
\examples{
require(stats)

x <- swiss$Education[1:25]
x; sort(x); sort(x, partial = c(10, 15))

## illustrate 'stable' sorting (of ties):
sort(c(10:3, 2:12), method = "shell", index.return = TRUE) # is stable
## $x : 2  3  3  4  4  5  5  6  6  7  7  8  8  9  9 10 10 11 12
## $ix: 9  8 10  7 11  6 12  5 13  4 14  3 15  2 16  1 17 18 19
sort(c(10:3, 2:12), method = "quick", index.return = TRUE) # is not
## $x : 2  3  3  4  4  5  5  6  6  7  7  8  8  9  9 10 10 11 12
## $ix: 9 10  8  7 11  6 12  5 13  4 14  3 15 16  2 17  1 18 19

x <- c(1:3, 3:5, 10)
is.unsorted(x)                  # FALSE: is sorted
is.unsorted(x, strictly = TRUE) # TRUE : is not (and cannot be)
                                # sorted strictly
\dontrun{
set.seed(1)
## Small speed comparison simulation:
N <- 2000
Sim <- 20
rep <- 1000 # << adjust to your CPU
c1 <- c2 <- numeric(Sim)
for(is in seq_len(Sim)){
  x <- rnorm(N)
  c1[is] <- system.time(for(i in 1:rep) sort(x, method = "shell"))[1]
  c2[is] <- system.time(for(i in 1:rep) sort(x, method = "quick"))[1]
  stopifnot(sort(x, method = "shell") == sort(x, method = "quick"))
}
rbind(ShellSort = c1, QuickSort = c2)
cat("Speedup factor of quicksort over shellshort:\n")
summary({qq <- c1 / c2; qq[is.finite(qq)]})

## A larger test
x <- rnorm(1e7)
system.time(x1 <- sort(x, method = "shell"))
system.time(x3 <- sort(x, method = "merge"))
system.time(x2 <- sort(x, method = "quick"))
system.time(x4 <- sort(x, method = "radix"))
stopifnot(identical(x1, x2))
stopifnot(identical(x1, x3))
stopifnot(identical(x1, x4))
}}
\keyword{univar}
\keyword{manip}
\keyword{arith}
