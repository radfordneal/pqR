% File src/library/base/man/rep.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Modifications for pqR Copyright (c) 2014, 2017 Radford M. Neal.
% Distributed under GPL 2 or later

\name{rep}
\alias{rep}
\alias{rep.factor}
\alias{rep.int}
\alias{rep_len}
\alias{rep.POSIXct}
\alias{rep.POSIXlt}
\alias{rep.Date}

\title{Replicate Elements of Vectors and Lists}
\description{
  \code{rep} replicates the values in \code{x}.  It is a generic
  function, and the (internal) default method is described here.

  \code{rep.int} and \code{rep_len} are simplified versions for two
  common cases, which do not preserve names, and hence are faster
  when names are present.  They are not generic.
}
\usage{
rep(x, \dots)

rep.int(x, times)

rep_len(x, length.out)
}
\arguments{
  \item{x}{a vector (of any mode including a list) or a pairlist or a
    factor or (except for \code{rep.int}) a \code{POSIXct} or
    \code{POSIXlt} or \code{date} object; or also, an S4 object containing
    a vector of the above kind.}
  \item{\dots}{further arguments to be passed to or from other methods.
    For the internal default method these can include:
    \describe{
      \item{\code{times}}{A vector with integer values giving the 
        (non-negative) number of times to repeat each element if of 
        length \code{length(x)}, or to repeat the whole vector if of length 1.
        Negative or \code{NA} values are an error.}
      \item{\code{length.out}}{A non-negative integer, the desired length of the
        output vector.  Other inputs will be coerced to an integer
	vector and the first element taken.  Ignored if \code{NA} or invalid.}
      \item{\code{each}}{A non-negative integer.  Each element of \code{x}
	is repeated \code{each} times.  Other inputs will be coerced to
	an integer vector and the first element taken.  Treated as
	\code{1} if \code{NA} or invalid.}
    }
  }
  \item{times, length.out}{see \code{\dots}.}
}

\details{
  The default behaviour of \code{rep} is as if the call was 
  \code{rep(x, times=1, length.out=NA, each=1)}.  Normally just one of 
  the additional arguments is specified, but if \code{each} is specified 
  with either of the other two, its replication is performed first, and then 
  that implied by \code{times} or \code{length.out}.

  If \code{times} consists of a single integer, the result consists of
  the whole input repeated this many times.  If \code{times} is a
  vector of the same length as \code{x} (after replication by
  \code{each}), the result consists of \code{x[1]} repeated
  \code{times[1]} times, \code{x[2]} repeated \code{times[2]} times and
  so on.

  \code{length.out} may be given in place of \code{times},
  in which case \code{x} is repeated as many times as is
  necessary to create a vector of this length.  If both are given,
  \code{length.out} takes priority and \code{times} is ignored.

  Non-integer values of \code{times} will be truncated towards zero.
  If \code{times} is a computed quantity it is prudent to add a small fuzz.

  If \code{x} has length zero and \code{length.out} is supplied and is
  positive, the values are filled in using the extraction rules, that is
  by an \code{NA} of the appropriate class for an atomic vector
  (\code{0} for raw vectors) and \code{NULL} for a list.
}
\value{
  An object of the same type as \code{x} (except that \code{rep}, though
  not \code{rep.int}, will coerce pairlists to vector lists).

  \code{rep.int} and \code{rep_len} return no attributes (except the
  class if returning a factor).

  The default method of \code{rep} gives the result names (which will
  almost always contain duplicates) if \code{x} had names, but retains
  no other attributes except for factors.

  The value returned by the default method for \code{rep} is guaranteed 
  not to be shared.
  This is visible to the user only in so far as this value is passed
  as an argument to \code{.Call} or \code{.External}, or to \code{.C} or
  \code{.Fortran} with \code{DUP=FALSE}.
}
\note{
  Function \code{rep.int} is a simple case 
  provided as a separate function for S compatibility, and for speed
  when names are present but not needed in the result.

  Function \code{rep} is a primitive, but (partial) matching of argument
  names is performed as for normal functions.  You can no longer pass a
  missing argument to e.g. \code{length.out}.

  For historical reasons \code{rep} works on \code{NULL}: the result is
  always \code{NULL} even when \code{length.out} is positive.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{seq}}, \code{\link{sequence}}, \code{\link{replicate}}.
}
\examples{
rep(1:4, 2)
rep(1:4, each = 2)       # not the same.
rep(1:4, c(2,2,2,2))     # same as second.
rep(1:4, c(2,1,2,1))
rep(1:4, each = 2, len = 4)    # first 4 only.
rep(1:4, each = 2, len = 10)   # 8 integers plus two recycled 1's.
rep(1:4, each = 2, times = 3)  # length 24, 3 complete replications

rep(1, 40*(1-.8)) # length 7 on most platforms
rep(1, 40*(1-.8)+1e-7) # better

## replicate a list
fred <- list(happy = 1:10, name = "squash")
rep(fred, 5)

# date-time objects
x <- .leap.seconds[1:3]
rep(x, 2)
rep(as.POSIXlt(x), rep(2, 3))

## named factor
x <- factor(LETTERS[1:4]); names(x) <- letters[1:4]
x
rep(x, 2)
rep(x, each=2)
rep.int(x, 2)  # no names
rep_len(x, 10)
}
\keyword{manip}
\keyword{chron}
