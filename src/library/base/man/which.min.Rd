% File src/library/base/man/which.min.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2010 R Core Team
% Modifications for pqR Copyright (c) 2017 Radford M. Neal.
% Distributed under GPL 2 or later

\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{which.min}
\alias{which.min}
\alias{which.max}
\title{Where is the Min() or Max() ?}
\concept{argmin}
\concept{argmax}
\concept{index of minimum}
\concept{index of maximum}
\description{
  Determines the location, i.e., index of the (first) minimum or maximum
  of a numeric vector.
}
\usage{
which.min(x)
which.max(x)
}
\arguments{
  \item{x}{numeric (logical, integer, or double) vector, or an object
    that can be coerced to numeric, whose \code{\link{min}} or 
    \code{\link{max}} is searched for.}
}
\details{
  \code{NA} and \code{NaN} values are discarded.  For logical
  vectors, the result is based on \code{FALSE < TRUE}.
}
\value{
  An \code{\link{integer}} of length 1 giving the index of the 
  \emph{first} minimum or maximum respectively of \code{x}, 
  or an integer vector of length 0 if \code{x} has only NA or NaN values.

  If this extremum is unique (or empty), the results are the same as
  (but more efficient than) \code{which(x == min(x))} or
  \code{which(x == max(x))} respectively.
}
\author{Martin Maechler}
\seealso{
  \code{\link{which}}, \code{\link{max.col}}, \code{\link{max}}, etc.

  Use \code{\link{arrayInd}()}, if you need array/matrix indices instead
  of 1D vector ones.

  \code{\link[nnet]{which.is.max}} in package \CRANpkg{nnet} differs in
  breaking ties at random (and having a \sQuote{fuzz} in the definition
  of ties).
}
\examples{
x <- c(1:4,0:5,11)
which.min(x)
which.max(x)

## it *does* work with NA's present, by discarding them:
presidents[1:30]
range(presidents, na.rm = TRUE)
which.min(presidents) # 28
which.max(presidents) #  2
}
\keyword{utilities}
