% File src/library/base/man/length.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Modifications for pqR Copyright (c) 2019 Radford M. Neal.
% Distributed under GPL 2 or later

\name{length}
\alias{length}
\alias{length<-}
\alias{length<-.factor}
\title{Length of an Object}
\description{
  Get or set the length of vectors (including lists) and factors, and of
  any other \R object for which a method has been defined.
}
\usage{
length(x)
length(x) <- value
}
\arguments{
  \item{x}{an \R object.  For replacement, a vector or factor.}
  \item{value}{an integer: double values will be coerced to integer.}
}

\details{
  Both functions are generic: you can write methods to handle specific
  classes of objects, see \link{InternalMethods}.  \code{length<-} has a
  \code{"factor"} method.

  The replacement form can be used to reset the length of a vector (or
  pairlist).  If a vector is shortened, extra values are discarded;
  when a vector is lengthened, it is padded out to its new length with
  \code{\link{NULL}} for lists, 0 for raw vectors, and
  \code{\link{NA}} for other vectors.
  
  Both are \link{primitive} functions.
}

\value{
  The default method currently returns an \code{\link{integer}} of
  length 1.  Since this may change in the future and may
  differ for other methods, programmers should not rely on it.

  For vectors (including lists and expressions) and factors the length
  is the number of elements.  For an environment it is the number of
  objects in the environment, and \code{NULL} has length 0.  For
  pairlists (including language objects and dotlists)
  it is the length of the pairlist chain.  All other objects
  (including functions) have length one: note that for functions this
  differs from S.

  The replacement form removes all the attributes of \code{x} except its
  names (except that all attributes are kept if the new length is the
  same as the old length.)
}

\section{Warning}{
  Package authors have written methods that return a result of
  length other than one (\pkg{Formula}) and that return a vector type
  \code{\link{double}} (\pkg{Matrix}), even with non-integer values
  (\pkg{sets}).
}

\seealso{\code{nchar} for counting the number of characters in
  character vectors.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\examples{
length(diag(4))                             # = 16 (4 x 4)
length(formals(function (u,v,w,x,y,z) 0))   # = 6
length(y ~ x1 + x2 + x3)                    # = 3
length(expression(x, {y <- x^2; y+2}, x^y)) # = 3

## from example(warpbreaks)
require(stats)

fm1 <- lm(breaks ~ wool * tension, data = warpbreaks)
length(fm1$call)     # = 3, lm and two arguments.
length(formula(fm1)) # = 3, ~ lhs rhs
}
\keyword{attribute}

