% File src/library/base/man/pnamedcnt.Rd
% Part of pqR.
% Copyright (C) 2013 Radford M. Neal
% Distributed under GPL 2 or later

\name{get_rm}
\alias{get_rm}
\title{Get value of variable and then remove it}
\description{
This function removes a variable from the current environment, and returns
its value just before it was removed.  It is useful primarily when
the value returned may have been stored only in the variable removed,
in which case the value will be known to not be used elsewhere, and
will not have to be copied if it is later modified.
}
\usage{
get_rm(x)
}
\arguments{
  \item{x}{A variable in the current environment.}
}
\details{
  For each object, R maintains a count of how many variables/names reference 
  the object, called "named".  If this count is greater than one, modifications
  to part of the variable will cause it to first be copied, so that the
  other references will see the original value.  When the value in a variable 
  has a count of one, this can be reduced to zero when that variable is removed,
  but on its own, this is not useful, since the value can no longer be 
  accessed.  However, with \code{get_rm}, the count can be reduced to zero
  and the value still be used, for example as a function return value,
  or an operand of an arithmetic operator, whose space may be reused for 
  the result.  See the examples below.

  An error is signaled if the variable does not exist in the current
  environment.
}
\value{
  The value of \code{x}, just before it was removed.
}

\seealso{
  \code{\link{get}}, \code{\link{rm}}
}
\examples{
f <- function () { v <- 1:1000; v <- v + v^2; get_rm(v) }
u <- f()
u[500] <- 0L  # the change to u[500] can be done without copying

g <- function (v) { v[1:(length(v)/2)] <- 1; get_rm(v) }
u <- g(numeric(1000))
u[500] <- 9  # just one allocation of a vector of length 1000 will be done

A <- numeric(1000)+2
B <- numeric(1000)+3
A <- get_rm(A)*B # doesn't allocate space for a third 1000-element vector
}
\keyword{programming}
