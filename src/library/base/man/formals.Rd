% File src/library/base/man/formals.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Development Team
% Modifications for pqR Copyright (c) 2014 Radford M. Neal.
% Distributed under GPL 2 or later

\name{formals}
\alias{formals}
\alias{formals<-}
\title{Access to and Manipulation of the Formal Arguments}
\description{
  Get or set the formal arguments of a function.
}
\usage{
formals(fun = sys.function(sys.parent()))
formals(fun, envir = environment(fun)) <- value
}
\arguments{
  \item{fun}{a function object, or see \sQuote{Details}.}
  \item{envir}{environment in which the function should be defined.}
  \item{value}{a list (or pairlist) of \R expressions.}
}
\details{
  
  For the first form, \code{fun} can also be a character string naming
  the function to be manipulated, which is searched for from the parent
  frame.  If \code{fun} is not specified, the function calling \code{formals} 
  is used.

  Only \emph{closures} can have their formals set, but primitive functions
  can have their putative formals (as will be printed, or returned by
  \code{args}) returned with \code{formals}.
}
\value{
  \code{formals} returns the formal argument list of the function
  specified, as a \code{\link{pairlist}}, or \code{NULL} for a
  non-function.

  The replacement form sets the formals of a function to the
  list/pairlist on the right hand side, and (potentially) resets the
  environment of the function.
}
\seealso{
  \code{\link{args}} for a human-readable version,
  \code{\link{alist}},
  \code{\link{body}},
  \code{\link{function}}.
}
\examples{
require(stats); require(graphics)
length(formals(lm))      # the number of formal arguments
names(formals(boxplot))  # formal arguments names

f <- function(x) a+b
formals(f) <- alist(a=,b=3) # function(a,b=3)a+b
f(2) # result = 5
}
\keyword{programming}
