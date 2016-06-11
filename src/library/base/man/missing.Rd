% File src/library/base/man/missing.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{missing}
\alias{missing}
\alias{missing_from_underline}
\alias{_}
\title{Does a Formal Argument have a Value?}
\usage{
missing(x)
missing_from_underline(x)
}
\description{
\code{missing} can be used to test whether an argument to a function
is missing in the call, either because the actual argument is empty, or
because it is \code{_} (distinguished in \code{missing_from_underline}),
or because it is an argument that is itself missing.
}
\arguments{
  \item{x}{a formal argument.}
}
\details{
  \code{missing(x)} is only reliable if \code{x} has not been altered
  since entering the function: in particular it will \emph{always}
  be false after \code{x <- match.arg(x)}. 
  
  The example shows how a plotting function can be written to work with
  either a pair of vectors giving x and y coordinates of points to be
  plotted or a single vector giving y values to be plotted against their
  indices.

  Currently \code{missing} can only be used in the immediate body of
  the function that defines the argument, not in the body of a nested
  function or a \code{local} call.  This may change in the future.

  \code{missing_from_underline} is like \code{missing} except that
  it returns \code{TRUE} only if the argument was an explicit missing
  argument, indicated by \code{_}, rather than by the argument being
  empty.  See the examples below.
  
  These are \sQuote{special} \link{primitive} functions, as they must not
  evaluate their argument.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Chambers, J. M. (1998)
  \emph{Programming with Data.  A Guide to the S Language}.
  Springer.
}
\seealso{
  \code{\link{substitute}} for argument expression;
  \code{\link{NA}} for missing values in data.
}
\examples{
myplot <- function(x,y) {
                if(missing(y)) {
                        y <- x
                        x <- 1:length(y)
                }
                plot(x,y)
          }

f <- function (x) c(missing(x),missing_from_underline(x))
f(1)
f()
f(x=)
f(_)
f(x=_)

g <- function (y) f(y)
g()   # x in f will be missing in these examples, since 
g(_)  # it links to y in g, which is missing in these calls
}

\keyword{programming}
