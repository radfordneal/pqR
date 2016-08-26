% File src/library/base/man/assignOps.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Modifications for pqR Copyright (c) 2015 Radford M. Neal.
% Distributed under GPL 2 or later

\name{assignOps}
\alias{<-}
\alias{=}
\alias{<<-}
\alias{->}
\alias{->>}
\alias{assignOps}
\title{Assignment Operators}
\description{
  Assign a value to a name.
}
\usage{
\special{x <- value}
\special{x <<- value}
\special{value -> x}
\special{value ->> x}

\special{x = value}
}
\arguments{
  \item{x}{a variable name (possibly quoted).}
  \item{value}{a value to be assigned to \code{x}.}
}
\details{
  There are three different assignment operators: two of them
  have leftwards and rightwards forms.

  The \code{<-}, \code{=}, and \code{->} operators assign into the
  environment in which they are evaluated.  The \code{<-} and
  \code{->} operators can be used anywhere, whereas the \code{=}
  operator is not allowed as the outer operator in arguments to
  functions, \code{if} or \code{while} conditions, or the object
  iterated over for \code{for}.

  The \code{<<-} and \code{->>} operators cause a search to made
  through succesive enclosing environments for an existing definition
  of the variable being assigned.  If such a variable is found (and
  its binding is not locked) then its value is redefined, otherwise
  assignment takes place in the immediate enclosing environment
  (which will be the global environment if the function was defined
  in the global environment).

  In all the assignment operator expressions, \code{x} can be a name
  or an expression defining a part of an object to be replaced (e.g.,
  \code{z[[1]]}).  A syntactic name does not need to be quoted,
  though it can be (preferably by \link{backtick}s).

  The leftwards forms of assignment \code{<-}, \code{=}, and
  \code{<<-}, group right to left, whereas \code{->} and \code{->>}
  group from from left to right.  The \code{=} operator has lowest
  precedence, followed by \code{<-} and \code{<<-}, with \code{->} and
  \code{->>} having highest precedence.
}
\value{
  \code{value}.  Thus one can use \code{a <- b <- c <- 6} to set
  three variables to 6.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Chamber, J. M. (1998)
  \emph{Programming with Data.  A Guide to the S Language}.
  Springer (for \code{=}).
}

\seealso{
  \code{\link{assign}}, for \dQuote{subassignment} such
  as \code{x[i] <- v}, \code{\link{[<-}}; % ./Extract.Rd
  \code{\link{environment}}.
}
\keyword{data}
