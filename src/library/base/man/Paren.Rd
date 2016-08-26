% File src/library/base/man/Paren.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Team
% Modifications for pqR Copyright (c) 2014, 2016 Radford M. Neal.
% Distributed under GPL 2 or later

\name{Paren}
\alias{Paren}
\alias{(}
\alias{\{}
\title{Parentheses and Braces}
\usage{
\special{( \dots )}

\special{{ \dots }}
}
\description{
  Open parenthesis, \code{(}, and open brace, \code{\{}, are
  \code{\link{.Primitive}} functions in \R.

  Effectively, \code{(} is semantically equivalent to the identity
  \code{function(x) x}, whereas \code{\{} is slightly more interesting,
  see examples.  However, unless the \code{keep.parens} option is
  set, parentheses necessary to over-ride operator precedence (except 
  when inside a formula) are eliminated from the internal representation 
  of an expression.  Such necessary parentheses are automatically
  provided when an expression is deparsed.
}
\value{
  For \code{(}, the result of evaluating the argument.  This has
  visibility set, so will auto-print if used at top-level.

  For \code{\{}, the result of the last expression evaluated.  This has
  the visibility of the last evaluation.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{if}}, \code{\link{return}}, etc for other objects used in
  the \R language itself.

  \code{\link{Syntax}} for operator precedence.

  \code{deparse}.
}
\examples{
f <- get("(")
e <- expression(3 + 2 * 4)
identical(f(e), e)

do <- get("\{")
do(x <- 3, y <- 2*x-3, 6-x-y); x; y

## note the differences
(2+3)
{2+3; 4+5}
(invisible(2+3))
{invisible(2+3)}
}
\keyword{programming}
