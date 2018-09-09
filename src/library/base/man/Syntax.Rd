% File src/library/base/man/Syntax.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Modifications for pqR Copyright (c) 2016 Radford M. Neal.
% Distributed under GPL 2 or later

\name{Syntax}
\alias{Syntax}
\title{Operator Syntax and Precedence}
\description{
  Outlines \R syntax and gives the precedence of operators
}
\details{
  The following unary and binary operators are defined.  They are listed
  in precedence groups, from highest to lowest.
  \tabular{ll}{
    \code{::} \code{:::}\tab access variables in a namespace\cr
    \code{$} \code{@}\tab component / slot extraction\cr
    \code{[} \code{[[}\tab indexing\cr
    \code{^}\tab exponentiation (right to left)\cr
    \code{-} \code{+}\tab unary minus and plus\cr
    \code{:}\tab sequence operator\cr
    \code{\%op\%}\tab special operators (includes \code{\%\%} \code{\%/\%})\cr
    \code{*} \code{/}\tab multiply, divide\cr
    \code{+} \code{-}\tab (binary) add, subtract\cr
    \code{..}\tab upward sequence operator (no ordering)\cr
    \code{<} \code{>} \code{<=} \code{>=} \code{==} \code{!=}\tab 
             ordering and comparison (no ordering)\cr
    \code{!}\tab negation\cr
    \code{&} \code{&&}\tab and\cr
    \code{|} \code{||}\tab or\cr
    \code{~}\tab (unary and binary) as in formulae\cr
    \code{->} \code{->>}\tab rightwards assignment\cr
    \code{=}\tab assignment (right to left)\cr
    \code{<-} \code{<<-}\tab assignment (right to left)\cr
    \code{?}\tab help (unary and binary)\cr
  }
  Within an expression operators of equal precedence are evaluated
  from left to right (eg, \code{a/b/c} means \code{(a/b)/c}), except 
  if right to left evaluation is specified above (eg, \code{a^b^c}
  means \code{a^(b^c)}), or no ordering is specified above, so
  combining them without parentheses is illegal (eg, \code{a<b<c}).

  The binary operators \code{::}, \code{:::}, \code{$} and \code{@} require
  names or string constants on the right hand side, and the first two
  also require them on the left.

  Language elements such as \code{if} are effectively lower
  precedence than all of these operators.  For example, \code{if (a>0)
  a else 2 * a} means the same as \code{if (a>0) a else (2 * a)}, which is
  not the same as \code{(if (a>0) a else 2) * a}.

  The links in the \bold{See Also} section cover most other aspects of
  the basic syntax.
}
\note{
  There are substantial precedence differences between \R and S.  In
  particular, in S \code{?} has the same precedence as (binary) \code{+}
  and \code{-} and \code{&}, \cod{&&}, \code{|}, and \code{||} have 
  equal precedence.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{Arithmetic}}, \code{\link{Comparison}}, \code{\link{Control}},
  \code{\link{Extract}}, \code{\link{Logic}},
  \code{\link{NumericConstants}}, \code{\link{Paren}},
  \code{\link{Quotes}}, \code{\link{Reserved}}.

  The \emph{R Language Definition} manual.
}
\keyword{documentation}
\keyword{programming}
