% File src/library/base/man/Control.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Modifications for pqR Copyright (c) 2016 Radford M. Neal.
% Distributed under GPL 2 or later

\name{Control}
\alias{Control}
\alias{if}
\alias{else}
\alias{for}
\alias{in}
\alias{along}
\alias{across}
\alias{down}
\alias{while}
\alias{repeat}
\alias{break}
\alias{next}
\title{Control Flow}
\description{
  These are the basic control-flow constructs of the \R language.  They
  function in much the same way as control statements in any Algol-like
  language.  They are all (except \code{along}) \link{reserved} words.
}
\usage{
if (cond) expr
if (cond) cons.expr  else  alt.expr

for (var in seq) expr
for (var along vector) expr
for (var1, var2 along matrix) expr
for (var1, var2, var3 along array) expr
for (var down matrix) expr
for (var across matrix) expr

while (cond) expr
repeat expr
break
next
}
\arguments{
  \item{cond}{A length-one logical vector that is not \code{NA}.
    Conditions of length greater than one are accepted with a warning, but
    only the first element is used.  Other types are coerced to logical
    if possible, ignoring any class.
  }
  \item{var, var1, var2, var3}{A syntactical name for a variable.}
  \item{seq}{An expression evaluating to a vector (including a list and
    an \link{expression}) or to a \link{pairlist} or \code{NULL}.  A
    factor value will be coerced to a character vector.}
  \item{vector}{Also an expression evaluating to a vector of some length 
                (or actually anything, with non-vectors consider to have
                 length one).}
  \item{matrix}{An expression evaluating to a matrix.}
  \item{array}{An expression evaluating to an array with some number of
               dimensions (three dimensions in the example above).}
  \item{expr, cons.expr, alt.expr}{
    An \emph{expression} in a formal sense.  This is either a
    simple expression or a so called \emph{compound expression}, usually
    of the form \code{\{ expr1 ; expr2 \}}.
  }
}
\details{
  \code{break} breaks out of a \code{for}, \code{while} or \code{repeat}
  loop; control is transferred to the first statement outside the
  inner-most loop. \code{next} halts the processing of the current
  iteration and advances the looping index.  Both \code{break} and
  \code{next} apply only to the innermost of nested loops.
  
  Note that it is a common mistake to forget to put braces (\code{\{ .. \}})
  around your statements, e.g., after \code{if (..)} or \code{for (....)}.
  For that reason, one (somewhat extreme) attitude of defensive programming
  is to always use braces, e.g., for \code{if} clauses.

  To avoid a syntax error in entering a \code{if ... else}
  construct at the keyboard or via \code{source}, you need to avoid
  putting a newline between the end of \code{cons.expr} (often a \code{\}})
  and \code{else}.

  The \code{seq}, \code{vector}, \code{matrix}, or \code{array} in a 
  \code{for} loop is evaluated at the start of
  the loop; changing it subsequently does not affect the loop.  

  For the \code{in} variant of \code{for}, if
  \code{seq} has length zero the body of the loop is skipped. Otherwise the
  variable \code{var} is assigned in turn the value of each element of
  \code{seq}. You can assign to \code{var} (or remove it) within the body of 
  the loop,
  but this will not affect the next iteration.  When the loop terminates,
  \code{var} remains as a variable containing its latest value.

  For the \code{along} variant of \code{for}, if a single variable is
  given before \code{along}, the loop is performed with this variable set 
  to integer values from 1 to the length of \code{vector}.  If more than one 
  variable is given before \code{along}, the \code{matrix} or \code{array} value
  must have a dimension attribute with the number of dimensions matching
  the number of variables given.  The loop is done for all combinations
  of values for these variables, in sequence from 1 to the corresponding
  dimension, with the first variable varying the fastest.

  For the \code{down} and \code{across} variants of \code{for}, the
  loop is performed with the variable set from 1 to the number of rows
  in the matrix (for \code{down}) or from 1 to the number of columns in
  the matrix (for \code{across}).  Although the expression after \code{down}
  or \code{across} is normally a matrix, any expression is actually 
  allowed --- for higher dimensional arrays, the first or second dimension
  is used, and for non-arrays, \code{down} and \code{across} act the
  same as \code{along}.

  The class of the \code{seq} iterated over by \code{for} with \code{in} is 
  ignored, as if it were \code{unclass(seq)}.  This is also true
  for the class of the \code{vector}, \code{matrix}, 
  or \code{array} in a \code{for} with \code{along}, \code{down}, or
  \code{across}, but in future it may be that \code{length} and \code{dim} 
  methods for these objects will be used, if they are defined.  Accordingly,
  if the intent is that \code{along}, \code{down}, or \code{across} be
  applied to an unclassed object, expliclit use of \code{\link{unclass}}
  is recommended.  The performance cost of using \code{unclass} in this 
  context is negligible.
}
\value{
  \code{if} returns the value of the expression evaluated, or
  \code{NULL} invisibly if none was (which may happen if there is no
  \code{else}).

  \code{for}, \code{while} and \code{repeat} return \code{NULL} invisibly.

  \code{for} leaves the variables it controls set to the values they
  had at the end of the last iteration of the loop.  If the loop was
  done zero times, the variable is set to \code{NULL} with the
  \code{in} form, the variable is set to zero with the \code{across}
  and \code{down} forms, and the variables are set to the size of the
  corresponding dimension with the \code{along} form (at least one
  of these sizes will of course be zero).

  \code{break} and \code{next} do not return a value as they transfer
  control within the loop.  When \code{break} terminates a \code{for} loop,
  the variables controlled by the \code{for} loop will have the values
  they had at the time of the \code{break}.
}

\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{Syntax}} for the basic \R syntax and operators,
  \code{\link{Paren}} for parentheses and braces.
  
  \code{\link{ifelse}}, \code{\link{switch}} for other ways to control flow.
}
\examples{
for (i in 1:5) print(1:i)
for (n in c(2,5,10,20,50)) {
   x <- stats::rnorm(n)
   cat(n,":", sum(x^2),"\n")
}
f = factor(sample(letters[1:5], 10, replace=TRUE))
for (i in unique(f)) print(i)

s <- character()
for (i along letters) s[i] <- paste0(".",letters[i])

identical(s,paste0(".",letters))  # These should be TRUE
identical(i,length(letters))

M <- matrix(0,nrow=4,ncol=5)
for (i, j along M) M[i,j] <- i*j

identical(M,outer(1:4,1:5))  # These should be TRUE
identical(c(i,j),c(4L,5L))

N <- matrix(0,nrow=4,ncol=5)
for (j across N) for (i down N) N[i,j] <- i*j

identical(N,outer(1:4,1:5))  # These also should be TRUE
identical(c(i,j),c(4L,5L))

# Some senseless operations for demonstration purposes...
v <- c(7,0,-1,3,-2,0,1,-3,9)
for (i along v) {
    if (v[i] < 0) {
        v[i] <- -v[i]
        next
    }
    if (v[i] == 1)
        break
    v[i] <- v[i] + if (v[i] < 5) 2 else 1
}

identical(v,c(8,2,1,5,2,2,1,-3,9))  # These should be TRUE
identical(i,7L)

# Recommended use of unclass.
a <- c(3,1)
class(a) <- "fred"
length.fred <- function (x) 4
for (i along unclass(a)) print(i) # Will print just 1 and 2
for (i along a) print(i)          # Does the same thing now, but perhaps
                                  #  in future it will print 1, 2, 3, and 4
}
\keyword{programming}
\keyword{iteration}
\keyword{logic}
