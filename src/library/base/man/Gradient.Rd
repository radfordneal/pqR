% File src/library/base/man/Gradient.Rd
% pqR : A pretty quick version of R
% Copyright (C) 2018, 2019 by Radford M. Neal
% Distributed under GPL 2 or later

\name{Gradient}
\alias{Gradient}
\alias{with gradient}
\alias{track gradient}
\alias{compute gradient}
\alias{gradient_of}
\alias{no_gradient}
\title{Gradient Computation}

\description{
  These are the facilities for computing gradients via run-time
  automatic differentiation.  For symbolic differentiation facilities,
  see \code{\link{deriv}}.  For numerical differentiation, see
  \code{\link{numericDeriv}}.

  The present implementation of these facilities is a preliminary
  one that supports only gradients of scalar values, with respect to scalar
  variables.  Support for vectors, matrices, arrays, and lists, and for
  higher-order derivatives, is planned.
}

\usage{
with gradient (var=v.expr) expr
with gradient (var) expr
with gradient (var1=v1.expr,var2=v2.expr) expr
with gradient (var1,var2) expr

track gradient (var=v.expr) expr
track gradient (var) expr
track gradient (var1=v1.expr,var2=v2.expr) expr
track gradient (var1,var2) expr

compute gradient (var1=v1.expr, var2=v2.expr) expr as (g1.expr, g2.expr)
compute gradient (var1, var2) expr as (g1.expr, g2.expr)

gradient_of (e)
no_gradient (e)
}

\arguments{
  \item{var, var1, var2}{(And so forth for var3, etc.) Names (not strings)
    of variables with respect to which gradients will be tracked or computed.}
  \item{expr}{An expression, often a compound expression, of a form such as
    \code{\{expr1;expr2\}}, which is evaluated in a new environment
    containing the new variable or variables listed.}
  \item{g1.expr, g2.expr}{(and so forth for g3.expr, etc.) Expressions
    giving the gradients of the specified \code{expr} with respect to
    \code{var1}, \code{var2}, etc.}
  \item{e}{An expression whose gradient is computed (if possible), or
           is not computed.}
}

\details{
The \code{with gradient} construct evaluates \code{expr} in a new
environment (with the current environment as its parent) containing
the variable \code{var} (or variables \code{var1}, \code{var2}, etc),
with initial value \code{v.expr} (or initial values \code{v1.expr},
\code{v2.expr}, etc.).  The value of \code{expr} is returned as that
of \code{with gradient}, with a \code{"gradient"} attribute attached
containing the derivative of the value of \code{expr} with respect to
\code{var}, or the derivatives with respect to \code{var1},
\code{var2}, etc..  (Any existing \code{"gradient"} attribute is
discarded.)

During the evaluation of \code{expr} within \code{with gradient},
assignments to local variables (in the new environment) will record
the gradient of the assigned value with respect to \code{var} (or
\code{var1}, \code{var2}, etc.), and with respect to the variables
listed in any enclosing \code{with gradient} or \code{track gradient}
constructs, so that if the value of the variable is used to compute
the final value for \code{with gradient}, its gradient can be
computed.  Note, however, that the gradients of attribute values are
not recorded, nor are gradients recorded when non-local assignments
are made with \code{<<-}.

Tracking of gradients continues when a function is called with 
one or more arguments with tracked gradients.  This includes functions
for S3 methods, but not S4 methods.

Within \code{expr}, the gradient of an expression, \code{e}, with
respect the variable or variables of the enclosing \code{with
gradient} construct can be found with \code{gradient_of(e)}.  The
value of \code{gradient_of} does not itself have any gradient
information --- hence \code{gradient_of(gradient_of(e))} will not
produce a second derivative (it will always be zero).

Tracking of gradients can be explicitly suppressed (to save time) with
\code{no_gradient(e)}.  Gradient tracking is automatically suppressed
when evaluating expressions that are used as \code{if} or \code{while}
conditions, as indexes, or as expressions iterated over with
\code{for}.

The \code{track gradient} construct is like \code{with gradient},
except that the gradient is not attached to the final result.  It is
therefore useful only in combination with calls of \code{gradient_of},
or if it is inside another \code{track gradient} or
\code{with gradient} construct.  When these constructs are nested,
derivatives with respect to an inner variable may determine
derivatives with respect to an outer variable, by application of the
chain rule.

In forms of \code{with gradient} and \code{track gradient} in which no
expression follows a variable, the expression is assumed to be the
same as the variable (evaluated in the current (not the new)
environment).

The gradient of an expression can be specified explicity using the
\code{compute gradient} construct, as an alternative to simply letting
the gradient be obtained automatically, or as a necessary measure if
the expression contains built-in functions for which automatic
differentiation has not yet been implemented.  The \code{expr} within
\code{compute gradient} is evaluated in a new environment in which one
or more variables have been defined (two in the above templates).  The
initial values of these variables are as specified, defaulting to the
variable's value evaluated in the current environment.  Gradients with
respect to these new variables are not tracked automatically, but are
instead specified by the expressions after \code{as}.  The chain rule
is used to translate these gradients to gradients with respect to
variables used to compute \code{v1.expr}, \code{v2.expr}, etc.

If computation of a gradient has not been requested, \code{compute gradient}
will evaluate only the value, skipping evaluation of the expressions after
\code{as}.  Computation of gradients for built-in functions is also
skipped when it is known that the gradient will not be needed.

Gradients can be computed for expressions that are not differentiable at
some points, with the gradient returned at such points being arbitrary.

Gradients may not have been defined for some builtin functions (even if
they exist mathematically), in which case they will appear to be zero.
When a builtin functions returns \code{NA} or a \code{NaN} value, the
gradient will be regarded as zero (without an error or warning).

Gradients may be defined for real-valued random generation functions
(eg, \code{rnorm}).  The gradient for these functions indicates how a
change in the distribution parameters would produce a change in the
generated random value, if the state of the random number generator
when calling the function were kept fixed.

The following built-in functions and operators will compute gradients,
with respect to all their real-valued arguments (unless noted):
\tabular{l}{
+, -, *, /, ^ (+ and - may be unary) \cr
\cr
abs, sqrt, expm1, exp, log1p, log2, log10, log (one-argument form only) \cr
cos, sin, tan, acos, asin, atan, atan2 \cr
cosh, sinh, tanh, acosh, asinh, atanh \cr
gamma, lgamma, digamma, trigamma, beta, lbeta \cr
\cr
dbeta, pbeta (1st argument only), qbeta (1st argument only) \cr
dbinom, pbinom \cr
dcauchy, pcauchy, qcauchy, rcauchy \cr
dexp, pexp, qexp, rexp \cr
dgeom, pgeom \cr
dlogis, plogis, qlogis, rlogis \cr
rlnorm \cr
dnorm, pnorm, qnorm, rnorm \cr
dpois, ppois \cr
dt (only with no ncp parameter)\cr
dunif, punif, qunif, runif \cr
rweibull
}
}

\value{
The gradient returned by \code{gradient_of} or attached as a \code{"gradient"}
attribute by \code{with gradient} will be a scalar real if the gradient
is with respect to only one variable.  When the \code{with gradient} or
\code{track gradient} construct has more than one variable, the gradient
will be a list of scalar real values, with names corresponding to the
variables.
}

\examples{
a <- with gradient (x=3) sin(x)
attr(a,"gradient")               # should be cos(3)

a <- with gradient (x=3,y=2) sin(x+2*y)
attr(a,"gradient")$x             # should be cos(7)
attr(a,"gradient")$y             # should be 2*cos(7)

x <- 3
a <- with gradient (x) { r <- sin(x); r^2 }
attr(a,"gradient")               # should be 2*sin(3)*cos(3)

sqr <- function (y) y^2          # gradients can be tracked through sqr
x <- 3
a <- with gradient (x) { r <- sin(x); sqr(r) }
attr(a,"gradient")               # should be 2*sin(3)*cos(3)

funny <- function (x,y) {        # has a discontinuity
    q <- no_gradient(2*x)          # gradient of 2*x won't be tracked
    if (q>y/2)                     # gradient of y/2 won't be tracked
        sin(x+y) 
    else 
        cos(x+y)
}

track gradient (a = 3) {
    print (gradient_of(funny(a,a)))   # prints 2*cos(3+3)
    print (gradient_of(funny(a,8*a))) # prints -9*sin(3+24)
}

sigmoid <- function (x)
    compute gradient (x) { v <- 1 / (1+exp(-x)); v }
    as (v * (1-v))

sigmoid(1)                       # no gradient computed, only value
with gradient (x=1) sigmoid(x)   # both value and gradient computed

track gradient (x=1)             # should compute the same gradient
    gradient_of (1/(1+exp(-x)))  # as above, but perhaps more slowly
                                 # (though maybe not since x is scalar)

set.seed(123); with gradient (r=5) rexp(1,r)

set.seed(123); v1<-rexp(1,4.999)
set.seed(123); v2<-rexp(1,5.001)
(v2-v1) / 0.002                  # should be close to gradient above

}
\keyword{programming}
