% File src/library/base/man/Gradient.Rd
% pqR : A pretty quick version of R
% Copyright (C) 2018 by Radford M. Neal
% Distributed under GPL 2 or later

\name{Gradient}
\alias{Gradient}
\alias{with_gradient}
\alias{track_gradient}
\alias{compute_gradient}
\alias{gradient_of}
\alias{no_gradient}
\title{Gradient Computation}

\description{
  These are the facilities for computing gradients via run-time
  automatic differentiation.  For symbolic differentiation facilities,
  see \code{\link{deriv}}.

  The present implementation of these facilities is a preliminary
  one that supports only gradients of scalar values, with respect to scalar
  variables.  Support for vectors, matrices, arrays, and lists is planned.
}

\usage{
with_gradient (var=v.expr) expr
with_gradient (var) expr

track_gradient (var=v.expr) expr
track_gradient (var) expr

compute_gradient (var1=v1.expr, var2=v2.expr) expr as (g1.expr, g2.expr)
compute_gradient (var1, var2) expr as (g1.expr, g2.expr)

gradient_of (e)
no_gradient (e)
}

\details{
The \code{with_gradient}, \code{track_gradient}, and \code{compute_gradient}
keywords used in these constructs are reserved words; \code{as} is not
reserved. 

The \code{with_gradient} construct evaluates \code{expr} in a new
environment (with the current environment as its parent) containing
the variable \code{var}, with initial value \code{v.expr}.  The value
of \code{expr} is returned as that of \code{with_gradient}, with a
\code{"gradient"} attribute attached containing the derivative of the
value of \code{expr} with respect to \code{var}.  (Any existing
\code{"gradient"} attribute is discarded.)

During the evaluation of \code{expr} within \code{with_gradient}, assignments
to local variables (in the new environment) will record the gradient
of the assigned value with respect to \code{var}, and with respect to
the \code{var} for any enclosing \code{with_gradient} or \code{track_gradient}
constructs, so that if the value of the variable is used to compute
the final value for \code{with_gradient}, its gradient can be computed.
Note, however, that the gradients of attribute values are not recorded,
nor are gradients recorded when non-local assignments are made with \code{<<-}.

Within \code{expr}, the gradient of an expression, \code{e}, with
respect to \code{var} can be found with \code{gradient_of(e)}.
Tracking of gradients can be explicitly suppressed (to save time) with
\code{no_gradient(e)}.  Gradient tracking is automatically suppressed
when evaluating expressions that are used as \code{if} or \code{while}
conditions, as indexes, or as expressions iterated over with
\code{for}.

The \code{track_gradient} construct is like \code{with_gradient},
except that the gradient is not attached to the final result.  It is
therefore useful only in combination with calls of \code{gradient_of},
or if it is inside another \code{track_gradient} or
\code{with_gradient} construct.  When these constructs are nested,
derivatives with respect to the inner variable may determine derivatives
with respect to the outer variable, by application of the chain rule.

In the forms of \code{with_gradient} and \code{track_gradient} in which
no expression follows \code{var}, the expression is assumed to be \code{var},
evaluated in the current (not the new) environment.

User-defined functions can specify the gradient using the
\code{compute_gradient} construct.  The \code{expr} within
\code{compute_gradient} is evaluated in a new environment in which one
or more variables have been defined (two in the above templates).  The
initial values of these variables are as specified, defaulting to the
variable's value evaluated in the current environment.  Gradients with
respect to these new variables are not tracked automatically, but are
instead specified by the expressions after \code{as}.  (These
expressions may be unnamed, or may be named identically to the
corresponding variable.)  The chain rule is used to translate these
gradients to gradients with respect to variables used to compute
\code{v1.expr}, \code{v2.expr}, etc.

If computation of a gradient has not been requested, \code{compute_gradient}
will evaluate only the value, skipping evaluation of the expressions after
\code{as}.  Computation of gradients for built-in functions is also
skipped when the gradient is known to not be needed.

Gradients can be computed for functions that are not differentiable at
some points, with the gradient returned at such points being arbitrary.

Gradients may not have been defined for some builtin functions (even if
they exist mathematically), in which case they will appear to be zero.

Gradients may be defined for real-valued random generation functions
(eg, \code{rnorm}), with the gradient being defined as if the
generation was done by applying the corresponding quantile function to
a uniform (0,1) random variate (considered fixed).

The following built-in functions and operators will compute gradients,
with respect all their real-valued arguments (unless noted):
\tabular{l}{
+, -, *, /, ^ (+ and - may be unary) \cr
\cr
abs, sqrt, exp, expm1, log, log1p \cr
cos, sin, tan, acos, asin, atan \cr
cosh, sinh, tanh, acosh, asinh, atanh \cr
gamma, lgamma, digamma, trigamma \cr
\cr
dnorm, pnorm, qnorm, rnorm \cr
dpois, ppois 
}
}

\examples{
a <- with_gradient (x=3) sin(x)
attr(a,"gradient")               # should be cos(3)

x <- 3
a <- with_gradient (x) { r <- sin(x); r^2 }
attr(a,"gradient")               # should be 2*sin(3)*cos(3)

sqr <- function (y) y^2
x <- 3
a <- with_gradient (x) { r <- sin(x); sqr(r) }
attr(a,"gradient")               # should also be 2*sin(3)*cos(3)

funny <- function (x,y) {  # has a discontinuity
    q <- no_gradient(2*x)    # gradient of 2*x won't be tracked
    if (q>y/2)               # gradient of y/2 won't be tracked
        sin(x+y) 
    else 
        cos(x+y)
}

track_gradient (a = 3) {
    print (gradient_of(funny(a,a)))    # prints 2*cos(3+3)
    print (gradient_of(funny(a,8*a)))  # prints -9*sin(3+24)
}

sigmoid <- function (x)
    compute_gradient (x) { v <- 1 / (1+exp(-x)); v }
    as (v * (1-v))

print(sigmoid(1))                    # no gradient computed, only value

a <- with_gradient (x=1) sigmoid(x)  # both value and gradient computed
attr(a,"gradient")

track_gradient (x=1)                 # should compute the same gradient
    gradient_of (1/(1+exp(-x)))      # as above, but perhaps more slowly
                                     # (though maybe not since x is scalar)
}
\keyword{programming}
