% File src/library/base/man/Gradient.Rd
% pqR : A pretty quick version of R
% Copyright (C) 2018, 2019, 2020 by Radford M. Neal
% Distributed under GPL 2 or later

\name{Gradient}
\alias{Gradient}
\alias{with gradient}
\alias{track gradient}
\alias{back gradient}
\alias{compute gradient}
\alias{gradient_of}
\alias{no_gradient}
\title{Gradient Computation}

\description{
  These are the facilities for computing gradients via run-time
  automatic differentiation.  For symbolic differentiation facilities,
  see \code{\link{deriv}}.  For numerical differentiation, see
  \code{\link{numericDeriv}}.

  The present implementation of these facilities is preliminary.
  Gradient computation for some operations is planned but not yet
  implemented, and for some operations that are supported, gradient computations
  are much less efficient than is planned for a full-fledged version.

  At present, there is no support for higher-order derivatives, or for
  differentiation of complex values.
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

back gradient (var=v.expr) expr
back gradient (var) expr
back gradient (var1=v1.expr,var2=v2.expr) expr
back gradient (var1,var2) expr

compute gradient (var1=v1.expr, var2=v2.expr) expr as g1.expr, g2.expr
compute gradient (var1, var2) expr as g1.expr, g2.expr

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

The \code{with gradient}, \code{track gradient}, and \code{back
gradient} constructs all ask that gradients of an expression with
respect to certain variables be computed, with differences between
them as described below.  The \code{gradient_of} and
\code{no_gradient} functions may be used within these constructs.  
The \code{compute gradient} construct specifies how gradients are
computed; it is not needed for the many functions for which 
gradient computations are built in.

Gradients can be computed for expressions that are not differentiable at
some points, with the gradient returned at such points being arbitrary.

Asking for gradient information:

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
listed in any enclosing \code{with gradient}, \code{track gradient},
or active \code{back gradient} constructs, so that if the value of the
variable is used to compute the final value for \code{with gradient},
or is the argument of call of \code{gradient_of}, its gradient can be
computed.  This includes assignments made to a for loop variable.
Note, however, that the gradients of attribute values are
not recorded, nor are gradients recorded when non-local assignments
are made with \code{<<-}.

Tracking of gradients continues when a function is called with 
one or more arguments with tracked gradients.  This includes functions
for S3 methods, but not S4 methods.  Tracking will also be performed
when an expression is evaluated with \code{eval}, if the environment
used for the evaluation is one in which gradients are being tracked.

Within \code{expr}, or a function called from within it, the gradient
of an expression, \code{e}, with respect the variable or variables of
the innermost enclosing \code{with gradient}, \code{track gradient},
or active \code{back gradient} construct can be found with
\code{gradient_of(e)}.  The value of \code{gradient_of} does not
itself have any gradient information --- hence
\code{gradient_of(gradient_of(e))} will not produce a second
derivative (it will always be zero).

Tracking of gradients can be explicitly suppressed (to save time) with
\code{no_gradient(e)}.  Gradient tracking is automatically suppressed
when evaluating expressions that are used as \code{if} or \code{while}
conditions, as indexes, or as expressions iterated over with
\code{for}.

The \code{track gradient} construct is like \code{with gradient},
except that the gradient is not attached to the final result.  It is
therefore useful only in combination with calls of \code{gradient_of},
or if it is inside (in a dynamic sense) another \code{track gradient},
\code{with gradient}, or active \code{back gradient} construct.  When
these gradient constructs are nested, derivatives with respect to an inner
variable may determine derivatives with respect to an outer variable,
by application of the chain rule (backpropagation).

The \code{back gradient} construct is like \code{track gradient},
except that it does not allow \code{gradient_of} unless it is
evaluated within a \code{with gradient} or \code{track gradient}
construct, and hence only needs to track gradients if they will be
backpropagated to give gradients for such a (dynamically) enclosing
gradient construct.  It may therefore be included at little
performance cost in a function that may either be called for only its
value, or may be called for both its value and that value's gradient,
depending on whether it is called from within a gradient construct.

In forms of \code{with gradient}, \code{track gradient}, and
\code{back gradient} in which no expression follows a variable, the
expression is assumed to be the same as the variable (evaluated in the
current (not the new) environment).

When a \code{with gradient}, \code{track gradient}, or \code{back
gradient} construct is exitted, either normally or by an error exit,
the variables defined within its environment are removed.  This
prevents unnecessary duplicaton of objects, but does mean that
functions that have been created with this environment as their
enclosing environment will no longer have access to these variables.

Specifying how gradients should be computed:

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

The gradient computed by the expressions after \code{as} should in the
same form as is returned by \code{gradient_of} or attached as a
\code{gradient} attribute by \code{with gradient}, as described in the
value section below, except that Jacobain matrices that are square
with zero values off the diagonal may be returned as a vector of just
the diagonal elements.  The \code{dim} attribute (if present) of the
Jacobian matrices returned is ignored, but the numeric values returned
must nevertheless correspond to those for a matrix with the correct
dimensions.

A Jacobian matrix may be replaced by a function that computes the
Jacobian matrix, or computes products of it with another matrix.  Such
a function must have 0, 1, or 2 arguments, which if present must be
named \code{right} or \code{left}.  The function may be called with no
arguments, with just a \code{right} argument (if present), or with
just a \code{left} argument (if present); the function must be
prepared for any of these possibilities.  If called with no arguments
(testable with \code{missing}), the function must return the Jacobian
matrix (which may be just the diagonal part, as above).  If called
with a \code{right} argument (a matrix, or a vector representing a
matrix with one column), the function must return the product of the
Jacobian matrix with the \code{right} matrix.  If called with a
\code{left} argument (a matrix, or vector representing a matrix with
one row), the function must return the product of the \code{left}
matrix with the Jacobian matrix.  For these latter cases, it is
possible that the function may be able to compute the product
efficiently without ever creating the full Jacobian matrix.  The
function will not be called with both \code{right} and \code{left}
arguments.  The function may be called more than once, and may be
called after evaluation of the \code{compute gradient} expression has
completed.

NOTE: At present, the function will always be called with no
arguments, even if it has been written to handle \code{right} or
\code{left} arguments, so this feature is presently mostly pointless,
but it will become useful in future.

If computation of a gradient has not been requested, \code{compute gradient}
will evaluate only the value, skipping evaluation of the expressions after
\code{as}.  It is possible for the gradient expression to be
evaluted for some variables but not others (e.g., in the form shown above,
\code{g2.expr} might be evaluated but \code{g1.expr} not be evaluated).

Functions and operators that know how to compute gradients:

Many functions and operators defined in the base and other packages
know how to compute gradients.  Computation of gradients for built-in
functions is skipped when it is known that the gradient will not be
needed, so the overhead of the gradient facility when it is not being
used is quite small.

For those builtin functions that don't know how to compute gradients
(but for which gradients exist mathematically), gradients will appear
to be zero.  When a builtin functions returns \code{NA} or a
\code{NaN} value, the gradient will be regarded as zero (without an
error or warning, and maybe not consistently at present).

Gradients may be defined for real-valued random generation functions
(eg, \code{rnorm}).  The gradient for these functions indicates how a
change in the distribution parameters would produce a change in the
generated random value, if the state of the random number generator
when calling the function were kept fixed.

The following built-in functions and operators will compute gradients,
with respect to all their real scalar, vector, matrix, or array arguments
(unless noted), or when applicable, arguments that are lists of reals 
(or lists of lists of reals, etc.):
\preformatted{  if, ifelse, switch

  c, rbind, cbind, list, matrix, array, unlist, rep, rep.int, rep_len

  [   (for vector lists and numeric vectors, including matrices & arrays) 
  [<- (for vector lists and numeric vectors, including matrices & arrays) 

  [[   (for vector lists and numeric vectors, including matrices & arrays) 
  [[<- (for vector lists and numeric vectors, including matrices & arrays; 
        vector as index (indexing at multiple levels) not supported yet) 

  $   (for vector lists, not pairlists or environments)
  $<- (for vector lists, not pairlists for environments) 

  data.frame, as.data.frame, cbind.data.frame, rbind.data.frame
  methods for access and assignment to data frames with [, [[, and $
  
  +, -, *, /, ^ (+ and - may be unary) 
  
  abs, sqrt, expm1, exp, log1p, log2, log10, log (one-argument form only) 
  cos, sin, tan, acos, asin, atan, atan2 
  cosh, sinh, tanh, acosh, asinh, atanh 
  gamma, lgamma, digamma, trigamma, psigamma, beta, lbeta 
  
  dbeta, pbeta (1st argument only), qbeta (1st argument only) 
  dchisq (no ncp arg), pchisq (1st only, no ncp), qchisq (1st only, no ncp) 
  dbinom, pbinom 
  dcauchy, pcauchy, qcauchy, rcauchy 
  dexp, pexp, qexp, rexp 
  df (no ncp argument), pf (1st arg only, no ncp), qf (1st arg only,no ncp) 
  dgamma, pgamma (1st and 3rd args only), qgamma (1st and 3rd args only) 
    Note: 3rd argument of dgamma/pgamma/qgamma may be either rate or scale 
  dgeom, pgeom 
  dlogis, plogis, qlogis, rlogis 
  dlnorm, plnorn, qlnorm, rlnorm 
  dnbinom (3rd arg only), pnbinom (3rd arg only)
    Note: 3rd argument of dnbinom/pnbinom can be either prob or mu
  dnorm, pnorm, qnorm, rnorm 
  dpois, ppois 
  dt (with no ncp arg), pt (1st arg only, no ncp), qt (1st arg only,no ncp) 
  dunif, punif, qunif, runif 
  dweibull, pweibull, qweibull, rweibull

  apply, lapply, sapply, vapply, replicate, simplify2array
  as.vector, as.list, as.double, as.numeric, as.real, as.matrix
  unclass, drop, structure, get_rm
  storage.mode<-, length<-
  invisible

  mean, sum, prod, min, max, pmin, pmax, cumsum, cumprod, cummax, cummin
  rowSums, rowMeans, .rowSums, .rowMeans, 
  colSums, colMeans, .colSums, .colMeans

  t, \%*\%, crossprod, tcrossprod
  det, determinant

  solve (default method only, for matrices, not QR decompositions)
  diag (for creating diagonal matrix or extracting diagonal from matrix)
  diag<-
}

% TO COME: mapply, rapply
%          backsolve, forwardsolve, chol
%          aperm, sweep, scale
%          cov, cor, var

The following replacement functions do not compute any gradient information,
but do leave undisturbed any gradient information that is associated with 
the variable that they update:
\preformatted{  attr<-, attributes<-, names<-, dim<-, dimnames<-, class<-, oldClass<-
}
}

\value{
When the gradient is with respect to only one variable, the value
returned by \code{gradient_of} or attached as a \code{"gradient"}
attribute by \code{with gradient} will be a scalar real if this
variable has a scalar real value and the expression this is the gradient of 
also has a scalar real value.

When the variable and the expression for which the gradient is found
are both vectors not of length one, the gradient will be a matrix
(referred to as the "Jacobian" matrix) whose number of columns equals
the length of the variable's value, and whose number of rows equals
the length of the expression's value.  Any \code{dim} attribute for
the expression or variable is ignored when creating the Jacobian
matrix, which is always a matrix (not a higher-dimensional array).

When either the variable or the expression for which the gradient is
found are vectors of length one (so the Jacobian matrix would have one
column or one row), the gradient will be a simple vector, rather than
a matrix. 

The gradient of a list is a list of the same form, with gradients as
described above replacing the numeric elements.  For a gradient with
respect to a list variable, the gradient value is a list (or nested
lists) of the same form, with elements that are the gradient of the
expression value with respect to that element of the list; note that
the gradient of an expression value could itself be a list.

When the \code{with gradient} or
\code{track gradient} construct has more than one variable, the gradient
will be a list of gradient values, with names corresponding to the
variables.  Note that this is the same form as would be obtained if the
values of these variables were combined into a named list which was then
used as a single variable in the \code{with gradient} or \code{track gradient}
construct (see the example below).
}

\seealso{
\code{\link{Listops}}, which documents arithmetic on lists, which is
useful in conjunction with automatic differentiation (see the last example).
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
    as v * (1-v)

sigmoid(1)                       # no gradient computed, only value
with gradient (x=1) sigmoid(x)   # both value and gradient computed

track gradient (x=1)             # should compute the same gradient
    gradient_of (1/(1+exp(-x)))  # as above

track gradient (x=c(-3,1,2))     # gradient will be a diagonal jacobian
    gradient_of (1/(1+exp(-x)))  # matrix, given by its diagonal elements

set.seed(123); with gradient (r=5) rexp(1,r)

set.seed(123); v1<-rexp(1,4.999)
set.seed(123); v2<-rexp(1,5.001)
(v2-v1) / 0.002                  # should be close to rexp gradient above

r <- with gradient (a=7) list(a,a^2)
attr(r,"gradient")               # should be a list of 1 and 14

r <- with gradient (a=7) list(square=a^2,cube=a^3)
attr(r,"gradient")$cube          # gradients of lists retain names

with gradient (a=7,b=9) {
    r <- list()
    r$aplusb <- a+b
    r$atimesb <- a*b
    r$asqbsq <- r$atimesb^2
    list (r, a^2*b^2)            # a^2*b^2 should be the same as asqbsq
}

with gradient (a=7) {
    L <- list(square=a^2,cube=a^3)
    list (L$square*L$cube, a^5)  # both values and gradients of the two
}                                #   list elements should be the same

with gradient (a=3)              # only the gradient with respect to a
  with gradient (b=10*a)         #   will be shown, but the chain rule 
    c(a,b,a*b)                   #   is used to convert derivatives wrt
                                 #   to b into derivatives wrt to a

with gradient (a=5,b=6) c(a^2,a*b)                # these give the same
with gradient (x=list(a=5,b=6)) c(x$a^2,x$a*x$b)  #   value and gradient

with gradient (a=2) {            # find derivatives of powers of a, from
    L <- list(a)                 #   a^1 to a^10, evaluated at a=2
    for (i in 2..10) L[[i]] <- L[[i-1]] * a
    L
}

with gradient (a=2) {            # same as above, but with a real vector
    V <- a                       #   rather than a list
    for (i in 2..10) V[i] <- V[i-1] * a
    V
}

L <- as.list(seq(0,1,length=11)) # list for use below...

with gradient (L) {              # tracks gradient w.r.t. 11 elements of L
    p <- 0
    for (i along L) 
        p <- p + i*L[[i]]
    p^2 + p^3 + p^4 + p^5        # every operation computes derivatives
}                                #   w.r.t. all 11 elements of L

with gradient (L) {              # compute same result more efficiently...
    p <- 0
    for (i along L) 
        p <- p + i*L[[i]]
    back gradient (p)            # operations in the expresson below 
        p^2 + p^3 + p^4 + p^5    #   compute derivative w.r.t. p only, then
}                                #   chain rule gives gradient w.r.t. L

V <- seq(0,1,length=11)          # same as above, but using a real vector

with gradient (V) {              # the gradient will be a 1 x 11 matrix
    p <- 0
    for (i along V) 
        p <- p + i*V[i]
    back gradient (p)
        p^2 + p^3 + p^4 + p^5
}

track gradient (a=c(7,5))
    gradient_of (c(a,a^2,a^3,a[1]*a[2])) # produces a 7 x 2 Jacobian matrix

# Make table of (a+j)^i and derivatives w.r.t. a, for a equal to 1.1.

tbl <- function (a) {            # a function to compute such a table
    T <- matrix (nrow=4, ncol=5)
    for (i, j along T)
        T[i,j] <- if (i == 1) a+j else T[i-1,j] * (a+j)
    T
}

T <- with gradient (a=1.1) tbl(a)  # call tbl asking for the gradient

T                                # note the gradient is a simple vector,
array(attr(T,"gradient"),dim(T)) #   but dimensions can be added if needed

# Gradient tracking for numeric data in a data frame.

a <- c(3,1,5)
b <- c(9,2,7)

r <- with gradient (a,b) data.frame(a^2,a*b,b^2)

print(r)                         # no gradient seen, since print.data.frame
                                 #   doesn't print attributes...

attr(r,"gradient")               # ...but it's there

# Defining and using a function to combine a value and a gradient.

val_and_grad <- function (v)     # the call of gradient_of here will be
    list (v, gradient_of(v))     #   valid when val_and_grad is called from
                                 #   a place where gradient_of is valid...

track gradient (a = 7) { b <- a^2; val_and_grad(10*b) }  # ... such as here

}

\keyword{programming}
