% File src/library/base/man/Listops.Rd
% pqR : A pretty quick version of R
% Copyright (C) 2020 by Radford M. Neal
% Distributed under GPL 2 or later

\name{Listops} 
\alias{Listops} 
\title{Arithmetic and Mathematical Operations on Lists}

\description{
  Describes how arithmetic operators and some mathematical functions can
  be applied to lists (or lists of lists, etc.), with the operation or 
  function being done recursively on list elements.
}

\details{

In pqR, all arithmetic operators and some numerical functions of one
or two arguments can be applied to lists, rather than just to numeric
vectors, with the result being a list of the results of applying the
operator or function to elements of the list or lists.  (Here, "list"
does not include pairlists or expression lists.)

List operations are currently supported for the following operators
and functions:
\preformatted{  + (may be unary), - (may be unary), *, /, ^, \%\%, \%/\% 
  
  abs, sqrt, exp, expm1, log1p, log2, log10, log (1 or 2 arguments),
  cos, sin, tan, acos, asin, atan, atan2,
  cosh, sinh, tanh, acosh, asinh, atanh,
  gamma, lgamma, digamma, trigamma, factorial, lfactorial, sign,
  floor, ceiling, round (1 or 2 arguments), signif (1 or 2 arguments)
}

For unary operators and functions of one argument, the operation is
applied to each element of the list, with the result being a list of
the same length, containing the results of these operations. The
names and other attributes of the result are the same as those of
the operand. If some list elements are themselves lists, the operation
is applied recursively to these elements, producing a list of lists
as the result.

For binary operators and functions of two arguments, one or both
arguments may be lists.  If one operand (or argument) is a list and
the other is numeric, the numeric operand must be scalar (i.e., of
length one).  The operation is performed recursively in the same way
as for unary operations, with the scalar operand applied to each
element of the list (recursively, for lists of lists).  The attributes
of the resulting list will be the same as the attributes of the list
operand.  (Attributes of the scalar may affect the attributes of the
elements of the result.)

If both operands (or arguments) are lists, the lists must be the same
length and have identical names (if either has any names).  The
operation is applied to corresponding elements of the two lists, with
the result being a list of these results.  The attributes of the
result are those of the first operand (or argument), plus those
attributes of the second operand that do not conflict with attributes
of the first.  When corresponding elements are themselves lists, the
operation is applied recursively.  If one of the corresponding
elements is a list and the other is numeric, the operation is also
applied recursively, as described above (in particular, note that the
numeric element must be scalar in this case).

List operations are particularly useful when automatic differentiation
is done with respect to a list, so the resulting gradient is also a list.
See the example below.
}

\seealso{
  \code{\link{relist}} in the \code{utils} package for an alternative
  (older) approach.

  \code{Gradient} for information on automatic differentiation.
}

\examples{

list(a=10,b=1:10) * list(a=2,b=10:1)# Corresponding elements are multiplied
list(a=10,b=1:10) * 2               # Scalar 2 multiplies each list element
list(a=10,b=1:10) * list(a=3,b=4)   # 4 is recycled for 1:10, in usual way

list(a=10,b=list(p=20,q=30)) * 2    # Scalar 2 multiplies element a of list
                                    #   and elements of element b of list

sqrt (list (2:4, 10:12))            # Takes square root of all numbers in
                                    #   this list of vectors
abs (list (x=list(a=-1,b=2), y=-3)) # Value is a list with same structure
                                    #   as argument, with absolute values

round(list(list(3.12,X=4.66),8.82), # Rounds elements of list, recursively,
      list(1,0))                    #   to number of digits given by list;
                                    #   1 applies to all of the first list

with gradient (x=7)                 # Gradients are correctly handled (for
    list(x,x^2) * list(x,x)         #   operations that can compute them)


# Gradient descent training of a simple neural network, using automatic
# differentiation and arithmetic on lists of parameter vectors/matrices.

net <- function (params, X)   # Compute the outputs of a network with 
    params$bias_out +         #   parameters 'params' on the rows of X
      tanh (params$bias_hid + 
            X \%*\% params$in_hid) \%*\% params$hid_out

train <- function (params, X, y, r, n)  # Train for n iterations with
{                                       #   learning rate r on data X, y
    for (i in 1..n) {
        sq_err <- with gradient (params) sum ((y - net(params,X))^2)
        cat("Iteration",i,": squared error is",sq_err,"\n")
        params <- 
          params - r * attr(sq_err,"gradient")  # DOES ARITHMETIC ON LISTS
    }

    params
}

y <- iris[,"Petal.Length"]       # Try to predict petal length in iris data
X <- as.matrix(iris[,c("Sepal.Length","Sepal.Width")])
                                 # ... from sepal length and width
N_hid <- 5                       # ... using a net with 5 hidden units

initial_params <-                # Set initial parameter values randomly
  list (in_hid = matrix (rnorm (2*N_hid, 0, 0.1), 2, N_hid),
        bias_hid = rnorm (N_hid, 0, 0.1),
        hid_out = rnorm (N_hid, 0, 0.1),
        bias_out = mean(y))      # ... except initial output bias is mean y

trained_params <- train (initial_params, X, y, 0.01, 10)
}

\keyword{programming}
