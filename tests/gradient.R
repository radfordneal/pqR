# Test gradient computations.
#
# Added for pqR, 2018 Radford M. Neal.

x <- 0.32739

stopifnot <- print

stopifnot (identical (track_gradient (x) gradient_of (sqrt(x)),
                      eval (D (quote (sqrt(x)), "x")) ))

stopifnot (identical (track_gradient (x) gradient_of (exp(x)),
                      eval (D (quote (exp(x)), "x")) ))

#stopifnot (identical (track_gradient (x) gradient_of (expm1(x)),
#                      eval (D (quote (expm1(x)), "x")) ))

#stopifnot (identical (track_gradient (x) gradient_of (log1p(x)),
#                      eval (D (quote (log1p(x)), "x")) ))

stopifnot (identical (track_gradient (x) gradient_of (log(x)),
                      eval (D (quote (log(x)), "x")) ))

stopifnot (identical (track_gradient (x) gradient_of (cos(x)),
                      eval (D (quote (cos(x)), "x")) ))

stopifnot (identical (track_gradient (x) gradient_of (sin(x)),
                      eval (D (quote (sin(x)), "x")) ))

stopifnot (identical (track_gradient (x) gradient_of (tan(x)),
                      eval (D (quote (tan(x)), "x")) ))

stopifnot (identical (track_gradient (x) gradient_of (acos(x)),
                      eval (D (quote (acos(x)), "x")) ))

stopifnot (identical (track_gradient (x) gradient_of (asin(x)),
                      eval (D (quote (asin(x)), "x")) ))

stopifnot (identical (track_gradient (x) gradient_of (cosh(x)),
                      eval (D (quote (cosh(x)), "x")) ))

stopifnot (identical (track_gradient (x) gradient_of (sinh(x)),
                      eval (D (quote (sinh(x)), "x")) ))

stopifnot (identical (track_gradient (x) gradient_of (tanh(x)),
                      eval (D (quote (tanh(x)), "x")) ))

#stopifnot (identical (track_gradient (x) gradient_of (acosh(x)),
#                      eval (D (quote (acosh(x)), "x")) ))

#stopifnot (identical (track_gradient (x) gradient_of (asinh(x)),
#                      eval (D (quote (asinh (x)), "x")) ))

#stopifnot (identical (track_gradient (x) gradient_of (atanh(x)),
#                      eval (D (quote (atanh(x)), "x")) ))

stopifnot (identical (track_gradient (x) gradient_of (lgamma(x)),
                      eval (D (quote (lgamma(x)), "x")) ))

stopifnot (identical (track_gradient (x) gradient_of (gamma(x)),
                      eval (D (quote (gamma(x)), "x")) ))

stopifnot (identical (track_gradient (x) gradient_of (digamma(x)),
                      eval (D (quote (digamma(x)), "x")) ))

stopifnot (identical (track_gradient (x) gradient_of (trigamma(x)),
                      eval (D (quote (trigamma (x)), "x")) ))
