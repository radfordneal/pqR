source("time.r")
 
# GAUSSIAN BASIS FUNCTIONS WITH CROSS VALIDATION.

# These functions give an example a Gaussian basis function model for
# a real-valued target given a single real-valued input in the
# interval (0,1).  Includes selection of the basis function width and
# penalty magnitude by cross validation.


# CREATE A MATRIX OF BASIS FUNCTION VALUES.  Takes the vector of input
# values, xv, and width of a basis function, s, as arguments.  Returns the
# matrix of basis function values, in which the first column is all 1s,
# and subsequent columns are for basis functions centred at -2s, -s, 0,
# s, ..., 1, 1+s, 1+2s (assuming that 1/s is an integer).

Phi <- test.cmp (function (xv, s)
{
  m <- seq (-2*s, 1+2*s, by=s)
  Phi <- matrix (1, length(xv), length(m)+1)
  for (j in 1:length(m))
  { Phi[,1+j] <- exp(-0.5*(xv-m[j])^2/s^2)
  }
  Phi
})


# FIND A PENALIZED LEAST SQUARES ESTIMATE.  Takes a matrix of basis function
# values, Phi, (with first column all 1s), the vector of target values, tv, 
# and the magnitude of the penalty, lambda, as arguments.  A penalty magnitude 
# of zero (the default) will give the unpenalized least squares estimate.  
# Returns the vector of estimates for the regression coefficients.

penalized.least.squares <- test.cmp (function (Phi, tv, lambda=0)
{
  S <- diag(ncol(Phi)) 
  S[1,1] <- 0
  as.vector (solve (lambda*S + t(Phi)%*%Phi) %*% t(Phi) %*% tv)
})


# COMPUTE PREDICTIONS FOR TEST CASES.  Takes a matrix of basis function 
# values for test cases, Phi, and a vector of regression coefficients, w, 
# as arguments.  Returns a vector of predicted values for the targets in 
# these test cases.

predictions <- test.cmp (function (Phi, w)
{
  as.vector (Phi %*% w)
})


# FIND VALIDATION ERROR.  Computes the validation squared error using specified
# values of s and lambda, with a specified subset of training cases reserved
# for a validation set.  Arguments are the input and target vectors for the
# training cases, xv and tv, the values of s and lambda, and a vector of
# indexes for the validation cases, vix.

validation.error <- test.cmp (function (xv, tv, s, lambda, vix)
{ 
  w <- penalized.least.squares (Phi(xv[-vix],s), tv[-vix], lambda)
  p <- predictions (Phi(xv[vix],s), w)
  mean ((tv[vix]-p)^2)
})


# FIND ARRAY OF VALIDATION ERRORS.  Computes the validation error for
# all combinations of s and lambda in vectors try.s and try.lambda, using
# a specified validation set, given by vix, with inputs and targets given
# by xv and tv.  Returns an array of average squared errors on validation 
# cases, with row and column names indicating the values of s and lambda used.

val.array <- test.cmp (function (xv, tv, try.s, try.lambda, vix)
{
  V <- matrix (NA, length(try.s), length(try.lambda))
  rownames(V) <- paste("s=",try.s,sep="")
  colnames(V) <- paste("lambda=",try.lambda,sep="")

  for (i in 1:length(try.s))
  { for (j in 1:length(try.lambda))
    { V[i,j] <- validation.error (xv, tv, try.s[i], try.lambda[j], vix)
    }
  }

  V
})


# FIND ARRAY OF S-FOLD CROSS-VALIDATION ERRORS.  Computes the S-fold
# cross-validation error for all combinations of s and lambda in vectors
# try.s and try.lambda, with inputs and targets given by xv and tv.  
# Returns an array of square roots of average squared error on validation 
# cases, with row and column names indicating the values of s and lambda 
# used.  The training cases are assumed to be in random order, so that 
# the S validation sets can be taken consecutively.

cross.val.array <- test.cmp (function (xv, tv, try.s, try.lambda, S)
{
  n <- length(tv)
  for (r in 1:S)
  { vix <- floor(1 + n*(r-1)/S) : floor(n*r/S)
    V <- val.array (xv, tv, try.s, try.lambda, vix)
    V.sum <- if (r==1) V else V.sum + V
  }

  V.sum / S
})


# TRY OUT THE ABOVE FUNCTIONS.

test.name <- "cv-basisfun"

set.seed(2)

xt <- runif(50)
tt <- sin(1+xt^2) + rnorm(50,0,0.03)

try.lambda <- c(0.001,0.01,0.1,1,10)
try.s <- c(0.02,0.1,0.5,2.5)

f <- test.cmp (function (n) 
{ 
  for (i in 1:n) r <- round(sqrt(cross.val.array(xt,tt,try.s,try.lambda,5)),4)
  r
})

sys.time(r <- f(10))

cat("\nCross-validation array:\n\n")
print(r)
