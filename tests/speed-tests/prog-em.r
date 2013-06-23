source("time.r")
 
# AN EM ALGORITHM FOR FINDING THE MLE FOR A CENSORED POISSON MODEL.
#
# The data consists of n observed counts, whose mean is m, plus c counts
# that are observed to be less than 2 (ie, 0 or 1), but whose exact value
# is not known.  The counts are assumed to be Poisson distributed with 
# unknown mean, lambda.  
#
# The function below finds the maximum likelihood estimate for lambda given
# the data, using the EM algorithm started from the specified guess at lambda
# (default being the mean count with censored counts set to 1), run for the
# specified number of iterations (default 20).  A check is made that the
# log likeihood never decreases (by more than 1e-6).

EM.censored.poisson <- 
  test.cmp (function (n, m, c, lambda0=(n*m+c)/(n+c), iterations=20)
{
  # Log likelihood function.

  log.likelihood <- function (lambda)
  {
    n*m*log(lambda) - (n+c)*lambda + c*log(1+lambda)
  }

  # Set initial guess, and compute its log likelihood.

  lambda <- lambda0

  old.ll <- log.likelihood(lambda)

  # Do EM iterations.

  for (i in 1:iterations)
  {
    # The E step: Figure out the distribution of the unobserved data.  For 
    # this model, we need the probability that an unobserved count that is 
    # either 0 or 1 is actually equal to 1, which is p1 below.

    p1 <- lambda / (1+lambda)
    
    # The M step: Find the lambda that maximizes the expected log likelihood
    # with unobserved data filled in according to the distribution found in
    # the E step.

    lambda <- (n*m + c*p1) / (n+c)

    # Check that the log likelihood didn't decrease.

    new.ll <- log.likelihood(lambda)

    if (new.ll-old.ll < -1e-6)
    { stop("Log likelihood decreased!")
    }

    old.ll <- new.ll
  }

  # Return the value for lambda from the final EM iteration.

  lambda
})


# Run the EM algorithm (for far more iterations than actually needed). 

test.name <- "em.run"

f <- test.cmp (function () 
{ for (i in 1:3) lambda <- EM.censored.poisson(5,6.1,20,iterations=100000)
  lambda
})
sys.time(lambda <- f())
print(lambda)

g <- test.cmp (function () 
{ for (i in 1:30000) lambda <- EM.censored.poisson(5,6.1,20,iterations=10)
  lambda
})
sys.time(lambda <- g())
print(lambda)

test.name <- "em.compile"

if (!exists("cmpfun"))  # define cmpfun to be a meaningless stub
  cmpfun <- function (f) { a <- 0; for (i in 1:100000) a <- a+1; f }

h <- test.cmp (function (n) 
       { for (i in 1:n) f <- cmpfun(EM.censored.poisson); f })

sys.time(h(100))
