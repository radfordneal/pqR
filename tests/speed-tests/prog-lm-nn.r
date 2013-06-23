source("time.r")
 
# Implementation of a hybrid linear model / nearest neighbor method.


# FIT TO A TRAINING SET AND MAKE PREDICTIONS FOR A SET OF TEST CASES.
#
# Arguments:
#
#     x.train    Matrix of inputs in training cases (n rows, p columns)
#     y.train    Vector of responses in training cases (n long)
#     x.test     Matrix of inputs in test cases
#     K          Number of nearest neighbors to look at (default n, may be >n)
#     lambda     Size of penalty for linear model fit (default 0, may be Inf)
#
# Value:  A vector of predictions for the responses in the test cases.

lm.nn <- function (x.train, y.train, x.test, K=length(y.train), lambda=0)
{
  # Reduce K to the number of training case if it's bigger.

  if (K>length(y.train)) K <- length(y.train)

  # Add columns of 1s to the matrices of inputs.
  
  x.train <- cbind (1, as.matrix(x.train))
  x.test <- cbind (1, as.matrix(x.test))

  # Fit linear model by penalized least squares.

  if (is.finite(lambda))
  { Istar <- diag(ncol(x.train)) 
    Istar[1,1] <- 0
    S <- lambda*Istar + t(x.train)%*%x.train
    b <- as.vector (solve(S) %*% t(x.train) %*% y.train)
  }
  else 
  { # With an infinite penalty, all coefficients are 0 except that the
    # intercept is the sample mean of the responses.

    b <- c (mean(y.train), rep(0,ncol(x.train)-1))
  }

  # Find the residuals from the linear model fit.

  r <- y.train - x.train %*% b

  # Make predictions for test cases.

  yp <- numeric(nrow(x.test))

  for (i in 1:nrow(x.test))
  {
    x <- x.test[i,]
    
    # Find squared distances from this test case to all training cases.

    dsq <- colSums ((t(x.train) - x)^2)

    # Find indexes of K closest training cases.

    nn <- order(dsq)[1:K]

    # Find the prediction based on the linear model.

    lmp <- x %*% b

    # Final prediction is the linear model prediction plus the average residual
    # at the nearest neighbors.

    yp[i] <- lmp + mean(r[nn])
  }

  yp
}


# FIND ARRAY OF VALIDATION ERRORS FOR VARIOUS K AND LAMBDA.
#
# Arguments:
#
#     x.train    Matrix of inputs in training cases (n rows, p columns)
#     y.train    Vector of responses in training cases (n long)
#     val.ix     Indexes of validation cases
#     try.K      Values for K to try (may include Inf)
#     try.lambda Values for lambda to try (may include 0 or Inf)
#
# Value:  A matrix of average validation squared errors, with rows and
#         columns labelled with values for K and lambda.

lm.nn.val <- function (x.train, y.train, val.ix, try.K, try.lambda)
{
  x.train <- as.matrix(x.train)

  # Create matrix that will hold the results.

  V <- matrix (NA, length(try.K), length(try.lambda))
  rownames(V) <- paste("K=",try.K,sep="")
  colnames(V) <- paste("lambda=",try.lambda,sep="")

  # Find validation error for each combination of K and lambda.

  for (i in 1:length(try.K))
  { for (j in 1:length(try.lambda))
    { 
      yp <- lm.nn (x.train[-val.ix,], y.train[-val.ix], 
                   x.train[val.ix,,drop=FALSE], # drop=F needed if only 1 case
                   K = try.K[i], lambda = try.lambda[j])

      V[i,j] <- mean ((y.train[val.ix] - yp)^2)
    }
  }

  V
}


# FIND BEST K AND LAMBDA BY CROSS VALIDATION (AND PRINT ARRAY OF CV ERRORS).
#
# Arguments:
#
#     x.train    Matrix of inputs in training cases (n rows, p columns)
#     y.train    Vector of responses in training cases (n long)
#     S          Number of divisions of training data
#     try.K      Values for K to try (may include Inf)
#     try.lambda Values for lambda to try (may include 0 or Inf)
#
# Value:  A list with elements K and lambda giving the best values found.

# Also stores the array of average validation squared errors in sq_errs.

lm.nn.cross.val <- function (x.train, y.train, S, try.K, try.lambda)
{
  n <- length(y.train)

  # Find points that divide the S subsets (first 0, then end of each subset).

  m <- round((0:S)*n/S)

  # Find the average of validation errors for all divisions.

  for (h in 1:S)
  { V <- lm.nn.val (x.train, y.train, (m[h]+1) : m[h+1], try.K, try.lambda)
    Vsum <- if (h==1) V else Vsum+V
  }

  V <- Vsum / S
  sq_errs <<- V

  # Find the combination of K and lambda with smallest average validation error.

  best.err <- Inf

  for (i in 1:length(try.K))
  { for (j in 1:length(try.lambda))
    { if (V[i,j] < best.err)
      { best.K <- try.K[i]
        best.lambda <- try.lambda[j]
        best.err <- V[i,j]
      }
    }
  }

  # Return the best K and lambda as a list.

  list (K = best.K, lambda = best.lambda)
}

S <- 25
try.K <- c(1,2,3,4,5,8,11,16,21,32,Inf)
try.lambda <- c(0,0.003,0.1,0.3,1,3,10,30,100,300,1000,3000,Inf)

trn <- matrix (c (
0.880502, 0.184882,
1.028223, 0.702374,
0.972312, 0.573326,
0.909021, 0.168052,
0.95079, 0.943839,
0.887451, 0.943475,
0.839248, 0.129159,
1.043134, 0.833449,
0.973279, 0.468019,
0.961081, 0.549984,
0.949531, 0.552674,
0.91663, 0.238895,
0.990195, 0.760513,
0.896209, 0.18082,
0.938033, 0.405282,
0.990347, 0.853548,
0.925798, 0.976398,
0.842838, 0.225825,
0.983122, 0.444809,
0.815483, 0.074979,
1.008201, 0.661899,
0.899325, 0.38755,
0.958679, 0.836889,
0.815854, 0.150501,
0.965061, 0.347272,
0.955632, 0.488773,
0.869407, 0.149247,
0.932092, 0.357063,
0.979991, 0.962644,
0.845821, 0.132372,
0.865177, 0.010415,
0.837018, 0.164642,
1.056621, 0.810192,
1.027535, 0.868861,
0.958395, 0.514282,
1.006573, 0.627196,
0.966963, 0.844429,
0.926343, 0.284871,
0.951499, 0.667226,
0.816952, 0.15047,
0.966922, 0.981728,
0.96647, 0.297011,
0.890256, 0.115084,
0.888619, 0.163201,
0.940952, 0.944042,
1.032706, 0.794864,
0.950564, 0.974688,
0.902847, 0.349088,
0.947514, 0.50197,
1.032153, 0.810397),
ncol=2, byrow=TRUE)

x.train <- as.matrix(trn[,-1])
y.train <- as.vector(trn[,1])

test.name <- "lm-nn"

sys.time (result <- lm.nn.cross.val(x.train, y.train, S, try.K, try.lambda))

cat("\n")
print(round(sqrt(sq_errs),5))

cat("\nCross validation assessment:\n\n")
print(result)
