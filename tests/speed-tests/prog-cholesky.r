source("time.r")
 
# FIND CHOLESKY DECOMPOSITION.  Given a square matrix A that is symmetric
# and positive definite, this function finds an upper-triangular matrix U
# of the same dimensions for which t(U) %*% U = A.
#
# This function is just for illustration.  The built-in function "chol" 
# will do the same thing faster.

cholesky <- test.cmp (function (A)
{ 
  if (!is.matrix(A) || nrow(A)!=ncol(A))
  { stop("The argument for cholesky must be a square matrix")
  }

  p <- nrow(A)
  U <- matrix(0,p,p)

  for (i in 1:p)
  { 
    if (i==1)
    { U[i,i] <- sqrt (A[i,i])
    }
    else
    { U[i,i] <- sqrt (A[i,i] - sum(U[1:(i-1),i]^2))
    }

    if (i<p)
    { for (j in (i+1):p)
      { if (i==1)
        { U[i,j] <- A[i,j] / U[i,i]
        }
        else
        { U[i,j] <- (A[i,j] - sum(U[1:(i-1),i]*U[1:(i-1),j])) / U[i,i]
        }
      }
    }
  }

  U

})

test.name <- "cholesky"

set.seed(1)

A <- matrix(rnorm(40000),200,200)
M <- t(A) %*% A
U <- vector("list",10)

sys.time({ for (i in 1:10) U[[i]] <- cholesky(M) })

V <- vector("list",1000)
sys.time({ for (i in 1:1000) V[[i]] <- chol(M) })

print(sum((U[[1]]-V[[1]])^2))
