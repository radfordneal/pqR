# Test matrix multiplication, with %*%, crossprod, and tcrossprod, by
# BLAS and 'matprod' routines.
#
# Added for pqR, 2013, 2018 Radford M. Neal.

# Check matrix multiplication with various sizes of matrices, setting
# matrix elements to random values that are integer multiples of 1/8 so
# that floating point arithmetic will be exact (but accidental conversions
# to integer will be detected).  The crossprod and tcrossprod routines
# are also checked when give one argument (producing a symmetric result).
#
# Returns the last (largest) result matrix.

check_matprod <- function (print=TRUE)
{ 
  if (print) 
  { cat("\n")
    print(options()[c("mat_mult_with_BLAS","helpers_disable")])
  }

  # Matrix multiply the hard way, to check results.

  matmult <- function (A,B)
  { n <- nrow(A)
    m <- ncol(B)
    k <- ncol(A)
    stopifnot(nrow(B)==k)
    C <- matrix(0,n,m)
    for (i in seq_len(n))
    { for (j in seq_len(m))
      { C[i,j] <- sum (A[i,] * B[,j])
      }
    }
    C
  }

  # Do checks with given matrix sizes.

  check <- function (n, m, k)
  { A <- matrix (rgeom(n*k,0.1)/8, n, k)
    B <- matrix (rgeom(k*m,0.1)/8, k, m)
    C <- matmult(A,B)
    C1 <- matmult(A,B+1)
    C2 <- matmult(A,B)+2
    At <- t(A)
    Bt <- t(B)
    stopifnot(identical(A%*%B,C))
    stopifnot(identical(crossprod(At,B),C))
    stopifnot(identical(tcrossprod(A,Bt),C))
    stopifnot(identical(A%*%(B+1),C1))
    stopifnot(identical(crossprod(At,B+1),C1))
    stopifnot(identical(tcrossprod(A,Bt+1),C1))
    stopifnot(identical(A%*%B+2,C2))
    stopifnot(identical(crossprod(At,B)+2,C2))
    stopifnot(identical(tcrossprod(A,Bt)+2,C2))
    if (n==m) 
    { D <- matmult(At,A)
      stopifnot(identical(crossprod(A),D))
      stopifnot(identical(tcrossprod(At),D))
    }
    C
  }

  # Set seed to get consistent results.

  set.seed(1)

  # Try various sizes systematically.

  cat("---- (0..11) (0..11) (0..11)\n")

  for (n in 0..11)
  { for (m in 0..11)
    { for (k in 0..11)
      { C <- check(n,m,k)
      }
    }
  }

  if (print)
  { cat("\n")
    print(C)
  }

  cat("---- n (1 2 3 4 5) m\n")

  for (n in c(2,3,4,5,8,100,1000,2000,4000))
  { for (m in c(2,3,4,5,8,100,1000,2000,4000))
    { if (n*m <= 1000000)
      { if (print) print(c(n,m))
        for (k in c(1,2,3,4,5)) check(n,m,k)
      }
    }
  }

  cat("---- (1 2 3 4 5) k m\n")

  for (k in c(2,3,4,5,8,100,1000,2000))
  { for (m in c(2,3,4,5,8,100,1000,2000))
    { if (k*m <= 1000000)
      { if (print) print(c(k,m))
        for (n in c(1,2,3,4,5)) check(n,m,k)
      }
    }
  }

  cat("---- random\n")

  # Try various sizes randomly.  Can increase loop count for more testing
  # by setting the environment variable R_MATPROD_TEST_COUNT (minimum 200).

  count <- as.integer(Sys.getenv("R_MATPROD_TEST_COUNT"))
  if (is.na(count) || count < 200) count <- 200

  for (i in 1..count)
  {  n <- if (runif(1)<0.6) 15*rpois(1,10) + rpois(1,10) 
          else if (runif(1)>0.93) 50*rpois(1,12) + rpois(1,30) 
          else rpois(1,5)
     m <- if (runif(1)<0.6) 15*rpois(1,10) + rpois(1,10) 
          else if (runif(1)>0.93) 50*rpois(1,12) + rpois(1,30) 
          else rpois(1,5)
     k <- if (runif(1)<0.6) 15*rpois(1,10) + rpois(1,10) 
          else if (runif(1)>0.93) 50*rpois(1,12) + rpois(1,30) 
          else rpois(1,5)
     if (n > 1200) n <- 1200
     if (m > 1200) m <- 1200
     if (k > 1200) k <- 1200
     # Print n,m,k, but only for the first 200, so increasing the loop
     # repetitions won't invalidate saved output.
     if (print && i <= 200) print(c(n,m,k))
     check(n,m,k)
  }
}

# Check matrix products using BLAS and using 'matprod' routines, with
# or without helper threads enabled.

sv <- options()[c("mat_mult_with_BLAS","helpers_disable")]

options(mat_mult_with_BLAS=FALSE,helpers_disable=FALSE)
cat("\nNot BLAS, Helpers not disabled\n\n")
check_matprod()

options(mat_mult_with_BLAS=FALSE,helpers_disable=TRUE)
cat("\nNot BLAS, Helpers disabled\n\n")
check_matprod()

if (identical(Sys.getenv("R_MATPROD_TEST_BLAS"),"TRUE")) {

    cat("\nBLAS, Helpers not disabled\n\n")
    options(mat_mult_with_BLAS=TRUE,helpers_disable=FALSE)
    check_matprod(print=FALSE)

    cat("\nBLAS, Helpers disabled\n\n")
    options(mat_mult_with_BLAS=TRUE,helpers_disable=TRUE)
    check_matprod(print=FALSE)
}

options(sv)
