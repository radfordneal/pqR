# Test matrix multiplication, with %*%, crossprod, and tcrossprod, by
# BLAS and 'matprod' routines.
#
# Added for pqR, 2013, Radford M. Neal.

# Check matrix multiplication with various sizes of matrices, setting
# matrix elements to random values that are integer multiples of 1/8 so
# that floating point arithmetic will be exact (but accidental conversions
# to integer will be detected).  The crossprod and tcrossprod routines
# are also checked when give one argument (producing a symmetric result).
#
# Returns the last (largest) result matrix.

check_matprod <- function ()
{  
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

  # Set seed to get consistent results.

  set.seed(1)

  # Try various sizes.

  for (n in 0:11)
  { for (m in 0:11)
    { for (k in 0:11)
      { A <- matrix (rgeom(n*k,0.1)/8, n, k)
        B <- matrix (rgeom(k*m,0.1)/8, k, m)
        C <- matmult(A,B)
        At <- t(A)
        Bt <- t(B)
        stopifnot(identical(A%*%B,C))
        stopifnot(identical(crossprod(At,B),C))
        stopifnot(identical(tcrossprod(A,Bt),C))
        if (n==m) 
        { D <- matmult(At,A)
          stopifnot(identical(crossprod(A),D))
          stopifnot(identical(tcrossprod(At),D))
        }
      }
    }
  }

  C
}

# Check matrix products using BLAS and using 'matprod' routines.

sv <- getOption("mat_mult_with_BLAS")

options(mat_mult_with_BLAS=F)
print(check_matprod())

options(mat_mult_with_BLAS=T)
print(check_matprod())

options(mat_mult_with_BLAS=sv)
