source("time.r")
 
# MULTIPLY MATRICES.  
#
# These functions are just for testing and illustration.  The built-in "%*%"
# operator will be much faster.
#
# Times may be compared with those from test-matprod-pqR.r, for amusement.

matmult1 <- test.cmp (function (A, B)
{ 
  stopifnot (is.matrix(A) && is.matrix(B) && ncol(A)==nrow(B))

  n <- nrow(A)
  m <- ncol(B)
  k <- ncol(A)

  R <- matrix (numeric(0), n, m)

  for (i in seq_len(n))
  { for (j in seq_len(m))
    { s <- 0
      for (h in seq_len(k)) s <- s + A[i,h] * B[h,j] 
      R[i,j] <- s
    }
  }
 
  R
})

matmult2 <- test.cmp (function (A, B)
{ 
  stopifnot (is.matrix(A) && is.matrix(B) && ncol(A)==nrow(B))

  n <- nrow(A)
  m <- ncol(B)
  k <- ncol(A)

  R <- matrix (numeric(0), n, m)

  for (i in seq_len(n))
  { for (j in seq_len(m))
    { R[i,j] <- sum(A[i,]*B[,j])
    }
  }
 
  R
})

chk <- function (l, t)
{ for (i in 1:length(l))
  { if (!identical(l[[i]],t))
    { cat("NOT IDENTICAL!",i,"\n")
      break;
    }
  }
}

set.seed(1)

gen <- function (n) as.double(rbinom(n,1000,0.5))

A0 <- matrix(gen(1*40000),1,40000)
A1 <- matrix(gen(2*20000),2,20000)
A2 <- matrix(gen(200*200),200,200)
A3 <- matrix(gen(133*301),133,301)

B0 <- matrix(gen(40000*1),40000,1)
B1 <- matrix(gen(2*20000),20000,2)
B2 <- matrix(gen(200*200),200,200)
B3 <- matrix(gen(301*133),301,133)

R0 <- A0 %*% B0
R1 <- A1 %*% B1
R2 <- A2 %*% B2
R3 <- A3 %*% B3

test.name <- "matmult.1"

r <- 50
V <- vector("list",r)

sys.time(for (i in 1:r) V[[i]] <- matmult1(A0,B0))
chk(V,R0)

sys.time(for (i in 1:r) V[[i]] <- matmult1(A1,B1))
chk(V,R1)

sys.time(for (i in 1:(r/50)) V[[i]] <- matmult1(A2,B2))
chk(V[1:(r/50)],R2)

sys.time(for (i in 1:(r/50)) V[[i]] <- matmult1(A3,B3))
chk(V[1:(r/50)],R3)

test.name <- "matmult.2"

r <- 2000
V <- vector("list",r)

sys.time(for (i in 1:r) V[[i]] <- matmult2(A0,B0))
chk(V,R0)

sys.time(for (i in 1:r) V[[i]] <- matmult2(A1,B1))
chk(V,R1)

sys.time(for (i in 1:(r/50)) V[[i]] <- matmult2(A2,B2))
chk(V[1:(r/50)],R2)

sys.time(for (i in 1:(r/50)) V[[i]] <- matmult2(A3,B3))
chk(V[1:(r/50)],R3)
