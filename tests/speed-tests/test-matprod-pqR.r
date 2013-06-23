source("time.r")

# Tests matrix multiplication in pqR with BLAS versus 'matprod' routines.
# The mat_mult_with_BLAS option will be ignored without error if this
# feature is not present.
#
# Times may be compared with those from prog-matmult.r, for amusement.

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

test.name <- "matprod-pqR-BLAS"

options(mat_mult_with_BLAS=TRUE)

r <- 20000
V <- vector("list",r)

sys.time(for (i in 1:r) V[[i]] <- A0 %*% B0)
chk(V,R0)

sys.time(for (i in 1:r) V[[i]] <- A1 %*% B1)
chk(V,R1)

sys.time(for (i in 1:(r/50)) V[[i]] <- A2 %*% B2)
chk(V[1:(r/50)],R2)

sys.time(for (i in 1:(r/50)) V[[i]] <- A3 %*% B3)
chk(V[1:(r/50)],R3)

test.name <- "matprod-pqR-no-BLAS"

options(mat_mult_with_BLAS=FALSE)

r <- 20000
V <- vector("list",r)

sys.time(for (i in 1:r) V[[i]] <- A0 %*% B0)
chk(V,R0)

sys.time(for (i in 1:r) V[[i]] <- A1 %*% B1)
chk(V,R1)

sys.time(for (i in 1:(r/50)) V[[i]] <- A2 %*% B2)
chk(V[1:(r/50)],R2)

sys.time(for (i in 1:(r/50)) V[[i]] <- A3 %*% B3)
chk(V[1:(r/50)],R3)
