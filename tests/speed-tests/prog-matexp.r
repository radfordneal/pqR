source("time.r")
 
# COMPUTE MATRIX EXPONENTIAL.

matexp <- test.cmp (function (A, last_pow)
{
  S <- diag(nrow(A))
  if (last_pow > 0)
  { T <- A
    if (last_pow > 1)
    { for (pow in 2:last_pow)
      { S <- S + T
        T <- (A %*% T) * (1/pow)
      }
    }
    S <- S + T
  }
  S
})

test.name <- "matexp"

A <- matrix (seq(0,1,length=7^2), 7, 7)

s <- 0; 
sys.time (for (i in 1:20000) { R <- matexp(A,10); if (R[length(R)]<0) s <- 1; })
print(s)
print(R)

s <- 0; 
sys.time (for (i in 1:20000) { R <- matexp(A,20); if (R[length(R)]<0) s <- 1; })
print(s)
print(R)

A <- matrix (seq(0,0.5,length=25^2), 25, 25)

s <- 0; 
sys.time (for (i in 1:2000) { R <- matexp(A,30); if (R[length(R)]<0) s <- 1; })
print(s)
print(round(R,2))

A <- matrix (seq(0,0.2,length=100^2), 100, 100)

s <- 0; 
sys.time (for (i in 1:100) { R <- matexp(A,20); if (R[length(R)]<0) s <- 1; })
print(s)
print(sum(R))
