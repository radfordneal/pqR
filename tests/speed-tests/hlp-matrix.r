source("time.r")
 
# Assess degree of parallelism using helper threads for matrix operations.

test.name <- "hlp-matrix.mult-small"

g <- test.cmp (function (n) 
{ s <- 0
  for (i in 1:n) s <- s + v %*% (M1 %*% (M2 %*% (M1 %*% t(M2))))
  s
})

N <- 100

M1 <- matrix(c(3.1,3.2),N,N)
M2 <- matrix(c(4.1,4.2),N,N)

v <- seq(3,4,length=N)

# Comparisons to 0 below should force completion of all computations. 

options(helpers_disable=TRUE)
sys.time ( { sd <- g(500); as.integer(sd==0) } )

options(helpers_disable=FALSE)
sys.time ( { se <- g(500); as.integer(se==0) } )

print(identical(sd,se))

rm(v,M1,M2)


test.name <- "hlp-matrix.mult-large"

N <- 500

M1 <- matrix(c(3.1,3.2),N,N)
M2 <- matrix(c(4.1,4.2),N,N)

v <- seq(3,4,length=N)

options(helpers_disable=TRUE)
sys.time ( { sd <- g(5); as.integer(sd==0) } )

options(helpers_disable=FALSE)
sys.time ( { se <- g(5); as.integer(se==0) } )

print(identical(sd,se))

rm(v,M1,M2)


rs <- test.cmp (function (n) 
{ s <- 0
  for (i in 1:n) s <- s + rowSums(M1) + rowSums(M2)
  s
})


test.name <- "hlp-matrix.rowsum-small"

N <- 200

M1 <- matrix(c(3.1,3.2),N,N)
M2 <- matrix(c(4.1,4.2),N,N)

v <- seq(3,4,length=N)

options(helpers_disable=TRUE)
sys.time ( { sd <- rs(5000); as.integer(sd==0) } )

options(helpers_disable=FALSE)
sys.time ( { se <- rs(5000); as.integer(se==0) } )

print(identical(sd,se))

rm(v,M1,M2)


test.name <- "hlp-matrix.rowsum-large"

N <- 2000

M1 <- matrix(c(3.1,3.2),N,N)
M2 <- matrix(c(4.1,4.2),N,N)

v <- seq(3,4,length=N)

options(helpers_disable=TRUE)
sys.time ( { sd <- rs(50); as.integer(sd==0) } )

options(helpers_disable=FALSE)
sys.time ( { se <- rs(50); as.integer(se==0) } )

print(identical(sd,se))

rm(v,M1,M2)


cs <- test.cmp (function (n) 
{ s <- 0
  for (i in 1:n) s <- s + colSums(M1) + colSums(M2)
  s
})


test.name <- "hlp-matrix.colsum-small"

N <- 200

M1 <- matrix(c(3.1,3.2),N,N)
M2 <- matrix(c(4.1,4.2),N,N)

v <- seq(3,4,length=N)

options(helpers_disable=TRUE)
sys.time ( { sd <- cs(5000); as.integer(sd==0) } )

options(helpers_disable=FALSE)
sys.time ( { se <- cs(5000); as.integer(se==0) } )

print(identical(sd,se))

rm(v,M1,M2)


test.name <- "hlp-matrix.colsum-large"

N <- 2000

M1 <- matrix(c(3.1,3.2),N,N)
M2 <- matrix(c(4.1,4.2),N,N)

v <- seq(3,4,length=N)

options(helpers_disable=TRUE)
sys.time ( { sd <- cs(50); as.integer(sd==0) } )

options(helpers_disable=FALSE)
sys.time ( { se <- cs(50); as.integer(se==0) } )

print(identical(sd,se))

rm(v,M1,M2)
