source("time.r")
 
# Assess degree of parallelism using helper threads when writing to a file.

test.name <- "hlp-write"

g <- test.cmp (function ()
{ P <- M1 %*% M2
  write.table(P,file,quote=FALSE,row.names=FALSE,col.names=FALSE)
})

N <- 2
K <- 1500
M <- 100000

M1 <- matrix(c(1.1,0.2),N,K)
M2 <- matrix(c(4.1,2.2),K,M)

file <- tempfile(tmpdir=".")

g(); g()  # take time for memory allocation, etc. before test

options(helpers_disable=TRUE)
sys.time (g())

ans <- scan(file)
print(sum(ans))
print(sum(ans^2))

options(helpers_disable=FALSE)
sys.time (g())

stopifnot(identical(ans,scan(file)))

rm(ans)

unlink(file)
