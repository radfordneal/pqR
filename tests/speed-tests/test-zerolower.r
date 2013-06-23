source("time.r")
 
test.name <- "zerolower.1"

zerolower1 <- test.cmp (function (A,n)
{ for (k in 1:n)
    for (j in 1:min(nrow(A),ncol(A)))
      A[j:nrow(A),j] <- 0
  A
})

A <- matrix(1,7,10)
sys.time(B <- zerolower1(A,50000))
print(B)

A <- matrix(1,52,50)
sys.time(B <- zerolower1(A,5000))

test.name <- "zerolower.2"

zerolower2 <- test.cmp (function (A,n)
{ nr <- nrow(A)
  nc <- ncol(A)
  for (k in 1:n)
    for (j in 1:min(nr,nc))
      A[j:nr,j] <- 0
  A
})

A <- matrix(1,7,10)
sys.time(B <- zerolower2(A,100000))
print(B)

A <- matrix(1,52,50)
sys.time(B <- zerolower2(A,10000))

test.name <- "zerolower.3"

zerolower3 <- test.cmp (function (A,n)
{ nr <- nrow(A)
  nc <- ncol(A)
  for (k in 1:n)
    for (j in 1:min(nr,nc))
      for (i in j:nr)
        A[i,j] <- 0
  A
})


A <- matrix(1,7,10)
sys.time(B <- zerolower3(A,20000))
print(B)

A <- matrix(1,52,50)
sys.time(B <- zerolower3(A,1000))
