source("time.r")
 
test.name <- "near"

# Find squared distances of columns of M from vector v.

sq_dist <- test.cmp (function (M, v) colSums((M-v)^2))

# Find a column of M that is closest to each column of V, from among those 
# columns whose projections by P are at squared distance no more than t from
# the projection by P of V.  Returns a list with elements "count", containing
# the number of columns of M within projected distance t for each column 
# of V, and "dist", containing the distance of the column found from V
# (Inf if no column found).  The projection of M by P may be passed as
# a pre-computed argument.

near <- test.cmp (function (M,V,P,t,proj_M=P%*%M)
{ 
  count <- numeric(ncol(V))
  dist <- numeric(ncol(V))

  for (j in 1:ncol(V))
  { 
    sqd <- sq_dist (proj_M, as.vector(P %*% V[,j]))

    c <- 0
    d <- Inf

    for (i in 1:length(sqd))
    { if (sqd[i] < t)
      { c <- c + 1
        d2 <- sum((M[,i]-V[,j])^2)
        if (d2 < d) d <- d2
      }
    }

    count[j] <- c
    dist[j] <- sqrt(d)
  }

  list (count=count, dist=dist)
})

set.seed(1)

P <- matrix(runif(100,-2,+2),5,20)
M <- matrix(rnorm(4000),20,200)
proj_M <- P %*% M

V <- matrix(rnorm(400),20,20)

# Note: The call of unlist below should force completion of all computations.

sys.time (for (i in 1:500) res <- unlist(near(M,V,P,8^2,proj_M)))

print(res)

set.seed(2)

P <- matrix(runif(100000,-2,+2),100,1000)
M <- matrix(rnorm(20000000),1000,20000)
proj_M <- P %*% M

V <- matrix(rnorm(500000),1000,500)

sys.time (n <- near(M,V,P,425^2,proj_M))

print(c(mean(n$count),mean(n$count==0)))
print(n$dist[c(1,500)])
