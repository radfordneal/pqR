source("time.r")
 
test.name <- "alloc"

f <- function (i) { a <- (i-10000):(i+10000); c(a[1],a[length(a)]) }
g <- function (n) { b <- 0; for (i in 1:n) b <- b + f(i); b }

sys.time(x <- g(30000))
print(x)

f <- test.cmp (function (x, y, n)
{ if (n<1) return (x+y)
  for (k in 1:3)
  { a <- c(x,y,1.1)
    for (i in 1:2)
    { b <- as.integer(123*a)
      d <- as.logical(b)
      a <- f(0.11*d,0.32*a,n/2)
    }
  }
  return (a)
})

sys.time(a <- f(c(1.3,5.1),c(0.3,1.3),8))
cat(a[1],a[length(a)],sum(a),"\n")

f <- test.cmp (function (x, y, n)
{ if (n<1) return (x+y)
  a <- c(x,1.1) + y
  for (i in 1:5)
  { b <- as.integer(1234*a)
    a <- f(x+0.1,b/2000,n/2)
  }
  return (a)
})

sys.time(a <- f(3.1,c(0.3,1.3),128))
print(a)

f <- test.cmp (function (n,m,r)
{ 
  s <- sample(1:n)
  a <- vector("list",n)
  for (i in 1:r)
  { for (j in s) a[[j]] <- integer(m)
    for (j in s) a[[j]] <- integer(3*m)
    for (j in s) a[[j]] <- integer(5*m)
  }
})

set.seed(1)

sys.time(f(10000,10,20))

