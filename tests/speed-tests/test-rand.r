source("time.r")
 
test.name <- "rand.unif"

for (gen in c("default","Wichmann-Hill"))
{ 
  set.seed(123,gen)

  f <- test.cmp(function() { for (i in 1:1000000) r <- runif(1); r })
  sys.time(a<-f())
  print(a)

  f <- test.cmp(function() { for (i in 1:1000000) r <- runif(10); r })
  sys.time(a<-f())
  print(a)

  f <- test.cmp(function() { for (i in 1:50000) r <- runif(1000); r })
  sys.time(a<-f())
  print(a[c(1,77,876,1000)])
}

set.seed(1,"default")

test.name <- "rand.norm"

for (ngen in c("default","Box-Muller"))
{ 
  set.seed(1234)
  RNGkind(normal.kind=ngen)

  f <- test.cmp(function() { for (i in 1:1000000) r <- rnorm(1); r })
  sys.time(a<-f())
  print(a)

  f <- test.cmp(function() { for (i in 1:1000000) r <- rnorm(10); r })
  sys.time(a<-f())
  print(a)

  f <- test.cmp(function() { for (i in 1:20000) r <- rnorm(1000); r })
  sys.time(a<-f())
  print(a[c(1,77,876,1000)])
}


# Correctness checks.

set.seed(2,"default")

print(runif(1))

a <- .Random.seed
print(sum(0+.Random.seed))

print(runif(1))

print(sum(0+a))
print(sum(0+.Random.seed))
