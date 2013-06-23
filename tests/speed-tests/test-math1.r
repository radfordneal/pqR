source("time.r")
 
f <- test.cmp(function(n){ for (i in 1:n) a <- sqrt(b); a })

g <- test.cmp(function(n){ for (i in 1:n) a <- sin(b); a })

h <- test.cmp(function(n){ for (i in 1:n) a <- lgamma(b); a })

test.name <- "math1.scalar"

b <- 0.7

sys.time(a<-f(3000000))
print(a)

sys.time(a<-g(3000000))
print(a)

sys.time(a<-h(2000000))
print(a)

test.name <- "math1.vec50"

b <- seq(0.1,0.7,length=50)

sys.time(a<-f(900000))
print(a[c(1,50)])

sys.time(a<-g(500000))
print(a[c(1,50)])

sys.time(a<-h(100000))
print(a[c(1,50)])

test.name <- "math1.vec1000"

b <- seq(0.1,0.7,length=1000)

sys.time(a<-f(150000))
print(a[c(1,1000)])

sys.time(a<-g(70000))
print(a[c(1,1000)])

sys.time(a<-h(10000))
print(a[c(1,1000)])
