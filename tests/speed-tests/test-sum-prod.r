source("time.r")
 
test.name <- "sum-prod.sum"

a <- seq(0.001,1,length=1000)
a3 <- c(1,2,3)

f <- test.cmp (function () {for (i in 1:5000000) b <- sum(a3); b})
sys.time(b<-f()); print(b)

f <- test.cmp (function () {for (i in 1:500000) b <- sum(a); b})
sys.time(b<-f()); print(b)

f <- test.cmp (function () {for (i in 1:500000) b <- sum(a,na.rm=TRUE); b})
sys.time(b<-f()); print(b)

a[100] <- NA
f <- test.cmp (function () {for (i in 1:500000) b <- sum(a,na.rm=TRUE); b})
sys.time(b<-f()); print(b)

x <- 1:1000

f <- test.cmp (function () {for (i in 1:500000) b <- sum(x); b})
sys.time(b<-f()); print(b)

z <- seq(0.001,1,length=1000) + 1i

f <- test.cmp (function () {for (i in 1:500000) b <- sum(z); b})
sys.time(b<-f()); print(b)

test.name <- "sum-prod.sum-math1"

a <- seq(0.001,1,length=1000)
aa <- seq(0.001,1,length=10000)

f <- test.cmp (function () {for (i in 1:50000) b <- sum(log(a)); b})
sys.time(b<-f()); print(b)

f <- test.cmp (function () {for (i in 1:50000) b <- sum(exp(a)); b})
sys.time(b<-f()); print(b)

f <- test.cmp (function () {for (i in 1:100000) b <- sum(trunc(a)); b})
sys.time(b<-f()); print(b)

f <- test.cmp (function () {for (i in 1:500000) b <- sum(abs(a)); b})
sys.time(b<-f()); print(b)

f <- test.cmp (function () {for (i in 1:10000) b <- sum(sqrt(aa)); b})
sys.time(b<-f()); print(b)

test.name <- "sum-prod.prod"

a <- seq(0.9,1.1,length=1000)
a3 <- c(1,2,3)

f <- test.cmp (function () {for (i in 1:5000000) b <- prod(a3); b})
sys.time(b<-f()); print(b)

f <- test.cmp (function () {for (i in 1:500000) b <- prod(a); b})
sys.time(b<-f()); print(b)

f <- test.cmp (function () {for (i in 1:500000) b <- prod(a,na.rm=TRUE); b})
sys.time(b<-f()); print(b)

a[100] <- NA
f <- test.cmp (function () {for (i in 1:500000) b <- prod(a,na.rm=TRUE); b})
sys.time(b<-f()); print(b)

x <- rep(1:2,500)

f <- test.cmp (function () {for (i in 1:500000) b <- sum(x); b})
sys.time(b<-f()); print(b)

z <- seq(0.9,1.1,length=1000) + 1i

f <- test.cmp (function () {for (i in 1:500000) b <- sum(z); b})
sys.time(b<-f()); print(b)
