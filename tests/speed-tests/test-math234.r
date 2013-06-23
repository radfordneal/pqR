source("time.r")
 
test.name <- "math234.dpois"

f <- function (n,a,b) { for (i in 1:n) d <- dpois(a,b); d }

a <- 1:10
b <- 4:13

sys.time(r<-f(500000,a,b))
print(round(r,3))

sys.time(r<-f(500000,a,4.1))
print(round(r,3))

a <- 1:100
b <- 4:103

sys.time(r<-f(100000,a,b))
print(round(r,3))

sys.time(r<-f(100000,a,4.1))
print(round(r,3))

a <- 1:1000
b <- 4:1003

sys.time(r<-f(10000,a,b))
print(round(r[c(1,2,999,1000)],3))

sys.time(r<-f(10000,a,4.1))
print(round(r[c(1,2,999,1000)],3))

print (c (dpois(NA,1), dpois(1,NA), dpois(0/0,1), dpois(1,0/0), dpois(0/0,NA)))


test.name <- "math234,dnorm"

f <- function (n,a,b,c) { for (i in 1:n) d <- dnorm(a,b,c); d }

a <- seq(-3,3,length=10)
b <- seq(-1,1,length=10)
c <- seq(1,10,length=10)

sys.time(r<-f(1000000,a,b,c))
print(round(r,3))

sys.time(r<-f(1000000,a,4.1,2.1))
print(round(r,3))

a <- seq(-3,3,length=100)
b <- seq(-1,1,length=100)
c <- seq(1,10,length=100)

sys.time(r<-f(200000,a,b,c))
print(round(r,3))

sys.time(r<-f(200000,a,4.1,2.1))
print(round(r,3))

a <- seq(-3,3,length=1000)
b <- seq(-1,1,length=1000)
c <- seq(1,10,length=1000)

sys.time(r<-f(20000,a,b,c))
print(round(r[c(1,2,999,1000)],3))

sys.time(r<-f(20000,a,4.1,2.1))
print(round(r[c(1,2,999,1000)],3))

print (c (dnorm(NA,1,2), dnorm(1,NA,2), dnorm(1,2,NA), dnorm(0/0,1,2)))


test.name <- "math234.dhyper"

f <- function (n,a,b,c,d) { for (i in 1:n) r <- dhyper(a,b,c,d); r }

a <- 0:20
b <- 20:40
c <- 60:80
d <- 40:60

sys.time(r<-f(100000,a,b,c,d))
print(r)
