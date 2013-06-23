source("time.r")
 
test.name <- "transpose"

M <- matrix(seq(0,1,length=2100),70,30)
N <- matrix(seq(0,1,length=2100),30,70,byrow=TRUE)

f <- test.cmp (function () {for (i in 1:100000) S <- t(M); S})
sys.time(R<-f())
print(c(dim(R),sum(abs(R-N)))); cat("\n")

f <- test.cmp (function () {for (i in 1:100000) R <- t(N); R})
sys.time(R<-f())
print(c(dim(R),sum(abs(R-M)))); cat("\n")

M <- matrix(seq(0,1,length=12000),200,60)
N <- matrix(seq(0,1,length=12000),60,200,byrow=TRUE)

f <- test.cmp (function () {for (i in 1:30000) S <- t(M); S})
sys.time(R<-f())
print(c(dim(R),sum(abs(R-N)))); cat("\n")

f <- test.cmp (function () {for (i in 1:30000) R <- t(N); R})
sys.time(R<-f())
print(c(dim(R),sum(abs(R-M)))); cat("\n")

v <- seq(0,2,length=12000)
w <- matrix(v,1,12000)

f <- test.cmp (function () {for (i in 1:30000) u <- t(v); u})
sys.time(r<-f())
print(c(dim(r),sum(abs(r-w)))); cat("\n")

f <- test.cmp (function () {for (i in 1:30000) h <- t(w); h})
sys.time(r<-f())
print(c(dim(r),sum(abs(r-v)))); cat("\n")
