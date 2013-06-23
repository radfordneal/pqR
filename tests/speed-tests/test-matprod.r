source("time.r")
 
s1 <- seq(0,1,length=13)
s2 <- seq(0,1,length=13)

u <- seq(0,1,length=1000)
v <- seq(0,2,length=1000)
h <- seq(0,1,length=200)
x <- seq(-1,1,length=500000)
y <- seq(10,11,length=500000)
vt = t(v)

A2 <- matrix(2.1,2,1000)
A3 <- matrix(2.1,3,1000)
A2odd <- matrix(2.1,2,1001)
A4 <- matrix(2.1,4,1000)
A5 <- matrix(2.1,5,1000)
B3 <- matrix(3.2,1000,3)
B3odd <- matrix(3.2,1001,3)
A5t <- t(A5)

A10 <- matrix(2.1,10,100)
B10 <- matrix(3.2,100,10)
B11 <- matrix(3.2,100,11)

A50 <- matrix(2.2,50,1000)
A50t <- t(A50)
C100 <- matrix(0.3,100,200)

C <- matrix(x,500,1000)
D <- matrix(y,1000,500)

cat("mat_mult_with_BLAS =",getOption("mat_mult_with_BLAS"),"\n")

test.name <- "matprod.vec-dot"

f <- test.cmp (function () {for (i in 1:1000000) w <- s1 %*% s2; w})
sys.time(w<-f())
print(c(dim(w),sum(w))); cat("\n")

f <- test.cmp (function () {for (i in 1:500000) w <- u %*% v; w})
sys.time(w<-f())
print(c(dim(w),sum(w))); cat("\n")

f <- test.cmp (function () {for (i in 1:1000) z <- x %*% y; z})
sys.time(z<-f())
print(c(dim(z),sum(z))); cat("\n")

test.name <- "matprod.mat-vec"

f <- test.cmp (function () {for (i in 1:100000) w <- A2 %*% v; w})
sys.time(w<-f())
print(c(dim(w),sum(w))); cat("\n")

f <- test.cmp (function () {for (i in 1:100000) w <- A3 %*% v; w})
sys.time(w<-f())
print(c(dim(w),sum(w))); cat("\n")

f <- test.cmp (function () {for (i in 1:100000) w <- A4 %*% v; w})
sys.time(w<-f())
print(c(dim(w),sum(w))); cat("\n")

f <- test.cmp (function () {for (i in 1:100000) w <- A5 %*% v; w})
sys.time(w<-f())
print(c(dim(w),sum(w))); cat("\n")

f <- test.cmp (function () {for (i in 1:10000)  w <- A50 %*% v; w})
sys.time(w<-f())
print(c(dim(w),sum(w))); cat("\n")

f <- test.cmp (function () {for (i in 1:20000)  w <- C100 %*% h; w})
sys.time(w<-f())
print(c(dim(w),sum(w))); cat("\n")

test.name <- "matprod.vec-mat"

f <- test.cmp (function () {for (i in 1:100000) w <- vt %*% A5t; w})
sys.time(w<-f())
print(c(dim(w),sum(w))); cat("\n")

f <- test.cmp (function () {for (i in 1:10000)  w <- vt %*% A50t; w})
sys.time(w<-f())
print(c(dim(w),sum(w))); cat("\n")

test.name <- "matprod.vec-outer"

f <- test.cmp (function () {for (i in 1:2000) R <- h %*% vt; R})
sys.time(R<-f())
print(c(dim(R),sum(R))); cat("\n")

test.name <- "matprod.mat-mat"

f <- test.cmp (function () {for (i in 1:100000) R <- A2 %*% B3; R})
sys.time(R<-f())
print(R); cat("\n")

f <- test.cmp (function () {for (i in 1:100000) R <- A2odd %*% B3odd; R})
sys.time(R<-f())
print(R); cat("\n")

f <- test.cmp (function () {for (i in 1:100000) R <- A3 %*% B3; R})
sys.time(R<-f())
print(R); cat("\n")

f <- test.cmp (function () {for (i in 1:100000) R <- A4 %*% B3; R})
sys.time(R<-f())
print(R); cat("\n")

f <- test.cmp (function () {for (i in 1:100000) R <- A5 %*% B3; R})
sys.time(R<-f())
print(R); cat("\n")

f <- test.cmp (function () {for (i in 1:200000) R <- A10 %*% B10; R})
sys.time(R<-f())
print(R); cat("\n")

f <- test.cmp (function () {for (i in 1:200000) R <- A10 %*% B11; R})
sys.time(R<-f())
print(R); cat("\n")

f <- test.cmp (function () {for (i in 1:10) R <- C %*% D; R})
sys.time(R<-f())
print(c(dim(R),sum(R))); cat("\n")

# Test of behaviour w.r.t. precision.

try.mm <- function (s)
{
  cat ("\n\nTry with",s,"\n\n"); 

  u <- c(1.0, 1.0, 1.0)
  v1 <- c(s, 1.0, -1.0)
  v2 <- c(1.0, 1.0, 1.0)
  v <- cbind(v1,v2)
  
  cat ("+:     ",u[1]*v1[1] + u[2]*v1[2] + u[3]*v1[3],"\n")
  cat ("sum:   ",sum(u*v1),"\n")
  cat ("dot:   ",u%*%v1,"\n")
  cat ("mmul:  ",(u%*%v)[1,1],"\n")
  v[2,2] <- NA
  cat ("mmulNA:",(u%*%v)[1,1],"\n")
}

cat("Precision tests\n")  
try.mm (1e-17)
try.mm (1.23456e-15)

# Test of behaviour w.r.t. Inf, NaN, and NA.

cat("\n\nDot products with Inf, NaN, and NA\n\n")

print (c (as.vector (c(1,NA,NA)   %*% c(1,1,1)),
          as.vector (c(1,NA,NA)   %*% c(1,0,1)),
          as.vector (c(1,NA,NA)   %*% c(1,0,0))))

print (c (as.vector (c(1,1/0,1/0) %*% c(1,1,1)),
          as.vector (c(1,1/0,1/0) %*% c(1,0,1)),
          as.vector (c(1,1/0,1/0) %*% c(1,0,0))))

print (c (as.vector (c(1,0/0,0/0) %*% c(1,1,1)),
          as.vector (c(1,0/0,0/0) %*% c(1,0,1)),
          as.vector (c(1,0/0,0/0) %*% c(1,0,0))))

print (c (as.vector (c(1,0/0,NA)  %*% c(1,1,1)),
          as.vector (c(1,0/0,NA)  %*% c(1,0,1)),
          as.vector (c(1,0/0,NA)  %*% c(1,0,0))))

cat("\nMatrix-vector products with Inf, NaN, and NA\n\n")

A <- matrix (c(1,1/0,0/0,NA, 1,1,1,1), 4, 2)

print (as.vector (A %*% c(1,1)))
print (as.vector (A %*% c(1,1/0)))
print (as.vector (A %*% c(1,0/0)))
print (as.vector (A %*% c(1,NA)))

cat("\nMatrix-matrix product with Inf, NaN, and NA\n\n")

A <- matrix (c(1,1/0,0/0,NA, 1,1,1,1), 4, 2)
B <- matrix (c(1,1, 1,1/0, 1,0/0, 1,NA), 2, 4)

print (A %*% B)
