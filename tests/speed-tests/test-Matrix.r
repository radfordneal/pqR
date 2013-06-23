source("time.r")
 
test.name <- "Matrix"

library(Matrix)

f <- test.cmp (function (){for (i in 1:1000) A <- Matrix(c(1,0,0),30,30); A})

cat("\n")
sys.time(A<-f()); print(A)

f <- test.cmp (function (){for (i in 1:1000) X <- Matrix(c(1,2,3),30,30); X})

cat("\n")
sys.time(X<-f()); print(rowSums(X))

f <- test.cmp (function () {for (i in 1:10000) B <- t(A); B})

cat("\n")
sys.time(B<-f()); print(B)

f <- test.cmp (function () {for (i in 1:1000) C <- A + B; C})

cat("\n")
sys.time(C<-f()); print(C)

f <- test.cmp (function () {for (i in 1:10000) C <- A %*% B; C})

cat("\n")
sys.time(C<-f()); print(C)

f <- test.cmp (function (){for (i in 1:5000) M <- as.matrix(C); M})

cat("\n")
sys.time(M<-f()); print(rowSums(M))
