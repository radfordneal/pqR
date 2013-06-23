source("time.r")
 
if (!exists(".rowMeans")) # For early versions...
{ .rowMeans <- function (X, m, n, na.rm = FALSE) 
               .Internal(rowMeans(X, m, n, na.rm))
  .colMeans <- function (X, m, n, na.rm = FALSE) 
               .Internal(colMeans(X, m, n, na.rm))
}

s <- matrix (seq(0,1,length=1000)^1.3, 50, 2)
m <- matrix (seq(0,1,length=1200)^1.4, 30, 40)
M <- matrix (seq(0,1,length=120000)^0.8, 300, 400)

n <- m
n[17,12] <- NA
n[3,22] <- 0/0

b <- m>0.91 | m<0.17 | m>0.32 & m<0.56

ts <- t(s)
tm <- t(m)
tM <- t(M)
tn <- t(n)
tb <- t(b)

test.name <- "rowcolMeans.row-keep-na"

f <- test.cmp (function (m,n) { for (i in seq_len(n)) v <- rowMeans(m); v })
g <- test.cmp (function (m,n) 
     { for (i in seq_len(n)) v <- rowMeans(m,na.rm=TRUE); v })
h <- test.cmp (function (m,n) 
       { for (i in seq_len(n)) v <- .rowMeans(m,30,40); v })

sys.time(a<-f(s,100000))
print(a)

sys.time(a<-f(m,100000))
print(a)

sys.time(a<-f(n,100000))
print(a)

sys.time(a<-f(M,10000))
print(a[c(1:10,182,291:300)])

sys.time(a<-f(b,100000))
print(a)

sys.time(a<-h(m,100000))
print(a)

test.name <- "rowcolMeans.row-rm-na"

sys.time(a<-g(s,100000))
print(a)

sys.time(a<-g(m,100000))
print(a)

sys.time(a<-g(n,100000))
print(a)

sys.time(a<-g(M,10000))
print(a[c(1:10,182,291:300)])

sys.time(a<-g(b,100000))
print(a)

test.name <- "rowcolMeans.col-keep-na"

f <- test.cmp (function (m,n) { for (i in seq_len(n)) v <- colMeans(m); v })
g <- test.cmp (function (m,n) 
     { for (i in seq_len(n)) v <- colMeans(m,na.rm=TRUE); v })
h <- test.cmp (function (m,n) 
       { for (i in seq_len(n)) v <- .colMeans(m,40,30); v })

sys.time(a<-f(ts,100000))
print(a)

sys.time(a<-f(tm,100000))
print(a)

sys.time(a<-f(tn,100000))
print(a)

sys.time(a<-f(tM,10000))
print(a[c(1:10,182,291:300)])

sys.time(a<-f(tb,100000))
print(a)

sys.time(a<-h(tm,100000))
print(a)

test.name <- "rowcolMeans.col-rm-na"

sys.time(a<-g(ts,100000))
print(a)

sys.time(a<-g(tm,100000))
print(a)

sys.time(a<-g(tn,100000))
print(a)

sys.time(a<-g(tM,10000))
print(a[c(1:10,182,291:300)])

sys.time(a<-g(tb,100000))
print(a)


# Some correctness checks.

cat("a:\n")
print(rowMeans(b, na.rm=TRUE))

cat("b:\n")
print(rowMeans(matrix(as.integer(1000*m),40,30)))
print(rowMeans(matrix(as.integer(1000*m),40,30), na.rm=TRUE))

cat("c:\n")
print(colMeans(tb, na.rm=TRUE))

cat("d:\n")
print(colMeans(matrix(as.integer(1000*tm),30,40)))
print(colMeans(matrix(as.integer(1000*tm),30,40), na.rm=TRUE))

cat("e:\n")
a <- rowMeans(M[1:201,])
print(a[c(1:10,182,201)])
a <- rowMeans(M[1:201,], na.rm=TRUE)
print(a[c(1:10,182,201)])

cat("f:\n")
a <- rowMeans(M[1:299,])
print(a[c(1:10,182,299)])
a <- rowMeans(M[1:299,], na.rm=TRUE)
print(a[c(1:10,182,299)])

cat("g:\n")
a <- matrix (c (1.23456789e-15, 2, -1, -1,  0, 1.23456789e-15, 2, -2), 4, 2)
print(colMeans(a))
print(rowMeans(t(a)))
