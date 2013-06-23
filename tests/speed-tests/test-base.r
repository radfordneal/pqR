source("time.r")
 
test.name <- "base.data.frame"

M <- matrix(c(1.2,1.3,1.4),1000,30,byrow=TRUE)

f <- function (n,M) { for (i in 1:n) d <- as.data.frame(M); d }

sys.time(d<-f(5000,M))

g <- function (n,d) { for (i in 1:n) M <- as.matrix(d); M }

sys.time(M2<-g(3000,d))

print(any(M2!=M))

h <- function (n,d) 
{ for (i in 1:n) { d[2,] <- 3.1; d[,3] <- 4.2; d[3,2] <- 5.5 }
  d
}

sys.time(d2<-h(1000,d))

M2[2,] <- 3.1
M2[,3] <- 4.2
M2[3,2] <- 5.5

print(any(M2!=as.matrix(d2)))

e <- function (n,d) d$V2 * d[,2] * (1:nrow(d))

sys.time(w<-h(1000,d))

print(sum(w))

test.name <- "base.apply"

m <- matrix (seq(0,1,length=1200), 30, 40)
M <- matrix (seq(0,1,length=120000), 300, 400)

f <- test.cmp(function (d,m,n) { for (i in seq_len(n)) v <- apply(m,d,sum); v })

sys.time(a<-f(1,m,10000))
print(a)

sys.time(a<-f(2,m,10000))
print(a)

sys.time(a<-f(1,M,1000))
print(a[c(1:10,291:300)])

sys.time(a<-f(2,M,1000))
print(a[c(1:10,391:400)])

f <- test.cmp (function (m,n) {
  for (i in seq_len(n)) 
  { v <- numeric(nrow(m)); 
    for (j in seq_len(nrow(m))) v[j] <- sum(m[j,]) 
  }
  v
})

sys.time(a<-f(m,10000))
print(a)

sys.time(a<-f(M,1000))
print(a[c(1:10,291:300)])

test.name <- "base.ifelse"

a <- rep(c(T,F),100)
u <- seq(1,2,length=100)
v <- seq(4,5,length=100)

f <- test.cmp (function (n,a,u,v) 
{ for (i in seq_len(n)) r <- ifelse(a,u,v); r 
})

sys.time(r<-f(200000,T,10,20))
print(r)

sys.time(r<-f(50000,a,u,v))
print(r)

sys.time(r<-f(50000,a,u,c(10,20,30,40,50)))
print(r)

m <- matrix (seq(0,1,length=1200), 30, 30)
m <- m+t(m)
M <- matrix (seq(0,1,length=120000), 300, 300)
M <- M+t(M)

test.name <- "base.eigen"

f <- test.cmp (function (m,n,k) 
{ for (i in seq_len(n)) 
    v <- eigen(m,symmetric=TRUE)
  v$values 
})

f0 <- test.cmp (function (m,n,k) 
{ for (i in seq_len(n)) 
    v <- eigen(m,,symmetric=TRUE,only.values=TRUE)
  v$values 
})

sys.time(a<-f(m,10000))
print(a)

sys.time(a<-f0(m,10000))
print(a)

sys.time(a<-f(M,20))
print(a[1:30])

sys.time(a<-f0(M,20))
print(a[1:30])

test.name <- "base.svd"

f <- 
  test.cmp (function (m,n,k) { for (i in seq_len(n)) v <- svd(m); v$d })
f0 <- 
  test.cmp (function (m,n,k) { for (i in seq_len(n)) v <- svd(m,0,0); v$d })

sys.time(a<-f(m,10000))
print(a)

sys.time(a<-f0(m,10000))
print(a)

sys.time(a<-f(M,20))
print(a[1:30])

sys.time(a<-f0(M,20))
print(a[1:30])

test.name <- "base.diag"

f <- test.cmp (function (d,n)
{
  for (i in 1:n)
  { M <- diag(d)
    M[1,2] <- 7
    diag(M) <- diag(M) + 1
    M[1,1] <- 9
    v <- sum(diag(M))
  }

  v
})

sys.time(a<-f(30,10000))
print(a)

sys.time(a<-f(rep(1+2i,30),10000))
print(a)

test.name <- "base.matrix"

f <- test.cmp (function (d,n,m,r,...)
{
  for (i in 1:r)
  { a <- matrix(d,n,m,...)
    a[1,1] <- 0
  }

  a
})

v <- seq(1,2,length=300*210)

sys.time(a<-f(1.1,210,300,5000))
print(a[1,1:15])
print(a[1:15,1])

sys.time(a<-f(c(1.1,2.1,2.3),300,210,5000))
print(a[1,1:15])
print(a[1:15,1])

sys.time(a<-f(v,300,210,5000))
print(a[1,1:15])
print(a[1:15,1])

sys.time(a<-f(1.1,210,300,5000,byrow=TRUE))
print(a[1,1:15])
print(a[1:15,1])

sys.time(a<-f(c(1.1,2.1,2.3),300,210,5000,byrow=TRUE))
print(a[1,1:15])
print(a[1:15,1])

sys.time(a<-f(v,300,210,5000,byrow=TRUE))
print(a[1,1:15])
print(a[1:15,1])

