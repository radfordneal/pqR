source("time.r")
 
# Timing tests:

test.name <- "vec-subset.vec-by-scalar"

a <- seq(0,1,length=200)
k <- 1:200
l <- rep(c(TRUE,FALSE,TRUE,TRUE),50)
z <- a + 1i
s <- rep(c("ab","cd","efg","xyz"),50)
r <- 2.1

f <- test.cmp (function () { for (i in 1:500000) b <<- a[1] + a[r] + a[3L] })
sys.time(f())
print(b)

f <- test.cmp (function () { for (i in 1:500000) b <<- k[1] + k[r] + k[3L] })
sys.time(f())
print(b)

f <- test.cmp (function () { for (i in 1:500000) b <<- l[1] | l[r] | l[3L] })
sys.time(f())
print(b)

f <- test.cmp (function () { for (i in 1:500000) b <<- z[1] + z[r] + z[3L] })
sys.time(f())
print(b)

f <- test.cmp (function () { for (i in 1:500000) b <<- paste(s[1],s[r],s[3L]) })
sys.time(f())
print(b)

f <- test.cmp (function () { for (i in 1:500000) a[r] <<- a[1] + a[3] })
sys.time(f())
print(a[r])

test.name <- "vec-subset.vec-by-vec"

a <- seq(0,1,length=1000)
s <- rep(c("ab","cd","efg","xyz"),250)
t <- as.list(a)
w <- 101:200
x <- 1:100
y <- seq(1,200,by=2)
z <- seq(1,200,by=2) + 0.1

f <- test.cmp (function () { for (i in 1:1000000) b <<- a[10:12] + a[110:112] })
sys.time(f())
print(sum(b))

f <- test.cmp (function () { for (i in 1:300000) b <<- a[1:100] + a[101:200] })
sys.time(f())
print(sum(b))

f <- test.cmp (function () { for (i in 1:200000) b <<- a[11:510] + a[401:900]})
sys.time(f())
print(sum(b))

f <- test.cmp (function () { for (i in 1:300000) b <<- a[x] + a[w] })
sys.time(f())
print(sum(b))

f <- test.cmp (function () { for (i in 1:300000) b <<- a[x] + a[51:150] })
sys.time(f())
print(sum(b))

f <- test.cmp (function () { for (i in 1:300000) b <<- a[y] + a[51:150] })
sys.time(f())
print(sum(b))

f <- test.cmp (function () { for (i in 1:300000) b <<- a[z] + a[51:150] })
sys.time(f())
print(sum(b))

f <- test.cmp (function () { for (i in 1:300000) a[101:150] <<- a[1:50] })
sys.time(f())
print(sum(a))

f <- test.cmp (function () { for (i in 1:300000) b <<- s[101:200] })
sys.time(f())
print(b[c(1,2,99,100)])

an <- a
names(an) <- s

f <- test.cmp (function () { for (i in 1:300000) b <<- an[101:200] })
sys.time(f())
print(sum(b))
print(b[c(1,2,99,100)])

f <- test.cmp (function () { for (i in 1:100000) b <<- t[100:300] })
sys.time(f())
print(sum(unlist(t)))

test.name <- "vec-subset.vec-by-neg"

f <- test.cmp (function () { for (i in 1:100000) b <<- a[-(1:5)] + a[-(5:9)] })
sys.time(f())
print(sum(b))

f <- test.cmp (function () { for (i in 1:100000) b <<- a[-1] + a[-100] })
sys.time(f())
print(sum(b))

f <- test.cmp (function () { for (i in 1:100000) b <<- list(s[-1],s[-100]) })
sys.time(f())
print(c(length(b[[1]]),length(b[[2]])))

test.name <- "vec-subset.mat-by-scalar"

A <- matrix(seq(0,1,length=150000),300,500)
Z <- A+1i
r <- 2.1

f <- test.cmp (function () { for (i in 1:500000) b <<- A[3,4] + A[r,10] })
sys.time(f())
print(b)

f <- test.cmp (function () { for (i in 1:500000) b <<- A[3L,4L] + A[r,10L] })
sys.time(f())
print(b)

f <- test.cmp (function () { for (i in 1:500000) b <<- Z[3L,4L] + Z[r,10L] })
sys.time(f())
print(b)

f <- test.cmp (function () { for (i in 1:500000) A[3,4] <<- A[r,10] })
sys.time(f())
print(A[3,4])

test.name <- "vec-subset.mat-by-vec"

A <- matrix(seq(0,1,length=150000),300,500)
Z <- A+1i

f <- test.cmp (function () { for (i in 1:20000) B <<- A[100:200,300:400] })
sys.time(f())
print(c(sum(B),sum(B[10,]),sum(B[,10])))

f <- test.cmp (function () { for (i in 1:500000) v <<- A[10,300:400] })
sys.time(f())
print(c(sum(v),sum(exp(v))))

f <- test.cmp (function () { for (i in 1:500000) v <<- A[100:200,10] })
sys.time(f())
print(c(sum(v),sum(exp(v))))

f <- test.cmp (function () { for (i in 1:300000) v <<- A[10:11,] })
sys.time(f())
print(c(sum(v),sum(exp(v))))

f <- test.cmp (function () { for (i in 1:300000) v <<- A[,10:11] })
sys.time(f())
print(c(sum(v),sum(exp(v))))

f <- test.cmp (function () { for (i in 1:10000) v <<- A[-10,100:200] })
sys.time(f())
print(c(sum(v),sum(exp(v))))

f <- test.cmp (function () { for (i in 1:10000) v <<- A[100:200,-10] })
sys.time(f())
print(c(sum(v),sum(exp(v))))

f <- test.cmp (function () { for (i in 1:20000) B <<- Z[100:200,300:400] })
sys.time(f())
print(c(sum(B),sum(B[10,]),sum(B[,10])))

f <- test.cmp (function () { for (i in 1:20000) A[101:130,301:400] <<- 99 })
sys.time(f())
print(sum(A))

test.name <- "vec-subset.small-strings"

f <- test.cmp (function (s,n) { for (i in 1:n) x <- v[s]; x })

v <- c(xyz=1, pqr=2, a=3, b=4, c=5, d=6, def=7)

sys.time(r<-f("pqr",2000000))
print(r)

sys.time(r<-f("def",2000000))
print(r)

sys.time(r<-f(c("a","d"),2000000))
print(r)

M <- matrix(1:12,3,2)
rownames(M) <- c("aa","bb","cc")
colnames(M) <- c("xx","yy")

f <- test.cmp (function (a,b) { for (i in 1:1000000) x <- M[a,b]; x })

sys.time(r<-f("bb","yy"))
print(r)

test.name <- "vec-subset.large-strings"

f <- test.cmp (function (s,n) { for (i in 1:n) x <- v[s]; x })

v <- 1:500
names(v) <- paste("a",1:500,sep="")

sys.time(r<-f("a400",500000))
print(r)

v <- 1:2000
names(v) <- paste("a",1:2000,sep="")

sys.time(r<-f("a1000",200000))
print(r)

sys.time(r<-f(c("a100","a500","a300","a200","a1900","a700","a600","a10"),20000))
print(r)

sys.time(r<-f(c("a100","a500","a300","a200","a1900","a700","a600","a10",
                "a101","a501","a301","a201","a1901","a701","a601","a11"),20000))
print(r)

u <- rep(c("aa","bb","cc"),5)
v <- c(aa=1,cc=3,bb=2)

sys.time(r<-f(u,1000000))
print(sum(r*(1:length(r))))

u <- rep(c("aa","bb","cc"),10)
v <- c(aa=1,cc=3,bb=2)

sys.time(r<-f(u,1000000))
print(sum(r*(1:length(r))))

u <- rep(c("aa","bb","cc"),100)
v <- c(aa=1,cc=3,bb=2)

sys.time(r<-f(u,100000))
print(sum(r*(1:length(r))))

u <- rep(c("aa","bb","cc"),1000)
v <- c(aa=1,cc=3)

sys.time(r<-f(u,10000))
print(r[1:6])

# Correctness checks:

a <- c(aa=10,bb=20,cc=30,dd=40)
print(a[c(2,0,5,1)])
print(a[-2])
print(a[c("cc","aa","cc","ee")])

a[c("aa","ee")] <- c(100,500)
print(a)

a[c("bb","ff","gg","ff")] <- c(1000,2000,3000,4000)
print(a)

print(a[c("","aa",NA_character_)])
a[c("","aa",NA_character_)] <- 123
print(a)

M <- matrix(1:15,3,5)
print(M[c(2,NA,1,1),c(5,2,NA,5,3,NA)])

M[2:3,1:2] <- 999
print(M)

m <- matrix(1:12,3,4)
print(m[drop=1:1,2:3])

