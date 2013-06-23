source("time.r")
 
test.name <- "bind.c"

u <- rep(1.1,80)
j <- 1:80
v <- rep(2.2,800)
w <- rep(3.3,80)

f <- function (n,...) { for (i in 1:n) x <- c(...); x }

sys.time(x<-f(5000000,2.1,3.2))
print(x)

sys.time(x<-f(5000000,"ab","xy","pqrs"))
print(x)

sys.time(x<-f(5000000,list(1.1,2.2),list(3.3),list(4.4)))
print(x)

sys.time(x<-f(2000000,u,w))
print(x)

sys.time(x<-f(500000,u,j,v,w))
print(sum(x))

sys.time(x<-f(30000,a=u,b=w))
print(x)

names(u) <- paste("x",1:length(u),sep=":")
names(w) <- paste("y",1:length(u),sep=":")

sys.time(x<-f(100000,u,w))
print(x)

sys.time(x<-f(100000,a=u,b=w))
print(x)


test.name <- "bind.unlist"

u <- rep(1.1,80)
v <- rep(2.2,800)
w <- rep(3.3,80)

f <- function (n,xl) { for (i in 1:n) x <- unlist(xl); x }

sys.time(x<-f(1000000,list(u,w)))
print(x)

sys.time(x<-f(300000,list(u,v,w)))
print(sum(x))

sys.time(x<-f(30000,list(a=u,b=w)))
print(x)

names(u) <- paste("x",1:length(u),sep=":")
names(w) <- paste("y",1:length(u),sep=":")

sys.time(x<-f(100000,list(u,w)))
print(x)

sys.time(x<-f(100000,list(a=u,b=w)))
print(x)


test.name <- "bind.cbind"

u <- rep(1.1,100)
j <- 1:100
v <- rep(0.7,10)
M <- matrix(2.2,100,50)
N <- matrix(3.3,100,40)
W <- matrix(7.1,100,2)

Mnamed <- M
rownames(Mnamed) <- paste("R",1:nrow(M),sep="")
colnames(Mnamed) <- paste("C",1:ncol(M),sep="")

Nnamed <- N
rownames(Nnamed) <- paste("r",1:nrow(N),sep="")
colnames(Nnamed) <- paste("c",1:ncol(N),sep="")

f <- function (n,...) { for (i in 1:n) x <- cbind(...); x }

sys.time(x<-f(30000,M,N))
print(rowSums(x))
print(colSums(x))

sys.time(x<-f(30000,Mnamed,Nnamed))
print(rowSums(x))
print(colSums(x))

sys.time(x<-f(30000,M,u,j,N))
print(rowSums(x))
print(colSums(x))

sys.time(x<-f(30000,M,u))
print(rowSums(x))
print(colSums(x))

sys.time(x<-f(30000,M,v))
print(rowSums(x))
print(colSums(x))

sys.time(x<-f(200000,u,u,u,u,u))
print(c(dim(x),sum(x)))

sys.time(x<-f(200000,W,W,W))
print(c(dim(x),sum(x)))

sys.time(x<-f(100000,100,M,200))
print(x[1,])
print(x[100,])


test.name <- "bind.rbind"

u <- rep(1.1,100)
j <- 1:100
v <- rep(0.7,10)
M <- matrix(2.2,50,100)
N <- matrix(3.3,40,100)
W <- matrix(7.1,2,100)

Mnamed <- M
rownames(Mnamed) <- paste("R",1:nrow(M),sep="")
colnames(Mnamed) <- paste("C",1:ncol(M),sep="")

Nnamed <- N
rownames(Nnamed) <- paste("r",1:nrow(N),sep="")
colnames(Nnamed) <- paste("c",1:ncol(N),sep="")

f <- function (n,...) { for (i in 1:n) x <- rbind(...); x }

sys.time(x<-f(30000,M,N))
print(rowSums(x))
print(colSums(x))

sys.time(x<-f(30000,Mnamed,Nnamed))
print(rowSums(x))
print(colSums(x))

sys.time(x<-f(30000,M,u,j,N))
print(rowSums(x))
print(colSums(x))

sys.time(x<-f(30000,M,u))
print(rowSums(x))
print(colSums(x))

sys.time(x<-f(30000,M,v))
print(rowSums(x))
print(colSums(x))

sys.time(x<-f(200000,u,u,u,u,u))
print(c(dim(x),sum(x)))

sys.time(x<-f(200000,W,W,W))
print(c(dim(x),sum(x)))

sys.time(x<-f(100000,100,M,200))
print(x[,1])
print(x[,100])
