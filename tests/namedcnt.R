# Test possible bugs involving NAMEDCNT/NAMED.
#
# Added for pqR, 2013, Radford M. Neal.

# mk2(1) makes 2 in a way unlikely to be optimized with constant folding

mk2 <- function (x) x+x

# eapply

X <- mk2(1)
f <- function(a) X
e <- as.environment(list(a=1,b=2))
A <- eapply(e,f)
X[1] <- 99
stopifnot(all(A==2))

# mapply

X <- mk2(1)
f <- function(a,b) X
A <- mapply(f,c(1,2,3),c(4,5,6),SIMPLIFY=FALSE)
X[1] <- 99
stopifnot(all(A==2))

# rapply

X <- mk2(1)
f <- function(a) X
l <- list(1,list(2,3),4)
A <- rapply(l,f,how="replace")
X[1] <- 99
stopifnot(all(unlist(A)==2))

# vapply

X <- list(456)
f <- function(a) X
A <- list(1,2)
B <- vapply(A,f,list(0))
X[[1]][1] <- 444
stopifnot(all(B==456))

# mget

a <- mk2(1)
x <- mget("a",as.environment(1))
a[1] <- 99
stopifnot(x$a==2)

# rep.int

a<-list(1,2)
b<-rep.int(a,c(2,2))
b[[1]][1]<-9
stopifnot(a[[1]]==1)

# Vector/matrix/array list assignments

w <- list(1)
a <- list(1)
b <- (a[[1]] <- a)
stopifnot(identical(b,w))

w <- list(x=1)
a <- list(x=1)
b <- (a$x <- a)
stopifnot(identical(b,w))

a <- list(c(1,2),c(3,4))
b <- list(1,2,3)
b[2:3] <- a
b[[2]][2] <- 99
stopifnot(a[[1]][2]==2)

a <- list(mk2(1),mk2(1))
b <- list(1,1,1,1)
b[1:4] <- a
b[[1]][1] <- 1
stopifnot(all(b[2:4]==2))

b <- list(1,mk2(1),3)[c(2,2)]
b[[1]][1] <- 9
stopifnot(b[[2]]==2)
