# Test possible bugs involving subset and subassign.
#
# Added for pqR, 2014, 2017, Radford M. Neal.

# General tests of subset extraction with scalar indexes, not array.

vi <- 11:15
vr <- (11:15) + 0.1
L <- list(11.1,12L,13.1,14L,TRUE)
Ln <- list(e1=11.1,e2=12L,e3=13.1,e4=14L,e5=TRUE)

vin <- vi
names(vin) <- paste0("a",1:length(vin))
vrn <- vr
names(vrn) <- paste0("a",1:length(vrn))

a <- vi[3L]
stopifnot(identical(a,13L))
a <- vi[3]
stopifnot(identical(a,13L))
a <- vr[3]
stopifnot(identical(a,13.1))
a <- vin[3]
stopifnot(identical(a,c(a3=13L)))
a <- vrn[3]
stopifnot(identical(a,c(a3=13.1)))
a <- L[3]
stopifnot(identical(a,list(13.1)))
a <- Ln[3]
stopifnot(identical(a,list(e3=13.1)))

a <- vi[[3]]
stopifnot(identical(a,13L))
a <- vr[[3]]
stopifnot(identical(a,13.1))
a <- vin[[3]]
stopifnot(identical(a,13L))
a <- vrn[[3]]
stopifnot(identical(a,13.1))
a <- L[[3]]
stopifnot(identical(a,13.1))
a <- Ln[[3]]
stopifnot(identical(a,13.1))

a <- vin["a3"]
stopifnot(identical(a,c(a3=13L)))
a <- vin[["a3"]]
stopifnot(identical(a,13L))
a <- Ln[["e3"]]
stopifnot(identical(a,13.1))
a <- Ln$e3
stopifnot(identical(a,13.1))

j <- 2

a <- vi[j+1]
stopifnot(identical(a,13L))
a <- vr[j+1]
stopifnot(identical(a,13.1))
a <- vin[j+1]
stopifnot(identical(a,c(a3=13L)))
a <- vrn[j+1]
stopifnot(identical(a,c(a3=13.1)))
a <- L[j+1]
stopifnot(identical(a,list(13.1)))
a <- Ln[j+1]
stopifnot(identical(a,list(e3=13.1)))

a <- vi[[j+1]]
stopifnot(identical(a,13L))
a <- vr[[j+1]]
stopifnot(identical(a,13.1))
a <- vin[[j+1]]
stopifnot(identical(a,13L))
a <- vrn[[j+1]]
stopifnot(identical(a,13.1))
a <- L[[j+1]]
stopifnot(identical(a,13.1))
a <- Ln[[j+1]]
stopifnot(identical(a,13.1))


# General tests of subset replacement with scalar indexes, not array.

vi <- 11:15
vr <- (11:15) + 0.1
L <- list(11.1,12L,13.1,14L,TRUE)
Ln <- list(e1=11.1,e2=12L,e3=13.1,e4=14L,e5=TRUE)

vin <- vi
names(vin) <- paste0("a",1:length(vin))
vrn <- vr
names(vrn) <- paste0("a",1:length(vrn))

vi[3] <- 20L
stopifnot(identical(vi,c(11L,12L,20L,14L,15L)))
vi[2] <- 30
stopifnot(identical(vi,c(11,30,20,14,15)))
vr[3] <- 20.2
vr[2] <- 30.2
stopifnot(identical(vr,c(11.1,30.2,20.2,14.1,15.1)))

vin[3] <- 20L
stopifnot(identical(vin,c(a1=11L,a2=12L,a3=20L,a4=14L,a5=15L)))
vin[2] <- 30
stopifnot(identical(vin,c(a1=11,a2=30,a3=20,a4=14,a5=15)))
vrn[3] <- 20.2
vrn[2] <- 30.2
stopifnot(identical(vrn,c(a1=11.1,a2=30.2,a3=20.2,a4=14.1,a5=15.1)))

L[3] <- list(FALSE)
stopifnot(identical(L,list(11.1,12L,FALSE,14L,TRUE)))
L[[2]] <- 1.1
stopifnot(identical(L,list(11.1,1.1,FALSE,14L,TRUE)))
Ln[3] <- list(FALSE)
stopifnot(identical(Ln,list(e1=11.1,e2=12L,e3=FALSE,e4=14L,e5=TRUE)))
Ln$e5 <- "abc"
stopifnot(identical(Ln,list(e1=11.1,e2=12L,e3=FALSE,e4=14L,e5="abc")))
Ln$e1 <- c(1,2)
stopifnot(identical(Ln,list(e1=c(1,2),e2=12L,e3=FALSE,e4=14L,e5="abc")))

vi <- 11:15
vr <- (11:15) + 0.1
L <- list(11.1,12L,13.1,14L,TRUE)
Ln <- list(e1=11.1,e2=12L,e3=13.1,e4=14L,e5=TRUE)

vin <- vi
names(vin) <- paste0("a",1:length(vin))
vrn <- vr
names(vrn) <- paste0("a",1:length(vrn))

vi[[3]] <- 20L
stopifnot(identical(vi,c(11L,12L,20L,14L,15L)))
vi[[2]] <- 30
stopifnot(identical(vi,c(11,30,20,14,15)))
vr[[3]] <- 20.2
vr[[2]] <- 30.2
stopifnot(identical(vr,c(11.1,30.2,20.2,14.1,15.1)))

vin[[3]] <- 20L
stopifnot(identical(vin,c(a1=11L,a2=12L,a3=20L,a4=14L,a5=15L)))
vin[[2]] <- 30
stopifnot(identical(vin,c(a1=11,a2=30,a3=20,a4=14,a5=15)))
vrn[[3]] <- 20.2
vrn[[2]] <- 30.2
stopifnot(identical(vrn,c(a1=11.1,a2=30.2,a3=20.2,a4=14.1,a5=15.1)))

vin["a1"] <- 10L
stopifnot(identical(vin,c(a1=10,a2=30,a3=20,a4=14,a5=15)))
vin[["a1"]] <- 12L
stopifnot(identical(vin,c(a1=12,a2=30,a3=20,a4=14,a5=15)))
# vin$a4 <- 50L
# stopifnot(identical(vin,c(a1=12,a2=30,a3=20,a4=50,a5=15)))

vi <- 11:15
vr <- (11:15) + 0.1
L <- list(11.1,12L,13.1,14L,TRUE)
Ln <- list(e1=11.1,e2=12L,e3=13.1,e4=14L,e5=TRUE)

vin <- vi
names(vin) <- paste0("a",1:length(vin))
vrn <- vr
names(vrn) <- paste0("a",1:length(vrn))

j <- 2
k <- 20L

vi[[j+1]] <- k
stopifnot(identical(vi,c(11L,12L,20L,14L,15L)))
vi[[j]] <- k+10
stopifnot(identical(vi,c(11,30,20,14,15)))
vr[[j+1]] <- k+0.2
vr[[j]] <- k+10.2
stopifnot(identical(vr,c(11.1,30.2,20.2,14.1,15.1)))

vin[[j+1]] <- k
stopifnot(identical(vin,c(a1=11L,a2=12L,a3=20L,a4=14L,a5=15L)))
vin[[j]] <- k+10
stopifnot(identical(vin,c(a1=11,a2=30,a3=20,a4=14,a5=15)))
vrn[[j+1]] <- k+0.2
vrn[[j]] <- k+10.2
stopifnot(identical(vrn,c(a1=11.1,a2=30.2,a3=20.2,a4=14.1,a5=15.1)))

vin["a1"] <- k-10L
stopifnot(identical(vin,c(a1=10,a2=30,a3=20,a4=14,a5=15)))
vin[["a1"]] <- k-8L
stopifnot(identical(vin,c(a1=12,a2=30,a3=20,a4=14,a5=15)))
# vin$a4 <- k+30L
# stopifnot(identical(vin,c(a1=12,a2=30,a3=20,a4=50,a5=15)))


# Replacement of pairlist or language elements.

L <- pairlist(1,2,3)
L[[2]] <- 9
stopifnot(identical(L,pairlist(1,9,3)))
L[[2]] <- NULL
stopifnot(identical(L,pairlist(1,3)))
L[[1]] <- NULL
stopifnot(identical(L,pairlist(3)))

L <- pairlist(a=1,b=2,c=3)
L$b <- 9
stopifnot(identical(L,pairlist(a=1,b=9,c=3)))
L$b <- NULL
stopifnot(identical(L,pairlist(a=1,c=3)))
L$a <- NULL
stopifnot(identical(L,pairlist(c=3)))

L <- parse(text="f(1,2)")[[1]]
L[[2]] <- 3
stopifnot(identical(L,parse(text="f(3,2)")[[1]]))
L[[1]] <- NULL
stopifnot(identical(L,pairlist(3,2)))


# Tests with ...

M <- matrix(101:112,3,4)

f <- function (w,...) stopifnot(identical(M[...],w))
f(104L,4)
f(104L,1,2)
f(105L,2,2)
f(M[])

f <- function (w,x,...) stopifnot(identical(M[x,...],w))
f(104L,4)
f(104L,1,2)
f(105L,2,2)

f <- function (w,...) stopifnot(identical(M[[...]],w))
f(104L,4)
f(104L,1,2)
f(105L,2,2)

f <- function (w,x,...) stopifnot(identical(M[[x,...]],w))
f(104L,4)
f(104L,1,2)
f(105L,2,2)

M2 <- M
M2[2,3] <- 9L

f <- function (M,W,...) { M[...] <- 9L; stopifnot(identical(M,W)) }
f(M,M2,2,3)

f <- function (M,W,x,...) { M[x,...] <- 9L; stopifnot(identical(M,W)) }
f(M,M2,2,3)

f <- function (M,W,...) { M[[...]] <- 9L; stopifnot(identical(M,W)) }
f(M,M2,2,3)

f <- function (M,W,x,...) { M[[x,...]] <- 9L; stopifnot(identical(M,W)) }
f(M,M2,2,3)


# Check that name after $ gets dispatched as a string.

a <- list(p=3,q=4)
class(a) <- "fred"
`$.fred` <-
  function (x,n)
    { stopifnot(identical(n,substitute(n))); x[[n]] }
`$<-.fred` <-
  function (x,n,value) 
    { stopifnot(identical(n,substitute(n))); x[[n]] <- value; x }
a$q <- a$q + 1
stopifnot(identical(a,structure(list(p=3,q=5),class="fred")))

cat("DONE\n")
