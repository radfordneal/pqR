# Test performance of complex assignments, and other stuff, by seeing
# what storage allocation is done.

options(keep.source=FALSE)  # avoid allocations for source references

print(1)  # get some stuff allocated before Rprofmemt activatived.

vsize <- 100  # must be multiple of 4
Rprofmemt(nelem=vsize)


# One-level assignments.

a <- numeric(vsize)
a[10] <- 3        # shouldn't duplicate
a[[11]] <- 4      # shouldn't duplicate
print(a)

a <- c(list(a=4,b=FALSE),rep(list(7),vsize-2))
a$b <- TRUE       # shouldn't duplicate
a[10] <- 3        # shouldn't duplicate
a[[11]] <- 4      # shouldn't duplicate
print(unlist(a))

a <- c(list(a=4,b=FALSE),rep(list(7),vsize-2))
names(a) <- NULL  # shouldn't duplicate
print(unlist(a))

a <- numeric(vsize)
b <- a
a[[3]] <- -1      # will have to duplicate
b[[4]] <- -2      # shouldn't duplicate
print(a)
print(b)

a <- numeric(vsize)
n <- paste0("xx",1:vsize)
names(a) <- n     # shouldn't duplicate numeric part, might duplicate names

e <- new.env()
v <- numeric(vsize)
e$a <- v          # shouldn't duplicate

a <- 1
v <- numeric(vsize)
attr(a,"fred") <- v  # shouldn't duplicate, but does currently


# Two-level assignments.

a <- rep(list(a=1,b=1:vsize,c="xx",d="yy"),vsize/4)
a$b[3] <- 99L     # will need to duplicate, because of repeated elements
print(a$b+1000L)  # don't pass a$b itself, since affects NAMEDCNT
a$b[3] <- 99L     # shouldn't duplicate now
print(a$b+1000L)
print(a[[6]])     # should still be 1:vsize
a$b[4] <- 99.1    # will have to coerce to real, shouldn't duplicate int vec
print(a$b+1000)
a$b[5] <- 99.7    # shouldn't duplicate now
print(a$b+1000)
a[[10]][2] <- 40L # will need to duplicate, because of repeated elements
print(a[[10]]+1000L)
a[[10]][2] <- 40L # shouldn't duplicate now
print(a[[10]]+1000L)

a <- c(list(x=c(0,y=1,rep(0,vsize-2))),rep(list("a"),vsize-1))
a$x[2] <- 12      # shouldn't duplicate
print(a$x+1000)
a$x[[2]] <- 13    # shouldn't duplicate
print(a$x+1000)
a$x["y"] <- 14    # shouldn't duplicate
print(a$x+1000)
a$x[["y"]] <- 15  # shouldn't duplicate
print(a$x+1000)

e <- new.env()
e$a <- numeric(vsize)
e$a[2] <- 9       # shouldn't duplicate
e$b <- e$a        # shouldn't duplicate
e$a[3] <- 2       # should duplicate
e$a[[4]] <- 3     # shouldn't duplicate
e$c <- e$b        # shouldn't duplicate
e$c[[5]] <- 4     # should duplicate
print(e$a)
print(e$b)
print(e$c)

L <- c (list (new.env()), rep(list(1),vsize-1))
v <- numeric(vsize)
L[[1]]$a <- v     # shouldn't duplicate

e <- new.env()
e$a <- numeric(vsize)
e$a[2] <- 9       # shouldn't duplicate
e$b <- e$a        # shouldn't duplicate
e$a[3] <- 2       # should duplicate
e$a[[4]] <- 3     # shouldn't duplicate
e$c <- e$b
e$c[[5]] <- 4     # should duplicate
print(e$a)
print(e$b)
print(e$c)

a <- 1
v <- numeric(vsize)
attr(a,"fred") <- v     # shouldn't duplicate, but does currently
attr(a,"fred")[2] <- 3  # should duplicate
attr(a,"fred")[3] <- 4  # shouldn't duplicate, but does currently
print(a)
print(v)

a <- numeric(vsize)
n <- paste0("xx",1:vsize)
names(a) <- n     # shouldn't duplicate numeric part, maybe dup names
names(a)[2] <- "q"# shouldn't duplicate anything, but currently dups chars twice
print(a)
print(n)

a <- numeric(vsize)
b <- integer(vsize)
attr(a,"bert") <- b     # shouldn't duplicate, but does currently
attr(a,"bert")[2] <- 7L # shouldn't duplicate, but does currently
print(a)


# Three-level assignments.

a <- c(list (q = c(list (x = rep(3,vsize)), rep(list(1),vsize-1))),
       rep(list(2),vsize-1))

a[[1]]$x[2] <- 9     # shouldn't duplicate
a$q$x[vsize+2] <- 8  # should allocate for extension, not dup higher levels

print(a$q$x)


# Slot assignments.

Rprofmemt(NULL)
setClass ("george", representation(a="numeric",b="logical"))
Rprofmemt(nelem=vsize)

ia <- rep(1.2,vsize)
ib <- rep(c(TRUE,FALSE),length=vsize)

x <- new ("george", a=ia+1, b=ib)

v <- seq.int(1/vsize,1,length=vsize)
x@a <- v      # shouldn't duplicate, but currently dups three things
x@a[2] <- 9   # shouldn't duplicate, but currently dups four things
x@b[3] <- NA  # should duplicate at most one, but currently dups four things

Rprofmemt(NULL)
print(x)
Rprofmemt(nelem=vsize)


# Things that should use VARIANT_UNCLASS, VARIANT_ANY_ATTR, or
# VARIANT_ANY_ATTR_EX_DIM, and hence avoid some unnecessary copying.

vsize <- 11

Rprofmemt(NULL)

a <- numeric(vsize)
names(a) <- rep("a",vsize)

B <- matrix(1.1,vsize,vsize)
attr(B,"fred") <- rep("a",vsize)

o <- (1:vsize) + 0.5
class(o) <- "abc"

Rprofmemt(nelem=vsize)

for (i in a) print (i)
for (i in exp(a)) print (i)

for (i along a) print (i)
for (i along exp(a)) print(i)

for (i across B) print (i)
for (i across exp(B)) print (i)

print(sum(a))
print(sum(exp(a)))
print(prod(a))
print(prod(exp(a)))

print(length(a))
print(length(exp(a)))

print(length(o))
print(length(unclass(o)))

length.abc <- function (x) 123

print(length(o))
print(length(unclass(o)))
