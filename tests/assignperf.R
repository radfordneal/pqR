# Test performance of complex assignments, by seeing what storage
# allocation is done.

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
a$b[3] <- 99.1    # will have to coerce to real, shouldn't duplicate int vec
print(a$b+1000)
a$b[3] <- 99.1    # shouldn't duplicate now
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

L <- c(list(new.env()),rep(list(1),vsize-1)
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
names(a)[2] <- "q"# shouldn't duplicate anything, but currently dups everything
print(a)
print(n)


# Three-level assignments.

a <- c(list (q = c(list (x = rep(3,vsize)), rep(list(1),vsize-1))),
       rep(list(2),vsize-1))

a[[1]]$x[2] <- 9     # shouldn't duplicate
a$q$x[vsize+2] <- 8  # should allocate for extension, not dup higher levels

print(a$q$x)

