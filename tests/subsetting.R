# Test possible bugs involving subset and subassign.
#
# Added for pqR, 2014, Radford M. Neal.

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
print(a)

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
