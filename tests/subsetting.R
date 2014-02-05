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
