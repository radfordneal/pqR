# Tests of operations that expand and contract vectors, perhaps in place.
#
# Added for pqR, 2017, Radford M. Neal.


# Test expansions/contractions with `length<-`

cat("LENGTH<-\n\n")

show_expansions <- function (v)
{
    n <- length(v)
    cat("** "); print(v[1:length(v)])
    length(v) <- length(v) + 1; cat("** "); print(v[1:length(v)])
    length(v) <- length(v) + 1; cat("** "); print(v[1:length(v)])
    length(v) <- 2*length(v);   cat("** "); print(v[1:length(v)])
    length(v) <- length(v)+5;   cat("** "); print(v[1:length(v)])
    length(v) <- 12345;         cat("-- "); print(c(v[1:3],v[12343:12345]))
    length(v) <- n+1;           cat("** "); print(v[1:length(v)])
    length(v) <- n;             cat("** "); print(v[1:length(v)])
    length(v) <- n-1;           cat("** "); print(v[1:length(v)])
    length(v) <- 1;             cat("** "); print(v[1:length(v)])
    length(v) <- 2;             cat("** "); print(v[1:length(v)])
}

cat("\nRaw with attribute:\n\n")
r <- as.raw(c(9,3,1,5))
attr(r,"fred") <- 999
show_expansions (r)

cat("\nLogical:\n\n")
show_expansions (c(TRUE,FALSE,TRUE,TRUE,FALSE))

cat("\nInteger:\n\n")
show_expansions (1:7)

cat("\nReal:\n\n")
show_expansions (c(3,9,1))

cat("\nReal with names:\n\n")
show_expansions (c(abc=3,def=9,xyz=1))

cat("\nComplex:\n\n")
show_expansions (c(3+1i,8+9i))

cat("\nString:\n\n")
show_expansions (paste0("a",1:5))

cat("\nList:\n\n")
show_expansions (list (a=9, b=TRUE, c="fred"))

#cat("\nPairlist:\n\n")
#show_expansions (pairlist (a=9, b=TRUE, c="fred"))


# Test expansions/contractions with `[[<-` and `$<-`

cat("\n[[<- and $<-\n\n")

L <- list(a=9,b=8,c=7,d=6,e=5,f=4)
L[[2]] <- NULL
print(L[1:length(L)])
L[["d"]] <- NULL
print(L[1:length(L)])
L[["y"]] <- 99
print(L[1:length(L)])
L$z <- 100
print(L[1:length(L)])
L$e <- NULL
print(L[1:length(L)])
L[[10]] <- 88
print(L[1:length(L)])

cat("\n****\n")
L <- list(a=9,b=8,c=7,d=6,e=5,f=4)
L[2:4] <- NULL
print(L[1:length(L)])
L <- list(a=9,b=8,c=7,d=6,e=5,f=4)
L[c(3L,4L)] <- NULL
print(L[1:length(L)])
L <- list(a=9,b=8,c=7,d=6,e=5,f=4)
n <- names(L)
L[2:4] <- NULL
print(L[1:length(L)])

cat("\n****\n")
L <- list(a=9,b=8,c=7,d=6,e=5,f=4)
n <- names(L)
L[[3]] <- NULL
print(L[1:length(L)])
print(n)

cat("\n****\n")
M <- matrix(list(1,TRUE,"a",3i),2,2)
M[[2]] <- NULL
print(M)
M <- matrix(list(1,TRUE,"a",3i),2,2)
M[[5]] <- "x"
print(length(M))
print(M)

# Test expansions/contractions with `[<-`

cat("\n****\n")
v <- c(a=8,b=9,c=1,d=3)
print(v[1:length(v)])
v[8] <- 7
print(v[1:length(v)])
v[4:11] <- 6
print(v[1:length(v)])
v[14] <- c(x=99)
print(v[1:length(v)])
