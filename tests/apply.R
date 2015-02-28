# Test possible bugs involving the various apply functions.
#
# Added for pqR, 2015, Radford M. Neal.

# Check basic function, with and without extra arguments.

f1 <- function (x) sum(x)+1.23
f2 <- function (x,a) sum(x)+a

L <- list (a=3, b=c(1,7), c=c(5,2,9))

cat("\n-- lapply:\n")
print(lapply(L,f1))
print(lapply(L,f2,101.23))

cat("\n-- vapply:\n")
print(vapply(L,f1,numeric(1)))
print(vapply(L,f2,numeric(1),101.23))

cat("\n-- eapply:\n")
print(eapply(as.environment(L),f1))
print(eapply(as.environment(L),f2,101.23))

cat("\n-- apply:\n")
M <- matrix (1:12, 3, 4)
print(M)
print(apply(M,1,f1))
print(apply(M,2,f1))
print(apply(M,1,f2,101.23))
print(apply(M,2,f2,101.23))

# Check that delayed warnings refer to [[1L]] and [[3L]].  (They don't
# if later calls modifiy earlier calls.)  The first set of warnings are
# the undelayed ones, followed at the end of this script or end of this
# section (depending on how it's run) by the delayed versions.

cat("\n-- checking warnings:\n")
for (w in c(1,0)) {
    options(warn=w)
    print(lapply(c(-1,2,-1),sqrt))  
    print(vapply(c(-1,2,-1),sqrt,numeric(1)))
    print(eapply(as.environment(list(a=-1,b=2,c=-1)),sqrt))
    M <- matrix(-1,3,4)
    print(apply(M,1,sqrt))
}

# Test that indexed value is corectly retained when the applied function
# returns a function that references it.

cat("\n-- checking function environments:\n")

fns <- lapply (11:13, function(x) function () x)
print(fns)
print(c(fns[[1]](),fns[[2]](),fns[[3]]()))

fns <- vapply (11:13, function(x) list(function () x), list (function () 0))
print(fns)
print(c(fns[[1]](),fns[[2]](),fns[[3]]()))

fns <- eapply (as.environment(list(a=11,b=12,c=13)), function(x) function () x)
print(fns)
print(c(fns[[1]](),fns[[2]](),fns[[3]]()))

fns <- apply (matrix(11:13,3,1), 1, function(x) function () x)
print(fns)
print(c(fns[[1]](),fns[[2]](),fns[[3]]()))

