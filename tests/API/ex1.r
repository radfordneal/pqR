# A test function for the C API.
#
# Part of pqR.
# Copyright 2016 by Radford M. Neal.  
# Distributed under GPL version 2 or later.


# R and C implementations of a function for concatenating vectors, in
# which the last element of one of these vectors must be the same as
# the first element of the next vector, with this repeated element
# present only once.  For instance, it might be used to concatenate
# annual figures for 1900 to 1910 with annual figures for 1910 to 1920
# in order to produce annual figures for 1900 to 1920.
#
# The arguments must all be integer, double, or character vectors of
# non-zero length, which are coerced to the highest of these types (ie,
# character if any are character, otherwise double if any are double,
# otherwise integer).  An error is signaled if the arguments are not
# of the right form, or if the last element of a vector does not
# match the first element of the next vector.

dyn.load(paste0("ex1",.Platform$dynlib.ext))

ex1_Call_old <- function (...) .Call("ex1_Call_old",list(...))

ex1_Call_new <- function (...) .Call("ex1_Call_new",list(...))

ex1_Call_new2 <- function (...) .Call("ex1_Call_new2",list(...))


# R implementation.

ex1_R <- function (...)
{
    L <- list(...)

    if (length(L) == 0)
        stop ("nothing to concatenate")

    if (!all (sapply (L, is.numeric) | sapply (L, is.character)))
        stop ("argument is not numeric or character")

    if (!all (sapply (L, length) > 0))
        stop ("argument has zero length")

    R <- L

    for (i in 2..length(L)) {
        if (L[[i-1]][length(L[[i-1]])] != L[[i]][1])
            stop ("end of argument ",i-1," doesn't match start of argument ",i)
        R[[i]] <- R[[i]][-1]
    }

    c (R, recursive=TRUE)
}


# Test function.  Passed one of the functions defined above.

test_ex1 <- function (f) 
{
    r <- f (1..10,10..15,15..25)
    cat("type",typeof(r),"\nvalue",r,"\n")
    r <- f (1..10,10,10,c(10,20))
    cat("type",typeof(r),"\nvalue",r,"\n")
    r <- f (c("x","y"),c("y","z","99"),99..100,c("100","y","z"))
    cat("type",typeof(r),"\nvalue",r,"\n")
    r <- f (9)
    cat("type",typeof(r),"\nvalue",r,"\n")
    try (f(function()0))
    try (f(1..10,10..9,10..20))
    try (f(1..10,11..20))
    try (f(c("x"),c("x","y"),c("a")))
}
