#  File src/library/base/R/listops.R
#  Part of pqR.  
#  Copyright (c) 2020 Radford M. Neal.
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/


# R functions implementing arithmetic/mathematical operators/functions on
# list arguments, recursively applying operations to elements.

local ({ 

template <- quote (function (e1,e2) 
    if (missing(e2)) {
        for (i along e1) e1[[i]] <- FUN(e1[[i]])
        e1
    }
    else if (typeof(e2)=="list") {
        if (typeof(e1)=="list") {
            stopifnot(length(e1)==length(e2))
            stopifnot(identical(names(e1),names(e2)))
            for (i along e2) e2[[i]] <- FUN(e1[[i]],e2[[i]])
            a <- attributes(e1)
            n <- names(a)
            for (i along a) attr(e2,n[i]) <- a[[i]]
        }
        else {
            stopifnot(length(e1)==1)
            for (i along e2) e2[[i]] <- FUN(e1,e2[[i]])
        }
        e2
    }
    else {
        stopifnot(length(e2)==1)
        for (i along e1) e1[[i]] <- FUN(e1[[i]],e2)
        e1
    }
)

assign ("+.list",
        eval (do.call (substitute, list (template, list(FUN=quote(`+`))))),
        envir=baseenv())

assign ("-.list",
        eval (do.call (substitute, list (template, list(FUN=quote(`-`))))),
        envir=baseenv())

template <- quote (function (e1,e2) 
    if (typeof(e2)=="list") {
        if (typeof(e1)=="list") {
            stopifnot(length(e1)==length(e2))
            stopifnot(identical(names(e1),names(e2)))
            for (i along e2) e2[[i]] <- FUN(e1[[i]],e2[[i]])
            a <- attributes(e1)
            n <- names(a)
            for (i along a) attr(e2,n[i]) <- a[[i]]
        }
        else {
            stopifnot(length(e1)==1)
            for (i along e2) e2[[i]] <- FUN(e1,e2[[i]])
        }
        e2
    }
    else {
        stopifnot(length(e2)==1)
        for (i along e1) e1[[i]] <- FUN(e1[[i]],e2)
        e1
    }
)

assign ("*.list",
        eval (do.call (substitute, list (template, list(FUN=quote(`*`))))),
        envir=baseenv())

assign ("/.list",
        eval (do.call (substitute, list (template, list(FUN=quote(`/`))))),
        envir=baseenv())

assign ("^.list",
        eval (do.call (substitute, list (template, list(FUN=quote(`^`))))),
        envir=baseenv())

assign ("%%.list",
        eval (do.call (substitute, list (template, list(FUN=quote(`%%`))))),
        envir=baseenv())

assign ("%/%.list",
        eval (do.call (substitute, list (template, list(FUN=quote(`%/%`))))),
        envir=baseenv())

assign ("atan2.list",
        eval (do.call (substitute, list (template, list(FUN=quote(atan2))))),
        envir=baseenv())

assign ("round.list",
        eval (do.call (substitute, list (template, list(FUN=quote(round))))),
        envir=baseenv())

assign ("signif.list",
        eval (do.call (substitute, list (template, list(FUN=quote(signif))))),
        envir=baseenv())

assign ("log.list",
        eval (do.call (substitute, list (template, list(FUN=quote(log))))),
        envir=baseenv())
})

trunc.list    <- function (x,...) 
                 { for (i along x) x[[i]] <- trunc (x[[i]], ...); x }

abs.list      <- function (x) { for (i along x) x[[i]] <- abs (x[[i]]); x }
floor.list    <- function (x) { for (i along x) x[[i]] <- floor (x[[i]]); x }
ceiling.list  <- function (x) { for (i along x) x[[i]] <- ceiling (x[[i]]); x }
sqrt.list     <- function (x) { for (i along x) x[[i]] <- sqrt (x[[i]]); x }
sign.list     <- function (x) { for (i along x) x[[i]] <- sign (x[[i]]); x }
exp.list      <- function (x) { for (i along x) x[[i]] <- exp (x[[i]]); x }
expm1.list    <- function (x) { for (i along x) x[[i]] <- expm1 (x[[i]]); x }
log1p.list    <- function (x) { for (i along x) x[[i]] <- log1p (x[[i]]); x }
log2.list     <- function (x) { for (i along x) x[[i]] <- log2 (x[[i]]); x }
log10.list    <- function (x) { for (i along x) x[[i]] <- log10 (x[[i]]); x }
cos.list      <- function (x) { for (i along x) x[[i]] <- cos (x[[i]]); x }
sin.list      <- function (x) { for (i along x) x[[i]] <- sin (x[[i]]); x }
tan.list      <- function (x) { for (i along x) x[[i]] <- tan (x[[i]]); x }
acos.list     <- function (x) { for (i along x) x[[i]] <- acos (x[[i]]); x }
asin.list     <- function (x) { for (i along x) x[[i]] <- asin (x[[i]]); x }
atan.list     <- function (x) { for (i along x) x[[i]] <- atan (x[[i]]); x }
cosh.list     <- function (x) { for (i along x) x[[i]] <- cosh (x[[i]]); x }
sinh.list     <- function (x) { for (i along x) x[[i]] <- sinh (x[[i]]); x }
tanh.list     <- function (x) { for (i along x) x[[i]] <- tanh (x[[i]]); x }
acosh.list    <- function (x) { for (i along x) x[[i]] <- acosh (x[[i]]); x }
asinh.list    <- function (x) { for (i along x) x[[i]] <- asinh (x[[i]]); x }
atanh.list    <- function (x) { for (i along x) x[[i]] <- atanh (x[[i]]); x }
lgamma.list   <- function (x) { for (i along x) x[[i]] <- lgamma (x[[i]]); x }
gamma.list    <- function (x) { for (i along x) x[[i]] <- gamma (x[[i]]); x }
digamma.list  <- function (x) { for (i along x) x[[i]] <- digamma (x[[i]]); x }
trigamma.list <- function (x) { for (i along x) x[[i]] <- trigamma (x[[i]]); x }
