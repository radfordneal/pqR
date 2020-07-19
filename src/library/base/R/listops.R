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

`+.list` <- function (e1,e2) 
    if (typeof(e1)=="list")
        if (missing(e2)) 
            .Internal (lapply (e1,`+`))
        else if (typeof(e2)=="list") {
            stopifnot(length(e1)==length(e2))
            stopifnot(identical(names(e1),names(e2)))
            mapply(`+`,e1,e2,SIMPLIFY=FALSE)
        }
        else {
            stopifnot(length(e2)==1)
            .Internal (lapply (e1, function (x) x+e2))
        }
    else {
        stopifnot(length(e1)==1)
        .Internal (lapply (e2, function (x) e1+x))
    }

`-.list` <- function (e1,e2) 
    if (typeof(e1)=="list")
        if (missing(e2)) 
            .Internal (lapply (e1,`-`))
        else if (typeof(e2)=="list") {
            stopifnot(length(e1)==length(e2))
            stopifnot(identical(names(e1),names(e2)))
            mapply(`-`,e1,e2,SIMPLIFY=FALSE)
        }
        else {
            stopifnot(length(e2)==1)
            .Internal (lapply (e1, function (x) x-e2))
        }
    else {
        stopifnot(length(e1)==1)
        .Internal (lapply (e2, function (x) e1-x))
    }

`*.list` <- function (e1,e2) 
    if (typeof(e1)=="list")
        if (typeof(e2)=="list") {
            stopifnot(length(e1)==length(e2))
            stopifnot(identical(names(e1),names(e2)))
            mapply(`*`,e1,e2,SIMPLIFY=FALSE)
        }
        else {
            stopifnot(length(e2)==1)
            .Internal (lapply (e1, function (x) x*e2))
        }
    else {
        stopifnot(length(e1)==1)
        .Internal (lapply (e2, function (x) e1*x))
    }

`/.list` <- function (e1,e2) 
    if (typeof(e1)=="list")
        if (typeof(e2)=="list") {
            stopifnot(length(e1)==length(e2))
            stopifnot(identical(names(e1),names(e2)))
            mapply(`/`,e1,e2,SIMPLIFY=FALSE)
        }
        else {
            stopifnot(length(e2)==1)
            .Internal (lapply (e1, function (x) x/e2))
        }
    else {
        stopifnot(length(e1)==1)
        .Internal (lapply (e2, function (x) e1/x))
    }

`^.list` <- function (e1,e2) 
    if (typeof(e1)=="list")
        if (typeof(e2)=="list") {
            stopifnot(length(e1)==length(e2))
            stopifnot(identical(names(e1),names(e2)))
            mapply(`^`,e1,e2,SIMPLIFY=FALSE)
        }
        else {
            stopifnot(length(e2)==1)
            .Internal (lapply (e1, function (x) x^e2))
        }
    else {
        stopifnot(length(e1)==1)
        .Internal (lapply (e2, function (x) e1^x))
    }

`%%.list` <- function (e1,e2) 
    if (typeof(e1)=="list")
        if (typeof(e2)=="list") {
            stopifnot(length(e1)==length(e2))
            stopifnot(identical(names(e1),names(e2)))
            mapply(`%%`,e1,e2,SIMPLIFY=FALSE)
        }
        else {
            stopifnot(length(e2)==1)
            .Internal (lapply (e1, function (x) x%%e2))
        }
    else {
        stopifnot(length(e1)==1)
        .Internal (lapply (e2, function (x) e1%%x))
    }

`%/%.list` <- function (e1,e2) 
    if (typeof(e1)=="list")
        if (typeof(e2)=="list") {
            stopifnot(length(e1)==length(e2))
            stopifnot(identical(names(e1),names(e2)))
            mapply(`%/%`,e1,e2,SIMPLIFY=FALSE)
        }
        else {
            stopifnot(length(e2)==1)
            .Internal (lapply (e1, function (x) x%/%e2))
        }
    else {
        stopifnot(length(e1)==1)
        .Internal (lapply (e2, function (x) e1%/%x))
    }

abs.list <- function (x) .Internal (lapply (x,abs))
floor.list <- function (x) .Internal (lapply (x,floor))
ceiling.list <- function (x) .Internal (lapply (x,ceiling))
sqrt.list <- function (x) .Internal (lapply (x,sqrt))
sign.list <- function (x) .Internal (lapply (x,sign))
trunc.list <- function (x) .Internal (lapply (x,trunc))
exp.list <- function (x) .Internal (lapply (x,exp))
expm1.list <- function (x) .Internal (lapply (x,expm1))
log1p.list <- function (x) .Internal (lapply (x,log1p))
log.list <- function (x) .Internal (lapply (x,log))
log2.list <- function (x) .Internal (lapply (x,log2))
log10.list <- function (x) .Internal (lapply (x,log10))
cos.list <- function (x) .Internal (lapply (x,cos))
sin.list <- function (x) .Internal (lapply (x,sin))
tan.list <- function (x) .Internal (lapply (x,tan))
acos.list <- function (x) .Internal (lapply (x,acos))
asin.list <- function (x) .Internal (lapply (x,asin))
atan.list <- function (x) .Internal (lapply (x,atan))
cosh.list <- function (x) .Internal (lapply (x,cosh))
sinh.list <- function (x) .Internal (lapply (x,sinh))
tanh.list <- function (x) .Internal (lapply (x,tanh))
acosh.list <- function (x) .Internal (lapply (x,acosh))
asinh.list <- function (x) .Internal (lapply (x,asinh))
atanh.list <- function (x) .Internal (lapply (x,atanh))
lgamma.list <- function (x) .Internal (lapply (x,lgamma))
gamma.list <- function (x) .Internal (lapply (x,gamma))
digamma.list <- function (x) .Internal (lapply (x,digamma))
trigamma.list <- function (x) .Internal (lapply (x,trigamma))
