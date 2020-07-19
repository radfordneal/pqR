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

`atan2.list` <- function (y,x) 
    if (typeof(y)=="list")
        if (typeof(x)=="list") {
            stopifnot(length(y)==length(x))
            stopifnot(identical(names(y),names(x)))
            mapply(`atan2`,y,x,SIMPLIFY=FALSE)
        }
        else {
            stopifnot(length(x)==1)
            .Internal (lapply (y, function (z) atan2(z,x)))
        }
    else {
        stopifnot(length(y)==1)
        .Internal (lapply (x, function (z) atan2(y,z)))
    }

`round.list` <- function (x,digits) 
    if (typeof(x)=="list")
        if (missing(digits)) 
            .Internal (lapply (x,`round`))
        else if (typeof(digits)=="list") {
            stopifnot(length(x)==length(digits))
            stopifnot(identical(names(x),names(digits)))
            mapply(`round`,x,digits,SIMPLIFY=FALSE)
        }
        else {
            stopifnot(length(digits)==1)
            .Internal (lapply (x, function (z) round(z,digits)))
        }
    else {
        stopifnot(length(x)==1)
        .Internal (lapply (digits, function (z) round(x,z)))
    }

`signif.list` <- function (x,digits) 
    if (typeof(x)=="list")
        if (missing(digits)) 
            .Internal (lapply (x,`signif`))
        else if (typeof(digits)=="list") {
            stopifnot(length(x)==length(digits))
            stopifnot(identical(names(x),names(digits)))
            mapply(`signif`,x,digits,SIMPLIFY=FALSE)
        }
        else {
            stopifnot(length(digits)==1)
            .Internal (lapply (x, function (z) signif(z,digits)))
        }
    else {
        stopifnot(length(x)==1)
        .Internal (lapply (digits, function (z) signif(x,z)))
    }

`log.list` <- function (x,base) 
    if (typeof(x)=="list")
        if (missing(base)) 
            .Internal (lapply (x,`log`))
        else if (typeof(base)=="list") {
            stopifnot(length(x)==length(base))
            stopifnot(identical(names(x),names(base)))
            mapply(`log`,x,base,SIMPLIFY=FALSE)
        }
        else {
            stopifnot(length(base)==1)
            .Internal (lapply (x, function (z) log(z,base)))
        }
    else {
        stopifnot(length(x)==1)
        .Internal (lapply (base, function (z) log(x,z)))
    }

abs.list <- function (x) .Internal (lapply (x,abs))
floor.list <- function (x) .Internal (lapply (x,floor))
ceiling.list <- function (x) .Internal (lapply (x,ceiling))
sqrt.list <- function (x) .Internal (lapply (x,sqrt))
sign.list <- function (x) .Internal (lapply (x,sign))
trunc.list <- function (x,...) .Internal (lapply (x,trunc)) # accesses ...
exp.list <- function (x) .Internal (lapply (x,exp))
expm1.list <- function (x) .Internal (lapply (x,expm1))
log1p.list <- function (x) .Internal (lapply (x,log1p))
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
