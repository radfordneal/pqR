#  File src/library/base/R/pmax.R
#  Part of the R package, http://www.R-project.org
#  Modifications for pqR Copyright (c) 2013, 2017, 2018 Radford M. Neal.
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

pmin.int <- function(..., na.rm = FALSE) .Internal(pmin(na.rm, ...))
pmax.int <- function(..., na.rm = FALSE) .Internal(pmax(na.rm, ...))

pmax <- function (..., na.rm = FALSE)
{
    # Handle the typical case of two arguments specially for speed. 

    if (!missing(..2) && missing(..3)
          && is.atomic(a<-..1) && !is.object(a) 
          && is.atomic(b<-..2) && !is.object(b)) {
        mmm <- .Internal (pmax (na.rm, a, b))
        if (!is.null(attributes(a)))
            mostattributes(mmm) <- attributes(a)
    }

    else {  # the general case

        elts <- list(...)
        if (length (elts) == 0L) 
            stop("no arguments")
    
        if (all(vapply(elts, function(x) is.atomic(x) && !is.object(x), NA)))
            mmm <- .Internal(pmax(na.rm, ...))
        else {
            mmm <- elts[[1L]]
            attr(mmm, "dim") <- NULL  # dim<- would drop names
            has.na <- FALSE
            for (each in elts[-1L]) {
                attr(each, "dim") <- NULL
                l1 <- length(each); l2 <- length(mmm)
                if(l2 < l1) {
                    if (l2 && l1 %% l2)
                        warning("an argument will be fractionally recycled")
                    mmm <- rep(mmm, length.out = l1)
                } else if(l1 && l1 < l2) {
                    if (l2 %% l1)
                        warning("an argument will be fractionally recycled")
                    each <- rep(each, length.out = l2)
                }
                nas.mmm <- is.na(mmm)
                nas.each <- is.na(each)
                if(has.na || (has.na <- any(nas.mmm) || any(nas.each))) {
                    mmm[nas.mmm] <- each[nas.mmm]
                    each[nas.each] <- mmm[nas.each]
                }
                change <- mmm < each
                change <- change & !is.na(change)
                mmm[change] <- each[change]
                if (has.na && !na.rm) mmm[nas.mmm | nas.each] <- NA
            }
        }

        if (!is.null(attributes(elts[[1]])))
            mostattributes(mmm) <- attributes(elts[[1]])
    }

    get_rm(mmm)
}

pmin <- function (..., na.rm = FALSE)
{
    # Handle the typical case of two arguments specially for speed. 

    if (!missing(..2) && missing(..3)
          && is.atomic(a<-..1) && !is.object(a) 
          && is.atomic(b<-..2) && !is.object(b)) {
        mmm <- .Internal (pmin (na.rm, a, b))
        if (!is.null(attributes(a)))
            mostattributes(mmm) <- attributes(a)
    }

    else {  # the general case

        elts <- list(...)
        if (length (elts) == 0L) 
            stop("no arguments")
    
        if (all(vapply(elts, function(x) is.atomic(x) && !is.object(x), NA)))
            mmm <- .Internal(pmin(na.rm, ...))
        else {
            mmm <- elts[[1L]]
            attr(mmm, "dim") <- NULL  # dim<- would drop names
            has.na <- FALSE
            for (each in elts[-1L]) {
                attr(each, "dim") <- NULL
                l1 <- length(each); l2 <- length(mmm)
                if(l2 < l1) {
                    if (l2 && l1 %% l2)
                        warning("an argument will be fractionally recycled")
                    mmm <- rep(mmm, length.out = l1)
                } else if(l1 && l1 < l2) {
                    if (l2 %% l1)
                        warning("an argument will be fractionally recycled")
                    each <- rep(each, length.out = l2)
                }
                nas.mmm <- is.na(mmm)
                nas.each <- is.na(each)
                if(has.na || (has.na <- any(nas.mmm) || any(nas.each))) {
                    mmm[nas.mmm] <- each[nas.mmm]
                    each[nas.each] <- mmm[nas.each]
                }
                change <- mmm > each
                change <- change & !is.na(change)
                mmm[change] <- each[change]
                if (has.na && !na.rm) mmm[nas.mmm | nas.each] <- NA
            }
        }

        if (!is.null(attributes(elts[[1]])))
            mostattributes(mmm) <- attributes(elts[[1]])
    }

    get_rm(mmm)
}
