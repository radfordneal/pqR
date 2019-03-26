#  File src/library/base/R/solve.R
#  Part of the R package, http://www.R-project.org
#  Modifications for pqR Copyright (c) 2019 Radford M. Neal.
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

solve.qr <- function(a, b, ...)
{
    if( !is.qr(a) )
	stop("this is the \"qr\" method for the generic function solve()")
    nc <- ncol(a$qr); nr <- nrow(a$qr)
    if( a$rank != min(nc, nr) )
    if( a$rank != nc )
	stop("singular matrix 'a' in 'solve'")
    if( missing(b) ) {
	if( nc != nr )
	    stop("only square matrices can be inverted")
	b <- diag(1, nc)
    }
    res <- qr.coef(a, b)
    res[is.na(res)] <- 0
    res
}

solve.default <-
    function(a, b, tol = ifelse(LINPACK, 1e-7, .Machine$double.eps),
	     LINPACK = FALSE, ...)
{
    if (is.complex(a) || (!missing(b) && is.complex(b))) {
        a <- as.matrix(a)
        if (missing(b)) {
            if(nrow(a) != ncol(a))
                stop("only square matrices can be inverted")
            b <- diag(1.0+0.0i, nrow(a))
            colnames(b) <- rownames(a)
        }
        else if (!is.complex(b))
            b[] <- as.complex(b)
        if (!is.complex(a))
            a[] <- as.complex(a)
        return (
            if (is.matrix(b)) {
                if (ncol(a) != nrow(b)) stop("'b' must be compatible with 'a'")
                rownames(b) <- colnames(a)
                .Internal (La_zgesv (a, b))
            }
            else
                drop (.Internal (La_zgesv (a, as.matrix(b))))
        )
    }

    if (is.qr(a)) {
        warning("solve.default called with a \"qr\" object: use 'qr.solve'")
        return (solve.qr(a, b, tol))
    }

    a <- as.matrix(a)
    storage.mode(a) <- "double"

    if (missing(b)) {
        if (nrow(a) != ncol(a))
            stop("only square matrices can be inverted")
        b <- diag(1, nrow(a))
        colnames(b) <- rownames(a)
    } 
    else 
        storage.mode(b) <- "double"

    compute gradient (a, b) {
        if (!LINPACK) {
            if (is.matrix(b)) {
                if (ncol(a) != nrow(b)) stop("'b' must be compatible with 'a'")
                rownames(b) <- colnames(a)
                r <- .Internal (La_dgesv (a, b, tol))
            }
            else
                r <- drop (.Internal (La_dgesv (a, as.matrix(b), tol)))
        }
        else {
            a <- qr(a, tol = tol)
            nc <- ncol(a$qr)
            if (a$rank != nc)
                stop("singular matrix 'a' in 'solve'")
            r <- qr.coef(a, b)
        }
        r
    }
    as ({
        if (!exists("inv",inherits=FALSE)) {
            nv <- if (is.matrix(b)) ncol(b) else 1
            na <- nrow(a)
            inv <- solve.default (a, tol = tol, LINPACK = LINPACK)
        }
        g <- matrix (0, na*nv, na^2)
        for (j in 1..na) {
            tmp <- inv[j,_] %*% b
            for (i in 1..na)
                g[_,(j-1)*na+i] <- inv[_,i] %*% tmp
        }
        -g
    },
    {
        if (!exists("inv",inherits=FALSE)) {
            nv <- if (is.matrix(b)) ncol(b) else 1
            na <- nrow(a)
            inv <- solve.default (a, tol = tol, LINPACK = LINPACK)
        }
        g <- matrix (0, na*nv, na*nv)
        for (i in 1..nv) 
            g [(i-1)*na + (1..na), (i-1)*na + (1..na)] <- inv
        g
    })
}

solve <- function(a, b, ...) UseMethod("solve")

qr.solve <- function(a, b, tol = 1e-7)
{
    if( !is.qr(a) )
	a <- qr(a, tol = tol)
    nc <- ncol(a$qr); nr <- nrow(a$qr)
    if( a$rank != min(nc, nr) )
	stop("singular matrix 'a' in solve")
    if( missing(b) ) {
	if( nc != nr )
	    stop("only square matrices can be inverted")
	b <- diag(1, nc)
    }
    res <- qr.coef(a, b)
    res[is.na(res)] <- 0
    res
}

