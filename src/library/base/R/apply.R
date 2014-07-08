#  File src/library/base/R/apply.R
#  Part of the R package, http://www.R-project.org
#  Modifications for pqR Copyright (c) 2013 Radford M. Neal.
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

apply <- function(X, MARGIN, FUN, ...)
{
    FUN <- match.fun(FUN)

    ## Ensure that X is an array object
    dl <- length(dim(X))
    if(!dl) stop("dim(X) must have a positive length")
    if(is.object(X))
	X <- if(dl == 2L) as.matrix(X) else as.array(X)
    ## now record dim (not before) as coercion above can change it
    ## (e.g. when a data frame contains a matrix).
    d <- dim(X)
    dn <- dimnames(X)
    ds <- seq_along(d)

    ## Extract the margins and associated dimnames

    if (is.character(MARGIN)) {
        if(is.null(dnn <- names(dn))) # names(NULL) is NULL
           stop("'X' must have named dimnames")
        MARGIN <- match(MARGIN, dnn)
        if (any(is.na(MARGIN)))
            stop("not all elements of 'MARGIN' are names of dimensions")
    }

    if (length(MARGIN) != length(unique(MARGIN))
     || !all(MARGIN %in% ds) && !all(-MARGIN %in% ds))
        stop("Bad 'MARGIN' argument")

    s.ans  <- ds[MARGIN]
    s.call <- if (length(s.ans)) ds[-s.ans] else ds

    d.call <- d[s.call]
    d.ans  <- d[s.ans]
    dn.call<- dn[s.call]
    dn.ans <- dn[s.ans]

    d2 <- prod(d.ans)

    ## For arrays with some 0 extents: return ``empty result'' trying
    ## to use proper mode and dimension:

    if(d2 == 0L) {
        ## The following is still a bit `hackish': use non-empty X
        X <- array(vector(typeof(X), 1L), dim = c(prod(d.call), 1L))
        ans <- FUN(if(length(d.call) < 2L) X[,1] else
                   array(X[, 1L], d.call, dn.call), ...)
        return(if(is.null(ans)) ans else if(length(d.ans) < 2L) ans[1L][-1L]
               else array(ans, d.ans, dn.ans))
    }

    # Make the function calls.

    ans <- vector("list", d2)

    if (length(d) == 2 && length(s.ans) == 1 && !is.object(X)) { 

        # Quick version for a matrix.

        if (length(dn.call))
            dimnames(X) <- 
              if (s.call==1) c(dn.call, list(NULL)) else c(list(NULL), dn.call)

        if (s.ans == 1)
            if (missing(...))
                for (i in 1L:d2) {
                    tmp <- FUN (X[i,])
                    if (!is.null(tmp)) ans[[i]] <- tmp
                }
            else
                for (i in 1L:d2) {
                    tmp <- FUN (X[i,], ...)
                    if (!is.null(tmp)) ans[[i]] <- tmp
                }
        else
            if (missing(...))
                for (i in 1L:d2) {
                    tmp <- FUN (X[,i])
                    if (!is.null(tmp)) ans[[i]] <- tmp
                }
            else
                for (i in 1L:d2) {
                    tmp <- FUN (X[,i], ...)
                    if (!is.null(tmp)) ans[[i]] <- tmp
                }
    }

    else {

        # General version.

        X <- aperm(X, c(s.call, s.ans))
        dim(X) <- c(prod(d.call), d2)
    
        if(length(d.call) < 2L) {# vector
            if (length(dn.call)) 
                dimnames(X) <- c(dn.call, list(NULL))
            if (missing(...))
                for (i in 1L:d2) {
                    tmp <- FUN(X[,i])
                    if (!is.null(tmp)) ans[[i]] <- tmp
                }
            else
                for (i in 1L:d2) {
                    tmp <- FUN(X[,i], ...)
                    if (!is.null(tmp)) ans[[i]] <- tmp
                }
        } 
        else
            if (missing(...))
                for (i in 1L:d2) {
                    tmp <- FUN(array(X[,i], d.call, dn.call))
                    if (!is.null(tmp)) ans[[i]] <- tmp
                }
            else
                for (i in 1L:d2) {
                    tmp <- FUN(array(X[,i], d.call, dn.call), ...)
                    if (!is.null(tmp)) ans[[i]] <- tmp
                }
    }

    X <- NULL

    ## answer dims and dimnames

    ans.list <- is.recursive(ans[[1L]]) ||
                any (unlist(lapply(ans,length)) != length(ans[[1L]]))

    if (ans.list)
        len.a <- d2
    else {
        ans.names <- names(ans[[1L]])
        if (length(ans.names) &&
             !all (vapply (ans, function(x) identical(names(x),ans.names), NA)))
            ans.names <- NULL
        ans <- unlist (ans, recursive = FALSE)
        len.a <- length(ans)
    }

    if (len.a == d2) {
        if (length(s.ans) == 1L)
            names(ans) <- if (length(dn.ans[[1L]])) dn.ans[[1L]] else NULL
        else
            ans <- array(ans, d.ans, dn.ans)
    }
    else if (len.a && len.a %% d2 == 0L) {
        if(is.null(dn.ans)) dn.ans <- vector(mode="list", length(d.ans))
        dn.ans <- c(list(ans.names), dn.ans)
        ans <- array (ans, c(len.a %/% d2, d.ans),
                      if(!all(vapply(dn.ans, is.null, NA))) dn.ans)
    }

    get_rm(ans)
}
