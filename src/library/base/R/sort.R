#  File src/library/base/R/sort.R
#  Part of the R package, https://www.R-project.org
#  Modifications for pqR Copyright (c) 2017, 2018 Radford M. Neal.
#
#  Copyright (C) 1995-2016 The R Core Team
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
#  https://www.R-project.org/Licenses/

sort <- function(x, decreasing = FALSE, ...)

    if (!is.logical(decreasing) || length(decreasing) != 1L)
        stop("'decreasing' must be a length-1 logical vector.\nDid you intend to set 'partial'?")
    else
        UseMethod("sort")


sort.default <- function(x, decreasing = FALSE, na.last = NA, ...)

    ## The first case includes factors.
    if (is.object(x)) 
        x[order(x, na.last = na.last, decreasing = decreasing)]
    else 
        sort.int(x, na.last = na.last, decreasing = decreasing, ...)


sort.int <-
    function(x, partial = NULL, na.last = NA, decreasing = FALSE,
             method = c("auto", "merge", "shell", "quick", "radix"),
             index.return = FALSE)
{
    method <- if (missing(method)) "auto" 
      else match.arg (method, c("auto", "merge", "shell", "quick", "radix"))

    if (method == "auto") {
        if (is.null(partial) 
             && (is.numeric(x) || is.factor(x) || is.logical(x)))
            method <- if (length(x) < 500) "shell" else "radix"
        else
            method <- "merge"
    }
    else if (method == "quick" && !is.numeric(x))
        method = "merge" # explicitly prevent 'quick' for non-numeric data

    if (method == "radix") {
        if (!is.null(partial)) {
            stop("'partial' sorting not supported by radix method")
        }
        if (index.return && is.na(na.last)) {
            if (any(is.na(x))) x <- x[!is.na(x)]
            na.last <- TRUE
        }
        o <- order(x, na.last = na.last, decreasing = decreasing,
                   method = "radix")
        y <- x[o]
        return (if (index.return) list(x = y, ix = o) else y)
    }

    if (isfact <- is.factor(x)) {
        if(index.return) stop("'index.return' only for non-factors")
	lev <- levels(x)
	nlev <- nlevels(x)
 	isord <- is.ordered(x)
        x <- c(x) # drop attributes
    }
    else if(!is.atomic(x))
        stop("'x' must be atomic")

    has.na <- any(is.na(x))
    if (has.na) {
        ina <- is.na(x)
        if (!is.na(na.last)) nas <- x[ina]
        x <-  x[!ina]
    }

    if (index.return && !is.na(na.last))
        stop("'index.return' only for 'na.last = NA'")

    if (!is.null(partial)) {
        if (index.return || decreasing || isfact
                         || method != "shell" && method != "merge")
	    stop("unsupported options for partial sorting")
        if (!all(is.finite(partial))) stop("non-finite 'partial'")
        y <- if (length(partial) <= 10L) {
                partial <- .Internal(qsort(partial, FALSE))
               .Internal(psort(x, partial))
             } else if (is.double(x)) .Internal(qsort(x, FALSE))
             else .Internal (sort (x, FALSE, method))
    }
    else { # not partial
        nms <- names(x)
        if (method == "quick") {
            if (!is.null(nms)) {
                if(decreasing) x <- -x
                y <- .Internal(qsort(x, TRUE))
                if (decreasing) y$x <- -y$x
                names(y$x) <- nms[y$ix]
                if (!index.return) y <- y$x
            } 
            else {
                if (decreasing) x <- -x
                y <- .Internal(qsort(x, index.return))
                if (decreasing)
                    if (index.return) y$x <- -y$x else y <- -y
            }
        }
        else { # method == "shell" || method == "merge"
            if (index.return || !is.null(nms)) {
                o <- sort.list(x, decreasing=decreasing, method=method)
                y <- if (index.return) list(x = x[o], ix = o) else x[o]
            }
            else
                y <- .Internal(sort(x, decreasing, method))
        }
    }

    if (!is.na(na.last) && has.na)
        y <- if (na.last) c(y, nas) else c(nas, y)
    if (isfact)
        y <- 
         (if (isord) ordered else factor) (y, levels=seq_len(nlev), labels=lev)
    y
}

order <- function(..., na.last = TRUE, decreasing = FALSE,
                  method = c("auto", "merge", "shell", "radix"))
{
    z <- list(...)

    method <- if (missing(method)) "auto" 
              else match.arg (method, c("auto", "merge", "shell", "radix"))
    if (method == "auto") {
        method <- "merge"
        if (length(z) == 1) {
            x <- z[[1]]
            if (is.numeric(x) || is.factor(x) || is.logical(x))
                method <- if (length(x) < 200) "shell" else "radix"
        }
        else
            if (all (vapply (z, 
                 function (x) is.numeric(x) || is.factor(x) || is.logical(x),
                 TRUE)))
                method <- "radix"
    }

    if(any(unlist(lapply(z, is.object)))) {
        z <- lapply(z, function(x) if(is.object(x)) as.vector(xtfrm(x)) else x)
        if(method == "radix" || !is.na(na.last))
            return(do.call("order", c(z, na.last = na.last,
                                      decreasing = decreasing,
                                      method = method)))
    } else if(method != "radix" && !is.na(na.last)) {
        return (.Internal (order (method, na.last, decreasing, ...)))
    }

    if (method == "radix") {
        decreasing <- rep_len(as.logical(decreasing), length(z))
        return(.Internal(radixsort(na.last, decreasing, FALSE, TRUE, ...)))
    }

    ## na.last = NA case: remove nas
    if(any(diff((l.z <- lengths(z)) != 0L)))
        stop("argument lengths differ")
    na <- vapply(z, is.na, rep.int(NA, l.z[1L]))
    ok <- if(is.matrix(na)) rowSums(na) == 0L else !any(na)
    if(all(!ok)) return(integer())
    z[[1L]][!ok] <- NA
    ans <- do.call("order", c(z, decreasing = decreasing))
    ans[ok[ans]]
}

sort.list <- function(x, partial = NULL, na.last = TRUE, decreasing = FALSE,
                      method = c("auto", "merge", "shell", "quick", "radix"))
{
    method <- if (missing(method)) "auto" 
      else match.arg (method, c("auto", "merge", "shell", "quick", "radix"))
    if (method == "auto") {
        if (is.numeric(x) || is.factor(x) || is.logical(x))
            method <- if (length(x) < 200) "shell" else "radix"
        else
            method <- "merge"
    }
    if(!is.atomic(x))
        stop("'x' must be atomic for 'sort.list'\nHave you called 'sort' on a list?")
    if(!is.null(partial))
        .NotYetUsed("partial != NULL")
    if(method == "quick") {
        if(is.factor(x)) x <- as.integer(x) # sort the internal codes
        if(is.numeric(x))
            return(sort(x, na.last = na.last, decreasing = decreasing,
                        method = "quick", index.return = TRUE)$ix)
        else stop("method = \"quick\" is only for numeric 'x'")
    }
    if (is.na(na.last)) {
        x <- x[!is.na(x)]
        na.last <- TRUE
    }
    if(method == "radix") {
        return(order(x, na.last=na.last, decreasing=decreasing, method="radix"))
    }
    ## method is "shell" or "merge"
    .Internal (order (method, na.last, decreasing, x))
}


## xtfrm is now primitive
## xtfrm <- function(x) UseMethod("xtfrm")
xtfrm.default <- function(x)
    if(is.numeric(x)) unclass(x) else as.vector(rank(x, ties.method = "min",
                                                     na.last = "keep"))
xtfrm.factor <- function(x) as.integer(x) # primitive, so needs a wrapper
xtfrm.Surv <- function(x)
    order(if(ncol(x) == 2L) order(x[,1L], x[,2L]) else order(x[,1L], x[,2L], x[,3L])) # needed by 'party'
xtfrm.AsIs <- function(x)
{
    if(length(cl <- class(x)) > 1) oldClass(x) <- cl[-1L]
    NextMethod("xtfrm")
}

## callback from C code for rank/order
.gt <- function(x, i, j)
{
    xi <- x[i]; xj <- x[j]
    if (xi == xj) 0L else if(xi > xj) 1L else -1L;
}

## callback for C code for is.unsorted, hence negation.
.gtn <- function(x, strictly)
{
    n <- length(x)
    if(strictly) !all(x[-1L] > x[-n]) else !all(x[-1L] >= x[-n])
}

grouping <- function(...) {
    z <- list(...)
    if(any(vapply(z, is.object, logical(1L)))) {
        z <- lapply(z, function(x) if(is.object(x)) as.vector(xtfrm(x)) else x)
        return(do.call("grouping", z))
    }
    nalast <- TRUE
    decreasing <- rep_len(FALSE, length(z))
    group <- TRUE
    sortStr <- FALSE
    return(.Internal(radixsort(nalast, decreasing, group, sortStr, ...)))
}
