#  File src/library/base/R/by.R
#  Part of the R package, http://www.R-project.org
#  Modifications for pqR Copyright (c) 2017 Radford M. Neal.
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

by <- function(data, INDICES, FUN, ..., simplify = TRUE) UseMethod("by")

## prior to 2.7.0 this promoted vectors to data frames, but
## the data frame method dropped to a single column.
by.default <- function(data, INDICES, FUN, ..., simplify = TRUE)
{
    dd <- as.data.frame(data)
    if(length(dim(data)))
        by(dd, INDICES, FUN, ..., simplify = simplify)
    else {
        if(!is.list(INDICES)) {        # record the names for print.by
            IND <- vector("list", 1L)
            IND[[1L]] <- INDICES
            names(IND) <- deparse(substitute(INDICES))[1L]
        } else IND <- INDICES
        FUNx <- function(x) FUN(dd[x,], ...)
        nd <- nrow(dd)
        ans <- eval(substitute(tapply(1..nd, IND, FUNx, simplify = simplify)),
                    dd)
        attr(ans, "call") <- match.call()
        class(ans) <- "by"
        ans
    }
}

by.data.frame <- function(data, INDICES, FUN, ..., simplify = TRUE)
{
    if(!is.list(INDICES)) { # record the names for print.by
        IND <- vector("list", 1L)
        IND[[1L]] <- INDICES
        names(IND) <- deparse(substitute(INDICES))[1L]
    } else IND <- INDICES
    FUNx <- function(x) FUN(data[x,, drop=FALSE], ...) # (PR#10506)
    nd <- nrow(data)
    ans <- eval(substitute(tapply(1..nd, IND, FUNx, simplify = simplify)), data)
    attr(ans, "call") <- match.call()
    class(ans) <- "by"
    ans
}

print.by <- function(x, ..., vsep)
{
    d <- dim(x)
    dn <- dimnames(x)
    dnn <- names(dn)
    if(missing(vsep))
        vsep <- strrep ("-", 0.75*getOption("width"))
    lapply(X = seq_along(x), FUN = function(i, x, vsep, ...) {
        if(i != 1L && !is.null(vsep)) cat(vsep, "\n")
        ii <- i - 1L
        for(j along dn) {
            iii <- ii %% d[j] + 1L; ii <- ii %/% d[j]
            cat(dnn[j], ": ", dn[[j]][iii], "\n", sep = "")
        }
        print(x[[i]], ...)
    } , x, vsep, ...)
    invisible(x)
}
