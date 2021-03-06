#  File src/library/base/R/array.R
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

array <- function(data = NA, dim = length(data), dimnames = NULL)

    ## Changed as in R-3.4.1, with some mods (e.g. use of get_rm).

    ## allow for as.vector.factor (converts to character)
    if (is.atomic(data) && !is.object(data))
        .Internal (array (data, dim, dimnames))
    else {
        data <- as.vector(data)
        ## package rv has as.vector() method that leaves this as a classed list
        if (is.object(data)) {
            data <- as.vector(data)
            dim <- as.integer(dim)
            vl <- prod(dim)
            if(length(data) != vl) {
                if(vl > .Machine$integer.max)
                    stop("'dim' specifies too large an array")
                data <- rep(data, length.out=vl)
            }
            if(length(dim))
        	dim(data) <- dim
            if(is.list(dimnames) && length(dimnames))
        	dimnames(data) <- dimnames
            get_rm(data)
        }
        else
            .Internal (array (data, dim, dimnames))
    }


slice.index <-
function(x, MARGIN)
{
    d <- dim(x)
    if(is.null(d))
        d <- length(x)
    n <- length(d)

    if((length(MARGIN) > 1L) || (MARGIN < 1L) || (MARGIN > n))
        stop("incorrect value for 'MARGIN'")

    if(any(d == 0L)) return(array(integer(), d))

    y <- rep.int(rep.int(1L:d[MARGIN],
			 prod(d[seq_len(MARGIN - 1L)]) * rep.int(1L, d[MARGIN])),
		 prod(d[seq.int(from = MARGIN + 1L, length.out = n - MARGIN)]))
    dim(y) <- d
    get_rm(y)
}

# From R-3.5.0 (feature introduced in R-3.0.0).

provideDimnames <- function(x, sep = "", base = list(LETTERS), unique = TRUE)
{
    ## provide dimnames where missing - not copying x unnecessarily
    dx <- dim(x)
    dnx <- dimnames(x)
    if(new <- is.null(dnx))
	dnx <- vector("list", length(dx))
    k <- length(M <- lengths(base))
    for(i in which(vapply(dnx, is.null, NA))) {
	ii <- 1L+(i-1L) %% k # recycling
        ss <- seq_len(dx[i]) - 1L # dim could be zero
	bi <- base[[ii]][1L+ (ss %% M[ii])]
	dnx[[i]] <- if(unique) make.unique(bi, sep = sep) else bi
	new <- TRUE
    }
    if(new) dimnames(x) <- dnx
    x
}
