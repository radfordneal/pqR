#  File src/library/base/R/findInt.R
#  Part of the R package, http://www.R-project.org
#  Modifications for pqR Copyright (c) 2014 Radford M. Neal.
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

### This is a `variant' of  approx( method = "constant" ) :
### HAS A DUPLICATE IN STATS, FOR MYSTERIOUS REASONS...

findInterval <- function(x, vec, rightmost.closed = FALSE, all.inside = FALSE)
{
    ## Purpose: gives back the indices of  x in vec;  vec[] sorted
    ## -------------------------------------------------------------------------
    ## Author: Martin Maechler, Date:  4 Jan 2002, 10:16

    if(any(is.na(vec)))
	stop("'vec' contains NAs")
    if(is.unsorted(vec))
	stop("'vec' must be sorted non-decreasingly")
    ## deal with NA's in x:
    if (has.na <- any(is.na(x))) {
        ix <- is.na(x)
        x <- x[!ix]
    }
    nx <- length(x)  # lengths are always integer
    n <- length(vec)
    index <- .C("find_interv_vec",
       as.double(vec),
       n,
       as.double(x),
       nx,
       as.logical(rightmost.closed),
       as.logical(all.inside),
       index = integer(nx),
       DUP = FALSE, 
       NAOK = TRUE, # NAOK: 'Inf' only
       HELPER = nx*log(n) >= 50,
       PACKAGE = "base") $ index

    if(has.na) {
	ii <- integer(nx)
	ii[ix] <- NA
	ii[!ix] <- index
	ii
    } 
    else 
        index
}
