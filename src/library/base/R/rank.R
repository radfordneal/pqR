#  File src/library/base/R/rank.R
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

rank <- function(x, na.last = TRUE,
		 ties.method=c("average", "first", "random", "max", "min"))
{
    nas <- is.na(x)
    not.nas <- !nas
    any.nas <- any(nas)
    nm <- names(x)
    ties.method <- match.arg(ties.method)
    ## To preserve past behaviour
    if(is.factor(x)) x <- as.integer(x)
    if (any.nas) {
        y <- switch(ties.method,
		"average"= , "min"= , "max" =
		  .Internal (rank (x[not.nas], ties.method)),
		"first" = 
                  sort.list(sort.list(x[not.nas])),
		"random" = 
                  sort.list(order(x[not.nas], stats::runif(sum(not.nas)))))
    }
    else
        y <- switch(ties.method,
		"average"= , "min"= , "max" =
		   .Internal (rank (x, ties.method)),
		"first" = 
                   sort.list(sort.list(x)),
		"random" = 
                   sort.list(order(x, stats::runif(length(x)))))
    if(!is.na(na.last) && any.nas) {
	yy <- integer(length(x)) + NA_integer_
	NAkeep <- (na.last == "keep")
	if(NAkeep || na.last) {
	    yy[not.nas] <- y
	    if(!NAkeep) yy[nas] <- (length(y) + 1L) : length(yy)
	} else {
	    len <- sum(nas)
	    yy[not.nas] <- y + len
	    yy[nas] <- 1L : len
	}
	y <- yy
	names(y) <- nm
    } 
    else
        names(y) <- nm[not.nas]
    get_rm(y)
}
