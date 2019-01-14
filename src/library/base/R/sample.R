#  File src/library/base/R/sample.R
#  Part of the R package, http://www.R-project.org
#  Modifications for pqR Copyright (c) 2018, 2019 Radford M. Neal.
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

sample <- function(x, size, replace=FALSE, prob=NULL)
    if (length(x) == 1L && is.numeric(x) && x >= 1)
	.Internal (sample (x, if (missing(size)) x else size, replace, prob))
    else if (missing(size)) # check missing outside [], which may be nonstandard
	x [.Internal (sample (length(x), length(x), replace, prob)) ]
    else
	x [.Internal (sample (length(x), size, replace, prob)) ]

sample.int  <- function(n, size=n, replace=FALSE, prob=NULL)
    .Internal (sample (n, size, replace, prob))
