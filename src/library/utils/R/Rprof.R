#  File src/library/utils/R/Rprof.R
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

Rprof <- function(filename = "Rprof.out", append = FALSE, interval =  0.02,
                  memory.profiling = FALSE)
{
    if(is.null(filename)) filename <- ""
    invisible(.Internal(Rprof(filename, append, interval, memory.profiling)))
}

# Enhanced Rprofmem for pqR.  Rprofmemt has different defaults.

Rprofmem <- function(filename = "Rprofmem.out", append = FALSE, threshold = 0,
             nelem = 0, stack=TRUE, terminal = FALSE, pages = TRUE, details = FALSE)
{
    if(is.null(filename)) filename <- ""
    invisible(.Internal(Rprofmem(filename, append, as.double(threshold), 
                                 as.double(nelem), stack, terminal, pages, details)))
}

Rprofmemt <- function(filename = "", append = FALSE, threshold = 0,
             nelem = 0, stack=TRUE, terminal = TRUE, pages = FALSE, details = TRUE)
{
    if(is.null(filename)) { filename <- ""; terminal <- FALSE }
    invisible(.Internal(Rprofmem(filename, append, as.double(threshold), 
                                 as.double(nelem), stack, terminal, pages, details)))
}
