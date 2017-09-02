#  File src/library/base/R/assign.R
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

assign <-
    function (x, value, pos = -1, envir = as.environment(pos),
              inherits = FALSE, immediate = TRUE)
    .Internal(assign(x, value, envir, inherits))

## do_list2env in ../../../main/envir.c
list2env <- function(x, envir = NULL, parent = parent.frame(),
		     hash = (length(x) > 100), size = max(29L, length(x)))
    .Internal (list2env (x, 
     if (is.null(envir)) new.env(hash=hash,parent=parent,size=size) else envir))
