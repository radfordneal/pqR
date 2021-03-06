#  File src/library/tcltk/R/tcltk-defunct.R
#  Part of the R package, http://www.R-project.org
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

## deprecated at r16598, 2001-11-04
"$.tclvar" <- function(x, name)
	.Defunct("tclVar and tclvalue")

"$<-.tclvar" <- function(x, name, value)
    .Defunct("tclVar and tclvalue<-")

## deprecated in R 2.3.0
tkcmd <- function(...)
    .Defunct("tcl")

tkfile.tail <- function(...)
    .Defunct("tclfile.tail")

tkfile.dir <- function(...)
    .Defunct("tclfile.dir")

tkopen <- function(...)
    .Defunct("tclopen")

tkclose <- function(...)
    .Defunct("tclclose")

tkputs <- function(...)
    .Defunct("tclputs")

tkread <- function(...)
    .Defunct("tclread")

