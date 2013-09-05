#  File src/library/base/R/ifelse.R
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

ifelse <- function (test, yes, no)
{
    storage.mode(test) <- "logical"
    len <- length(test)

    if (len==1) {
        if (!is.na(test)) {
            if (test)
                test[1] <- yes[1]
            else
                test[1] <- no[1]
        }
    }
    else {
        not.na <- !is.na(test)
        test.and.not.na <- test & not.na
        not.test.and.not.na <- !test & not.na
        if (any(test.and.not.na)) {
            if (length(yes) != len)
                yes <- rep (yes, length.out = len)
            test [test.and.not.na] <- yes [test.and.not.na]
        }
        if (any(not.test.and.not.na)) {
            if (length(no) != len)
                no <- rep (no, length.out = len)
            test [not.test.and.not.na] <- no [not.test.and.not.na]
        }
    }

    get_rm(test)
}
