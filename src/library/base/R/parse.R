#  File src/library/base/R/parse.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
#  Modifications for pqR Copyright (c) 2018 Radford M. Neal.
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

parse <- function(file = "", n = NULL, text = NULL, prompt = "?",
                  keep.source = getOption("keep.source"),
                  srcfile = NULL, encoding = "unknown")
{
    if (is.null(text)) { # use 'file'

        if (is.character(file)) {
            if (file == "") {
                file <- stdin()
                if (missing(srcfile))
                    srcfile <- "<stdin>"
            } 
            else {
                filename <- file
                file <- file (filename, "r")
                if (missing(srcfile))
                    srcfile <- filename
                if (isTRUE(keep.source)) {
                    text <- readLines(file, warn = FALSE)
                    if (!length(text)) text <- ""
                    close(file)
                    file <- NULL
                    srcfile <- srcfilecopy (filename, text, 
                                 file.info(filename)[1,"mtime"], isFile = TRUE)
                }
                else
                    on.exit(close(file))
            }
        }
    } 

    else { # use 'text', not 'file'

        text <- as.character(text)
        if (length(text) == 0L)
            return (expression())
        if (missing(srcfile)) {
            if (isTRUE(keep.source))
               srcfile <- srcfilecopy ("<text>", text)
            else 
               srcfile <- "<text>"
        }
    }

    .Internal(parse(file, n, text, prompt, srcfile, encoding))
}
