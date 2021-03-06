#  File src/library/utils/R/zzz.R
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

.noGenerics <- TRUE

.onLoad <- function(libname, pkgname)
{
    ## Set default options() related to functionality in 'utils' pkg
    op <- options()
    op.utils <-
	list(help.try.all.packages = FALSE,
	     help.search.types = c("vignette", "demo", "help"),
	     internet.info = 2,
	     pkgType = .Platform$pkgType,
	     str = list(strict.width = "no", digits.d = 3, vec.len = 4),
	     demo.ask = "default", example.ask = "default",
	     HTTPUserAgent = defaultUserAgent(),
             repos = c(pqR = "http://repos.pqR-project.org",
                       CRAN = "http://cloud.r-project.org"),
	     menu.graphics = TRUE,
             mailer = "mailto")
    extra <-
        if(.Platform$OS.type == "windows") {
            list(unzip = "internal",
                 editor = if(length(grep("Rgui", commandArgs(), TRUE)))
                            "internal" else "notepad"
                 )
        } else
            list(unzip = Sys.getenv("R_UNZIPCMD"),
                 editor = Sys.getenv("EDITOR"))
    op.utils <- c(op.utils, extra)
    toset <- !(names(op.utils) %in% names(op))
    if(any(toset)) options(op.utils[toset])
}
