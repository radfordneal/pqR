#  File src/library/base/R/match.R
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

match <- function(x, table, nomatch = NA_integer_, incomparables = NULL)
    .Internal(match(x, table, nomatch, incomparables))

match.call <-
    function(definition=NULL, call=sys.call(sys.parent()), expand.dots=TRUE)
    .Internal(match.call(definition,call,expand.dots))

pmatch <- function(x, table, nomatch = NA_integer_, duplicates.ok = FALSE)
    .Internal(pmatch(as.character(x), as.character(table), nomatch,
                     duplicates.ok))

`%in%` <- function(x, table) .Internal(x %in% table)

match.arg <- function (arg, choices, several.ok = FALSE)
{
    if (missing(choices)) {
        formal.args <- formals(sys.function(sys.parent()))
        choices <- eval(formal.args[[substitute(arg)]])
    }
    if (is.null(arg))
        return(choices[1L])
    if (!is.character(arg))
        stop("'arg' must be NULL or a character vector")

    if (several.ok) {
        if (length(arg) == 0L) 
            stop("'arg' must be of length >= 1")
        i <- pmatch (arg, choices, nomatch = 0L, duplicates.ok = TRUE)
        i <- i[i > 0L]
        if (length(i) == 0)
            stop(gettextf("'arg' should be one of %s",
                          paste(dQuote(choices), collapse = ", ")),
                 domain = NA)
    }
    else { # !several.ok - most important (default) case
        ## the arg can be the whole of choices as a default argument.
        if (identical (arg, choices))
            return(arg[1L])
        if (length(arg) != 1L)
            stop("'arg' must be of length 1")
        i <- pmatch (arg, choices, nomatch = 0L, duplicates.ok = TRUE)
        if (sum(i > 0L) != 1) {
            if (all(i==0L))
                stop(gettextf("'arg' should be one of %s",
                              paste(dQuote(choices), collapse = ", ")),
                     domain = NA)
            else
                stop("there is more than one match in 'match.arg'")
        }
    }

    choices[i]
}

charmatch <- function(x, table, nomatch = NA_integer_)
    .Internal(charmatch(as.character(x), as.character(table), nomatch))

char.expand <- function(input, target, nomatch = stop("no match"))
{
    if(length(input) != 1L)
	stop("'input' must have length 1")
    if(!(is.character(input) && is.character(target)))
	stop("'input' and 'target' must be character vectors")
    y <- .Internal(charmatch(input, target, NA_integer_))
    if(any(is.na(y))) eval(nomatch)
    target[y]
}
