#  File src/library/base/R/character.R
#  Part of the R package, http://www.R-project.org
#  Modifications for pqR Copyright (c) 2017, 2018 Radford M. Neal.
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

substr <- function(x, start, stop)
    .Internal (substr (if (is.character(x)) x else as.character(x),
                       as.integer(start), as.integer(stop)))

substring <- function(text, first, last=.Machine$integer.max)
{
    if (!is.character(text)) text <- as.character(text)
    n <- max(lt <- length(text), length(first), length(last))
    if (lt && lt < n) text <- rep(text, length.out = n)
    .Internal(substr(text, as.integer(first), as.integer(last)))
}

startsWith <- function(x, prefix) .Internal(startsWith(x, prefix))
endsWith   <- function(x, suffix) .Internal(endsWith  (x, suffix))

`substr<-` <- function(x, start, stop, value)
    .Internal(`substr<-`(x, as.integer(start), as.integer(stop), value))

`substring<-` <- function(text, first, last=.Machine$integer.max, value)
    .Internal(`substr<-`(text, as.integer(first), as.integer(last), value))

abbreviate <-
    function(names.arg, minlength = 4L, use.classes = TRUE, dot = FALSE,
             strict = FALSE, method = c("left.kept", "both.sides"))
{
    ## we just ignore use.classes
    if(minlength <= 0L)
	return(rep.int("", length(names.arg)))
    ## need to remove leading/trailing spaces before we check for dups
    ## This is inefficient but easier than modifying do_abbrev (=> FIXME !)
    names.arg <- sub("^ +", "", sub(" +$", "", as.character(names.arg)))
    dups <- duplicated(names.arg)
    old <- names.arg
    if(any(dups))
	names.arg <- names.arg[!dups]
    x <- names.arg
    if(strict) {
	x[] <- .Internal(abbreviate(x, minlength, use.classes))
    } else {
	method <- match.arg(method)
	if(method == "both.sides")
	    ## string reversion: FIXME reverse .Internal(abbreviate(.))
	    chRev <- function(x)
		sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")
	dup2 <- rep.int(TRUE, length(names.arg))
	these <- names.arg
	repeat {
	    ans <- .Internal(abbreviate(these, minlength, use.classes))
	    ## NB: fulfills   max(nchar(ans)) <= minlength
	    x[dup2] <- ans
	    if(!any(dup2 <- duplicated(x))) break
	    if(method == "both.sides") { ## abbreviate the dupl. ones from the other side:
		x[dup2] <- chRev(.Internal(abbreviate(chRev(names.arg[dup2]),
						      minlength, use.classes)))
		if(!any(dup2 <- duplicated(x))) break
	    }
	    minlength <- minlength+1
	    dup2 <- dup2 | match(x, x[dup2], 0L)
	    these <- names.arg[dup2]
	}
    }
    if(any(dups))
	x <- x[match(old,names.arg)]
    if(dot) {			    # add "." where we did abbreviate:
	chgd <- x != old
	x[chgd] <- paste0(x[chgd],".")
    }
    names(x) <- old
    x
}

make.names <- function(names, unique = FALSE, allow_ = TRUE, no.. = 
                if (!getOption("parse_dotdot")) 0L else if (unique) 2L else 1L)
{
    names <- .Internal (make.names (as.character(names), allow_, no..)) 
    if (unique) names <- make.unique(names)
    names
}

make.unique <- function (names, sep = ".") .Internal(make.unique(names, sep))

chartr <- function(old, new, x)
    .Internal (chartr(old, new, if (is.character(x)) x else as.character(x)))

tolower <- function(x)
    .Internal (tolower (if (is.character(x)) x else as.character(x)))

toupper <- function(x)
    .Internal (toupper (if (is.character(x)) x else as.character(x)))

casefold <- function(x, upper = FALSE)
    if (upper) toupper(x) else tolower(x)

sQuote <- function(x) {
    before <- after <- "'"
    q <- getOption("useFancyQuotes")
    if(!is.null(q)) {
        if(identical(q, TRUE)) {
            li <- l10n_info()
            if(li$"UTF-8") q <- "UTF-8"
            if(!is.null(li$codepage) && li$codepage > 0L) {
                ## we can't just use iconv, as that seems to think
                ## it is in latin1 in CP1252
                if(li$codepage >= 1250L && li$codepage <= 1258L
                   || li$codepage == 874L) {
                    before <- "\x91"; after <- "\x92"
                } else {
                    z <- iconv(c("\xe2\x80\x98", "\xe2\x80\x99"), "UTF-8", "")
                    before <- z[1L]; after <- z[2L]
                }
            }
        }
        if(identical(q, "TeX")) {
            before <- "`"; after <- "'"
        }
        if(identical(q, "UTF-8")) {
            before <- "\xe2\x80\x98"; after <- "\xe2\x80\x99"
        }
        if(is.character(q) && length(q) >= 4L) {
            before <- q[1L]; after <- q[2L]
        }
        ## we do not want these strings marked as in the encoding
        ## R was built under
        Encoding(before) <- Encoding(after) <- "unknown"
    }
    paste0(before, x, after)
}

dQuote <- function(x) {
    before <- after <- "\""
    q <- getOption("useFancyQuotes")
    if(!is.null(q)) {
        if(identical(q, TRUE)) {
            li <- l10n_info()
            if(li$"UTF-8") q <- "UTF-8"
            if(!is.null(li$codepage) && li$codepage > 0L) {
                if(li$codepage >= 1250L && li$codepage <= 1258L
                    || li$codepage == 874L) {
                    before <- "\x93"; after <- "\x94"
                } else {
                    z <- iconv(c("\xe2\x80\x9c", "\xe2\x80\x9d"), "UTF-8", "")
                    before <- z[1L]; after <- z[2L]
                }
            }
        }
        if(identical(q, "TeX")) {
            before <- "``"; after <- "''"
        }
        if(identical(q, "UTF-8")) {
            before <- "\xe2\x80\x9c"; after <- "\xe2\x80\x9d"
        }
        if(is.character(q) && length(q) >= 4L) {
            before <- q[3L]; after <- q[4L]
        }
        Encoding(before) <- Encoding(after) <- "unknown"
    }
    paste0(before, x, after)
}

strtoi <- function(x, base = 0L)
    .Internal(strtoi(as.character(x), as.integer(base)))

# From R-3.3.0, slightly tweaked for efficiency in pqR.

strrep <- function (x, times)
    .Internal (strrep (if (is.character(x)) x else as.character(x), 
                       as.integer(times)))
