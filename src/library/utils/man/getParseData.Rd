% File src/library/utils/man/getParseData.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2012 R Core Team
% Modifications for pqR Copyright (c) 2015, 2018 Radford M. Neal.
% Distributed under GPL 2 or later

\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{getParseData}
\alias{getParseData}
\alias{getParseText}
\title{
Get detailed parse information from object.
}
\description{
  If the \code{"keep.source"} option is \code{TRUE}, \R's parser
  will attach detailed information on the object it has parsed.  These
  functions retrieve that information.
}
\usage{
getParseData(x, includeText = NA)
getParseText(parseData, id)
}
\arguments{
  \item{x}{
    an expression returned from \code{\link{parse}}, or a function or other
    object with source reference information
  }
  \item{includeText}{
    logical; whether to include the text of parsed items in the result
  }
  \item{parseData}{
    a dataframe returned from \code{getParseData}
  }
  \item{id}{
    a vector of item identifiers whose text is to be retrieved
  }
}
\details{
  In R Core version 3.0.0, the \R{} parser was modified to include code written
  by Romain Francois in his \pkg{parser} package.  This constructs a
  detailed table of information about every token and higher level
  construct in parsed code.  This table is stored in the
  \code{\link{srcfile}} record associated with source references in the
  parsed code, and retrieved by the \code{getParseData} function.  

  In pqR, the parser has been rewritten, but this functionality has been
  included, apart from some differences in internal representation, and
  a few differences in which parent comments are attached to.
}
\value{
  For \code{getParseData}:\cr
  If parse data is not present, \code{NULL}.  Otherwise
  a dataframe is returned, containing the following columns:
  \item{line1}{integer. The line number where the item starts.  This is the
    parsed line number called \code{"parse"} in \code{\link{getSrcLocation}},
    which ignores \verb{#line} directives.}
  \item{col1}{integer. The column number where the item starts.  The first character
    is column 1.  This corresponds to \code{"column"} in \code{\link{getSrcLocation}}.}
  \item{line2}{integer. The line number where the item ends.}
  \item{col2}{integer. The column number where the item ends.}
  \item{id}{integer. An identifier associated with this item.}
  \item{parent}{integer. The \code{id} of the parent of this item.  Note
  that id numbers are arbitrary and subject to change.}
  \item{token}{character. The type of the token.}
  \item{terminal}{logical.  Whether the token is \dQuote{terminal}, i.e.
    a leaf in the parse tree.}
  \item{text}{character. If \code{includeText} is \code{TRUE}, the
    text of all tokens; if it is \code{NA} (the default), the text of terminal
    tokens.  If \code{includeText == FALSE}, this column is not included.}

  The rownames of the dataframe will be equal to the \code{id} values,
  and the dataframe will have a \code{"srcfile"} attribute containing
  the \code{\link{srcfile}} record which was used.  The rows will be
  ordered first by starting position within the source file
  (\code{line1} and \code{col1}), then by decreasing end position
  (\code{line2} and \code{col2}), and finally with terminals before
  non-terminals.

  For \code{getParseText}:\cr
  A character vector of the same length as \code{id} containing the associated
  text items.  Some of these items are included in \code{parseData}; those
  that are not are retrieved from the original file.
}
\references{
  Romain Francois (2012). parser: Detailed R source code parser. R
  package version 0.0-16. \url{https://github.com/halpo/parser}.
}
\author{
Original R Core version by Duncan Murdoch, rewritten for pqR by Radford Neal
}
\note{
  There are a number of differences in the results returned by
  \code{getParseData} relative to those in the original \pkg{parser}
  code:
  \itemize{
    \item Fewer columns are kept.
    \item The internal token number is not returned.
    \item \code{col1} starts counting at 1, not 0.
    \item The \code{id} values are not attached to the elements of the parse
    tree, they are only retained in the table returned by \code{getParseData}.
    \item \verb{#line} directives are identified, but other comment
    markup (e.g., \CRANpkg{roxygen} comments) are not.
  }
}

\seealso{
\code{\link{parse}}, \code{\link{srcref}}
}
\examples{
sv <- options(keep.source=TRUE)
expr <- parse (text = "y <- 1; fn <- function(x) {
      x   +   y # A comment
  }")
d <- getParseData(expr)
print(d)

plus <- which(d$token == "'+'")
sum <- d$parent[plus]
print (getParseText(d,sum))
body <- d[as.character(sum),"parent"]
print (getParseText(d,body))
b <- d[as.character(body),]
print (c (b$line1, b$col1, b$line2, b$col2))

options(sv)
}
\keyword{  utilities  }
