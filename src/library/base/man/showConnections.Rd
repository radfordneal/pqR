% File src/library/base/man/showConnections.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Modifications for pqR Copyright (c) 2018 Radford M. Neal.
% Distributed under GPL 2 or later

\name{showConnections}
\alias{showConnections}
\alias{getConnection}
\alias{getAllConnections}
\alias{closeAllConnections}
\alias{stdin}
\alias{stdout}
\alias{stderr}
\alias{isatty}
\title{Display Connections}
\description{
  Display aspects of \link{connections}.
}
\usage{
showConnections(all = FALSE)
getConnection(what)
closeAllConnections()

stdin()
stdout()
stderr()

isatty(con)
}
\arguments{
  \item{all}{logical: if true all connections, including closed ones
    and the standard ones are displayed.  If false only open user-created
    connections are included.}

  \item{what}{integer: A number or a string (having the form of a number)
    giving the name of a row in the table produced by
    \code{showConnections}.}

  \item{con}{a connection.}
}
\details{
  \code{stdin()}, \code{stdout()} and \code{stderr()} are standard
  connections corresponding to input, output and error on the console
  respectively (and not necessarily to file streams).  They are text-mode
  connections of class \code{"terminal"} which cannot be opened or
  closed, and are read-only, write-only and write-only respectively.
  The \code{stdout()} and \code{stderr()} connections can be
  re-directed by \code{\link{sink}} (and in some circumstances the
  output from \code{stdout()} can be split: see the help page).

  The encoding for \code{\link{stdin}()} when redirected can
  be set by the command-line flag \option{--encoding}.

  \code{closeAllConnections} closes (and destroys) all user
  connections, restoring all \code{\link{sink}} diversions as it does
  so.

  If there is no \R level object referring to the
  connection, it will be closed automatically at the next garbage
  collection (except for \code{\link{gzcon}} connections).

  \code{showConnections} returns a matrix of information, with named
  rows that correspond to connections.  \code{getConnection} takes the
  name in this table of a row (or a number that converts to such a
  name) and returns a connection object for the connection in that
  row, which can be used, for example, to close the connection.

  \code{isatty} returns true if the connection is one of the class
  \code{"terminal"} connections and it is apparently connected to a
  terminal, otherwise false.  This may not be reliable in embedded
  applications, including GUI consoles.
}
\note{
  \code{stdin()} refers to the \sQuote{console} and not to the C-level
  \file{stdin} of the process.  The distinction matters in GUI consoles
  (which may not have an active \file{stdin}, and if they do it may not
  be connected to console input), and also in embedded applications.
  If you want access to the C-level file stream \file{stdin}, use
  \code{\link{file}("stdin")}.

  When \R is reading a script from a file, the \emph{file} is the
  \sQuote{console}: this is traditional usage to allow in-line data (see
  \sQuote{An Introduction to R} for an example).
}
\value{
  \code{stdin()}, \code{stdout()} and \code{stderr()} return connection
  objects.

  \code{showConnections} returns a character matrix of information with
  a row for each connection, by default only for open non-standard connections.

  \code{getConnection} returns a connection object, or \code{NULL}.
}
\seealso{\code{\link{connections}}}

\examples{
showConnections(all = TRUE)

txtc <- textConnection(letters)
showConnections()

c(isatty(stdin()), isatty(stdout()), isatty(stderr()), isatty(txtc))

rm(txtc); gc()
showConnections(all = TRUE)
}
\keyword{connection}
