% File src/library/base/man/pnamedcnt.Rd
% Part of pqR.
% Copyright 2013 Radford M. Neal
% Distributed under GPL 2 or later

\name{pnamedcnt}
\alias{pnamedcnt}
\title{Print Count of Names Referencing an Object}
\description{
This function prints a count of how many names reference an object,
along with identifying information.  It is meant primarily for 
maintainers of the R interpreter, but it may also be useful to
sophisticated users in diagnosing time and memory inefficiencies.
}
\usage{
pnamedcnt(x, ...)
}
\arguments{
  \item{x}{An R object.}
  \item{...}{Zero or more character vectors}
}
\details{
  For each object, R maintains a count of how many variables/names reference 
  the object, called "named".  However, this count is not accurate --- it
  goes to a maximum of two, and is usually not decremented when a 
  reference ceases to exist.  When a variable is modifed with a subseting
  operator such as \code{[} or \code{$}, the object will be duplicated
  if its "named" count is greater than one, so seeing the "named" count 
  for an object helps in understanding a program's memory usage and 
  computation time.

  The \code{pnamedcnt} function prints a line consisting of "PNAMEDCNT:", 
  the "named" count for \code{x}, the hexadecimal memory address of \code{x}, 
  the type of \code{x}, and any additional arguments to \code{pnamedcnt}. 
  These additional arguments must be character vectors.  Each is printed
  preceded by ":", with spaces between elements.

  This function is \link{primitive}, so that it will not itself change
  the "named" count of \code{x}.
}
\value{
  The value of \code{x}.  Hence \code{pnamedcnt} may be used to print
  the "named" count for an intermediate value in a complex expression.
}

\seealso{
  \code{\link{tracemem}}, \code{\link{Rprofmem}}, the undocumented 
  \code{inspect} function callable using \code{\link{.Internal}}.
}
\examples{\dontrun{
a <- 1:10
pnamedcnt(a,"a")
b <- a
pnamedcnt(a,"a")
pnamedcnt(b,"b")
b[1] <- 1
pnamedcnt(a,"a")
pnamedcnt(b,"b")
}}
\keyword{utilities}

