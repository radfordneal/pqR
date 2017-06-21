% File src/library/base/man/gc.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Modifications for pqR Copyright (c) 2013, 2017 Radford M. Neal.
% Distributed under GPL 2 or later

\name{gc}
\title{Garbage Collection}
\usage{
gc(verbose = getOption("verbose"), reset=FALSE, level=2)
gcinfo(verbose)
}
\alias{gc}
\alias{gcinfo}
\arguments{
  \item{verbose}{logical; if \code{TRUE}, the garbage collection prints
    statistics about numbers of objects and space used.}
  \item{reset}{logical; if \code{TRUE} the values for maximum counts and
    space used are reset to the current values.}
  \item{level}{integer; the level of garbage collection to do.}
}
\description{
  A call of \code{gc} causes a garbage collection to take place.  
  If the level is 2, all unreferenced storage will be recovered.
  Levels 1 and 0 may recover less storage, but are faster.

  \code{gcinfo} sets a flag so that
  automatic collection is either silent (\code{verbose=FALSE}) or
  prints memory usage statistics (\code{verbose=TRUE}).
}
\details{
  A call of \code{gc} causes a garbage collection to take place.
  This will also take place automatically without user intervention, and the
  primary purpose of calling \code{gc} is for the report on memory usage.

  However, it can be useful to call \code{gc} after a large object
  has been removed, as this may prompt \R to return memory to the
  operating system.

  When \code{gcinfo(TRUE)} is in force, messages are sent to the message
  connection at each garbage collection of the form
\preformatted{  Garbage collection 12 = 10+0+2 (level 0), 12.345 Megabytes, automatic
}
  In this message, the number of garbage collections done is broken
  down by level, and the level of the current collection is displayed
  (for an explanation see the \sQuote{R Internals} manual).  This is
  followed by the total memory usage after collection (excluding
  constants), and by what prompted the current collection --- either
  ``requested'', ``automatic'', ``space needed'', or ``gctorture''.
  The last reason indicates a collection forced by \code{gctorture}.  The
  ``space needed'' reason indicates that an attempt at allocating a new object
  failed, and collection is being done in an attempt to recover enough memory 
  for it to succeed.
}

\value{
  \code{gc} returns a matrix with columns \code{"Objects"} giving
  the number of underlying objects (which is not entirely the same as the
  number visible in R), \code{"Megabytes"} giving the approximate number of 
  Megabytes of storage in use for R objects, and \code{"Segments"} giving
  the number of segments for objects (see \link{Memory}).  The value
  returned has two rows, \code{"Current"} for current usage, and 
  \code{"Maximum"} for the maximum usage since startup, or a call
  of \code{gc} with \code{reset=TRUE}.  A small number of constant 
  objects (eg, 0 and \code{TRUE}) are not included in any of these counts.
  
  \code{gcinfo} returns the previous value of the flag.
}
\seealso{
  The \sQuote{R Internals} manual.

  \code{\link{Memory}} on \R's memory management,
  and \code{\link{gctorture}} if you are an \R developer.
 
  \code{\link{reg.finalizer}} for actions to happen at garbage
  collection.

  \code{\link{gc.time}} for time spent in garbage collection.

  \code{\link{gctorture}} for forcing a garbage collection on every allocation.
}
\examples{\donttest{
gc() #- do it now
gcinfo(TRUE) #-- in the future, show when R does it
x <- integer(100000); for(i in 1:18) x <- c(x,i)
gcinfo(verbose = FALSE)#-- don't show it anymore

bigvec <- numeric(50000)  # Ensure that maximum usage
gc()                      # is greater than current
bigvec <- NULL 

gc(TRUE)
gc(reset=TRUE)

}}
\keyword{environment}
