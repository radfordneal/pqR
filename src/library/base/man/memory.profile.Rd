% File src/library/base/man/memory.profile.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Modifications for pqR Copyright (c) 2017 Radford M. Neal.
% Distributed under GPL 2 or later

\name{memory.profile}
\alias{memory.profile}
\title{Profile Memory Usage by Type}
\description{
    Lists the number of objects of each\code{SEXPREC} type that are currently 
    in use.
}
\usage{
memory.profile()
}
\details{
    The current types and their uses are listed in the include file
    \file{Rinternals.h}.
}
% There will be blanks in the list corresponding
% to types that are no longer in use (types 11 and 12 at the time of
% writing).
% Also \code{FUNSXP} is not included.
% Nonsense: that is a mode, not a type.
\value{
  A vector of counts, named by the types.  See \code{\link{typeof}} for
  an explanation of types.
}

\seealso{
  \code{\link{gc}} for the overall usage counts.
  \code{\link{Rprofmem}} allows memory profiling.
}

\examples{\donttest{
memory.profile()
}}
\keyword{utilities}
