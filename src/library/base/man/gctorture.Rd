% File src/library/base/man/gctorture.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Modifications for pqR Copyright (c) 2018 Radford M. Neal.
% Distributed under GPL 2 or later

\name{gctorture}
\title{Torture Garbage Collector}
\usage{
gctorture(on = TRUE)
gctorture2(step, wait = step, inhibit_release = FALSE)
}
\alias{gctorture}
\alias{gctorture2}
\arguments{
  \item{on}{logical; turning it on/off.}
  \item{step}{integer; run GC every \code{step} allocations; \code{step
      = 0} turns the GC torture off.}
  \item{wait}{integer; number of allocations to wait before starting
    GC torture.}
  \item{inhibit_release}{logical; do not release free objects for
    re-use: use with caution.}
}
\description{
  Provokes garbage collection on (nearly) every memory allocation.
  Intended to ferret out memory protection bugs.  Also makes \R run
  \emph{very} slowly, unfortunately.
}
\details{
  Calling \code{gctorture(TRUE)} instructs the memory manager to force a
  full GC on every allocation. 

  \code{gctorture2} provides a more refined interface that allows the
  start of the GC torture to be deferred and also gives the option of
  running a GC only every \code{step} allocations.  

  A \code{TRUE} value for the \code{inhibit_release} argument of
  \code{gctorture2} prevents free objects from being re-used.  This
  will cause memory to grow and should be used with caution and in
  conjunction with operating system facilities to monitor and limit
  process memory use.  It is most useful if options have been set when
  building pqR that cause memory in freed objects to be overwritten
  with values that are likely to cause an error if the object is
  mistakenly referenced after being freed.

  Environment variables can also be used to turn on GC torture:
  \code{R_GCTORTURE} corresponds to the \code{step} argument to
  \code{gctorture}, \code{R_GCTORTURE_WAIT} to \code{wait}, and
  \code{R_GCTORTURE_INHIBIT_RELEASE} to \code{inhibit_release}.
}

\value{
  Previous value of first argument.
}
\author{Peter Dalgaard and Luke Tierney}
\keyword{environment}

