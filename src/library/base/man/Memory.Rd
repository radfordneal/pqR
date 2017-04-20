% File src/library/base/man/Memory.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Modifications for pqR copyright (C) 2013, 2014 Radford M. Neal
% Distributed under GPL 2 or later

\name{Memory}
\alias{Memory}
\title{Memory Available for Data Storage}
\description{
  How \R manages its workspace.
}
\details{
  Workspaces in pqR are of variable size.  Prior to \R 2.15.0 there were
  (rarely-used) command-line options to control the workspace size, but it is 
  now sized automatically.
#ifdef windows
  
  On Windows the \option{--max-mem-size} option (or environment variable
  \env{R_MAX_MEM_SIZE}) sets the maximum memory allocation: it has a
  minimum allowed value of 32M.  This is intended to catch attempts to
  allocate excessive amounts of memory which may cause other processes
  to run out of resources.  See also \code{\link{memory.limit}}.
#endif

  Memory is occupied both by visible objects (eg, vectors, symbols,
  language elements), and by other objects that are used internally
  (eg, internal representations of argument lists).  In pqR, objects
  of both sorts are stored in ``segments'', and can be referenced by
  the index of the segment in which they are located and an offset
  within this segment.  When pqR is configured with the
  \code{--enable-compressed-pointers} option, such (index,offset)
  pairs are the primary way of storing references to objects --- in
  particular, the SEXP type in C code is such a pair.  When
  uncompressed pointers are used, the SEXP type is a standard C
  pointer, but (index,offset) pairs are still used within the memory
  allocation and garbage collection routines.

  Some segments can contain several objects, which for each kind of
  segment are limited to being no more than a certain size, and of
  certain types.  Other ``big'' segments contain only one object,
  which may be of any size, up to the maximum supported by the
  platform.  All non-vector objects and vector objects with small
  lengths will be placed in small segments.  Larger vectors (with more
  than roughly a hundred elements) will be placed in big segments.

  The amount of memory in use and the maximum used now or previously
  are reported by \code{\link{gc}} when given \code{TRUE} as its
  \code{verbose} argument.  The report contains the number of objects
  (including those not visible to the user), and the total amount of
  memory used for all objects, including memory for parts of small
  segments that are not currently being used, and other overhead (but
  not the memory for holding the the R interpreter's instructions, or
  memory for things like file buffers that are not closely connected
  to R objects).  The number of segments allocated is also reported.
  Currently, segments are never freed, so the current value of this
  will also be the maximum.  The limit on the number of segments can
  be set when pqR is configured --- see the R Installation and
  Administration manual.

  Memory usage will vary with the platform and pqR configuration, both
  because uncompressed pointers are of different sizes in 32-bit and
  64-bit platforms, and because memory usage is affected by whether
  pqR was configured with the \code{--enable-compressed-pointers} or
  \code{--enable-aux-for-attrib} options.
  
  The command-line option \option{--max-ppsize} controls the maximum
  size of the pointer protection stack.  This defaults to 50000, but can
  be increased to allow deep recursion or large and complicated
  calculations to be done.  Currently the maximum value accepted is 500000.
}

\seealso{
  \emph{An Introduction to R} for more command-line options.

  \code{\link{Memory-limits}} for the design limitations.

  \code{\link{gc}} for information on the garbage collector and total
  memory usage.

  \code{\link{object.size}(a)} for the (approximate)
  size of \R object \code{a}. 

  \code{\link{memory.profile}} for profiling the usage of cons cells.
#ifdef windows

  On Windows, \code{\link{memory.size}} to monitor total memory usage and
  \code{\link{memory.limit}} for the current limit.
#endif
}

\keyword{environment}
