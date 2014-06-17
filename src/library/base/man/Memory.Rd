% File src/library/base/man/Memory.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Development Team
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
  
  On Windows the \option{--max-mem-size} option sets the maximum memory
  allocation: it has a minimum allowed value of 32M.  This is intended
  to catch attempts to allocate excessive amounts of memory which may
  cause other processes to run out of resources.  See also
  \code{\link{memory.limit}}.
#endif

  All objects in pqR (eg, vectors, symbols, language elements, and
  internal represenations of argument lists) are stored in nodes.
  There are several pools of small nodes, used for non-vector objects
  and for small vectors, that are allocated in pages of many nodes
  each.  Typically, the small nodes used for pairlists, language
  elements, and some very small vectors occupy 32 bytes for a 32-bit
  platform, and 56 bytes for a 64-bit platform.  Small nodes for
  somewhat larger vectors may be up to a few hundred bytes in size,
  depending on settings of tuning options.  Larger vectors are
  allocated individually.

  The terminology used in describing memory usage is tied to past
  implementations, and does not correspond well with this scheme (nor
  did it in correspond well with the somewhat different scheme used in
  \R 2.15.0).  The total number of nodes for all objects is referred
  to as the number of \code{Ncells} (also called ``cons'' cells) ---
  though note that what looks like a single object to the user may be
  represented internally by several nodes.  The amount of memory said
  to be occupied by \code{Ncells} (as reported by \code{\link{gc}}) is
  the total size of the small nodes, plus the total size of headers
  for the large vector nodes. The number of \code{Vcells} is the total
  amount of data space, counted in 8-byte chunks, for large vector
  nodes (not including the space for the header).  The amount of
  memory said to be occupied by \code{Vcells} is the number of
  \code{Vcells} times 8.  

  The sum of the memory said to be occupied by
  \code{Ncells} and said to be occupied by \code{Vcells} is the total
  memory used for R objects (though note that this does not include
  some memory that is unused but cannot be freed, because it shares the
  same page as objects that are still in use).  Note that this memory usage
  may include some padding in small nodes, to the fixed size for the
  class of node used.  Note also that memory usage will vary with the
  platform and R build, both because pointers are of different sizes
  in 32-bit and 64-bit platforms, and because the amount of padding
  can vary with tuning options.  
  
  Target values for the number of \code{Ncells} and \code{Vcells}
  are maintained, with initial values (currently) of 
  350,000 \code{Ncells} and 1,000,000 \code{Vcells} (which totals
  about 18 Megabytes for a 32-bit implementation, or 29 Megabytes for
  a 64-bit implementation).
  This space is not actually allocated initially: rather these values are 
  the sizes for triggering garbage collection.  
  The target values will grow or shrink depending on usage, but never 
  decreasing below the initial values.

  You can find out the current memory consumption by typing \code{\link{gc}()} 
  at the \R prompt.  Note that following \code{\link{gcinfo}(TRUE)}, automatic
  garbage collection always prints memory use statistics.
  
  The command-line option \option{--max-ppsize} controls the maximum
  size of the pointer protection stack.  This defaults to 50000, but can
  be increased to allow deep recursion or large and complicated
  calculations to be done.  Currently the maximum value accepted is 500000.
}

\seealso{
  \emph{An Introduction to R} for more command-line options.

  \code{\link{Memory-limits}} for the design limitations.

  \code{\link{gc}} for information on the garbage collector and total
  memory usage,  \code{\link{object.size}(a)} for the (approximate)
  size of \R object \code{a}.  \code{\link{memory.profile}} for
  profiling the usage of cons cells.
#ifdef windows

  \code{\link{memory.size}} to monitor total memory usage,
  \code{\link{memory.limit}} for the current limit.
#endif
}

\keyword{environment}
