% File src/library/base/man/helpers.Rd
% Part of pqR
% Copyright 2013 Radford M. Neal
% Distributed under GPL 2 or later

\name{helpers}
\alias{helpers}
\title{Helper threads}
\description{
  How some numerical computations may be done in helper threads.
}
\details{
  In pqR, it is possible for some computations to be done in ``helpers
  threads'', which may execute in parallel with each other, and with
  the main thread of the interpreter, if the system used has multiple
  processors or multiple processor cores.  When this facility is
  enabled, such parallel execution occurs without any change to the
  program, though the degree of parallelism achieved will depend on
  the program's structure.  The numerical results obtained should be 
  exactly the same as when no helper threads are used.

  To use this facility, it must have been enabled when pqR was
  configured, with the \code{--enable-helper-threads} option (though
  this is the default).  If it was enabled, the number of helper
  threads is specified with the \code{--helpers} option (short form
  \code{-p}) to the command starting pqR.  A \code{--helpers} option
  with no argument specifies that one helper thread will be used (two
  threads total, counting the thread for the interpreter).  An option
  of \code{--helpers=N} specifies that the number of helper threads
  should be \code{N} (silently reduced to 127 if it is larger than
  this maximum).  }

\seealso{
  \code{\link{options}} for information on options for controlling
  the helpers facility.
}

\keyword{arith}
