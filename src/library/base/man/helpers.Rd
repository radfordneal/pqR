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
  the master thread of the interpreter, if the system used has multiple
  processors or multiple processor cores.  When this facility is
  enabled, such parallel execution occurs without any change to the
  program, though the degree of parallelism achieved will depend on
  the program's structure.  The numerical results obtained should be 
  exactly the same as when no helper threads are used.

  To use this facility, it must have been enabled when pqR was
  configured, with the \code{--enable-helper-threads} option (though
  this is the default).  The deferred evaluation apparatus must also
  be enabled (with \code{--enable-deferred-evaluation}), but this also
  is the default.

  The number of helper threads is specified with the \code{--helpers}
  option (short form \code{-p}) to the command starting pqR.  A
  \code{--helpers} option with no argument specifies that one helper
  thread will be used (two threads total, counting the master thread
  runnign the interpreter).  An option of \code{--helpers=N} specifies
  that the number of helper threads should be \code{N} (silently
  reduced to 127 if it is larger than this maximum).  If no
  \code{--helpers} or \code{-p} argument is given, zero helper threads
  are used. An argument of \code{--helpers=-1} or \code{-p=-1}
  specifies zero helper threads and also disables deferred evaluation
  (so all tasks are done by the master thread immediately, rather than
  being scheduled and possibly done later).

  Only some computations are structured as ``tasks'' that may be done in
  helper threads.  A computational task will be scheduled by the
  master thread (which does all the interpretation of R code), at
  which point it may be noticed and done by a helper thread.  The
  master thread will (usually) need the result of the task at some
  point, and will then wait for it to be completed by the helper thread, or
  do the task itself if no helper thread has started work on it.

  Tasks can be scheduled even when one or more of their inputs are the
  outputs of other tasks that have not completed.  If both these
  inputs and the corresponding output are "pipelined", the task may
  start (in a helper thread or the master) as soon as the task
  computing the pipelined inputs has started --- the task taking
  pipelined input will process this input only as it becomes
  available.  Not all tasks can take pipelined input, however, and not
  all will produce partial output before all output has been computed.
  When pipelining for a task is not possible, it will not be started
  until all its inputs have been fully computed.  Some tasks can 
  only be done in the master thread (for example, because they may
  produce a warning message), but can still take pipelined input
  produced by tasks executing concurrently in a helper thread.

  The following table summarizes what operations can currently be done
  in helper threads, or can take pipelined input (even if they are
  always done in the master thread):

\tabular{llll}{
Functions \tab Output piped? \tab Input piped? \tab Remarks\cr
\tab\tab\tab\cr
+, -, *, /, ^, \%/\% \tab If real op
                   \tab If real op
                   \tab Integer +,-,*\cr
                   \tab
                   \tab (see below)
                   \tab done by master\cr
\tab\tab\tab\cr
\%\%               \tab No
                   \tab If real op
                   \tab Done by master\cr
\tab\tab\tab\cr
floor, ceil, sign, \tab Yes
                   \tab Yes
                   \tab \cr
trunc, exp, expm1, \tab\tab\tab\cr
atan, cosh, sinh,  \tab\tab\tab\cr
tanh, asinh        \tab\tab\tab\cr
\tab\tab\tab\cr
sqrt, log, log1p,  \tab No
                   \tab Yes
                   \tab Done by master\cr
cos, sin, tan, acos,\tab\tab\tab\cr
asin, acosh, atanh,\tab\tab\tab\cr
lgammafn, gammfn,  \tab\tab\tab\cr
digamma, trigramma \tab\tab\tab\cr
\tab\tab\tab\cr
\%*\% by BLAS      \tab No
                   \tab No
                   \tab \cr
\tab\tab\tab\cr
\%*\% not by BLAS  \tab For vec*mat
                   \tab 2nd operand
                   \tab \cr
                   \tab and mat*mat
                   \tab only
                   \tab \cr
\tab\tab\tab\cr
t (transpose)      \tab If numeric
                   \tab No
                   \tab If non-numeric \cr
\tab\tab           \tab done by master\cr
\tab\tab\tab\cr
rowSums, rowMeans  \tab Yes
                   \tab No
                   \tab \cr
\tab\tab\tab\cr
colSums, colMeans  \tab Yes
                   \tab Yes
                   \tab \cr
\tab\tab\tab\cr
sum                \tab N/A (scalar)
                   \tab (see below)
                   \tab (see below)\cr
\tab\tab\tab\cr
row, col           \tab Yes
                   \tab N/A (ignored)
                   \tab \cr
\tab\tab\tab\cr
write.table        \tab N/A (NULL)
                   \tab If a matrix
                   \tab Done by master\cr
\tab\tab\tab\cr
\code{[}           \tab No
                   \tab If vector with
                   \tab Done by master\cr
\tab\tab                a scalar index\tab
}

For +, -, *, /, ^, and \%\/\%, pipelining is done only for real
operations, in which at least one operand is real, and neither
operand is complex (but one may be integer or logical), and only
for operands whose length is equal to the length of the result.

The \code{sum} function will currently pass on a result computed by a
task that sums the results of a function of one argument in the first
group above (using pqR's internal ``variant result'' mechanism), with
pipelining, but \code{sum} does not operate as a task otherwise.

}

\seealso{
  \code{\link{options}} for information on the options \code{helpers_disable},
  \code{helpers_no_multithreading}, \code{helpers_no_pipelining}, and 
  \code{helpers_trace}.  \code{\link{\%*\%}} for information on matrix 
  multiplication with or without the BLAS.
}

\keyword{arith}
