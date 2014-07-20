% File src/library/base/man/Foreign.Rd
% Part of the R package, http://www.R-project.org
% Modifications for pqR Copyright (c) 2014 Radford M. Neal.
% This documentation file is derived from the one in R-2.15.2.
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{Foreign}
\alias{Foreign}
\alias{.C}
\alias{.Fortran}
\title{Foreign Function Interface}
\description{
  Functions to make calls to compiled code that has been loaded into \R.
}
\usage{
       .C(.NAME, \dots, NAOK = FALSE, DUP = TRUE, HELPER=FALSE, 
           PACKAGE, ENCODING)
 .Fortran(.NAME, \dots, NAOK = FALSE, DUP = TRUE, HELPER=FALSE, 
           PACKAGE, ENCODING)
}
\arguments{
  \item{.NAME}{a character string giving the name of a C function or
    Fortran subroutine, or an object of class
    \code{"\link{NativeSymbolInfo}"}, \code{"\link{RegisteredNativeSymbol}"}
    or \code{"\link{NativeSymbol}"} referring to such a name.}
  \item{\dots}{arguments to be passed to the foreign function.  Up to 65.}
  \item{NAOK}{if \code{TRUE} then any \code{\link{NA}} or
    \code{\link{NaN}} or \code{\link{Inf}} values in the arguments are
    passed on to the foreign function.  If \code{FALSE}, the presence of
    \code{NA} or \code{NaN} or \code{Inf} values is regarded as an error.}
  \item{DUP}{if \code{TRUE} then arguments are duplicated before
    their address is passed to C or Fortran.}
  \item{HELPER}{if \code{TRUE} then the C or Fortran routine may be
    done in a helper thread, perhaps in parallel with other computations,
    rather than being done immediately in the master thread.  This should
    be specified only if the C or Fortran routine has no side effects,
    and otherwise operates in a thread-safe manner.}
  \item{PACKAGE}{if supplied, confine the search for a character string
    \code{.NAME} to the DLL given by this argument (plus the
    conventional extension, \file{.so}, \file{.dll}, \dots).

    This is intended to add safety for packages, which can ensure by
    using this argument that no other package can override their external
    symbols, and also speeds up the search (see \sQuote{Note}).}
  
  \item{ENCODING}{optional name for an encoding to be assumed for
    character vectors.  Allowed but ignored for \code{.Fortran},
    deprecated for \code{.C}.  See \sQuote{Details}.}
}

\details{
  These functions can be used to make calls to compiled C and Fortran 77
  code.  The interfaces of \code{\link{.Call}} and
  \code{\link{.External}} are more flexible, which may sometimes allow
  for better performance, although their flexibility also prevents
  such routines from being done in parallel in a helper thread.

  Character strings will be translated from the value of \code{ENCODING}
  or any declared encoding (see \code{\link{Encoding}}) to the current
  locale before being passed to the compiled C code.  They will be
  returned encoded in the current locale unless \code{ENCODING} was
  specified, when the output strings are translated to the specified
  encoding.  This is deprecated: convert code to use \code{\link{iconv}}.
  
  These functions are \link{primitive}, and \code{.NAME} is always
  matched to the first argument supplied (which should not be named and
  there will be a warning if it is---and an error in future.).
  The other named arguments follow \code{\dots} and so cannot be
  abbreviated.  For clarity, should avoid using names in the arguments
  passed to \code{\dots} that match or partially match \code{.NAME}.
}

\value{
  A list similar to the \code{\dots} list of arguments passed in
  (including any names given to the arguments), but reflecting any
  changes made by the C or Fortran code.
}

\section{Argument types}{
  The mapping of the types of \R arguments to C or Fortran arguments is

  \tabular{lll}{
    \R \tab     C \tab     Fortran\cr
    integer \tab int * \tab integer\cr
    numeric \tab double * \tab double precision\cr
    -- or -- \tab float * \tab real\cr
    complex \tab Rcomplex * \tab double complex\cr
    logical \tab int * \tab integer \cr
    character \tab char ** \tab [see below]\cr
    raw \tab unsigned char * \tab not allowed\cr
    list \tab SEXP *\tab not allowed\cr
    other \tab SEXP\tab not allowed\cr
  }

  Numeric vectors in \R will be passed as type \code{double *} to C (and
  as \code{double precision} to Fortran) unless (i) \code{DUP} is true
  and (ii) the argument has attribute \code{Csingle} set to \code{TRUE}
  (use \code{\link{as.single}} or \code{\link{single}}).  This mechanism
  is only intended to be used to facilitate the interfacing of existing
  C and Fortran code.

  The C type \code{Rcomplex} is defined in \file{Complex.h} as a
  \code{typedef struct {double r; double i;}}.  It may or may not be
  equivalent to the C99 \code{double complex} type, depending on the
  compiler used.

  Logical values are sent as \code{0} (\code{FALSE}), \code{1}
  (\code{TRUE}) or \code{INT_MIN = -2147483648} (\code{NA}, but only if
  \code{NAOK = TRUE}), and the compiled code should return one of these
  three values: however non-zero values other than \code{INT_MIN} are
  mapped to \code{TRUE}.

  \emph{Note:} The C types corresponding to \code{integer} and
  \code{logical} are \code{int}, not \code{long} as in S.  This
  difference matters on most 64-bit platforms, where \code{int} is
  32-bit and \code{long} is 64-bit (but not on 64-bit Windows).
  
  \emph{Note:} The Fortran type corresponding to \code{logical} is
  \code{integer}, not \code{logical}: the difference matters on some
  Fortran compilers.
  
  Missing (\code{NA}) string values are passed to \code{.C} as the string
  "NA". As the C \code{char} type can represent all possible bit patterns
  there appears to be no way to distinguish missing strings from the
  string \code{"NA"}.  If this distinction is important use \code{\link{.Call}}.

  \code{.Fortran} passes the first (only )character string of a character
  vector as a C character array to Fortran, which may be usable
  as \code{character*255} if its true length is passed separately.  Only
  up to 255 characters of the string are passed back.  (How well this
  works, and even if it works at all, depends on the C and Fortran
  compilers and the platform.)
  
  Lists, functions are other \R objects can (for historical reasons) be
  passed to \code{.C}, but the \code{\link{.Call}} interface is much
  preferred.  All inputs apart from atomic vectors should be regarded as
  read-only, and use of any inputs other that vectors (including lists), 
  functions and environments are now deprecated.
}

\section{Use of \code{DUP = FALSE}}{
  Using \emph{\code{DUP = FALSE}} is dangerous, since if not used properly
  it can result in inadvertant modification of a local variable, or even 
  a non-local variable.  This will be very hard to trace.  Note that use of
  \code{DUP = FALSE} is intended purely as a way of improving performance,
  not as an alternative way of returning information to the caller.
  A program that operates correctly with \code{DUP = FALSE} should also
  operate correctly with \code{DUP = TRUE}.  If it does not, it may well
  stop working with \code{DUP = FALSE} when the implementation changes.
  In particular, use of \code{DUP = FALSE} does not \emph{guarantee} that the
  argument will not be duplicated before being passed to the C or Fortran
  routine.

  Character vectors will be duplicated even if \code{DUP = FALSE}.  However,
  passing character vectors is not allowed when \code{DUP = FALSE} in
  pqR-2014-06-19 or earlier, and in R Core versions of R (up to at least 
  R-3.1.1), so they should not be passed with \code{DUP = FALSE} if 
  compatibility with these versions is needed.

  Non-vector objects are read-only to the C code
  and are never copied, regarless of the setting of \code{DUP}.

  Atomic vectors (numeric, integer, complex, logical, and raw) of length 
  one are always copied even if \code{DUP = FALSE},
  and so may safely be modified by the C or Fortran routine.  However, this
  is not true for pqR-2014-06-19 and earlier, and for R Core versions of R
  (up to at least R-3.1.1), so if compatibility with these versions is needed,
  vectors of length one should be handled the same as longer vectors.

  Vectors created with certain functions are guaranteed
  to not be referred to in any other way, and so may be passed with
  \code{DUP = FALSE} and modified by the C or Fortran routine, either 
  for its internal use, or as a way of returning information to the caller.
  The set of such functions is currently as follows:

  \code{numeric}, \code{integer}, \code{complex}, \code{logical},
  \code{raw}, \code{matrix}, \code{array}, \code{rep}

  For example, in the following call:

  \code{.C("Cfunction", input = x, output = numeric(10), DUP = FALSE)}

  it is permited for \code{Cfunction} to modify \code{output}, but
  it should not modify \code{input}, since that would change the
  variable \code{x}, and perhaps other variables as well, if they
  share their value with \code{x}.  Using \code{DUP = FALSE} in
  this situation will save time and memory, since \code{input} will
  not be copied.  Note that \code{output} will not be copied regardless
  of the setting of \code{DUP}, since the interpreter is smart enough 
  to realize that it is unshared.
}

\section{Parallel execution in a helper thread}{
  Rather than being done immediately in the master thread, the C or 
  Fortran routine may, in certain situations, instead be scheduled for 
  possible execution in a helper thread, in parallel with other operations
  (possibly including other calls of the same routine).

  The C or Fortran routine will be done in a helper thread only if the
  \code{HELPER} argument is set to \code{TRUE}, which is not the
  default.  \code{HELPER} should be specified to be \code{TRUE} only
  if the routine is ``thread safe'' - in particular, it should not
  have side effects, should not use statically allocated storage, and
  should only call other routines that are themselves thread-safe.
  Whether some operations, such as creation, use, and deletion of a
  scratch file with a unique name, are thread-safe may depend on the
  compiler and operating system used.  It is also desirable to set
  \code{HELPER} to \code{FALSE} if the time for the routine to execute
  is so small that the overhead of scheduling it is not worthwhile.

  Even if \code{HELPER} is \code{TRUE}, the routine will be scheduled
  for execution in a helper thread only if it is used in a context
  in which only a single argument is referred to by the caller after
  the routine completes.  Currently,
  this situation is recognized only with constructions such as

    \code{.C("myroutine",result=numeric(100),x,y,HELPER=TRUE)$result}
  
  There must be exactly one named argument to \code{.C} or
  \code{.Fortran}, and the immediate use of the value of \code{.C} or
  \code{.Fortran} must be to extract the element with that
  argument's name using the \code{$} operator.

  The routine will not be done in a helper thread if the one
  named argument is a character string, or is flagged as being passed 
  in single-precision floating point format.  The routine will also
  not be done in a helper thread if it has any arguments that are not
  of numeric, complex, logical, integer, raw, or character type
  (such as lists or functions).

  Finally, the routine will of course not be done in a helper thread
  if there are no helper threads, or if multithreading has been
  disabled, or if the master thread decides to do it while all the
  helper threads are otherwise busy.
}

\section{Fortran symbol names}{
  All Fortran compilers known to be usable to compile \R map symbol names
  to lower case, and so does \code{.Fortran}.

  Symbol names containing underscores are not valid Fortran 77 (although
  they are valid in Fortran 9x).  Many Fortran 77 compilers will allow
  them but may translate them in a different way to names not containing
  underscores.  Such names will often work with \code{.Fortran} (since
  how they are translated is detected when \R is built and the
  information used by \code{.Fortran}), but portable code should not use
  Fortran names containing underscores.

  Use \code{.Fortran} with care for compiled Fortran 9x code: it may not
  work if the Fortran 9x compiler used differs from the Fortran 77 compiler
  used when configuring \R, especially if the subroutine name is not
  lower-case or includes an underscore.  It is also possible to use
  \code{.C} and do any necessary symbol-name translation yourself.
}

\note{
  If one of these functions is to be used frequently, do specify
  \code{PACKAGE} (to confine the search to a single DLL) or pass
  \code{.NAME} as one of the native symbol objects.  Searching for
  symbols can take a long time, especially when many namespaces are loaded.
  
  You may see \code{PACKAGE = "base"} for symbols linked into \R.  Do
  not use this in your own code: such symbols are not part of the API
  and may be changed without warning.
  
  \code{PACKAGE = ""} is accepted, but deprecated.
  
  The way \link{pairlist}s were passed by \code{.C} prior to \R 2.15.0
  was not as documented.  This has been corrected, but the
  \code{\link{.Call}} and \code{\link{.External}} interfaces are much
  preferred for passing anything other than atomic vectors.
}

\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{dyn.load}}, \code{\link{.Call}}.

  The \sQuote{Writing R Extensions} manual.
}

\examples{

\dontrun{
  x <- 1.2; y <- seq(10,20,length.out=100)

  # The following might be done in a helper thread
  a <- .C ("myroutine", result=numeric(100), x, y, HELPER=TRUE) $ result

  # Neither of the following will be done in a helper thread
  a <- .C ("myroutine", result=numeric(100), scale=x, y, HELPER=TRUE) $ result
  a <- .C ("myroutine", numeric(100), x, y, HELPER=TRUE) [[1]]

  # The .C in the function defined below might be done in a helper thread 
  # if called as in the following example.
  f <- function (y) .C ("myroutine", result=numeric(length(y)),
                                     1.2, as.double(y), HELPER=length(y)>50)
  a <- f(1:100)$result
}
}
\keyword{programming}
