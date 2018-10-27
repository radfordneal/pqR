% File src/library/base/man/matmult.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Team
% Modifications for pqR Copyright (c) 2013, 2018 Radford M. Neal.
% Distributed under GPL 2 or later

\name{matmult}
\alias{\%*\%}
\alias{matmult}
\title{Matrix Multiplication}
\description{
  Multiplies two matrices, if they are conformable.
  If one argument is a vector, it will be promoted to either a row or
  column matrix to make the two arguments conformable.
  If both are vectors it will return the inner product (as a matrix).
}
\usage{
x \%*\% y
}
\arguments{
  \item{x, y}{numeric or complex matrices or vectors.}
}
\details{
  This operator is S4 generic but not S3 generic.  S4 methods need to be
  written for a function of two arguments named \code{x} and \code{y}.

  With the default real or complex method, when one operand of
  \code{\%*\%} can be recognized as being the result of a transpose
  operation (\code{\link{t}}), the transpose is not actually computed,
  but instead a matrix multiplication routine (either from the BLAS or
  the appropriate C routine) is used that operates directly on the
  transposed operand.  This is equivalent to use of the
  \code{crossprod} or \code{tcrossprod} functions.  If both operands
  are transposed, the transposed first operand will not actually be
  computed, but the second will be.

  With the default real or complex method, when a vector is promoted
  to a matrix, its names are not promoted to row or column names,
  unlike \code{\link{as.matrix}}.
}

\value{
  A double or complex matrix product.  Use \code{\link{drop}} to remove
  dimensions which have only one level.
}

\section{BLAS versus C routines}{
  The default method for \code{\%*\%} on real matrices uses either a
  built-in C routine or the appropriate BLAS routine, as controlled
  by the \code{mat_mult_with_BLAS} option (see below), as set by
  default or by using \code{\link{options}}.

  The built-in C routines guarantee correct behaviour when some
  elements are \code{NA}, \code{NaN}, or infinite, which may not be
  the case with all BLAS packages.  Also, the order of addition in the
  C routines is the same as for the obvious matrix multiplication
  algorithm, and products are consistently computed to double
  precision (only).  This may not be the case for some BLAS
  implementations, which may therefore produce round-off errors that
  differ from those produced by the obvious method (and may even
  differ between runs of the same BLAS routines).

  If the builtin C routines are used when helper threads are enabled,
  they will take piplelined input for their second operand, and
  produce pipelined output for vector-matrix and matrix-matrix
  products.  When helper threads are enabled, the C routines may also
  use multiple threads to compute a single product.

  Matrix multiplication with the BLAS routines may also be done in
  helper threads, depending on the \code{BLAS_in_helpers} option (see
  below), but with no pipelining of inputs or output.  Some BLAS
  routines may use multiple threads to compute a product, but if so, 
  their use will not be coordinated with helper threads used by pqR,
  which can lead to degraded performance.

  The built-in C routines are significantly faster (for most
  operations) than the BLAS routines for matrix multiplicaton supplied
  with pqR, but may be slower (at least for large operations) than the
  routines in whatever optimized BLAS you may have available (eg,
  OpenBLAS or Apple's veclib).  However, the advantage of optimized
  BLAS routines over the C routines is seldom more than a factor of
  two (assuming both are allowed to use the same number of threads),
  and the C routines can be faster for some operations.

  Note that some other linear algebra operations make internal use
  of the BLAS matrix multiply routines, regardless of how the
  \code{mat_mult_with_BLAS} option is set.

  The \code{mat_mult_with_BLAS} option is a vector of four logical
  values, specifying whether a BLAS routine should be used when the
  operands of \code{\%*\%} result in the operation being a vector dot
  product, a product of a matrix and column vector, a product of a row
  vector and a matrix, or a general matrix-matrix product.  (Setting
  this option to a single logical value sets all four of these
  values.)  A value of \code{NA} is the same as \code{FALSE} except
  for matrix-matrix multiplies, for which the BLAS is used except when
  an operand contains an \code{NA} or \code{NaN} value, or the
  operands are of a size where checking this takes time comparable to
  just doing the operation with the C routines. 

  The default settings of the \code{mat_mult_with_BLAS} option is
  determined by the configuration options when pqR is built. It can be
  set by a \code{mat_mult_with_BLAS=X} argument to \code{configure},
  where \code{X} is \code{NA}, \code{TRUE}, or \code{FALSE} (or
  \code{T} or \code{F}).  If no such argument is present, \code{NA} is
  assumed if a \code{--with-blas} argument is given to
  \code{configure}, and \code{FALSE} is assumed if there is no
  \code{--with-blas} argument.

  The default setting of the \code{BLAS_in_helpers} option is also
  determined by \code{configure} when pqR is built.  It can be set by
  a \code{BLAS_in_helpers=X} argument, where \code{X} is \code{TRUE}
  or \code{FALSE} (or \code{T} or \code{F}).  If no such argument is
  present, \code{FALSE} is assumed if a \code{--with-blas} argument is
  given to \code{configure}, and \code{TRUE} is assumed if there is no
  \code{--with-blas} argument.

  If the default setting of \code{BLAS_in_helpers} determined by
  \code{configure} is \code{FALSE}, attempts to change this option
  with \code{options} are silently ignored.  This is appropriate if
  the BLAS is not thread-safe, and hence calling a BLAS routine in a
  helper thread could produce incorrect results, or result in a crash.
  If the default setting of \code{BLAS_in_helpers} is \code{TRUE}, it
  can be changed to \code{FALSE} (and later back to \code{TRUE}) using
  \code{options}.

  The default method for complex matrices uses a BLAS routine, unless
  such a routine does not exist because Fortran double complex values
  are not supported, in which case a C routine is used.
}

\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{crossprod}},
  \code{\link{matrix}},
  \code{\link{Arithmetic}},
  \code{\link{diag}},
  \code{\link{options}}.
}
\examples{
x <- 1:4
(z <- x \%*\% x)    # scalar ("inner") product (1 x 1 matrix)
drop(z)           # ... as scalar

y <- diag(x)
z <- matrix(1:12, ncol = 3, nrow = 4)
y \%*\% z
y \%*\% x
x \%*\% z

a <- matrix(c(1.1,2.2),10,10)
f <- function (m) t(m+1i)
f(a) \%*\% a          # the transpose of a+1i is not actually computed

}
\keyword{array}
\keyword{arith}
