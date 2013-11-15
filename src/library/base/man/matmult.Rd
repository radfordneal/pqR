% File src/library/base/man/matmult.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Development Team
% Modifications for pqR Copyright (c) 2013 Radford M. Neal.
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
  When a vector is promoted to a matrix, its names are not
  promoted to row or column names, unlike \code{\link{as.matrix}}.

  This operator is S4 generic but not S3 generic.  S4 methods need to be
  written for a function of two arguments named \code{x} and \code{y}.

  The default method for real matrices uses either built-in C routines
  or the appropriate BLAS routine, as controlled by the
  \code{mat_mult_with_BLAS} option.  This option is a vector of four
  logical values, specifying whether a BLAS routine should be used
  when the operands of \code{\%*\%} result in the operation being a
  vector dot product, a product of a matrix and column vector, a
  product of a row vector and a matrix, or a general matrix-matrix
  product.  The default is to use the builtin C routines for all operations, 
  unless the \code{--enable-mat-mult-with-BLAS-by-default} option is used
  when configuring pqR.

  If the builtin C routines are used when helper threads are enabled,
  they will take piplelined input for their second operand, and produce
  pipelined output for vector-matrix and matrix-matrix products.  Matrix
  multiplication with the BLAS routines can be done in helper threads
  (though this can be disabled when configuring pqR with the 
  \code{--disable-mat-mult-with-BLAS-in-helpers} option), 
  but with no pipelining of inputs or output.

  Another reason one might wish to use the built-in C routines is that
  they guarantee correct behaviour when some elements are \code{NA},
  \code{NaN}, or infinite, which may not be the case with all BLAS
  packages.  Also, the order of addition in the C routines is the same
  as for the obvious matrix multiplication algorithm, whereas it may
  differ in some BLAS implementations, which affects roundoff errors.
  It's also conceivable that a built-in C routine might be
  faster than the corresponding BLAS routine.

  The default method for complex matrices uses a BLAS routine, unless
  such a routine does not exist because Fortran double complex values
  are not supported, in which case a C routine is used.

  Note that some other linear algebra operations make use internally
  of the BLAS matrix multiply routines, regardless of how the
  \code{mat_mult_with_BLAS} option is set.

  With the default real or complex method, when one operand of
  \code{\%*\%} can be recognized as being the result of a transpose
  operation (\code{\link{t}}), the transpose is not actually computed,
  but instead a matrix multiplication routine (either from the BLAS or
  the appropriate C routine) is used that operates directly on the
  transposed operand.  This is equivalent to use of the
  \code{crossprod} or \code{tcrossprod} functions.  If both operands
  are transposed, the transposed second operand will actually be
  computed, but not the first.

}
\value{
  A double or complex matrix product.  Use \code{\link{drop}} to remove
  dimensions which have only one level.
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
(z <- x \%*\% x)      # scalar ("inner") product (1 x 1 matrix)
drop(z)             # as scalar

y <- diag(x)
z <- matrix(1:12, ncol = 3, nrow = 4)
y \%*\% z
y \%*\% x
x \%*\% z

a <- matrix(1.1,10,10)
f <- function (m) t(m+1i)
f(a) \%*\% a          # the transpose of a+1i is not actually computed

}
\keyword{array}
\keyword{arith}
