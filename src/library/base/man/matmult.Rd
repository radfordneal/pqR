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

  The default method for real matrices uses either built-in
  C routines or the appropriate BLAS routine, as controlled by the 
  \code{mat_mult_with_BLAS} option.  This option is a vector of four 
  logical values, specifying whether a BLAS routine should be used when 
  the operands of \code{\%*\%} result in the operation being a vector dot 
  product, a product of a matrix and column vector, a product of a row vector 
  and a matrix, or a general matrix-matrix product.  The default is to use
  the builtin C routines for all operations, unless the 
  \code{--enable-mat-mult-with-BLAS-by-default} option is used 
  when configuring R.

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
  packages. It's also conceivable that a built-in C routine might be
  faster.  Note that some other linear algebra operations make use of
  the BLAS matrix multiply routines, regardless of how this option is
  set.  The BLAS routines are also always used for \code{crossprod}
  and \code{tcrossprod} (which combine a transpose with a matrix
  multiply).
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
(z <- x \%*\% x)    # scalar ("inner") product (1 x 1 matrix)
drop(z)             # as scalar

y <- diag(x)
z <- matrix(1:12, ncol = 3, nrow = 4)
y \%*\% z
y \%*\% x
x \%*\% z
}
\keyword{array}
\keyword{arith}
