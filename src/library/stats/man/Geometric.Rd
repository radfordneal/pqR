% File src/library/stats/man/Geometric.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Modifications for pqR Copyright (c) 2019 Radford M. Neal.
% Distributed under GPL 2 or later

\name{Geometric}
\alias{Geometric}
\alias{dgeom}
\alias{pgeom}
\alias{qgeom}
\alias{rgeom}
\title{The Geometric Distribution}
\description{
  Density, distribution function, quantile function and random
  generation for the geometric distribution with parameter \code{prob}.
}
\usage{
dgeom(x, prob, log = FALSE)
pgeom(q, prob, lower.tail = TRUE, log.p = FALSE)
qgeom(p, prob, lower.tail = TRUE, log.p = FALSE)
rgeom(n, prob)
}
\arguments{
  \item{x, q}{vector of quantiles representing the number of failures in
    a sequence of Bernoulli trials before success occurs.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If \code{length(n) > 1}, the length
    is taken to be the number required.}
  \item{prob}{probability of success in each trial. \code{0 < prob <= 1}.}
  \item{log}{logical; if TRUE, the log of the density is returned.}
  \item{log.p}{logical; if TRUE, the quantile function takes the
    log of the tail probability, or the distribution function returns the
    log of the tail probability. Does not alter the meaning of \code{prob}.}
  \item{lower.tail}{logical; if TRUE (which is the default), tail probabilities
    are \eqn{P[X \le x]}, otherwise, \eqn{P[X > x]}.}
}
\details{
  The geometric distribution with \code{prob} \eqn{= p} has density
  \deqn{p(x) = p {(1-p)}^{x}}{p(x) = p (1-p)^x}
  for \eqn{x = 0, 1, 2, \ldots}, \eqn{0 < p \le 1}.

  If an element of \code{x} is not integer, the result of \code{dgeom}
  is zero, with a warning.

  The quantile is defined as the smallest value \eqn{x} such that
  \eqn{F(x) \ge p}, where \eqn{F} is the distribution function.
}
\value{
  \code{dgeom} gives the density,
  \code{pgeom} gives the distribution function,
  \code{qgeom} gives the quantile function, and
  \code{rgeom} generates random deviates.

  Invalid \code{prob} will result in return value \code{NaN}, with a warning.
}
\source{
  \code{dgeom} computes via \code{dbinom}, using code contributed by
  Catherine Loader (see \code{\link{dbinom}}).

  \code{pgeom} and \code{qgeom} are based on the closed-form formulae.
  
  \code{rgeom} uses the derivation as an exponential mixture of Poissons, see

  Devroye, L. (1986) \emph{Non-Uniform Random Variate Generation.}
  Springer-Verlag, New York. Page 480.
}
\seealso{
  \link{Distributions} for other standard distributions, including
  \code{\link{dnbinom}} for the negative binomial which generalizes
  the geometric distribution.
}
\examples{
qgeom((1:9)/10, prob = .2)
Ni <- rgeom(20, prob = 1/4); table(factor(Ni, 0:max(Ni)))
}
\keyword{distribution}
