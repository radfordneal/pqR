% File src/library/base/man/file.path.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Modifications for pqR Copyright (c) 2017 Radford M. Neal.
% Distributed under GPL 2 or later

\name{file.path}
\alias{file.path}
\title{Construct Path to File}
\description{
  Construct the path to a file from components in a platform-independent
  way.
}
\usage{
file.path(\dots, fsep = .Platform$file.sep)
}
\arguments{
  \item{\dots}{character vectors.}
  \item{fsep}{the path separator to use.}
}
\details{
  This function can also be used for environment variables 
  representing sets of paths such as \env{PATH} and
  \env{R_LIBS}, with \code{fsep = .Platform$path.sep}.
}
\value{
  A character vector of the arguments concatenated term-by-term and
  separated by \code{fsep} if all arguments have positive length;
  otherwise, an empty character vector (unlike \code{\link{paste}}).
}
\note{
  The components are separated by \code{/} (not \code{\\}) on Windows.
}
\keyword{file}
