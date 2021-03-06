% File src/library/base/man/serialize.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{serialize}
\alias{serialize}
\alias{unserialize}
\title{Simple Serialization Interface}
\description{
  A simple low-level interface for serializing to connections.
}
\usage{
serialize(object, connection, ascii, xdr = TRUE,
          version = NULL, refhook = NULL, nosharing = FALSE)

unserialize(connection, refhook = NULL)
}
\arguments{
  \item{object}{\R object to serialize.}
  \item{connection}{an open \link{connection} or (for \code{serialize})
    \code{NULL} or (for \code{unserialize}) a raw vector
    (see \sQuote{Details}).}
  \item{ascii}{a logical.  If \code{TRUE}, an ASCII representation is
    written; otherwise binary one.  The default is \code{TRUE} for a
    text-mode connection and \code{FALSE} otherwise.
    See also the comments in the help for \code{\link{save}}.}
  \item{xdr}{a logical: If \code{TRUE}, when a binary representation is 
    used, data is written in big-endian (XDR) format, regardless of the
    platform's native byte order.  The data can then be read back 
    on a platform with either byte-order.}
  \item{version}{the workspace format version to use.  \code{NULL}
    specifies the current default version (2).  Versions prior to 2 are not
    supported, so this will only be relevant when there are later versions.}
  \item{refhook}{a hook function for handling reference objects.}
  \item{nosharing}{if \code{TRUE}, parts of non-reference objects that are
    shared constants, or otherwise shared, will be serialized as if
    they were unshared.}
}
\details{
  The function \code{serialize} serializes \code{object} to the specified
  connection.  If \code{connection} is \code{NULL} then \code{object} is
  serialized to a raw vector, which is returned as the result of
  \code{serialize}.
  
  Sharing of reference objects is preserved within the object but not
  across separate calls to \code{serialize}.  Other parts of objects
  that may be implemented with sharing may be serialized as being
  shared, unless the \code{nosharing} argument is \code{TRUE} (see
  examples below). 

  \code{unserialize} reads an object (as written by \code{serialize})
  from \code{connection} or a raw vector.

  The \code{refhook} functions can be used to customize handling of
  non-system reference objects (all external pointers and weak
  references, and all environments other than namespace and package
  environments and \code{.GlobalEnv}).  The hook function for
  \code{serialize} should return a character vector for references it
  wants to handle; otherwise it should return \code{NULL}.  The hook for
  \code{unserialize} will be called with character vectors supplied to
  \code{serialize} and should return an appropriate object.

  For a text-mode connection, the default value of \code{ascii} is set
  to \code{TRUE}: only ASCII representations can be written to text-mode
  connections and attempting to use \code{ascii = FALSE} will throw an
  error.

  The option of \code{xdr = FALSE} was introduced in \R 2.15.0; it allows
  for binary data to be written in the platform's native byte-order.  As
  almost all systems in current use are little-endian, this can be used
  to avoid byte-shuffling at both ends when transferring data from one
  little-endian machine to another.  The speed gain is small, however.
  
  The format consists of a single line followed by the data: the first
  line contains a single character: \code{X} for binary XDR serialization,
  \code{A} for ASCII serialization, or \code{B} for native byte-order
  binary serialization.  This is followed by a new line.  (The
  format used is identical to that used by \code{\link{readRDS}}.)
}
\section{Warning}{
  These functions have provided a stable interface since \R 2.4.0 (when
  the storage of serialized objects was changed from character to raw
  vectors).  However, the serialization format may change in future
  versions of \R, so this interface should not be used for long-term
  storage of \R objects.

  A raw vector is limited to \eqn{2^{31} - 1}{2^31 - 1} bytes, but \R
  objects can exceed this and their serializations will normally be
  larger than the objects.
}
\value{
  For \code{serialize}, \code{NULL} unless \code{connection = NULL}, when
  the result is returned in a raw vector.

  For \code{unserialize} an \R object.
}
\seealso{
  \code{\link{saveRDS}} for a more convenient interface to serialize an
  object to a file or connection.

  \code{\link{save}} and \code{\link{load}} to serialize and restore one
  or more named objects.

  The \sQuote{R Internals} manual for details of the format used.
}
\examples{
x <- serialize(list(1,2,3), NULL)
unserialize(x)

identical(1L+2L,3L)               # 3L is a shared constant in pqR
identical(serialize(1L+2L,NULL),  # These serializations are not identical
          serialize(3L,NULL))
identical(serialize(1L+2L,NULL,nosharing=TRUE),   # ... but they are if
          serialize(3L,NULL,nosharing=TRUE))      #     nosharing=TRUE

## see also the examples for saveRDS
}
\keyword{file}
\keyword{connection}
