% File src/library/graphics/man/grid.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{grid}
\alias{grid}
\title{Add Grid to a Plot}
\description{
  \code{grid} adds an \code{nx} by \code{ny} rectangular grid to an
  existing plot.
}
\usage{
grid(nx = NULL, ny = nx, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
}
\arguments{
  \item{nx,ny}{number of cells of the grid in x and y direction.  When
    \code{NULL}, as per default, the grid aligns with the tick marks on
    the corresponding \emph{default} axis (i.e., tickmarks as computed by
    \code{\link{axTicks}}).  When \code{\link{NA}}, no grid lines are
    drawn in the corresponding direction.}
  \item{col}{character or (integer) numeric; color of the grid lines.}
  \item{lty}{character or (integer) numeric; line type of the grid lines.}
  \item{lwd}{non-negative numeric giving line width of the grid lines.}
  \item{equilogs}{logical, only used when \emph{log} coordinates and
    alignment with the axis tick marks are active.  Setting \code{equilogs =
      FALSE} in that case gives \emph{non equidistant} tick aligned
    grid lines.}
}
\note{
  If more fine tuning is required, use \code{\link{abline}(h = ., v = .)}
  directly.
}
\seealso{
  \code{\link{plot}}, \code{\link{abline}}, \code{\link{lines}},
  \code{\link{points}}.
}
\references{
  Murrell, P. (2005) \emph{R Graphics}. Chapman & Hall/CRC Press.
}
\examples{
plot(1:3)
grid(NA, 5, lwd = 2) # grid only in y-direction

## maybe change the desired number of tick marks:  par(lab=c(mx,my,7))
op <- par(mfcol = 1:2)
with(iris,
     {
     plot(Sepal.Length, Sepal.Width, col = as.integer(Species),
          xlim = c(4, 8), ylim = c(2, 4.5), panel.first = grid(),
          main = "with(iris,  plot(...., panel.first = grid(), ..) )")
     plot(Sepal.Length, Sepal.Width, col = as.integer(Species),
          panel.first = grid(3, lty=1,lwd=2),
          main = "... panel.first = grid(3, lty=1,lwd=2), ..")
     }
    )
par(op)

}
\keyword{aplot}
