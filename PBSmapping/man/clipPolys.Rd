\name{clipPolys}
\alias{clipPolys}
\title{Clip a PolySet as Polygons}
\description{
  Clip a \link{PolySet}, where each unique (\code{PID}, \code{SID}) describes
  a polygon.
}
\usage{
clipPolys (polys, xlim, ylim, keepExtra = FALSE)
}
\arguments{
  \item{polys}{\link{PolySet} to clip.}
  \item{xlim}{range of X-coordinates.}
  \item{ylim}{range of Y-coordinates.}
  \item{keepExtra}{Boolean value; if \code{TRUE}, tries to
    carry forward any non-standard columns into the result.}
}
\details{
  For each discrete polygon, the function connects vertices 1 and N.  It
  recalculates the \code{POS} values for each vertex, saving the old
  values in a column named \code{oldPOS}.  For new vertices, it sets
  \code{oldPOS} to \code{NA}.
}
\value{
  \link{PolySet} containing the input data, with some points added or
  removed.  A new column \code{oldPOS} records the original \code{POS}
  value for each vertex.
}
\seealso{
  \code{\link{clipLines}},
  \code{\link{fixBound}}.
}
\examples{
#--- create a triangle that will be clipped
polys <- data.frame(PID=rep(1, 3), POS=1:3, X=c(0,1,.5), Y=c(0,0,1))
#--- clip the triangle in the X direction, and plot the results
plotPolys(clipPolys(polys, xlim=c(0,.75), ylim=range(polys[, "Y"])),
          col=2);
}
\keyword{manip}
