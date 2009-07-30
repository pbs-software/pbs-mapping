\name{nepacLL}
\docType{data}
\alias{nepacLL}
\title{Data: Shoreline of the NE Pacific Ocean (Normal Resolution)}
\description{
  \link{PolySet} of polygons for the northeast Pacific Ocean shoreline.
}
\usage{data(nepacLL)}
\format{
  Data frame consisting of 4 columns: \code{PID} = primary polygon ID,
  \code{POS} = position of each vertex within a given polygon, \code{X}
  = longitude coordinate, and \code{Y} = latitude coordinate.   Attributes:
  \code{projection = "LL"}.
}
\note{
  In R, the data must be loaded using the \code{\link{data}} function.
}
\source{
  Polygon data from the GSHHS (Global Self-consistent, Hierarchical,
  High-resolution Shoreline) database \code{gshhs_h.b}. \cr
  Download from \url{http://www.soest.hawaii.edu/wessel/gshhs/gshhs.html}

\preformatted{
nepacLL <-importGSHHS("gshhs_h.b", xlim=c(-190,-110), ylim=c(34,72), 
                      level=1, n=15, xoff=-360)
}
}
\references{
  Wessel, P. and Smith, W.H.F. (1996) A global, self-consistent,
  hierarchical, high-resolution shoreline database. \emph{Journal of
  Geophysical Research} \bold{101}, 8741--8743. \cr
  \url{http://www.soest.hawaii.edu/pwessel/pwessel_pubs.html}
}
\seealso{
Data:  \code{\link{nepacLLhigh}}, \code{\link{worldLL}}, \code{\link{worldLLhigh}}, \code{\link{bcBathymetry}}

  \code{\link{importGSHHS}}, \code{\link{addPolys}}, \code{\link{clipPolys}}, 
  \code{\link{plotPolys}}, \code{\link{plotMap}}, \code{\link{thickenPolys}},
  \code{\link{thinPolys}}
}
\keyword{datasets}
