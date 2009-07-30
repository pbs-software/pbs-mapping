\name{worldLL}
\docType{data}
\alias{worldLL}
\title{Data: Shorelines of the World (Normal Resolution)}
\description{
  \link{PolySet} of polygons for the global shorelines.
}
\usage{data(worldLL)}
\format{
  Data frame consisting of 4 columns: \code{PID} = primary polygon ID,
  \code{POS} = position of each vertex within a given polygon, \code{X}
  = longitude coordinate, and \code{Y} = latitude coordinate. Attributes:
  \code{projection = "LL"}.
}
\note{
  In R, the data must be loaded using the \code{\link{data}} function.
}
\source{
  Polygon data from the GSHHS (Global Self-consistent, Hierarchical,
  High-resolution Shoreline) database \code{gshhs_l.b}. \cr
  Download from \url{http://www.soest.hawaii.edu/wessel/gshhs/gshhs.html}

\preformatted{
worldLL <-importGSHHS("gshhs_l.b", xlim=c(-20,360), ylim=c(-90,90), 
                      level=1, n=15, xoff=0)
worldLL <- .fixGSHHSWorld(worldLL)
}
}
\references{
  Wessel, P. and Smith, W.H.F. (1996) A global, self-consistent,
  hierarchical, high-resolution shoreline database. \emph{Journal of
  Geophysical Research} \bold{101}, 8741--8743. \cr
  \url{http://www.soest.hawaii.edu/pwessel/pwessel_pubs.html}
}
\seealso{
Data:  \code{\link{worldLLhigh}}, \code{\link{nepacLL}}, \code{\link{nepacLLhigh}}

  \code{\link{importGSHHS}}, \code{\link{addPolys}}, \code{\link{clipPolys}},
  \code{\link{plotPolys}}, \code{\link{plotMap}}, \code{\link{thickenPolys}},
  \code{\link{thinPolys}}
}
\keyword{datasets}
