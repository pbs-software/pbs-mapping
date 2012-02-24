\name{thinPolys}
\alias{thinPolys}
\title{Thin a PolySet of Polygons}
\description{
  Thin a \link{PolySet}, where each unique (\code{PID}, \code{SID})
  describes a polygon.
}
\usage{
thinPolys (polys, tol = 1, filter = 3)
}
\arguments{
  \item{polys}{\link{PolySet} to thin.}
  \item{tol}{tolerance (in kilometres when \code{proj} is
    \code{"LL"} and \code{"UTM"}; otherwise, same units as
    \code{polys}).}
  \item{filter}{minimum number of vertices per result polygon.}
}
\details{
  This function executes the Douglas-Peuker line simplification
  algorithm on each polygon within \code{polys}.
}
\value{
  \link{PolySet} containing the thinned data.  The function recalculates
  the \code{POS} values for each polygon.
}
\seealso{
  \code{\link{thickenPolys}}.
}
\examples{
#--- load the data (if using R)
if (!is.null(version$language) && (version$language == "R"))
  data(nepacLL)
#--- plot a thinned version of Vancouver Island (3 km tolerance)
plotMap(thinPolys(nepacLL[nepacLL$PID == 33, ], tol = 3))
#--- add the original Vancouver Island in a different line type to
#--- emphasize the difference
addPolys(nepacLL[nepacLL$PID == 33, ], border=2, lty=8, density=0)
}
\keyword{manip}
