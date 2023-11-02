\name{thickenPolys}
\alias{thickenPolys}
\title{Thicken a PolySet of Polygons}
\description{
  Thicken a \link{PolySet}, where each unique (\code{PID}, \code{SID})
  describes a polygon.
}
\usage{
thickenPolys (polys, tol = 1, filter = 3, keepOrig = TRUE,
              close = TRUE)
}
\arguments{
  \item{polys}{\link{PolySet} to thicken.}
  \item{tol}{tolerance (in kilometres when \code{proj} is
    \code{"LL"} and \code{"UTM"}; otherwise, same units as
    \code{polys}).}
  \item{filter}{minimum number of vertices per result polygon.}
  \item{keepOrig}{Boolean value; if \code{TRUE}, keep the original
    points in the \link{PolySet}.}
  \item{close}{Boolean value; if \code{TRUE}, create intermediate vertices
    between each polygon's last and first vertex, if necessary.}
}
\details{
  This function thickens each polygon within \code{polys} according to
  the input arguments.

  If \code{keepOrig = TRUE}, all of the original vertices appear in the
  result.  It calculates the distance between two sequential original
  vertices, and if that distance exceeds \code{tol}, it adds a
  sufficient number of vertices spaced evenly between the two original
  vertices so that the distance between vertices no longer exceeds
  \code{tol}.  If \code{close = TRUE}, it adds intermediate vertices
  between the last and first vertices when necessary.

  If \code{keepOrig = FALSE}, only the first vertex of each polygon is
  guaranteed to appear in the results.  From this first vertex, the
  algorithm walks the polygon summing the distance between vertices.
  When this cumulative distance exceeds \code{tol}, it adds a vertex on
  the line segment under inspection.  After doing so, it resets the
  distance sum, and walks the polygon from this new vertex.  If
  \code{close = TRUE}, it will walk the line segment from the last
  vertex to the first.
}
\value{
  \link{PolySet} containing the thickened data.  The function
  recalculates the \code{POS} values for each polygon.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2013-04-10}
}
\seealso{
  \code{\link{thinPolys}}.
}
\examples{
local(envir=.PBSmapEnv,expr={
  oldpar = par(no.readonly=TRUE)
  #--- load the data (if using R)
  if (!is.null(version$language) && (version$language=="R"))
    data(nepacLL,envir=.PBSmapEnv)
  #--- plot Vancouver Island
  plotMap(nepacLL[nepacLL$PID == 33, ])
  #--- calculate a thickened version using a 30 kilometres tolerance,
  #--- without keeping the original points
  p <- thickenPolys(nepacLL[nepacLL$PID == 33, ], tol = 30, keepOrig = FALSE)
  #--- convert the PolySet to EventData by dropping the PID column and
  #--- renaming POS to EID
  p <- p[-1]; names(p)[1] <- "EID"
  #--- convert the now invalid PolySet into a data frame, and then into
  #--- EventData
  p <- as.EventData(as.data.frame(p), projection="LL")
  #--- plot the results
  addPoints(p, col=2, pch=19)
  par(oldpar)
})
}
\keyword{manip}

