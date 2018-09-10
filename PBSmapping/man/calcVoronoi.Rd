\name{calcVoronoi}
\alias{calcVoronoi}
\title{Calculate the Voronoi (Dirichlet) Tesselation for a Set of Points}
\description{
  Calculate the Voronoi (Dirichlet) tesselation for a set of points.
}
\usage{
calcVoronoi (xydata, xlim = NULL, ylim = NULL, eps = 1e-09, frac = 0.0001)
}
\arguments{
  \item{xydata}{a data frame with columns \code{X} and \code{Y}
    containing the points.}
  \item{xlim}{range of X-coordinates; a bounding box for the
    coordinates.}
  \item{ylim}{range of Y-coordinates; a bounding box for the
    coordinates.}
  \item{eps}{the value of epsilon used in testing whether a quantity is
    zero.}
  \item{frac}{used to detect duplicate input points, which meet the
    condition \eqn{\left | x1 - x2 \right | < \mbox{frac} \times
    (\mbox{xmax} - \mbox{xmin}) \mbox{and} \left | y1 - y2 \right | <
    \mbox{frac} \times (\mbox{ymax} - \mbox{ymin})}{abs(x1-x2) <
    frac*(xmax-xmin) and abs(y1-y2) < frac*(ymax-ymin)}.}
}
\details{
  This routine ignores all columns other than \code{X} and \code{Y}.

  If the user leaves \code{xlim} and \code{ylim} unspecified, the
  function defaults to the range of the data with each extent expanded
  by ten percent of the range.
  
  This function sets the attribute \code{projection} to \code{1} and the
  attribute \code{zone} to \code{NULL} as it assumes this projection in
  its calculations.
}
\value{
  \link{PolySet} with columns \code{PID}, \code{POS}, \code{X}, and \code{Y}.
}
\author{
  \href{https://academic.macewan.ca/boersn/}{Nicholas M. Boers}, Associate Professor -- Computer Science\cr
  MacEwan University, Edmonton AB\cr
  Last modified \code{Rd: 2013-04-10}
}
\seealso{
  \code{\link{addPoints}},
  \code{\link{addPolys}},
  \code{\link{calcArea}},
  \code{\link{calcCentroid}},
  \code{\link{calcConvexHull}},
  \code{\link{calcMidRange}},
  \code{\link{calcSummary}},
  \code{\link{locateEvents}},
  \code{\link{plotMap}},
  \code{\link{plotPoints}},
  \code{\link{plotPolys}}.
}
\examples{
local(envir=.PBSmapEnv,expr={
  oldpar = par(no.readonly=TRUE)
  #--- create some EventData
  events <- as.EventData(data.frame(
    EID=1:200, X=rnorm(200), Y=rnorm(200)), projection=1)
  #--- calculate the Voronoi tesselation
  polys <- calcVoronoi(events)
  #--- create PolyData to color it based on area
  polyData <- calcArea(polys)
  names(polyData)[is.element(names(polyData), "area")] <- "Z"
  colSeq <- seq(0.4, 0.95, length=4)
  polyData <- makeProps(polyData,
    breaks=quantile(polyData$Z,c(0,.25,.5,.75,1)),
    propName="col", propVals=rgb(colSeq,colSeq,colSeq))
  #--- plot the tesselation
  plotMap(polys, polyProps=polyData)
  #--- plot the points
  addPoints(events, pch=19)
  par(oldpar)
})
}
\keyword{manip}

