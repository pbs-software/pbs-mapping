\name{calcVoronoi}
\alias{calcVoronoi}
\alias{.expandEdges}
\title{
  Calculate Voronoi Tesselation
}
\description{
  Calculate the Voronoi (Dirichlet) tesselation for a set of points.
}
\usage{
calcVoronoi(xydata, xlim=NULL, ylim=NULL, eps=1e-09, frac=0.0001)

.expandEdges(polys, pts, xlim, ylim)
}
\arguments{
  \item{xydata}{\code{numeric} -- data frame with columns \code{X} and \code{Y}
    containing the points.}
  \item{xlim}{\code{numeric} -- range of X-coordinates; a bounding box for the
    coordinates.}
  \item{ylim}{\code{numeric} -- range of Y-coordinates; a bounding box for the
    coordinates.}
  \item{eps}{\code{numeric} -- the value of epsilon used in testing whether a quantity is
    zero.}
  \item{frac}{\code{numeric} -- used to detect duplicate input points, which meet the
    condition \eqn{\left | x1 - x2 \right | < \mbox{frac} \times
    (\mbox{xmax} - \mbox{xmin}) \mbox{and} \left | y1 - y2 \right | <
    \mbox{frac} \times (\mbox{ymax} - \mbox{ymin})}{abs(x1-x2) <
    frac*(xmax-xmin) and abs(y1-y2) < frac*(ymax-ymin)}}
  \item{polys}{\code{numeric} -- valid \code{PolySet} data frame}
  \item{pts}{\code{numeric} -- data.frame with \code{'X'} and \code{'Y'} columns}
}
\details{
  This routine ignores all columns other than \code{'X'} and \code{'Y'}.

  If the user leaves \code{'xlim'} and \code{'ylim'} unspecified, the
  function defaults to the range of the data with each extent expanded
  by ten percent of the range.
  
  This function sets the attribute \code{'projection'} to \code{1} and the
  attribute \code{'zone'} to \code{NULL} as it assumes this projection in
  its calculations.
}
\value{
  \code{'PolySet'} with columns \code{'PID'}, \code{'POS'}, \code{'X'}, and \code{'Y'}.

  Auxiliary dot function \code{'.expandEdges'} returns an expanded \code{'PolySet'}.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers},
  Software Engineer, Jobber, Edmonton AB

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-09-03}
}
\seealso{
In package \pkg{PBSmapping}:\cr
  \code{\link[PBSmapping]{addPoints}},
  \code{\link[PBSmapping]{addPolys}},
  \code{\link[PBSmapping]{calcArea}},
  \code{\link[PBSmapping]{calcCentroid}},
  \code{\link[PBSmapping]{calcConvexHull}},
  \code{\link[PBSmapping]{calcMidRange}},
  \code{\link[PBSmapping]{calcSummary}},
  \code{\link[PBSmapping]{locateEvents}},
  \code{\link[PBSmapping]{plotMap}},
  \code{\link[PBSmapping]{plotPoints}},
  \code{\link[PBSmapping]{plotPolys}},
  \code{\link[PBSmapping]{PolySet}}
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
\concept{PBSmapping.r}
\concept{dotFuns.r}

