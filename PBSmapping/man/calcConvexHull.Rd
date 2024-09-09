\name{calcConvexHull}
\alias{calcConvexHull}
\alias{.closestPoint}
\title{Calculate Convex Hull for a Set of Points}
\description{
  Calculate the convex hull for a set of points.
}
\usage{
calcConvexHull(xydata, keepExtra=FALSE)
.closestPoint(pts, pt)
}
\arguments{
  \item{xydata}{\code{numeric} -- data frame with columns \code{'X'} and \code{'Y'}
    containing spatial coordinates}
  \item{keepExtra}{\code{logical} -- if \code{TRUE}, retain any additional columns
    from the input data frame \code{'xydata'}.}
  \item{pts}{\code{numeric} -- data frame with columns \code{'X'} and \code{'Y'}}
  \item{pt}{\code{numeric} -- data frame with columns \code{'X'} and \code{'Y'} and ONE row}
}
\details{
  Uses the function \code{chull()} in the package grDevices.
  By default, it ignores all columns other than \code{X} and \code{Y};
  however, the user can choose to retain additional columns in \code{xydata}
  by specifying \code{keepExtra=TRUE}.
}
\value{
  \link{PolySet} with columns \code{PID}, \code{POS}, \code{X}, \code{Y},
  and additional columns in \code{xydata} if \code{keepExtra=TRUE}.

  Auxiliary dot function \code{'.closestPoint'} returns a vector of length
  \code{'pts'} where \code{TRUE} indicates that the point is closest to \code{'pt'}.
  Returns several \code{TRUE} values when several points are equidistant.
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
  \code{\link{addPoints}},
  \code{\link{addPolys}},
  \code{\link{calcArea}},
  \code{\link{calcCentroid}},
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
  data(surveyData,envir=.PBSmapEnv)
  #--- plot the convex hull, and then plot the points
  plotMap(calcConvexHull(surveyData),col="moccasin")
  addPoints(surveyData,col="blue",pch=17,cex=.6)
  par(oldpar)
})
}
\keyword{manip}
\concept{PBSmapping.r}
\concept{dotFuns.r}
