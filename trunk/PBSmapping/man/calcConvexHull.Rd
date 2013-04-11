\name{calcConvexHull}
\alias{calcConvexHull}
\title{Calculate the Convex Hull for a Set of Points}
\description{
  Calculate the convex hull for a set of points.
}
\usage{
calcConvexHull (xydata, keepExtra=FALSE)
}
\arguments{
  \item{xydata}{a data frame with columns \code{X} and \code{Y}
    containing spatial coordinates.}
  \item{keepExtra}{logical: if \code{TRUE}, retain any additional columns
    from the input data frame \code{xydata}.}
}
\details{
  This routine uses the function \code{chull()} in the package grDevices.
  By default, it ignores all columns other than \code{X} and \code{Y};
  however, the user can choose to retain additional columns in \code{xydata}
  by specifying \code{keepExtra=TRUE}. 
}
\value{
  \link{PolySet} with columns \code{PID}, \code{POS}, \code{X}, \code{Y},
  and additional columns in \code{xydata} if \code{keepExtra=TRUE}.
}
\seealso{
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

