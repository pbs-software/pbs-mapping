\name{calcCentroid}
\alias{calcCentroid}
\title{Calculate the Centroids of Polygons}
\description{
  Calculate the centroids of polygons found in a \link{PolySet}.
}
\usage{
calcCentroid (polys, rollup = 3)
}
\arguments{
  \item{polys}{\link{PolySet} to use.}
  \item{rollup}{level of detail in the results; \code{1} = \code{PID}s
    only, \code{2} = outer contours only, and \code{3} = no roll-up.  When
    \code{rollup} equals \code{1} and \code{2}, the function appropriately
    adjusts for polygons with holes.}
}
\details{
  If \code{rollup} equals \code{1}, the results contain a centroid for
  each unique \code{PID} only.  When it equals \code{2}, they contain
  entries for outer contours only.  Finally, setting it to \code{3}
  prevents roll-up, and they contain a centroid for each unique
  (\code{PID}, \code{SID}).
}
\value{
  \link{PolyData} with columns \code{PID}, \code{SID} (\emph{may be missing}),
  \code{X}, and \code{Y}.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2013-04-10}
}
\seealso{
  \code{\link{calcArea}},
  \code{\link{calcLength}},
  \code{\link{calcMidRange}},
  \code{\link{calcSummary}},
  \code{\link{locateEvents}},
  \code{\link{locatePolys}}.
}
\examples{
local(envir=.PBSmapEnv,expr={
  #--- load the data (if using R)
  if (!is.null(version$language) && (version$language=="R"))
    data(nepacLL,envir=.PBSmapEnv)
  #--- calculate and print the centroids for several polygons
  print(calcCentroid(nepacLL[is.element(nepacLL$PID,c(33,39,47)),]))
})
}
\keyword{manip}

