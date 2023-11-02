\name{calcSummary}
\alias{calcSummary}
\title{Apply Functions to Polygons in a PolySet}
\description{
  Apply functions to polygons in a \link{PolySet}.
}
\usage{
calcSummary (polys, rollup = 3, FUN, \dots)
}
\arguments{
  \item{polys}{\link{PolySet} to use.}
  \item{rollup}{level of detail in the results; \code{1} = \code{PID}s
    only, by removing the \code{SID} column, and then passing each
    \code{PID} into \code{FUN}, \code{2} = outer contours only, by making
    hole \code{SID}s equal to their parent's \code{SID}, and then passing
    each (\code{PID}, \code{SID}) into \code{FUN}, and \code{3} = no
    roll-up.}
  \item{FUN}{the function to apply; it must accept a vector and return a
    vector or scalar.}
  \item{\dots}{optional arguments for \code{FUN}.}
}
\details{
  If \code{rollup} equals \code{1}, the results contain an entry for
  each unique \code{PID} only.  When it equals \code{2}, they contain
  entries for outer contours only.  Finally, setting it to \code{3}
  prevents roll-up, and they contain an entry for each unique
  (\code{PID}, \code{SID}).
}
\value{
  \link{PolyData} with columns \code{PID}, \code{SID} (\emph{may be missing}),
  \code{X}, and \code{Y}.  If \code{FUN} returns a vector of length
  greater than 1 (say \emph{n}), names the columns \code{X1}, \code{X2},
  ..., \code{X}\emph{n} and \code{Y1}, \code{Y2}, ..., \code{Y}\emph{n}.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2013-04-10}
}
\seealso{
  \code{\link{calcArea}},
  \code{\link{calcCentroid}},
  \code{\link{calcConvexHull}},
  \code{\link{calcLength}},
  \code{\link{calcMidRange}},
  \code{\link{combineEvents}},
  \code{\link{findPolys}},
  \code{\link{locateEvents}},
  \code{\link{locatePolys}},
  \code{\link{makeGrid}},
  \code{\link{makeProps}}.
}
\examples{
local(envir=.PBSmapEnv,expr={
  #--- load the data (if using R)
  if (!is.null(version$language) && (version$language=="R"))
    data(nepacLL,envir=.PBSmapEnv)
  #--- calculate and print the centroids for several polygons
  print(calcSummary(nepacLL[is.element(nepacLL$PID,c(33,39,47)),],
    rollup=3, FUN=mean))
})
}
\keyword{manip}

