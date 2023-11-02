\name{calcLength}
\alias{calcLength}
\title{Calculate the Length of Polylines}
\description{
  Calculate the length of polylines found in a \link{PolySet}.
}
\usage{
calcLength (polys, rollup = 3, close = FALSE)
}
\arguments{
  \item{polys}{\link{PolySet} to use.}
  \item{rollup}{level of detail in the results; \code{1} = \code{PID}s
    only, summing the lengths of each \code{SID} within each \code{PID},
    and \code{3} = no roll-up. Note: rollup \code{2} has no meaning in
    this function and, if specified, will be reset to \code{3}.}
  \item{close}{Boolean value; if \code{TRUE}, include the distance
    between each polygon's last and first vertex, if necessary.}
}
\details{
  If \code{rollup} equals \code{1}, the results contain an entry for
  each unique \code{PID} only.  Setting it to \code{3} prevents roll-up,
  and they contain an entry for each unique (\code{PID}, \code{SID}).

  If the \code{projection} attribute equals \code{"LL"}, this routine uses
  Great Circle distances to compute the surface length of each polyline.
  In doing so, the algorithm simplifies Earth to a sphere.

  If the \code{projection} attribute equals \code{"UTM"} or \code{1}, this
  routine uses Pythagoras' Theorem to calculate lengths.
}
\value{
  \link{PolyData} with columns \code{PID}, \code{SID} (\emph{may be missing}),
  and \code{length}.  If \code{projection} equals \code{"UTM"} or
  \code{"LL"}, lengths are in kilometres.  Otherwise, lengths are in the
  same unit as the input \link{PolySet}.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2013-04-10}
}
\seealso{
  \code{\link{calcArea}},
  \code{\link{calcCentroid}},
  \code{\link{calcMidRange}},
  \code{\link{calcSummary}},
  \code{\link{locatePolys}}.
}
\examples{
local(envir=.PBSmapEnv,expr={
  #--- load the data (if using R)
  if (!is.null(version$language) && (version$language=="R"))
    data(nepacLL,envir=.PBSmapEnv)
  #--- calculate the perimeter of Vancouver Island
  print(calcLength(nepacLL[nepacLL$PID==33, ]))
})
}
\keyword{manip}

