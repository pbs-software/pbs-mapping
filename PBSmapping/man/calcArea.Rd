\name{calcArea}
\alias{calcArea}
\title{Calculate the Areas of Polygons}
\description{
  Calculate the areas of polygons found in a \link{PolySet}.
}
\usage{
calcArea (polys, rollup = 3)
}
\arguments{
  \item{polys}{\link{PolySet} to use.}
  \item{rollup}{level of detail in the results; \code{1} = \code{PID}s
    only, by summing all the polygons with the same \code{PID}, \code{2} =
    outer contours only, by subtracting holes from their parent, and
    \code{3} = no roll-up.}
}
\details{
  If \code{rollup} equals \code{1}, the results contain an area for each
  unique \code{PID} only.  When it equals \code{2}, they contain entries
  for outer contours only.  Finally, setting it to \code{3} prevents
  roll-up, and they contain areas for each unique (\code{PID},
  \code{SID}).

  Outer polygons have positive areas and inner polygons negative areas.
  When polygons are rolled up, the routine sums the positive and
  negative areas and consequently accounts for holes.

  If the \link{PolySet}'s \code{projection} attribute equals
  \code{"LL"}, the function projects the \link{PolySet} in UTM first.
  If the \link{PolySet}'s \code{zone} attribute exists, it uses it for
  the conversion.  Otherwise, it computes the mean longitude and uses
  that value to determine the zone.  The longitude range of zone
  \eqn{i} is \eqn{-186 + 6 i^\circ < x \le -180 + 6 i^\circ}{-186 + 6i < x <= -180 + 6i degrees}.
}
\value{
  \link{PolyData} with columns \code{PID}, \code{SID} (\emph{may be
  missing}), and \code{area}.  If the projection equals \code{"LL"} or
  \code{"UTM"}, the units of area are square kilometres.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2022-09-06}
}
\seealso{
  \code{\link{calcCentroid}},
  \code{\link{calcLength}},
  \code{\link{calcMidRange}},
  \code{\link{calcSummary}},
  \code{\link{locatePolys}}.
}
\examples{
local(envir=.PBSmapEnv,expr={
  #--- load the data (if using R)
  if (!is.null(version$language) && (version$language == "R"))
    data(nepacLL,envir=.PBSmapEnv)
  #--- convert LL to UTM so calculation makes sense
  attr(nepacLL, "zone") <- 9
  nepacUTM <- convUL(nepacLL)
  #--- calculate and print the areas
  print(calcArea(nepacUTM))
})
}
\keyword{manip}
