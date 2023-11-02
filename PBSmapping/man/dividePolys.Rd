\name{dividePolys}
\alias{dividePolys}
\title{Divide a Single Polygon into Several Polygons}
\description{
  Divide a single polygon (with several outer-contour components) into
  several polygons, a polygon for each outer contour, by modifying the
  \code{PID} and \code{SID} indices.
}
\usage{
dividePolys (polys)
}
\arguments{
  \item{polys}{\link{PolySet} with one or more polygons, each with
    possibly several components/holes.}
}
\details{
  Given the input \link{PolySet}, this function renumbers the \code{PID}
  and \code{SID} indices so that each outer contour has a unique PID and
  is followed by all of its holes, identifying them with \code{SID}s
  greater than one.
}
\value{
  \link{PolySet}, possibly with the addition of an \code{SID} column if
  it did not already exist.  The function may also reorder columns such
  that \code{PID}, \code{SID}, \code{POS}, \code{X} and \code{Y} appear
  first, in that order.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2007-06-06}
}
\seealso{
  \code{\link{combinePolys}}.
}
\keyword{manip}
