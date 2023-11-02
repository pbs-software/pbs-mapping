\name{closePolys}
\alias{closePolys}
\title{Close a PolySet}
\description{
  Close a \link{PolySet} of polylines to form polygons.
}
\usage{
closePolys (polys)
}
\arguments{
  \item{polys}{\link{PolySet} to close.}
}
\details{
  Generally, run \code{fixBound} before this function. The ranges of a
  \link{PolySet}'s \code{X} and \code{Y} columns define the boundary.
  For each discrete polygon, this function determines if the first and
  last points lie on a boundary.  If both points lie on the same
  boundary, it adds no points.  However, if they lie on different
  boundaries, it may add one or two corners to the polygon.
  
  When the boundaries are adjacent, one corner will be added as
  follows:
  \itemize{
    \item top boundary + left boundary implies add top-left corner;
    \item top boundary + right boundary implies add top-right corner;
    \item bottom boundary + left boundary implies add bottom-left
    corner;
    \item bottom boundary + right boundary implies add bottom-right
    corner.
  }
  When the boundaries are opposite, it first adds the corner closest to
  a starting or ending polygon vertex.  This determines a side
  (left-right or bottom-top) that connects the opposite boundaries.
  Then, it adds the other corner of that side to close the polygon.
}
\value{
  \link{PolySet} identical to \code{polys}, except for possible
  additional corner points.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2013-04-10}
}
\seealso{
  \code{\link{fixBound}},
  \code{\link{fixPOS}}.
}
\examples{
local(envir=.PBSmapEnv,expr={
  oldpar = par(no.readonly=TRUE)
  #--- 4 corners
  polys <- data.frame(
    PID = c(1, 1, 2, 2, 3, 3, 4, 4),
    POS = c(1, 2, 1, 2, 1, 2, 1, 2),
    X   = c(0, 1, 2, 3, 0, 1, 2, 3),
    Y   = c(1, 0, 0, 1, 2, 3, 3, 2))
  plotPolys(closePolys(polys), col=2)

  #--- 2 corners and 1 opposite
  polys <- data.frame(
    PID = c(1, 1, 2, 2, 3, 3, 3),
    POS = c(1, 2, 1, 2, 1, 2, 3),
    X   = c(0, 1, 0, 1, 5, 6, 1.5),
    Y   = c(1, 0, 2, 3, 0, 1.5, 3))
  plotPolys(closePolys(polys), col=2)
  par(oldpar)
})
}
\keyword{manip}

