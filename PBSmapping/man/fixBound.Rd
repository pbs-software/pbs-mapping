\name{fixBound}
\alias{fixBound}
\title{Fix the Boundary Points of a PolySet}
\description{
  The ranges of a \link{PolySet}'s \code{X} and \code{Y} columns define
  its boundary.  This function fixes a \link{PolySet}'s vertices by
  moving vertices near a boundary to the actual boundary.
}
\usage{
fixBound (polys, tol)
}
\arguments{
  \item{polys}{\link{PolySet} to fix.}
  \item{tol}{vector (length 1 or 2) specifying a percentage of
    the ranges to use in defining \emph{near} to a boundary. If
    \code{tol} has two elements, the first specifies the tolerance for
    the x-axis and the second the y-axis.  If it has only one element,
    the function uses the same tolerance for both axes.}
}
\details{
  When moving vertices to a boundary, the function moves them strictly
  horizontally or vertically, as appropriate.
}
\value{
  \link{PolySet} identical to the input, except for possible changes in
  the \code{X} and \code{Y} columns.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2013-04-10}
}
\seealso{
  \code{\link{closePolys}},
  \code{\link{fixPOS}},
  \code{\link{isConvex}},
  \code{\link{isIntersecting}},
  \link{PolySet}.
}
\examples{
local(envir=.PBSmapEnv,expr={
  oldpar = par(no.readonly=TRUE)
  #--- set up a long horizontal and long vertical line to extend the plot's
  #--- limits, and then try fixing the bounds of a line in the top-left
  #--- corner and a line in the bottom-right corner
  polys <- data.frame(PID=c(1, 1, 2, 2,  3, 3, 4, 4),
    POS=c(1, 2, 1, 2,  1, 2, 1, 2),
    X = c(0, 10, 5, 5, 0.1, 4.9, 5.1, 9.9),
    Y = c(5, 5, 0, 10, 5.1, 9.9, 0.1, 4.9))
  polys <- fixBound(polys, tol=0.0100001)
  plotLines(polys)
  par(oldpar)
})
}
\keyword{manip}

