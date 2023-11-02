\name{clipLines}
\alias{clipLines}
\title{Clip a PolySet as Polylines}
\description{
  Clip a \link{PolySet}, where each unique (\code{PID}, \code{SID})
  describes a polyline.
}
\usage{
clipLines (polys, xlim, ylim, keepExtra = FALSE)
}
\arguments{
  \item{polys}{\link{PolySet} to clip.}
  \item{xlim}{range of X-coordinates.}
  \item{ylim}{range of Y-coordinates.}
  \item{keepExtra}{Boolean value; if \code{TRUE}, tries to
    carry forward any non-standard columns into the result.}
}
\details{
  For each discrete polyline, the function does not connect vertices 1
  and N.  It recalculates the \code{POS} values for each vertex, saving
  the old values in a column named \code{oldPOS}.  For new vertices, it
  sets \code{oldPOS} to \code{NA}.
}
\value{
  \link{PolySet} containing the input data, with some points added or
  removed.  A new column \code{oldPOS} records the original \code{POS}
  value for each vertex.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2013-04-10}
}
\seealso{
  \code{\link{clipPolys}},
  \code{\link{fixBound}}.
}
\examples{
local(envir=.PBSmapEnv,expr={
  oldpar = par(no.readonly=TRUE)
  #--- create a triangle to clip
  polys <- data.frame(PID=rep(1, 3), POS=1:3, X=c(0,1,0), Y=c(0,0.5,1))
  #--- clip the triangle in the X direction, and plot the results
  plotLines(clipLines(polys, xlim=c(0,.75), ylim=range(polys[, "Y"])))
  par(oldpar)
})
}
\keyword{manip}

