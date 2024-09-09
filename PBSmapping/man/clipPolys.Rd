\name{clipPolys}
\alias{clipPolys}
\alias{clipLines}
\alias{.clip}
\title{
  Clip a PolySet as Polygons or Polylines
}
\description{
  Clip a \code{'PolySet'}, where each unique (\code{'PID'}, \code{'SID'})
  describes a polygon or polyline.
}
\usage{
clipPolys (polys, xlim, ylim, keepExtra = FALSE)

clipLines (polys, xlim, ylim, keepExtra = FALSE)

.clip(polys, xlim, ylim, isPolygons, keepExtra)
}
\arguments{
  \item{polys}{\code{data.frame} -- \code{'PolySet'} to clip}
  \item{xlim}{\code{numeric} -- range of X-coordinates}
  \item{ylim}{\code{numeric} -- range of Y-coordinates}
  \item{keepExtra}{\code{logical} -- if \code{TRUE}, try to
    carry forward any non-standard columns into the result}
  \item{isPolygons}{\code{logical} -- if \code{TRUE}, then \code{PolySet} contains
    polygons, else \code{PolySet} contains polylines}
}
\details{
  For each discrete polygon, the function connects vertices 1 and N
  (does not connect vertices 1 and N for discrete polylines).
  It recalculates the \code{'POS'} values for each vertex, saving the old
  values in a column named \code{'oldPOS'}.  For new vertices, it sets
  \code{'oldPOS'} to \code{NA}.
}
\value{
  \code{'PolySet'} containing the input data, with some points added or
  removed.  A new column \code{'oldPOS'} records the original \code{'POS'}
  value for each vertex.
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
  \code{\link[PBSmapping]{addLines}},
  \code{\link[PBSmapping]{addPolys}},
  \code{\link[PBSmapping]{addStipples}},
  \code{\link[PBSmapping]{PolySet}}
}
\examples{
local(envir=.PBSmapEnv,expr={
  oldpar = par(no.readonly=TRUE)
  #--- create a triangle that will be clipped
  polys <- data.frame(PID=rep(1, 3), POS=1:3, X=c(0,1,.5), Y=c(0,0,1))
  #--- clip the triangle in the X direction, and plot the results
  plotPolys(clipPolys(polys,xlim=c(0,.75),ylim=range(polys[,"Y"])),col=2)
  par(oldpar)
})
}
\keyword{manip}
\concept{PBSmapping.r}
\concept{dotFuns.r}

