\name{nepacLL}
\docType{data}
\alias{nepacLL}
\alias{nepacLLhigh}
\alias{worldLL}
\alias{worldLLhigh}
\title{
  Data: Shorelines of the NE Pacific Ocean and of the World
}
\description{
  \link{PolySet} of polygons for the shorelines of the northeast Pacific Ocean
  and of the world, both in normal and high resolution.
}
\usage{
  data(nepacLL)
  data(nepacLLhigh)
  data(worldLL)
  data(worldLLhigh)
}
\format{
  Data frame consisting of 4 columns: \code{PID} = primary polygon ID,
  \code{POS} = position of each vertex within a given polygon, \code{X}
  = longitude coordinate, and \code{Y} = latitude coordinate.   Attributes:
  \code{projection = "LL"}.
}
\note{
  In R, the data must be loaded using the \code{\link[utils]{data}} function.
}
\source{
  Polygon data from the GSHHG (Global Self-consistent, Hierarchical,
  High-resolution Geography) Database. \cr
  Download the native binary files of shoreline polygons, rivers, and borders
  contained in the latest zip archive (version 2.3.4 ) at 
  \url{http://www.soest.hawaii.edu/pwessel/gshhg/}.

\preformatted{
nepacLL <- importGSHHS("gshhs_h.b", xlim=c(-190,-110), ylim=c(34,72), 
           level=1, n=15, xoff=-360)

nepacLLhigh <- importGSHHS("gshhs_f.b", xlim=c(-190,-110),
               ylim=c(34,72), level=1, n=0, xoff=-360)
nepacLLhigh <- thinPolys(nepacLLhigh, tol=0.1, filter=3)

worldLL <- importGSHHS("gshhs_l.b", xlim=c(-20,360), ylim=c(-90,90), 
           level=1, n=15, xoff=0)
worldLL <- PBSmapping:::.fixGSHHSWorld(worldLL)

worldLLhigh <- importGSHHS("gshhs_i.b", xlim=c(-20,360),
               ylim=c(-90,90), level=1, n=15, xoff=0)
worldLLhigh <- PBSmapping:::.fixGSHHSWorld(worldLLhigh)
}
}
\references{
  Wessel, P. and Smith, W.H.F. (1996) A global, self-consistent,
  hierarchical, high-resolution shoreline database. \emph{Journal of
  Geophysical Research} \bold{101}, 8741--8743. \cr
  \url{http://www.soest.hawaii.edu/pwessel/gshhg/Wessel+Smith_1996_JGR.pdf}
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2023-10-30}
}
\seealso{
Data:\cr
  \code{\link[PBSmapping]{bcBathymetry}},
  \code{\link[PBSmapping]{surveyData}},
  \code{\link[PBSmapping]{towData}}

Functions:\cr
  \code{\link[PBSmapping]{importGSHHS}},
  \code{\link[PBSmapping]{plotMap}},
  \code{\link[PBSmapping]{plotPolys}},
  \code{\link[PBSmapping]{addPolys}},
  \code{\link[PBSmapping]{clipPolys}},
  \code{\link[PBSmapping]{refocusWorld}},
  \code{\link[PBSmapping]{thickenPolys}},
  \code{\link[PBSmapping]{thinPolys}}
}
\keyword{datasets}
