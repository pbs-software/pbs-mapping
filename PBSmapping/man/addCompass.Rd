\name{addCompass}
\alias{addCompass}
\title{
  Add Compass to Map
}
\description{
  Add a compass rose to an existing map, similar to those found on 
  nautical charts showing both true north and magnetic north.
}
\usage{
addCompass(X, Y, rot="magN", cex=1, 
   col.compass=c("gainsboro","blue","yellow","black"), ...)
}
\arguments{
  \item{X}{Longitude coordinate (degrees N) for centroid of compass rose.}
  \item{Y}{Latitude coordinate (degrees W) for centroid of compass rose.}
  \item{rot}{Rotation (degrees) counterclockwise from 0 degrees (true North). See Details.}
  \item{cex}{Character expansion to use in the display.}
  \item{col.compass}{Colours for compass rose components (in order):\cr
    1=background compass, 2=rotated arms, 3=central button, 4=pch (broder).}
  \item{\dots}{Additional parameters to pass to the \code{text} function.}
}
\details{
  The basic idea comes from Jim Lemon (see References), but is modified here
  to reflect a compass rose used on BC nautical charts.

  The default rotation (\code{"magN"}) is a calculation of the initial
  bearing of a great-circle arc from the compass position to the north
  geomagnetic pole using the function \code{\link[PBSmapping]{calcGCdist}}.
}
\value{
  No valule returned.
}
\references{
  \href{https://stat.ethz.ch/pipermail/r-sig-geo/2010-February/007564.html}{%
  [R-sig-Geo] How to display a compass rose on a map}

  \href{http://wdc.kugi.kyoto-u.ac.jp/poles/polesexp.html}{%
  Magnetic North, Geomagnetic and Magnetic Poles}
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{opus locus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2016-04-04}
}
\seealso{
  \code{\link[PBSmapping]{addBubbles}},
  \code{\link[PBSmapping]{addLabels}},
  \code{\link[PBSmapping]{addPoints}},
  \code{\link[PBSmapping]{addStipples}},
  \code{\link[PBSmapping]{calcGCdist}}
}
\examples{
local(envir=.PBSmapEnv,expr={
  data(nepacLL,envir=.PBSmapEnv)
  par(mfrow=c(1,1),mar=c(3,4,0.5,0.5))
  plotMap(nepacLL, xlim=c(-134.5,-124.5), ylim=c(48,55), plt=NULL,
    col="lightyellow", cex.axis=1.2, cex.lab=1.5)
  addCompass(-132, 49.5, cex=1.5)
})
}
\keyword{aplot}

