\name{calcGCdist}
\alias{calcGCdist}
\title{
  Calculate Great-Circle Distance
}
\description{
  Calculate the great-circle distance between geographic (LL)
  coordinates. Also calculate the initial bearing of the 
  great-circle arc (at its starting point).
}
\usage{
calcGCdist(lon1, lat1, lon2, lat2, R=6371.2)
}

\arguments{
  \item{lon1}{\code{numeric} -- Longitude coordinate (degrees) of the start point.}
  \item{lat1}{\code{numeric} -- Latitude coordinate(degrees) of the start point.}
  \item{lon2}{\code{numeric} -- Longitude coordinate(degrees) of the end point.}
  \item{lat2}{\code{numeric} -- Latitude coordinate(degrees) of the end point.}
  \item{R}{\code{numeric} -- Mean radius (km) of the Earth.}
}
\details{
  The great-circle distance is calculated between two points along a
  spherical surface using the shortest distance and disregarding
  topography.
\cr\cr
  \bold{Method 1: Haversine Formula}
  \deqn{a = \sin^2((\phi_2 - \phi_1)/2) + \cos(\phi_1) \cos(\phi_2) \sin^2((\lambda_2 - \lambda_1)/2)}{%
        a = sin^2((phi2-phi1)/2) + cos(phi1) cos(phi2) sin^2((lambda2-lambda1)/2) }
  \deqn{c = 2~\mathrm{atan2}(\sqrt{a}, \sqrt{1-a})}{%
        c = 2 * atan2(sqrt(a), sqrt(1-a)) }
  \deqn{d = R c}{d = R * c}

  where\cr
  \eqn{\phi}{phi} = latitude (in radians),\cr
  \eqn{\lambda}{lambda} = longitude (in radians),\cr
  \eqn{R} = radius (km) of the Earth,\cr
  \eqn{a} = square of half the chord length between the points,\cr
  \eqn{c} = angular distance in radians,\cr
  \eqn{d} = great-circle distance (km) between two points.
\cr\cr
  \bold{Method 2: Spherical Law of Cosines}
  \deqn{d = \mathrm{acos}(\sin(\phi_1)\sin(\phi_2) + \cos(\phi_1)\cos(\phi_2)\cos(\lambda_2 - \lambda_1)) R}{%
        d = acos(sin(phi1) sin(phi2) + cos(phi1) cos(phi2) cos(lambda2-lambda1)) * R}

  The initial bearing (aka forward azimuth) for the start point can be calculated using:

  \deqn{\theta = \mathrm{atan2}(\sin(\lambda_2-\lambda_1)\cos(\phi_2), \cos(\phi_1)\sin(\phi_2) - \sin(\phi_1)\cos(\phi_2)\cos(\lambda_2-\lambda_1))}{%
        theta = atan2( sin(lambda2-lambda1) cos(phi2), cos(phi1) sin(phi2) - sin(phi1) cos(phi2) cos(lambda2-lambda1) )}
}
\value{
  A list obect containing:\cr
  \code{a} -- Haversine \eqn{a} = square of half the chord length between the points,\cr
  \code{c} -- Haversine \eqn{c} = angular distance in radians,\cr
  \code{d} -- Haversine \eqn{d} = great-circle distance (km) between two points,\cr
  \code{d2} -- Law of Cosines \eqn{d} = great-circle distance (km) between two points,\cr
  \code{theta} -- Initial bearing \eqn{\theta}{theta} (degrees) for the start point.
}
\references{
  Movable Type Scripts --
  \href{https://www.movable-type.co.uk/scripts/latlong.html}{Calculate distance, bearing and more between Latitude/Longitude points}
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-03-11}
}
\note{
  If one uses the north geomagnetic pole as an end point, 
  \eqn{\theta}{theta} crudely approximates the magnetic declination.
}
\seealso{
In package \pkg{PBSmapping}:\cr
  \code{\link[PBSmapping]{addCompass}},
  \code{\link[PBSmapping]{calcArea}},
  \code{\link[PBSmapping]{calcCentroid}},
  \code{\link[PBSmapping]{calcLength}}
}
\examples{
local(envir=.PBSmapEnv,expr={
  #-- Distance between southern BC waters and north geomagnetic pole
  print(calcGCdist(-126.5,48.6,-72.7,80.4))
})
}
\keyword{manip}

