\name{convUL}
\alias{convUL}
\title{Convert Coordinates between UTM and Lon/Lat}
\description{
  Convert coordinates between UTM and Lon/Lat.
}
\usage{
convUL (xydata, km=TRUE, southern=NULL)
}
\arguments{
  \item{xydata}{data frame with columns \code{X} and \code{Y}.}
  \item{km}{Boolean value; if \code{TRUE}, UTM coordinates within
    \code{xydata} are in kilometres; otherwise, metres.}
  \item{southern}{Boolean value; if \code{TRUE}, forces conversions from
    UTM to longitude/latitude to produce coordinates within the
    southern hemisphere.  For conversions from UTM, this argument defaults to
    \code{FALSE}. For conversions from LL, the function determines 
    \code{southern} from \code{xydata}.}
}
\details{
  The object \code{xydata} must possess a \code{projection} attribute that
  identifies the current projection.  If the data frame contains UTM
  coordinates, it must also have a \code{zone} attribute equal to a
  number between 1 and 60 (inclusive).  If it contains
  geographic (longitude/latitude) coordinates and the \code{zone} attribute is
  missing, the function computes the mean longitude and uses that value
  to determine the zone.  The longitude range of zone \emph{i} is
  \eqn{-186 + 6\emph{i}^\circ < x \le -180 + 6\emph{i}^\circ}{-186 + 
  6i < x <= -180 + 6i degrees}.
  
  This function converts the \code{X} and \code{Y} columns of
  \code{xydata} from \code{"LL"} to \code{"UTM"} or vice-versa.  If the
  data span more than \bold{one} zone to the right or left of the intended
  central zone, the underlying algorithm may produce erroneous
  results. This limitation means that the user should use 
  the most central zone of the mapped region, or allow the function to determine
  the central zone when converting from geographic to UTM coordinates.
  After the conversion, this routine adjusts the data frame's attributes accordingly.
}
\value{
  A data frame identical to \code{xydata}, except that the \code{X} and
  \code{Y} columns contain the results of the conversion, and the
  \code{projection} attribute matches the new projection.
}
\references{
  Ordnance Survey. (2010) A guide to coordinate systems
  in Great Britain. \emph{Report D00659} \bold{(v2.1)}. Southampton, UK. \cr
  \url{http://www.ordnancesurvey.co.uk/oswebsite/gps/docs/A_Guide_to_Coordinate_Systems_in_Great_Britain.pdf}.
}
\author{
  Nicholas Boers, Dept. of Computer Science, Grant MacEwan University, Edmonton AB
}
\seealso{
  \code{\link[PBSmapping]{closePolys}},
  \code{\link[PBSmapping]{fixBound}}.
}
\examples{
local(envir=.PBSmapEnv,expr={
  oldpar = par(no.readonly=TRUE)
  #--- load the data
  data(nepacLL,envir=.PBSmapEnv)
  #--- set the zone attribute
  #--- use a zone that is most central to the mapped region
  attr(nepacLL, "zone") <- 6
  #--- convert and plot the result
  nepacUTM <- convUL(nepacLL)
  plotMap(nepacUTM)
  par(oldpar)
})
}
\keyword{manip}

