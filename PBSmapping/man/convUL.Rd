\name{convUL}
\alias{convUL}
\title{Convert Coordinates between UTM and Lon/Lat}
\description{
  Convert coordinates between UTM and Lon/Lat.
}
\usage{
convUL (xydata, km=TRUE)
}
\arguments{
  \item{xydata}{data frame with columns \code{X} and \code{Y}.}
  \item{km}{use UTM coordinates in kilometres if true, or metres if false.}
}
\details{
  \code{xydata} must possess a \code{projection} attribute that
  identifies the current projection.  If the data frame contains UTM
  coordinates, it must also have a \code{zone} attribute equal to a
  number between 1 and 60 (inclusive).  If it contains
  longitude/latitude coordinates and the \code{zone} attribute is
  missing, the function computes the mean longitude and uses that value
  to determine the zone.  The longitude range of zone \emph{i} is
  \eqn{-186 + 6\emph{i}^\circ < x \le -180 + 6\emph{i}^\circ}{-186 + 
  6i < x <= -180 + 6i degrees}.
  
  This function converts the \code{X} and \code{Y} columns of
  \code{xydata} from \code{"LL"} to \code{"UTM"} or vice-versa.  After
  the conversion, it adjusts the data frame's attributes accordingly.
}
\value{
  A data frame identical to \code{xydata}, except that the \code{X} and
  \code{Y} columns contain the results of the conversion and the
  \code{projection} attribute matches the new projection.
}
\seealso{
  \code{\link{closePolys}},
  \code{\link{fixBound}}.
}
\examples{
#--- load the data
data(nepacLL, package="PBSmapping")
#--- set the zone attribute
attr(nepacLL, "zone") <- 9
#--- convert and plot the result
nepacUTM <- convUL(nepacLL)
plotMap(nepacUTM)
}
\keyword{manip}