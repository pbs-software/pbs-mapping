\name{makeTopography}
\alias{makeTopography}
\title{Make Topography Data From Freely Available Online Data}
\description{
  Make topography data suitable for the \code{\link{contour}} and
  \code{\link{contourLines}} functions using freely available global
  seafloor topography data.
}
\usage{
makeTopography (data, func=NULL)
}
\arguments{
  \item{data}{data frame with three optionally-named columns: \code{X},
    \code{Y}, and \code{Z}.  The columns must appear in that order.}
  \item{func}{function to summarize Z if (X,Y) points are duplicated. 
    Defaults to \code{mean()} if no function is specified.}
}
\details{
  Data obtained through the acquisition form at
  \url{http://topex.ucsd.edu/cgi-bin/get_data.cgi} is suitable for this
  function.  \code{\link{read.table}} will import its ASCII files into
  R/S, creating the \code{data} argument for this function.

  When creating data for regions with longitude values spanning
  -180\eqn{^\circ}{ degrees} to 0\eqn{^\circ}{ degrees}, consider
  subtracting 360 from the result's X coordinates (\code{x}).
  
  When creating bathymetry data, consider negating the result's
  elevations (\code{z}) to give depths positive values.
  
  Combinations of (X,Y) do not need to be complete (\code{z[x,y]=NA}) or 
  unique (\code{z[x,y]=func(Z[x,y])}).
}
\value{
  List with elements \code{x}, \code{y}, and \code{z}.  \code{x} and
  \code{y} are vectors, while \code{z} is a matrix.
  \code{\link{contour}} and \code{\link{contourLines}} expect data
  conforming to this list format.
}
\seealso{
  \code{\link{contour}},
  \code{\link{contourLines}},
  \code{\link{convCP}},
  \code{\link{read.table}}.
}
\examples{
#--- Example 1: Sample data frame and conversion.
file <- data.frame(X=c(1,1,2,2),Y=c(3,4,3,4),Z=c(5,6,7,8))
print(makeTopography(file));

#--- Example 2: Aleutian Islands bathymetry
require(PBSmapping);
isob <- c(100,500,1000,2500,5000);
icol <- rgb(0,0,seq(255,100,len=length(isob)),max=255);
afile <- paste(system.file(package="PBSmapping"),
"/Extra/aleutian.txt",sep="")
aleutian <- read.table(afile, header=FALSE,
   col.names=c("x","y","z"))
aleutian$x <- aleutian$x - 360
aleutian$z <- -aleutian$z
alBathy <- makeTopography(aleutian)
alCL <- contourLines(alBathy,levels=isob)
alCP <- convCP(alCL)
alPoly <- alCP$PolySet
attr(alPoly,"projection") <- "LL"
plotMap(alPoly,type="n");
addLines(alPoly,col=icol);
data(nepacLL); addPolys(nepacLL,col="gold");
legend(x="topleft",bty="n",col=icol,lwd=2,
   legend=as.character(isob));
}
\keyword{manip}
