\name{makeTopography}
\alias{makeTopography}
\title{
  Make Topography Data from Online Source
}
\description{
  Make topography data suitable for the \code{'graphics::contour'} and
  \code{'grDevices::contourLines'} functions using freely available global
  seafloor topography data.
}
\usage{
makeTopography (dat, digits=2, func=NULL)
}
\arguments{
  \item{dat}{\code{data.frame} -- data with three optionally-named columns: \code{x},
    \code{y}, and \code{z}.  The columns must appear in that order.}
  \item{digits}{\code{numeric} -- integer indicating the precision to be used by the function
    \code{round} on \code{(x,y)} values.}
  \item{func}{\code{function} -- to summarize \code{z} if \code{(x,y)} points are duplicated. 
    Defaults to \code{mean()} if no function is specified.}
}
\details{
  Suitable data can be obtained through the \href{https://topex.ucsd.edu/cgi-bin/get_data.cgi}{Topex acquisition form}.
  The function \code{'utils::read.table'} will import dowloaded ASCII files into R,
  creating objects suitable for the argument \code{'dat'} in \code{'makeTopography'}.

  When creating data for regions with longitude values spanning
  -180\eqn{^\circ}{ degrees} to 0\eqn{^\circ}{ degrees}, consider
  subtracting 360 from the result's longitude coordinates (\code{x}).
  
  When creating bathymetry data, consider negating the result's
  elevations (\code{z}) to give depths positive values.
  
  Combinations of \code{(x,y)} do not need to be complete (\code{z[x,y]=NA}) or 
  unique (\code{z[x,y] = func(z[x,y])}).
}
\value{
  List with elements \code{x}, \code{y}, and \code{z}. Elements \code{x} and
  \code{y} are vectors, while \code{z} is a matrix with rownames \code{x}
  and colnames \code{y}.
  The functions \code{'graphics::contour'} and \code{'grDevices::contourLines'}
  expect data conforming to this list format.
}
\seealso{
In package \pkg{graphics}:\cr
  \code{\link[graphics]{contour}}\cr
In package \pkg{grDevices}:\cr
  \code{\link[grDevices]{contourLines}}\cr
In package \pkg{PBSmapping}:\cr
  \code{\link[PBSmapping]{convCP}}
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2021-01-11}
}
\examples{
local(envir=.PBSmapEnv,expr={
  oldpar = par(no.readonly=TRUE)
  #--- Example 1: Sample data frame and conversion.
  file <- data.frame(X=c(1,1,2,2),Y=c(3,4,3,4),Z=c(5,6,7,8))
  print(makeTopography(file))

  #--- Example 2: Aleutian Islands bathymetry
  isob <- c(100,500,1000,2500,5000)
  icol <- rgb(0,0,seq(255,100,len=length(isob)),max=255)
  afile <- paste(system.file(package="PBSmapping"),
    "/Extra/aleutian.txt",sep="")
  aleutian <- read.table(afile, header=FALSE, col.names=c("x","y","z"))
  aleutian$x <- aleutian$x - 360
  aleutian$z <- -aleutian$z
  alBathy <- makeTopography(aleutian)
  alCL <- contourLines(alBathy,levels=isob)
  alCP <- convCP(alCL)
  alPoly <- alCP$PolySet
  attr(alPoly,"projection") <- "LL"
  plotMap(alPoly, type="n", cex.axis=1.2, cex.lab=1.5)
  addLines(alPoly,col=icol)
  data(nepacLL,envir=.PBSmapEnv)
  addPolys(nepacLL,col="gold")
  legend(x="topleft",bty="n",col=icol,lwd=2,legend=as.character(isob))
  par(oldpar)
})
}
\keyword{manip}

