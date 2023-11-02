\name{convCP}
\alias{convCP}
\title{Convert Contour Lines into a PolySet}
\description{
  Convert output from \code{\link{contourLines}} into a \link{PolySet}.
}
\usage{
convCP (data, projection = NULL, zone = NULL)
}
\arguments{
  \item{data}{contour line data, often from the
    \code{\link{contourLines}} function.}
  \item{projection}{optional \code{projection} attribute to add to
    the PolySet.}
  \item{zone}{optional \code{zone} attribute to add to the PolySet.}
}
\details{
  \code{data} contains a list as described below. The
  \code{\link{contourLines}} function create a list suitable for the
  \code{data} argument.

  A three-element list describes each contour.  The named elements in
  this list include the scalar \code{level}, the vector \code{x}, and
  the vector \code{y}.  Vectors \code{x} and \code{y} must have equal
  lengths.  A higher-level list (\code{data}) contains one or more of
  these contours lists.
}
\value{
  A list with two named elements \link{PolySet} and \link{PolyData}.
  The \link{PolySet} element contains a \link{PolySet} representation of the
  contour lines.  The \link{PolyData} element links each contour line
  (\code{PID}, \code{SID}) with a \code{level}.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2013-04-10}
}
\seealso{
  \code{\link{contour}},
  \code{\link{contourLines}},
  \code{\link{convLP}},
  \code{\link{makeTopography}}.
}
\examples{
local(envir=.PBSmapEnv,expr={
  oldpar = par(no.readonly=TRUE)
  #--- create sample data for the contourLines() function
  x <- seq(-0.5, 0.8, length=50);  y <- x
  z <- outer(x, y, FUN = function(x,y) { sin(2*pi*(x^2+y^2))^2; } )
  data <- contourLines(x, y, z, levels=c(0.2, 0.8))
  #--- pass that sample data into convCP()
  result <- convCP(data)
  #--- plot the result
  plotLines(result$PolySet, projection=1)
  print(result$PolyData)
  par(oldpar)
})
}
\keyword{manip}

