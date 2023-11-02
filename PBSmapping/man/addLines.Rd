\name{addLines}
\alias{addLines}
\title{Add a PolySet to an Existing Plot as Polylines}
\description{
  Add a \link{PolySet} to an existing plot, where each unique (\code{PID},
  \code{SID}) describes a polyline.
}
\usage{
addLines (polys, xlim = NULL, ylim = NULL,
    polyProps = NULL, lty = NULL, col = NULL, arrows = FALSE, \dots)
}
\arguments{
  \item{polys}{\link{PolySet} to add (\emph{required}).}
  \item{xlim}{range of X-coordinates.}
  \item{ylim}{range of Y-coordinates.}
  \item{polyProps}{\link{PolyData} specifying which polylines to plot and their
    properties.  \code{\link{par}} parameters passed as direct arguments
    supersede these data.}
  \item{lty}{vector of line types (cycled by \code{PID}).}
  \item{col}{vector of colours (cycled by \code{PID}).}
  \item{arrows}{Boolean value; if \code{TRUE}, add arrows using the
    \link{arrows} function and consider the arguments \code{angle},
    \code{length}, and \code{code}.}
  \item{\dots}{additional \code{\link{par}} parameters for the
    \code{\link{lines}} function.}
}
\details{
  The plotting routine does not connect the last vertex of each discrete
  polyline to the first vertex of that polyline. It clips \code{polys}
  to \code{xlim} and \code{ylim} before plotting.

  For additional help on the arguments \code{lty} and \code{col}, please
  see \code{\link{par}}.
}
\value{
  \link{PolyData} consisting of the \code{PolyProp}s used to create the plot.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2013-04-10}
}
\seealso{
  \code{\link{calcLength}},
  \code{\link{clipLines}},
  \code{\link{closePolys}},
  \code{\link{convLP}},
  \code{\link{fixBound}},
  \code{\link{fixPOS}},
  \code{\link{locatePolys}},
  \code{\link{plotLines}},
  \code{\link{thinPolys}},
  \code{\link{thickenPolys}}.
}
\examples{
local(envir=.PBSmapEnv,expr={
  oldpar = par(no.readonly=TRUE)
  #--- create a PolySet to plot
  polys <- data.frame(PID=rep(1,4),POS=1:4,X=c(0,1,1,0),Y=c(0,0,1,1))
  polys <- as.PolySet(polys, projection=1)
  #--- plot the PolySet
  plotLines(polys, xlim=c(-.5,1.5), ylim=c(-.5,1.5), projection=1)
  #--- add the PolySet to the plot (in a different style)
  addLines(polys, lwd=5, col=3)
  par(oldpar)
})
}
\keyword{aplot}
