\name{addPolys}
\alias{addPolys}
\title{Add a PolySet to an Existing Plot as Polygons}
\description{
  Add a \link{PolySet} to an existing plot, where each unique (\code{PID},
  \code{SID}) describes a polygon.
}
\usage{
addPolys (polys, xlim = NULL, ylim = NULL, polyProps = NULL,
          border = NULL, lty = NULL, col = NULL, colHoles = NULL,
          density = NA, angle = NULL, \dots)
}
\arguments{
  \item{polys}{\link{PolySet} to add (\emph{required}).}
  \item{xlim}{range of X-coordinates.}
  \item{ylim}{range of Y-coordinates.}
  \item{polyProps}{\link{PolyData} specifying which polygons to plot and their
    properties.  \code{\link{par}} parameters passed as direct arguments
    supersede these data.}
  \item{border}{vector describing edge colours (cycled by \code{PID}).}
  \item{lty}{vector describing line types (cycled by \code{PID}).}
  \item{col}{vector describing fill colours (cycled by \code{PID}).}
	\item{colHoles}{vector describing hole colours (cycled by \code{PID}).
		The default, \code{NULL}, should be used in most cases as it renders holes 
		transparent. \code{colHoles} is designed solely to eliminate retrace lines 
		when images are converted to PDF format. If \code{colHoles} is specified, 
		underlying information (i.e., previosuly plotted shapes) will be obliterated.
		If \code{NA} is specified, only outer polygons are drawn, consequently filling holes.}
  \item{density}{vector describing shading line densities (lines per
    inch, cycled by \code{PID}).}
  \item{angle}{vector describing shading line angles (degrees, cycled by
    \code{PID}).}
  \item{\dots}{additional \code{\link{par}} parameters for the
    \code{\link{polygon}} function.}
}
\details{
  The plotting routine connects the last vertex of each discrete polygon
  to the first vertex of that polygon.  It supports both
  borders (\code{border}, \code{lty}) and fills (\code{col},
  \code{density}, \code{angle}). It clips \code{polys} to \code{xlim}
  and \code{ylim} before plotting.

  For additional help on the arguments \code{border}, \code{lty},
  \code{col}, \code{density}, and \code{angle}, please see
  \code{\link{polygon}} and \code{\link{par}}.
}
\value{
  \link{PolyData} consisting of the \code{PolyProp}s used to create the plot.
}
\seealso{
  \code{\link{addLabels}},
  \code{\link{addStipples}},
  \code{\link{clipPolys}},
  \code{\link{closePolys}},
  \code{\link{fixBound}},
  \code{\link{fixPOS}},
  \code{\link{locatePolys}},
  \code{\link{plotLines}},
  \code{\link{plotMap}},
  \code{\link{plotPoints}},
  \code{\link{plotPolys}},
  \code{\link{thinPolys}},
  \code{\link{thickenPolys}}.
}
\examples{
#--- create a PolySet to plot
polys <- data.frame(PID=rep(1,4),POS=1:4,X=c(0,1,1,0),Y=c(0,0,1,1))
polys <- as.PolySet(polys, projection=1)
#--- plot the PolySet
plotPolys(polys,xlim=c(-.5,1.5),ylim=c(-.5,1.5),density=0,projection=1)
#--- add the PolySet to the plot (in a different style)
addPolys(polys, col=3)
}
\keyword{aplot}
