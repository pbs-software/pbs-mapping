\name{addPolys}
\alias{addPolys}
\alias{.insertNAs}
\alias{.preparePolyProps}
\alias{.rollupPolys}
\title{Add Polygon PolySet to Existing Plot}
\description{
  Add a \code{\link{PolySet}} to an existing plot, where each unique (\code{PID},
  \code{SID}) describes a polygon.
}
\usage{
addPolys(polys, xlim = NULL, ylim = NULL, polyProps = NULL,
         border = NULL, lty = NULL, col = NULL, colHoles = NULL,
         density = NA, angle = NULL, \dots)

.insertNAs(polys, idx)

.preparePolyProps(polysPID, polysSID, polyProps)

.rollupPolys(polys, rollupMode, exteriorCCW, closedPolys, addRetrace)
}
\arguments{
  \item{polys}{\code{data.frame} -- \code{\link{PolySet}} to add (\emph{required})}
  \item{xlim}{\code{numeric} -- range of X-coordinates.}
  \item{ylim}{\code{numeric} -- range of Y-coordinates.}
  \item{polyProps}{\code{data.frame} -- \code{\link{PolyData}} specifying which polygons to plot and their
    properties.  \code{\link[graphics]{par}} parameters passed as direct arguments
    supersede these data.}
  \item{border}{\code{character|numeric} -- vector describing edge colours (cycled by \code{PID}).}
  \item{lty}{\code{character|numeric} -- vector describing line types (cycled by \code{PID}).}
  \item{col}{\code{character|numeric} -- vector describing fill colours (cycled by \code{PID}).}
	\item{colHoles}{\code{character|numeric} -- vector describing hole colours (cycled by \code{PID}).
		The default, \code{NULL}, should be used in most cases as it renders holes 
		transparent. \code{colHoles} is designed solely to eliminate retrace lines 
		when images are converted to PDF format. If \code{colHoles} is specified, 
		underlying information (i.e., previously plotted shapes) will be obliterated.
		If \code{NA} is specified, only outer polygons are drawn, consequently filling holes.}
  \item{density}{\code{numeric} -- vector describing shading line densities (lines per
    inch, cycled by \code{PID}).}
  \item{angle}{\code{numeric} -- vector describing shading line angles (degrees, cycled by
    \code{PID}).}
  \item{\dots}{\code{dots} -- additional \code{\link[graphics]{par}} parameters for the
    \code{\link[graphics]{polygon}} function.}
  \item{idx}{\code{character|numeric} -- index of \code{PID}s used to extract select 
    polygons/polylines from a \code{PolySet}}
  \item{polysPID}{\code{numeric} -- vector of primary polygon identification numbers (\code{PID}s)}
  \item{polysSID}{\code{numeric} -- vector of secondary polygon identification numbers (\code{SID}s)}
  \item{rollupMode}{\code{numeric} -- method for rolling up a \code{PolySet};
    essentially controls when to introduce a new \code{PID} (or \code{PID,SID}). Options:\cr
    \code{1} = roll-up to the \code{PID} level (only \code{PID}s in the result)\cr
    \code{2} = roll-up to the outer contour level (only outer contours in the result)\cr
    \code{3} = do not roll-up}
  \item{exteriorCCW}{\code{numeric} -- modify orientation of vertices
    (clockwise or counter-clockwise):\cr
    \code{-1} = do not modify\cr
    \code{ 0}  = exterior should be clockwise\cr
    \code{+1} = exterior should be counter-clockwise}
  \item{closedPolys}{\code{numeric} -- whether the last and first vertices should be the same:\cr
    \code{-1} = do not modify\cr
    \code{ 0}  = ensure polygons do not close\cr
    \code{+1} = close the polygons}
  \item{addRetrace}{\code{numeric} -- determines whether to add retrace lines to 
    the first vertex of the parent after outputting a child:\cr
    \code{0} = do not add\cr
    \code{1} = add}
}
\details{
  The plotting routine connects the last vertex of each discrete polygon
  to the first vertex of that polygon.  It supports both
  borders (\code{border}, \code{lty}) and fills (\code{col},
  \code{density}, \code{angle}). It clips \code{polys} to \code{xlim}
  and \code{ylim} before plotting.

  For additional help on the arguments \code{border}, \code{lty},
  \code{col}, \code{density}, and \code{angle}, please see
  \code{\link[graphics]{polygon}} and \code{\link[graphics]{par}}.
}
\note{
  Auxiliary dot function \code{'.insertNAs'} facilitates (hastens)
  the plotting of polygons and polylines. It also reduces the incidence
  of retrace lines.
  
  Auxiliary dot function \code{'.preparePolyProps'} performs
  at least one of the following tasks:\cr
    1) creates \code{'polyProps'} if it equals \code{NULL};\cr
    2) adds \code{'SID'} column to \code{'polyProps'} if one exists in \code{'polys'};\cr
    3) removes from \code{'polyProps'} any \code{PID}s that do not exist in \code{'polys'}.\cr
  Returns a \code{polyProps} object.

  Auxiliary dot function \code{'.rollupPolys'} does not validate a \code{PolySet};
  returns a rolled-up \code{PolySet} or \code{NULL}.
}
\value{
  \code{\link{PolyData}} consisting of the \code{PolyProp}s used to create the plot.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers},
  Software Engineer, Jobber, Edmonton AB

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-09-03}
}
\seealso{
In package \pkg{PBSmapping}:\cr
  \code{\link[PBSmapping]{addLabels}},
  \code{\link[PBSmapping]{addLines}} (also uses \code{'.preparePolyProps'} and \code{'.insertNAs'}),
  \code{\link[PBSmapping]{addPoints}},
  \code{\link[PBSmapping]{addStipples}}\cr
  \code{'.rollupPolys'} is also called by:\cr
  \code{\link[PBSmapping]{calcArea}},
  \code{\link[PBSmapping]{calcCentroid}},
  \code{\link[PBSmapping]{calcLength}},
  \code{\link[PBSmapping]{calcSummary}},
  \code{\link[PBSmapping]{fixPOS}}
}
\examples{
local(envir=.PBSmapEnv,expr={
  oldpar = par(no.readonly=TRUE)
  #--- create a PolySet to plot
  polys <- data.frame(PID=rep(1,4),POS=1:4,X=c(0,1,1,0),Y=c(0,0,1,1))
  polys <- as.PolySet(polys, projection=1)
  #--- plot the PolySet
  plotPolys(polys,xlim=c(-.5,1.5),ylim=c(-.5,1.5),density=0,projection=1)
  #--- add the PolySet to the plot (in a different style)
  addPolys(polys,col="green",border="blue",lwd=3)
  par(oldpar)
})
}
\keyword{aplot}
\concept{PBSmapping.r}
\concept{dotFuns.r}
