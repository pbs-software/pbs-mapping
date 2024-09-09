\name{plotMap}
\alias{plotMap}
\alias{.plotMaps}
\alias{.initPlotRegion}
\title{Plot a PolySet as a Map}
\description{
  Plot a \link[PBSmapping]{PolySet} as a map, using the correct aspect ratio.
}
\usage{
plotMap(polys, xlim = NULL, ylim = NULL, projection = TRUE,
        plt = c(0.11, 0.98, 0.12, 0.88), polyProps = NULL,
        border = NULL, lty = NULL, col = NULL, colHoles = NULL,
        density = NA, angle = NULL, bg = 0, axes = TRUE,
        tckLab = TRUE, tck = 0.014, tckMinor = 0.5 * tck, ...)

.plotMaps(polys, xlim, ylim, projection, plt, polyProps,
          border, lty, col, colHoles, density, angle, bg,
          axes, tckLab, tck, tckMinor, isType, ...)

.initPlotRegion(projection, xlim, ylim, plt)
}
\arguments{
  \item{polys}{\code{data.frame} -- \link[PBSmapping]{PolySet} to plot (\emph{required}).}
  \item{xlim}{\code{numeric} -- range of X-coordinates.}
  \item{ylim}{\code{numeric} -- range of Y-coordinates.}
  \item{projection}{\code{character|logical} -- desired projection when \link[PBSmapping]{PolySet}
    lacks a \code{projection} attribute; one of \code{"LL"}, \code{"UTM"},
    or a numeric value.  If Boolean, specifies whether to check
    \code{polys} for a \code{projection} attribute.}
  \item{plt}{\code{numeric} -- four element vector \code{(x1, x2, y1, y2)} giving
    the coordinates of the plot region measured as a fraction of the
    figure region. Set to \code{NULL} if \code{mai} in \code{par} is
    desired.}
  \item{polyProps}{\code{data.frame} -- \link[PBSmapping]{PolyData} specifying which polygons to plot and their
    properties.  \code{\link[graphics]{par}} parameters passed as direct arguments
    supersede these data.}
  \item{border}{\code{numeric|character} -- vector describing edge colours (cycled by \code{PID}).}
  \item{lty}{\code{numeric|character} -- vector describing line types (cycled by \code{PID}).}
  \item{col}{\code{numeric|character} -- vector describing fill colours (cycled by \code{PID}).}
  \item{colHoles}{\code{numeric|character} -- vector describing hole colours (cycled by \code{PID}).
    The default, \code{NULL}, should be used in most cases as it renders
    holes transparent. \code{colHoles} is designed solely to eliminate
    retrace lines when images are converted to PDF format. If
    \code{colHoles} is specified, underlying information (i.e., previously
    plotted shapes) will be obliterated.  If \code{NA} is specified, only
    outer polygons are drawn, consequently filling holes.}
  \item{density}{\code{numeric} -- vector describing shading line densities (lines per
    inch, cycled by \code{PID}).}
  \item{angle}{\code{numeric} -- vector describing shading line angles (degrees, cycled by
    \code{PID}).}
  \item{bg}{\code{numeric|character} -- background colour of the plot}
  \item{axes}{\code{logical} -- if \code{TRUE}, plot axes}
  \item{tckLab}{\code{logical} -- vector (length 1 or 2); if \code{TRUE},
    label the major tick marks.  If given a two-element
    vector, the first element describes the tick marks on the
    x-axis and the second element describes those on the y-axis.}
  \item{tck}{\code{numeric} -- vector (length 1 or 2) describing the length
    of tick marks as a fraction of the smallest dimension. If
    \code{tckLab = TRUE}, these tick marks will be automatically
    labelled.  If given a two-element vector, the first element
    describes the tick marks on the x-axis and the second element
    describes those on the y-axis.}
  \item{tckMinor}{\code{numeric} -- vector (length 1 or 2) describing the length
    of tick marks as a fraction of the smallest dimension.  These tick
    marks can not be automatically labelled.  If given a two-element vector,
    the first element describes the tick marks on the x-axis and
    the second element describes those on the y-axis.}
  \item{\dots}{\code{dots} -- additional \code{\link[graphics]{par}} parameters, or the arguments
    \code{main}, \code{sub}, \code{xlab}, or \code{ylab} for the
    \code{\link[graphics]{title}} function.}
  \item{isType}{\code{character} -- one of \code{'points'}, \code{'polygons'}, or \code{'lines'}}
}
\details{
  This function plots a \link[PBSmapping]{PolySet}, where each unique (\code{PID},
  \code{SID}) describes a polygon.  It connects each polygon's last
  vertex to its first.  The function supports both borders
  (\code{border}, \code{lty}) and fills (\code{col}, \code{density},
  \code{angle}).  When supplied with the appropriate arguments, it can
  draw only borders or only fills .  Unlike \code{\link[PBSmapping]{plotLines}} and
  \code{\link[PBSmapping]{plotPolys}}, it uses the aspect ratio supplied in the
  \code{projection} attribute of \code{polys}.  If this attribute is
  missing, it attempts to use its \code{projection} argument.  In the
  absence of both, it uses a default aspect ratio of 1:1.  It clips
  \code{polys} to \code{xlim} and \code{ylim} before plotting.

  The function creates a blank plot when \code{polys} equals
  \code{NULL}.  In this case, the user must supply both \code{xlim} and
  \code{ylim} arguments.  Alternatively, it accepts the argument
  \code{type = "n"} as part of \dots, which is equivalent to specifying
  \code{polys = NULL}, but requires a \link[PBSmapping]{PolySet}.  In both cases,
  the function's behaviour changes slightly.  To resemble the
  \code{\link[graphics]{plot}} function, it plots the border, labels, and other
  parts according to \code{\link[graphics]{par}} parameters such as \code{col}.

  For additional help on the arguments \code{border}, \code{lty},
  \code{col}, \code{density}, and \code{angle}, please see
  \code{\link[graphics]{polygon}} and \code{\link[graphics]{par}}.
}
\value{
  \link[PBSmapping]{PolyData} consisting of the \code{PolyProp}s used to create the plot.
}
\note{
  To satisfy the aspect ratio, this plotting routine resizes the plot
  region.  Consequently, \code{\link[graphics]{par}} parameters such as
  \code{plt}, \code{mai}, and \code{mar} will change.  When the function
  terminates, these changes persist to allow for additions to the plot.
  
  Auxiliary dot function \code{'.initPlotRegion'} initialises the plot region,
  accounting for the aspect ratio.
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
  \code{\link[PBSmapping]{addPolys}},
  \code{\link[PBSmapping]{addStipples}},
  \code{\link[PBSmapping]{clipPolys}},
  \code{\link[PBSmapping]{closePolys}},
  \code{\link[PBSmapping]{fixBound}},
  \code{\link[PBSmapping]{fixPOS}},
  \code{\link[PBSmapping]{locatePolys}},
  \code{\link[PBSmapping]{plotLines}},
  \code{\link[PBSmapping]{plotPoints}},
  \code{\link[PBSmapping]{thinPolys}},
  \code{\link[PBSmapping]{thickenPolys}}.
}
\examples{
local(envir=.PBSmapEnv,expr={
  oldpar = par(no.readonly=TRUE)
  #--- create a PolySet to plot
  polys <- data.frame(PID=rep(1,4),POS=1:4,X=c(0,1,1,0),Y=c(0,0,1,1))
  #--- plot the PolySet
  plotMap(polys,xlim=c(-.5,1.5),ylim=c(-.5,1.5),density=0,projection=1)
  par(oldpar)
})
}
\keyword{hplot}
\concept{PBSmapping.r}
\concept{dotFuns.r}
