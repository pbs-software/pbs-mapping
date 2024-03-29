\name{plotMap}
\alias{plotMap}
\title{Plot a PolySet as a Map}
\description{
  Plot a \link[PBSmapping]{PolySet} as a map, using the correct aspect ratio.
}
\usage{
plotMap (polys, xlim = NULL, ylim = NULL, projection = TRUE,
         plt = c(0.11, 0.98, 0.12, 0.88), polyProps = NULL,
         border = NULL, lty = NULL, col = NULL, colHoles = NULL,
         density = NA, angle = NULL, bg = 0, axes = TRUE,
         tckLab = TRUE, tck = 0.014, tckMinor = 0.5 * tck, \dots)
}
\arguments{
  \item{polys}{\link[PBSmapping]{PolySet} to plot (\emph{required}).}
  \item{xlim}{range of X-coordinates.}
  \item{ylim}{range of Y-coordinates.}
  \item{projection}{desired projection when \link[PBSmapping]{PolySet} lacks a
    \code{projection} attribute; one of \code{"LL"}, \code{"UTM"},
    or a numeric value.  If Boolean, specifies whether to check
    \code{polys} for a \code{projection} attribute.}
  \item{plt}{four element numeric vector \code{(x1, x2, y1, y2)} giving
    the coordinates of the plot region measured as a fraction of the
    figure region. Set to \code{NULL} if \code{mai} in \code{par} is
    desired.}
  \item{polyProps}{\link[PBSmapping]{PolyData} specifying which polygons to plot and their
    properties.  \code{\link[graphics]{par}} parameters passed as direct arguments
    supersede these data.}
  \item{border}{vector describing edge colours (cycled by \code{PID}).}
  \item{lty}{vector describing line types (cycled by \code{PID}).}
  \item{col}{vector describing fill colours (cycled by \code{PID}).}
  \item{colHoles}{vector describing hole colours (cycled by \code{PID}).
    The default, \code{NULL}, should be used in most cases as it renders
    holes transparent. \code{colHoles} is designed solely to eliminate
    retrace lines when images are converted to PDF format. If
    \code{colHoles} is specified, underlying information (i.e., previously
    plotted shapes) will be obliterated.  If \code{NA} is specified, only
    outer polygons are drawn, consequently filling holes.}
  \item{density}{vector describing shading line densities (lines per
    inch, cycled by \code{PID}).}
  \item{angle}{vector describing shading line angles (degrees, cycled by
    \code{PID}).}
  \item{bg}{background colour of the plot.}
  \item{axes}{Boolean value; if \code{TRUE}, plot axes.}
  \item{tckLab}{Boolean vector (length 1 or 2); if \code{TRUE},
    label the major tick marks.  If given a two-element
    vector, the first element describes the tick marks on the
    x-axis and the second element describes those on the y-axis.}
  \item{tck}{numeric vector (length 1 or 2) describing the length
    of tick marks as a fraction of the smallest dimension. If
    \code{tckLab = TRUE}, these tick marks will be automatically
    labelled.  If given a two-element vector, the first element
    describes the tick marks on the x-axis and the second element
    describes those on the y-axis.}
  \item{tckMinor}{numeric vector (length 1 or 2) describing the length
    of tick marks as a fraction of the smallest dimension.  These tick
    marks can not be automatically labelled.  If given a two-element vector,
    the first element describes the tick marks on the x-axis and
    the second element describes those on the y-axis.}
  \item{\dots}{additional \code{\link[graphics]{par}} parameters, or the arguments
    \code{main}, \code{sub}, \code{xlab}, or \code{ylab} for the
    \code{\link[graphics]{title}} function.}
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
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2013-04-10}
}
\seealso{
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

