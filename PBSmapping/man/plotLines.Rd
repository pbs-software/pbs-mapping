\name{plotLines}
\alias{plotLines}
\title{Plot a PolySet as Polylines}
\description{
  Plot a \link{PolySet} as polylines.
}
\usage{
plotLines (polys, xlim = NULL, ylim = NULL, projection = FALSE,
           plt = c(0.11, 0.98, 0.12, 0.88), polyProps = NULL,
           lty = NULL, col = NULL, bg = 0, axes = TRUE,
           tckLab = TRUE, tck = 0.014, tckMinor = 0.5 * tck, \dots)
}
\arguments{
  \item{polys}{\link{PolySet} to plot (\emph{required}).}
  \item{xlim}{range of X-coordinates.}
  \item{ylim}{range of Y-coordinates.}
  \item{projection}{desired projection when \link{PolySet} lacks a
    \code{projection} attribute; one of \code{"LL"}, \code{"UTM"},
    or a numeric value.  If Boolean, specifies whether to check
    \code{polys} for a \code{projection} attribute.}
  \item{plt}{four element numeric vector \code{(x1, x2, y1, y2)} giving
    the coordinates of the plot region measured as a fraction of the
    figure region. Set to \code{NULL} if \code{mai} in \code{par} is
    desired.}
  \item{polyProps}{\link{PolyData} specifying which polylines to plot and their
    properties.  \code{\link{par}} parameters passed as direct arguments
    supersede these data.}
  \item{lty}{vector describing line types (cycled by \code{PID}).}
  \item{col}{vector describing colours (cycled by \code{PID}).}
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
  \item{\dots}{additional \code{\link{par}} parameters, or the arguments
    \code{main}, \code{sub}, \code{xlab}, or \code{ylab} for the
    \code{\link{title}} function.}
}
\details{
  This function plots a \link{PolySet}, where each unique (\code{PID},
  \code{SID}) describes a polyline.  It does not connect each polyline's
  last vertex to its first.  Unlike \code{\link{plotMap}}, the function
  ignores the aspect ratio.  It clips \code{polys} to \code{xlim} and
  \code{ylim} before plotting.

  The function creates a blank plot when \code{polys} equals
  \code{NULL}.  In this case, the user must supply both \code{xlim} and
  \code{ylim} arguments.  Alternatively, it accepts the argument
  \code{type = "n"} as part of \dots, which is equivalent to specifying
  \code{polys = NULL}, but requires a \link{PolySet}.  In both cases,
  the function's behaviour changes slightly.  To resemble the
  \code{\link{plot}} function, it plots the border, labels, and other
  parts according to \code{\link{par}} parameters such as \code{col}.

  For additional help on the arguments \code{lty} and \code{col}, please
  see \code{\link{par}}.
}
\value{
  \link{PolyData} consisting of the \code{PolyProp}s used to create the plot.
}
\note{
  To satisfy the aspect ratio, this plotting routine resizes the plot
  region.  Consequently, \code{\link{par}} parameters such as
  \code{plt}, \code{mai}, and \code{mar} will change.  When the function
  terminates, these changes persist to allow for additions to the plot.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2013-04-10}
}
\seealso{
  \code{\link{addLines}}, \code{\link{calcLength}}, \code{\link{clipLines}},
  \code{\link{closePolys}}, \code{\link{convLP}}, \code{\link{fixBound}},
  \code{\link{fixPOS}},

  \code{\link{locatePolys}}, \code{\link{thinPolys}}, \code{\link{thickenPolys}}.
}
\examples{
local(envir=.PBSmapEnv,expr={
  oldpar = par(no.readonly=TRUE)
  #--- create a PolySet to plot
  polys <- data.frame(PID=rep(1,4),POS=1:4,X=c(0,1,1,0),Y=c(0,0,1,1))
  #--- plot the PolySet
  plotLines(polys, xlim=c(-.5,1.5), ylim=c(-.5,1.5))
  par(oldpar)
})
}
\keyword{hplot}

