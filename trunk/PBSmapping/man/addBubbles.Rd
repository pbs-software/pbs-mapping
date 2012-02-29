\name{addBubbles}
\alias{addBubbles}
\title{
  Add Bubbles to Maps
}
\description{
  Add bubbles proportional to some \code{EventData}'s \code{Z} column
  (e.g., catch or effort) to an existing plot, where each unique
  \code{EID} describes a bubble.
}
\usage{
addBubbles(events, type=c("perceptual","surface","volume"),
   z.max=NULL, max.size=0.8, symbol.zero="+",
   symbol.fg=rgb(0,0,0,0.6), symbol.bg=rgb(0,0,0,0.3),
   legend.pos="bottomleft", legend.breaks=NULL,
   show.actual=FALSE, legend.type=c("nested","horiz","vert"),
   legend.title="Abundance", legend.cex=0.8, ...)
}
\arguments{
  \item{events}{\link[PBSmapping]{EventData} to use (\emph{required}).}
  \item{type}{scaling option for bubbles where \code{"perceptual"}
    emphasizes large z-values, \code{"volume"} emphasizes small z-values,
    and \code{"surface"} lies in between.}
  \item{z.max}{maximum value for z (default = \code{max(events$Z)});
    determines the largest bubble; keeps the same legend for different
    maps.}
  \item{max.size}{maximum size (inches) for a bubble representing
    \code{z.max}.  A legend bubble may exceed this size when
    \code{show.actual} is \code{FALSE} (on account of using
    \code{pretty(\dots)}).}
  \item{symbol.zero}{symbol to represent z-values equal to 0.}
  \item{symbol.fg}{bubble outline (border) colour.}
  \item{symbol.bg}{bubble interior (fill) colour.}
  \item{legend.pos}{position for the legend.}
  \item{legend.breaks}{break values for categorizing the z-values.
   The automatic method should work if zeroes are present;
   otherwise, you can specify your own break values for the legend.}
  \item{show.actual}{logical; if \code{FALSE}, legend values are
    obtained using \code{pretty(\dots)}, and consequently, the largest
    bubble may be larger than \code{z.max}.  If \code{TRUE}, the largest
    bubble in the legend will correspond to \code{z.max}.}
  \item{legend.type}{display format for legend.}
  \item{legend.title}{title for legend.}
  \item{legend.cex}{size of legend text.}
  \item{\dots}{additional arguments for \code{points} function that
    plots zero-value symbols.}
}
\details{
  Modified from (and for the legend, strongly inspired by) Tanimura
  et al. (2006) by Denis Chabot to work with \pkg{PBSmapping}.

  Furthermore, Chabot's modifications make it possible to draw
  several maps with bubbles that all have the same scale
  (instead of each bubble plot having a scale that depends on
  the maximum z-value for that plot). This is done by making
  \code{z.max} equal to the largest z-value from all maps that will be
  plotted.
  
  The user can also add a legend in one of four corners
  (see \code{\link[graphics]{legend}}) or at a specific \code{c(X,Y)} position.
  If \code{legend.pos} is \code{NULL}, no legend is drawn.
}
\references{
  Tanimura, S., Kuroiwa, C., and Mizota, T. (2006)
  Proportional symbol mapping in R.
  \emph{Journal of Statistical Software} \bold{15}(5).
}
\author{
  Denis Chabot, Maurice Lamontagne Institute, Fisheries and Oceans Canada, Mont-Joli QC
}
\seealso{
  \code{\link[PBSmapping]{addPolys}}, \code{\link[PBSmapping]{surveyData}}
}
\examples{
require(PBSmapping)
data(nepacLL,surveyData)
plotMap(nepacLL, xlim=c(-131.8,-127.2), ylim=c(50.5,52.7),
  col="gainsboro",plt=c(.08,.99,.08,.99))
surveyData$Z <- surveyData$catch
addBubbles(surveyData, symbol.bg=rgb(.9,.5,0,.6),
  legend.type="nested", symbol.zero="+", col="grey")
}
\keyword{aplot}
