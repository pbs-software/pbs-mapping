\name{addBubbles}
\alias{addBubbles}
\title{
  Add Bubbles to Maps
}
\description{
  Add bubbles proportional to some value (e.g., catch, effort) to 
  a map. The function takes x, y and z arguments and optional 
  arguments to draw bubbles of radius proportional to the z variable.
}
\usage{
addBubbles(x, y, z, type=c("perceptual","surface","volume"),
   z.max=max(z,na.rm=TRUE), max.size=0.05, symbol.zero="+", 
   symbol.fg=rgb(0,0,0,0.6), symbol.bg=rgb(0,0,0,0.3), 
   legend.pos="bottomleft", legend.breaks=pretty(range(z),3)[-1], 
   show.actual=FALSE, legend.type=c("nested","horiz","vert"), 
   legend.title="Abundance", legend.cex=0.8, ...)
}
\arguments{
  \item{x}{X-coordinates compatible with map already plotted.}
  \item{y}{Y-coordinates compatible with map already plotted.}
  \item{z}{values that will determine relative size of bubbles at (X,Y).}
  \item{type}{scaling option for bubbles where \code{"perceptual"} emphasises
    large z-values while \code{"volume"} emphasises small z-values. 
    Type \code{"surface"} lies in between.}
  \item{z.max}{maximum value for z (affects largest bubble).}
  \item{max.size}{maximum size (inches) of the largest bubble.}
  \item{symbol.zero}{symbol to reprezent zero-value z-values.}
  \item{symbol.fg}{bubble outline (border) colour.}
  \item{symbol.bg}{bubble interior (fill) colour.}
  \item{legend.pos}{position for the legend.}
  \item{legend.breaks}{break values for categorizing the z-values.}
  \item{show.actual}{logical: if \code{TRUE} scale categories/legend to 
    actual values (?).}
  \item{legend.type}{display format for legend.}
  \item{legend.title}{title for legend.}
  \item{legend.cex}{size of legend text.}
  \item{\dots}{additional arguments for \code{points} function that plots zero-value symbols.}
}
\details{
  Modified (and for the legend, strongly inspired) from Tanimura 
  et al. (2006) by Denis Chabot to work with PBSmapping.

	Furthermore, Chabot's modifications make it possible to draw 
	several maps with bubbles that all have the same scale 
	(instead of each bubble plot having a scale that depends on 
	the max z value for that plot).
	
	User can also add a legend in one of four corners or at a 
	specific x-y position.
}
\references{
  Tanimura, S., Kuroiwa, C., and Mizota, T. (2006)
  Proportional symbol mapping in R.
  \emph{Journal of Statistical Software} \bold{15}(5).
}
\author{
  Denis Chabot, Institut Maurice-Lamontagne, Mont-Joli QC
}
\note{
  This function has not yet been fully transformed to work with \link{EventData}.
}
\seealso{
  \code{\link{addPolys}}, \code{\link{surveyData}}
}
\examples{
require(PBSmapping)
data(nepacLL,surveyData)
plotMap(nepacLL, xlim=c(-131.8,-127.2), ylim=c(50.5,52.7),
  col="gainsboro",plt=c(.08,.99,.08,.99))
addBubbles(surveyData$X, surveyData$Y, surveyData$catch,
  symbol.bg=rgb(.9,.5,0,.6), legend.type="nested", 
  symbol.zero="+", col="grey")
}
\keyword{aplot}

