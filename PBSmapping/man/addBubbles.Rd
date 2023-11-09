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
   z.max=NULL, min.size=0, max.size=0.8, symbol.zero="+",
   symbol.fg=rgb(0,0,0,0.60), symbol.bg=rgb(0,0,0,0.30),
   legend.pos="bottomleft", legend.breaks=NULL,
   show.actual=FALSE, legend.type=c("nested","horiz","vert"),
   legend.title="Abundance", legend.cex=.8, neg.col="RYB", ...)
}
\arguments{
  \item{events}{\link[PBSmapping]{EventData} to use (\emph{required}).}
  \item{type}{\code{character} -- scaling option for bubbles where \code{"perceptual"}
    emphasizes large z-values, \code{"volume"} emphasizes small z-values,
    and \code{"surface"} lies in between.}
  \item{z.max}{\code{numeric} -- maximum value for z (default = \code{max(events$Z)});
    determines the largest bubble; keeps the same legend for different maps.}
  \item{min.size}{\code{numeric} -- minimum size (inches) for a bubble representing
    \code{min(events$Z)}.  The legend may not actually include a bubble of
    this size because the calculated legend.breaks does not include the \code{min(events$Z)}.}
  \item{max.size}{\code{numeric} -- maximum size (inches) for a bubble representing
    \code{z.max}.  A legend bubble may exceed this size when
    \code{show.actual} is \code{FALSE} (on account of using \code{pretty(\dots)}).}
  \item{symbol.zero}{\code{character} -- symbol to represent z-values equal to 0.}
  \item{symbol.fg}{\code{character} -- bubble outline (border) colour.}
  \item{symbol.bg}{\code{character} -- bubble interior (fill) colour.  If a vector, the
    first element represents \code{min(legend.breaks)} and the last
    element represents \code{max(legend.breaks)}; colours are interpolated
    for values of \code{events$Z} between those boundaries.  For values
    outside of those boundaries, interiors remain unfilled.}
  \item{legend.pos}{\code{character|numeric} -- position for the legend.}
  \item{legend.breaks}{\code{numeric} -- break values for categorizing the z-values.
   The automatic method should work if zeroes are present;
   otherwise, you can specify your own break values for the legend.
   If a single number, specifies the number of breaks; if a vector,
   specifies the breaks.}
  \item{show.actual}{\code{logical} -- if \code{FALSE}, legend values are
    obtained using \code{pretty(\dots)}, and consequently, the largest
    bubble may be larger than \code{z.max}; if \code{TRUE}, the largest
    bubble in the legend will correspond to \code{z.max}.}
  \item{legend.type}{\code{character} -- display format for legend.}
  \item{legend.title}{\code{character} -- title for legend.}
  \item{legend.cex}{\code{numeric} -- size of legend text.}
  \item{neg.col}{\code{character} -- bubble colour for any potential negative values.}
  \item{\dots}{\code{dots} -- additional arguments for \code{points} function that plots zero-value symbols.}
}
\details{
  Modified from (and for the legend, strongly inspired by) Tanimura
  et al. (2006) by Denis Chabot to work with \pkg{PBSmapping}.

  Furthermore, Chabot's modifications make it possible to draw
  several maps with bubbles that all have the same scale
  (instead of each bubble plot having a scale that depends on
  the maximum z-value for that plot).
  This is done by making \code{z.max} equal to the largest z-value
  from all maps that will be plotted.

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
  \href{https://profils-profiles.science.gc.ca/en/profile/denis-chabot}{Denis Chabot}, Research Scientist\cr
  Maurice-Lamontagne Institute, Fisheries & Oceans Canada (DFO), Mont-Joli QC\cr

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Offsite, Vancouver BC\cr
  Last modified \code{Rd: 2023-11-02}
}
\seealso{
In package \pkg{PBSmapping}:\cr
  \code{\link[PBSmapping]{addPolys}},
  \code{\link[PBSmapping]{EventData}},
  \code{\link[PBSmapping]{RGB2RYB}},
  \code{\link[PBSmapping]{surveyData}}
}
\examples{
local(envir=.PBSmapEnv,expr={
  oldpar = par(no.readonly=TRUE)
  #--- common code for both examples below
  data(nepacLL,surveyData,envir=.PBSmapEnv)
  surveyData$Z <- surveyData$catch

  #--- plot a version that only varies the size
  plotMap(nepacLL, xlim=c(-131.8,-127.2), ylim=c(50.5,52.7),
    col="gainsboro",plt=c(.08,.99,.08,.99), cex.axis=1.2, cex.lab=1.5)
  addBubbles(surveyData, symbol.bg=rgb(.9,.5,0,.6),
    legend.type="nested", symbol.zero="+", col="grey")

  #--- plot a version that uses different symbol colours
  plotMap(nepacLL, xlim=c(-131.8,-127.2), ylim=c(50.5,52.7),
  col="gainsboro",plt=c(.08,.99,.08,.99), cex.axis=1.2, cex.lab=1.5)
  subset <- surveyData[surveyData$Z <= 1000, ]
  addBubbles(subset, symbol.bg=c("red", "yellow", "green"),
    legend.type="horiz", legend.breaks=pretty(range(subset$Z), n=11),
    symbol.zero=FALSE, col="grey", min.size=0.1, max.size=0.4)
  par(oldpar)
})
}
\keyword{aplot}

