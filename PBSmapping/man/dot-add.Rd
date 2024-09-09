\name{dot-add}
\alias{.addAxis}
\alias{.addAxis2}
\alias{.addBubblesLegend}
\alias{.addCorners}
\alias{.addFeature}
\alias{.addLabels}
\alias{.addProps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Add Items to Plots
}
\description{
  Routines to add various items to existing plots.
}
\usage{
.addAxis(xlim, ylim, tckLab, tck, tckMinor, ...)
.addAxis2(side=1:2, xlim, ylim, tckLab, tck, tckMinor, ...)
.addBubblesLegend(radii.leg, usr.xdiff, usr.ydiff, symbol.zero, symbol.fg, symbol.bg,
   legend.pos, legend.breaks, legend.type, legend.title, legend.cex, ...)
.addCorners(polys, ptSummary)
.addFeature(feature, data, polyProps, isEventData, cex=NULL, col=NULL, font=NULL,
   pch=NULL, ...)
.addLabels(projection=NULL, ...)
.addProps(type, polyProps, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xlim}{\code{numeric} -- limits of the x-axis}
  \item{ylim}{\code{numeric} -- limits of the y-axis}
  \item{tckLab}{\code{character} -- tick labels}
  \item{tck}{\code{numeric} -- length of tick marks as a fraction of the smaller of 
    the width or height of the plotting region}
  \item{tckMinor}{\code{numeric} -- length of minor tick marks between major tick marks}
  \item{...}{\code{dots} -- additional arguments used by functions within function}
  \item{side}{\code{numeric} -- side of the plot (1=bottom, 2=left, 3=top, 4=right)}
  \item{radii.leg}{\code{numeric} -- radii of bubbles in legend}
  \item{usr.xdiff}{\code{numeric} -- difference between the limits of x}
  \item{usr.ydiff}{\code{numeric} -- difference between the limits of y}
  \item{symbol.zero}{\code{character} -- character to use for plotting zero values}
  \item{symbol.fg}{\code{numeric} -- colour of bubble outline}
  \item{symbol.bg}{\code{numeric} -- colour of bubble fill}
  \item{legend.pos}{\code{character} -- one of legends positional strings (e.g., `bottomleft')}
  \item{legend.breaks}{\code{numeric} -- break values for categorizing z-values}
  \item{legend.type}{\code{character} -- display format for legend}
  \item{legend.title}{\code{character} -- title for legend}
  \item{legend.cex}{\code{numeric} -- size of legend text}
  \item{polys}{\code{PolySet} -- polygon data file}
  \item{ptSummary}{\code{numeric} -- used in computation of distance from candidate points to corner}
  \item{feature}{\code{character} -- if \code{'points'}, add \code{'data'} using \code{'points()'}\cr
    if \code{'labels'}, add \code{'data'} using \code{'text()'}}
  \item{data}{\code{data.frame} -- one of the data structures used in \code{'PBSmapping'}}
  \item{polyProps}{\code{data.frame} -- polygon properties, like a \code{'PolyData'} object}
  \item{isEventData}{\code{logical} -- if \code{TRUE}, look for \code{'EID'} column in \code{'data'}}
  \item{cex}{\code{numeric} -- value to magnify text and symbols relative to the default}
  \item{col}{\code{character|numeric} -- colour for added feature}
  \item{font}{\code{numeric} -- font number for added feature}
  \item{pch}{\code{numeric} -- plotting symbol for added feature}
  \item{projection}{\code{character} -- projection of plot on which to add label
    (either \code{'LL'} or \code{'UTM'})}
  \item{type}{\code{character} -- data structure type, where \code{'e'} = \code{'EventData'} and
    \code{'p'} = \code{'PolyData'}}
}
\details{
  Internal functions add features to an existing map plot.
}
\value{
	Nothing in particular.
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
  \code{\link[PBSmapping]{plotMap}},
  \code{\link[PBSmapping]{addBubbles}},
  \code{\link[PBSmapping]{addPolys}},
  \code{\link[PBSmapping]{addLabels}},
  \code{\link[PBSmapping]{addStipples}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{aplot}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{dotFuns}
% \concept{ ~cpt2 }
% Use only one concept per line.
