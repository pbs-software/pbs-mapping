\name{dot-check}
\alias{.checkClipLimits}
\alias{.checkProjection}
\alias{.checkRDeps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Check Stuff
}
\description{
  Auxiliary (dot) functions to check various attributes of \code{PBmapping} objects.
}
\usage{
.checkClipLimits(limits)
.checkProjection(projectionPlot, projectionPoly)
.checkRDeps(caller="unspecified", requires=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{limits}{\code{numeric} -- 4-element vector of limits for \code{'X'} and \code{'Y'}}
  \item{projectionPlot}{\code{character} -- projection of the currently plotted map}
  \item{projectionPoly}{\code{character} -- projection of the \code{PolySet}, presumably to add to map}
  \item{caller}{\code{character} -- a function name}
  \item{requires}{\code{character} -- a vector of package names}
}
\details{
  Auxiliary dot functions to facilitate the machinations of \code{PBSmapping}.
}
\value{
	\code{.checkClipLimits} : nada; just a limit checker\cr
	\code{.checkProjection} : nada; just a projection checker\cr
	\code{.checkRDeps}      : nada; just a package checker
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
  \code{.checkClipLimits} used by:\cr
    \code{\link[PBSmapping]{importGSHHS}}\cr
  \code{.checkProjection} used by:\cr
    \code{\link[PBSmapping]{addLabels}},
    \code{\link[PBSmapping]{addLines}},
    \code{\link[PBSmapping]{addPoints}},
    \code{\link[PBSmapping]{addPolys}},
    \code{\link[PBSmapping]{addStipples}}\cr
  \code{.checkRDeps} used by:\cr
    \code{\link[PBSmapping]{calcVoronoi}},
    \code{importShapefile} (temporarily unavailable)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{manip}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{dotFuns}
% \concept{ ~cpt2 }
% Use only one concept per line.
