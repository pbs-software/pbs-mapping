\name{dot-is.in}
\alias{.is.in}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Are Points in Polygons?
}
\description{
  Determines which points (EventData) are located inside a polygon (PolySet).
}
\usage{
.is.in(events, polys, use.names=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{events}{\code{numeric} -- data frame of event data (points), 
    with X and Y coordinates; not necessarily a PBSmapping \code{EventData} object}
  \item{polys}{\code{numeric} -- a PBSmapping PolySet (polygon)}
  \item{use.names}{\code{logical} -- \emph{currently not used}}
}
\details{
  Taps into the PBSmapping C code for \code{'findPolys'}.
}
\value{
  Reports events inside the polygon, outside the polygon, and whether
  all events are inside, outside, or on the boundary of the polygon.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-03-11}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
In \pkg{PBSmapping}:\cr
  \code{\link[PBSmapping]{findPolys}},
  \code{\link[PBSmapping]{as.EventData}},
  \code{\link[PBSmapping]{is.PolySet}},
  \code{\link[PBSmapping]{findPolys}}
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
