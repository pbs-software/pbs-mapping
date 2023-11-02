\name{towTracks}
\docType{data}
\alias{towTracks}
\title{Data: Tow Track Polylines from Longspine Thornyhead Survey}
\description{
  \link{PolySet} of geo-referenced polyline tow track data from a
  longspine thornyhead survey (2001).
}
\usage{data(towTracks)}
\format{
  Data frame consisting of 4 columns: \code{PID} = primary polygon ID,
  \code{POS} = position of each vertex within a given polyline, \code{X}
  = longitude coordinate, and \code{Y} = latitude
  coordinate. Attributes: \code{projection = "LL"}, \code{zone = 9}.
}
\note{
  In R, the data must be loaded using the \code{\link{data}} function.
}
\source{
  The longspine thornyhead (\emph{Sebastolobus altivelis}) tow
  track spatial coordinates are available at the Pacific Biological
  Station (Fisheries and Oceans Canada, Nanaimo, BC V9T 6N7). The
  geo-referenced coordinates of the first 45 tows from the 2001 survey (Starr
  et al. 2002) are included here. Coordinates are recorded once per
  minute between winch lock-up and winch release.
}
\references{
  Starr, P.J., Krishka, B.A. and Choromanski, E.M. (2002) Trawl survey
  for thornyhead biomass estimation off the west coast of Vancouver
  Island, September 15 - October 2, 2001. \emph{Canadian Technical Report of
  Fisheries and Aquatic Sciences} \bold{2421}, 60 pp.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2008-09-03}
}
\seealso{
  \code{\link{addLines}},
  \code{\link{calcLength}},
  \code{\link{clipLines}},
  \code{\link{plotLines}},
  \link{PolySet},
  \code{\link{towData}}.
}
\keyword{datasets}
