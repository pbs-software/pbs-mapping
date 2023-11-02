\name{towData}
\docType{data}
\alias{towData}
\title{Data: Tow Information from Longspine Thornyhead Survey}
\description{
  \link{PolyData} of tow information for a longspine thornyhead survey (2001).
}
\usage{data(towData)}
\format{
  Data frame consisting of 8 columns: \code{PID} = primary polygon ID,
  \code{POS} = position of each vertex within a given polygon, \code{X}
  = longitude coordinate, \code{Y} = latitude coordinate, \code{depth}
  = fishing depth (m), \code{effort} = tow effort (minutes), \code{distance}
  = tow track distance (km), \code{catch} = catch of longspine
  thornyhead (kg), and \code{year} = year of survey. Attributes:
  \code{projection = "LL"}, \code{zone = 9}.
}
\note{
  In R, the data must be loaded using the \code{\link{data}} function.
}
\source{
  The GFBio database, maintained at the Pacific Biological Station
  (Fisheries and Oceans Canada, Nanaimo, BC V9T 6N7), archives catches
  and related biological data from commercial groundfish fishing trips
  and research/assessment cruises off the west coast of British Columbia
  (BC).  The longspine thornyhead (\emph{Sebastolobus altivelis}) survey data
  were extracted from GFBio. Information on the first 45 tows from the
  2001 survey (Starr et al. 2002) are included here. Effort is time
  (minutes) from winch lock-up to winch release.
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
  \code{\link{makeProps}},
  \link{PolyData},
  \code{\link{towTracks}}.
}
\keyword{datasets}
