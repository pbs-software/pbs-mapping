\name{surveyData}
\docType{data}
\alias{surveyData}
\title{Data: Tow Information from Pacific Ocean Perch Survey}
\description{
  \link{EventData} of Pacific ocean perch (POP) tow information (1966-89).
}
\usage{data(surveyData)}
\format{
  Data frame consisting of 9 columns: \code{PID} = primary polygon ID,
  \code{POS} = position of each vertex within a given polygon, \code{X}
  = longitude coordinate, \code{Y} = latitude coordinate,  \code{trip}
  = trip ID, \code{tow} = tow number in trip, \code{catch} = catch of
  POP (kg), \code{effort} = tow effort (minutes), \code{depth} = fishing
  depth (m), and \code{year} = year of survey trip. Attributes:
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
  (BC).
  
  The POP (\emph{Sebastes alutus}) survey data were extracted from
  GFBio. The data extraction covers bottom trawl surveys that focus
  primarily on POP biomass estimation: 1966-89 for the central BC coast
  and 1970-85 for the west coast of Vancouver Island. Additionally, a
  1989 cruise along the entire BC coast concentrated on the collection
  of biological samples. Schnute et al. (2001) provide a more
  comprehensive history of POP surveys including the subset of data
  presented here.
}
\references{
  Schnute, J.T., Haigh, R., Krishka, B.A. and Starr, P. (2001) Pacific
  ocean perch assessment for the west coast of Canada in 2001. \emph{Canadian
  Science Advisory Secretariat, Research Document} \bold{2001/138}, 90 pp.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2008-09-03}
}
\seealso{
  \code{\link{addPoints}},
  \code{\link{combineEvents}},
  \link{EventData},
  \code{\link{findPolys}},
  \code{\link{makeGrid}},
  \code{\link{plotPoints}}.
}
\keyword{datasets}
