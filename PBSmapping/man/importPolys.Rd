\name{importPolys}
\alias{importPolys}
\title{Import PolySet from Text File}
\description{
  Import a text file and convert it into a PolySet with optional PolyData attribute.
}
\usage{
importPolys(PolySet, PolyData=NULL, projection=NULL, zone=NULL)
}
\arguments{
  \item{PolySet}{\code{character} -- file name of PolySet text file.}
  \item{PolyData}{\code{character} -- optional file name of PolyData text file.}
  \item{projection}{\code{character} -- optional projection attribute to add to EventData.}
  \item{zone}{\code{numeric} -- optional zone attribute to add to EventData.}
}
\value{
  An imported PolySet with optional PolyData attribute.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2023-10-30}
}
\seealso{
  \code{\link{importEvents}}, \code{\link{importLocs}}, \code{\link{importGSHHS}}
}
\keyword{file}
