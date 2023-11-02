\name{importEvents}
\alias{importEvents}
\title{Import EventData from a Text File}
\description{
  Import a text file and convert into \code{EventData}.
}
\usage{
importEvents(EventData, projection=NULL, zone=NULL)
}
\arguments{
  \item{EventData}{\code{character} -- filename of \code{EventData} text file.}
  \item{projection}{\code{character} -- optional projection attribute to add to \code{EventData}.}
  \item{zone}{\code{numeric} -- optional zone attribute to add to \code{EventData}.}
}
\value{
  An imported \code{EventData}.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2023-10-30}
}
\seealso{
  \code{\link{importPolys}}, \code{\link{importLocs}}, \code{\link{importGSHHS}}
}
\keyword{file}
