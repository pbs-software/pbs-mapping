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
  \item{EventData}{filename of \code{EventData} text file.}
  \item{projection}{optional \code{projection} attribute to add to \code{EventData}.}
  \item{zone}{optional \code{zone} attribute to add to \code{EventData}.}
  
}
\value{
  An imported \code{EventData}.
}
\author{
  \href{https://academic.macewan.ca/boersn/}{Nicholas M. Boers}, Associate Professor -- Computer Science\cr
  MacEwan University, Edmonton AB\cr
  Last modified \code{Rd: 2008-08-25}
}
\seealso{
  \code{\link{importPolys}}, \code{\link{importLocs}}, \code{\link{importGSHHS}}, \code{\link{importShapefile}}
}
\keyword{file}
