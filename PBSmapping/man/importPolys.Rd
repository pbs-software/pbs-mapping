\name{importPolys}
\alias{importPolys}
\title{Import PolySet from a text file}
\description{
  Import a text file and convert into a PolySet with optional PolyData attribute.
}
\usage{
importPolys(PolySet, PolyData=NULL, projection=NULL, zone=NULL)
}
\arguments{
  \item{PolySet}{filename of PolySet text file.}
  \item{PolyData}{optional filename of PolyData text file.}
  \item{projection}{optional \code{projection} attribute to add to
    EventData.}
  \item{zone}{optional \code{zone} attribute to add to EventData.}
}
\value{
  An imported PolySet with optional PolyData attribute.
}
\seealso{
  \code{\link{importEvents}}, \code{\link{importLocs}}, \code{\link{importGSHHS}}, \code{\link{importShapefile}}
}
\keyword{file}
