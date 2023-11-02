\name{combinePolys}
\alias{combinePolys}
\title{Combine Several Polygons into a Single Polygon}
\description{
  Combine several polygons into a single polygon by modifying the
  \code{PID} and \code{SID} indices.
}
\usage{
combinePolys (polys)
}
\arguments{
  \item{polys}{\link{PolySet} with one or more polygons, each with
    possibly several components/holes.}
}
\details{
  This function accepts a \link{PolySet} containing one or more polygons
  (\code{PID}s), each with one or more components or holes
  (\code{SID}s).  The \code{SID} column need not exist in the input.
  The function combines these polygons into a single polygon by simply
  renumbering the \code{PID} and \code{SID} indices.  The resulting
  \link{PolySet} contains a single \code{PID} (with the value 1) and uses
  the \code{SID} value to differentiate between polygons, their
  components, and holes.  
}
\value{
  \link{PolySet}, possibly with the addition of an \code{SID} column if
  it did not already exist.  The function may also reorder columns such
  that \code{PID}, \code{SID}, \code{POS}, \code{X} and \code{Y} appear
  first, in that order.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2007-06-06}
}
\seealso{
  \code{\link{dividePolys}}
}
\keyword{manip}
