\name{makeProps}
\alias{makeProps}
\title{Make Polygon Properties}
\description{
  Append a column for a polygon property (e.g., \code{border} or
  \code{lty}) to \link{PolyData} based on measurements in the
  \link{PolyData}'s \code{Z} column.
}
\usage{
makeProps(pdata,breaks,propName="col",propVals=1:(length(breaks)-1))
}
\arguments{
  \item{pdata}{\link{PolyData} with a \code{Z} column.}
  \item{breaks}{either a vector of cut points or a scalar denoting the
    number of intervals that \code{Z} is to be cut into.}
  \item{propName}{name of the new column to append to \code{pdata}.}
  \item{propVals}{vector of values to associate with \code{Z} breaks.}
}
\details{
  This function acts like the \code{\link{cut}} function to produce
  \link{PolyData} suitable for the \code{polyProps} plotting argument
  (see \code{\link{addLabels}}, \code{\link{addLines}},
  \code{\link{addPoints}}, \code{\link{addPolys}},
  \code{\link{addStipples}}, \code{\link{plotLines}},
  \code{\link{plotMap}},\code{\link{plotPoints}}, and
  \code{\link{plotPolys}}).  The \code{Z} column of \code{pdata} is
  equivalent to the data vector \code{x} of the \code{\link{cut}}
  function.
}
\value{
  \link{PolyData} with the same columns as \code{pdata} plus an
  additional column \code{propName}.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2013-04-10}
}
\seealso{
  \code{\link{addLabels}},
  \code{\link{addLines}},
  \code{\link{addPoints}},
  \code{\link{addPolys}},
  \code{\link{addStipples}},
  \code{\link{plotLines}},
  \code{\link{plotMap}},
  \code{\link{plotPoints}},
  \code{\link{plotPolys}},
  \link{PolyData},
  \link{PolySet}.
}
\examples{
local(envir=.PBSmapEnv,expr={
  #--- create a PolyData object
  pd <- data.frame(PID=1:10, Z=1:10)

  #--- using 3 intervals, create a column named `col' and populate it with
  #--- the supplied values
  makeProps(pdata=pd, breaks=3, propName="col", propVals=c(1:3))
})
}
\keyword{manip}

