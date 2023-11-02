\name{convDP}
\alias{convDP}
\title{Convert EventData/PolyData into a PolySet}
\description{
  Convert \link{EventData}/\link{PolyData} into a \link{PolySet}.
}
\usage{
convDP (data, xColumns, yColumns)
}
\arguments{
  \item{data}{\link{PolyData} or \link{EventData}.}
  \item{xColumns}{vector of X-column names.}
  \item{yColumns}{vector of Y-column names.}
}
\details{
  This function expects \code{data} to contain several X- and Y-columns.
  For example, consider \code{data} with columns \code{x1}, \code{y1},
  \code{x2}, and \code{y2}.  Suppose \code{xColumns = c("x1", "x2")} and
  \code{yColumns = c("y1", "y2")}.  The result will contain
  \code{nrow(data)} polygons.  Each one will have two vertices,
  \code{(x1, y1)} and \code{(x2, y2)} and \code{POS} values 1 and 2,
  respectively.  If \code{data} includes an \code{SID} column, so will
  the result.

  If \code{data} contains an \code{EID} and not a \code{PID} column,
  the function uses the \code{EID}s as \code{PID}s.
  
  If \code{data} contains both \code{PID} and \code{EID} columns,
  the function assumes it is \link{PolyData} and ignores the \code{EID}
  column.
}
\value{
  \link{PolySet} with the same \code{PID}s as those given in \code{data}.  If
  \code{data} has an \code{SID} column, the result will include it.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2013-04-10}
}
\seealso{
  \code{\link{addPoints}},
  \code{\link{plotPoints}}.
}
\examples{
local(envir=.PBSmapEnv,expr={
  oldpar = par(no.readonly=TRUE)
  #--- create sample PolyData
  polyData <- data.frame(PID=c(1, 2, 3),
     x1=c(1, 3, 5), y1=c(1, 3, 2),
     x2=c(1, 4, 5), y2=c(2, 4, 1),
     x3=c(2, 4, 6), y3=c(2, 3, 1))
  #--- print PolyData
  print(polyData)
  #--- make a PolySet from PolyData
  polys <- convDP(polyData,
    xColumns=c("x1", "x2", "x3"),
    yColumns=c("y1", "y2", "y3"))
  #--- print and plot the PolySet
  print(polys)
  plotLines(polys, xlim=c(0,7), ylim=c(0,5), col=2)
  par(oldpar)
})
}
\keyword{manip}

