\name{extractPolyData}
\alias{extractPolyData}
\title{Extract PolyData from a PolySet}
\description{
  Extract \link{PolyData} from a \link{PolySet}.  Columns for the
  \link{PolyData} include those other than \code{PID}, \code{SID},
  \code{POS}, \code{oldPOS}, \code{X}, and \code{Y}.
}
\usage{
extractPolyData (polys)
}
\arguments{
  \item{polys}{\link{PolySet} to use.}
}
\details{
  This function identifies the \link{PolySet}'s extra columns and
  determines if those columns contain unique values for each
  (\code{PID}, \code{SID}).  Where they do, the (\code{PID}, \code{SID})
  will appear in the \link{PolyData} output with that unique value.
  Where they do not, the extra column will contain \code{NA}s for that
  (\code{PID}, \code{SID}).
}
\value{
  \link{PolyData} with columns \code{PID}, \code{SID}, and any extra
  columns.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2013-04-10}
}
\seealso{
  \code{\link{makeProps}},
  \link{PolyData},
  \link{PolySet}.
}
\examples{
local(envir=.PBSmapEnv,expr={
  #--- create a PolySet with an extra column
  polys <- data.frame(PID = c(rep(1, 10), rep(2, 10)),
    POS = c(1:10, 1:10),
    X = c(rep(1, 10), rep(1, 10)),
    Y = c(rep(1, 10), rep(1, 10)),
    colour = (c(rep("green", 10), rep("red", 10))))
  #--- extract the PolyData
  print(extractPolyData(polys))
})
}
\keyword{manip}

