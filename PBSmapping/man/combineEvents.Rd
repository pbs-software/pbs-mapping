\name{combineEvents}
\alias{combineEvents}
\title{Combine Measurements of Events}
\description{
  Combine measurements associated with events that occur in the same
  polygon.
}
\usage{
combineEvents (events, locs, FUN, \dots, bdryOK = TRUE)
}
\arguments{
  \item{events}{\link{EventData} with at least four columns (\code{EID},
    \code{X}, \code{Y}, \code{Z}).}
  \item{locs}{\link{LocationSet} usually resulting from a call to
    \code{\link{findPolys}}.}
  \item{FUN}{a function that produces a scalar from a vector
    (e.g., \code{\link{mean}}, \code{\link{sum}}).}
  \item{\dots}{optional arguments for \code{FUN}.}
  \item{bdryOK}{Boolean value; if \code{TRUE}, include boundary points.}
}
\details{
  This function combines measurements associated with events that occur
  in the same polygon.  Each event (\code{EID}) has a corresponding
  measurement \code{Z}.  The \code{locs} data frame (usually output from
  \code{\link{findPolys}}) places events within polygons.  Thus, each
  polygon (\code{PID}, \code{SID}) determines a set of events within it,
  and a corresponding vector of measurements \code{Zv}.  The function
  returns \code{FUN(Zv)}, a summary of measurements within each polygon.
}
\value{
  \link{PolyData} with columns \code{PID}, \code{SID} (\emph{if in}
  \code{locs}), and \code{Z}.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2013-04-10}
}
\seealso{
  \code{\link{findCells}},
  \code{\link{findPolys}},
  \code{\link{locateEvents}},
  \code{\link{locatePolys}},
  \code{\link{makeGrid}},
  \code{\link{makeProps}}.
}
\examples{
local(envir=.PBSmapEnv,expr={
  #--- create an EventData data frame: let each event have Z = 1
  events <- data.frame(EID=1:10, X=1:10, Y=1:10, Z=rep(1, 10))
  #--- example output from findPolys where 1 event occurred in the first
  #--- polygon, 3 in the second, and 6 in the third
  locs <- data.frame(EID=1:10,PID=c(rep(1,1),rep(2,3),rep(3,6)),Bdry=rep(0,10))
  #--- sum the Z column of the events in each polygon, and print the result
  print(combineEvents(events=events, locs=locs, FUN=sum))
})
}
\keyword{manip}

