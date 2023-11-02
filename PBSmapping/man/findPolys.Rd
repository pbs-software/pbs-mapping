\name{findPolys}
\alias{findPolys}
\title{Find Polygons that Contain Events}
\description{
  Find the polygons in a \link{PolySet} that contain events specified in
  \link{EventData}.
}
\usage{
findPolys (events, polys, maxRows = 1e+05, includeBdry=NULL)
}
\arguments{
  \item{events}{\link{EventData} to use.}
  \item{polys}{\link{PolySet} to use.}
  \item{maxRows}{estimated maximum number of rows in the output \link{LocationSet}.}
  \item{includeBdry}{numeric: determines how points on boundaries are handled: \cr
    if \code{NULL} then report all points on polygon boundaries (default behaviour); \cr
    if \code{0} then exclude all points on polygon boundaries; \cr
    if \code{1} then report only the first (lowest PID/SID) polygon boundary; \cr
    if \code{2,...,n} then report the last (highest PID/SID) polygon boundary.}
}
\details{
  The resulting data frame, a \link{LocationSet}, contains the columns
  \code{EID}, \code{PID}, \code{SID} (\emph{if in} \code{polys}), and
  \code{Bdry}, where an event (\code{EID}) occurs in a polygon
  (\code{PID}, \code{SID}) and \code{SID} does not correspond to an
  inner boundary.  The Boolean variable \code{Bdry} indicates whether an
  event lies on a polygon's edge.  Note that if an event lies properly
  outside of all the polygons, then a record with (\code{EID},
  \code{PID}, \code{SID}) does not occur in the output.  It may happen,
  however, that an event occurs in multiple polygons.  Thus, the same
  \code{EID} can occur more than once in the output.
  
  If an event happens to lie at the boundary intersection of two or more polygons
  then one \code{EID} will be associated with two or more polygons. A user
  can choose to manipulate this result by setting the argument \code{includeBdry}
  to a numeric value that constrains the association of a boundary event to
  0 or 1 polygon (see argument description above).
}
\value{
  \link{LocationSet} that links events with polygons.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2014-12-15}
}
\seealso{
  \code{\link{combineEvents}},
  \code{\link{findCells}},
  \code{\link{locateEvents}},
  \code{\link{locatePolys}},
  \link{LocationSet},
  \code{\link{makeGrid}}.
}
\examples{
local(envir=.PBSmapEnv,expr={
  oldpar = par(no.readonly=TRUE)
  #--- create some EventData: a column of points at X = 0.5
  events <- data.frame(EID=1:10, X=.5, Y=seq(0, 2, length=10))
  events <- as.EventData(events, projection=1)
  #--- create a PolySet: two squares with the second above the first
  polys <- data.frame(PID=c(rep(1, 4), rep(2, 4)), POS=c(1:4, 1:4),
    X=c(0, 1, 1, 0, 0, 1, 1, 0),
    Y=c(0, 0, 1, 1, 1, 1, 2, 2))
  polys <- as.PolySet(polys, projection=1)
  #--- show a picture
  plotPolys(polys, xlim=range(polys$X)+c(-0.1, 0.1),
    ylim=range(polys$Y)+c(-0.1, 0.1), projection=1);
  addPoints(events, col=2);
  #--- run findPolys and print the results
  print(findPolys(events, polys))
  par(oldpar)
})
}
\keyword{manip}

