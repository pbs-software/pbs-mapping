\name{findCells}
\alias{findCells}
\title{Find Grid Cells that Contain Events}
\description{
  Find the grid cells in a \link{PolySet} that contain events specified in
  \link{EventData}.  Similar to \code{\link{findPolys}}, except this
  function requires a \link{PolySet} resulting from
  \code{\link{makeGrid}}.  This restriction allows this function to
  calculate the result with greater efficiency.
}
\usage{
findCells (events, polys, includeBdry=NULL)
}
\arguments{
  \item{events}{\link{EventData} to use.}
  \item{polys}{\link{PolySet} to use.}
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
  (\code{PID}, \code{SID}). The Boolean (0,1) variable \code{Bdry} indicates
  whether an event lies on a polygon's edge.  Note that if an event lies
  properly outside of all the polygons, then a record with (\code{EID},
  \code{PID}, \code{SID}) does not occur in the output.  It may happen,
  however, that an event occurs in multiple polygons (i.e., on two or
  more boundaries).  Thus, the same \code{EID} can occur more than once
  in the output.
  
  If an event happens to lie at the boundary intersection of four (or two) grid cells 
  then one \code{EID} will be associated with four (or two) grid cells. A user
  can choose to manipulate this result by setting the argument \code{includeBdry}
  to a numeric value that constrains the association of a boundary event to 
  0 or 1 grid cell (see argument description above).
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
  \code{\link{findPolys}},
  \code{\link{makeGrid}},
  \code{\link{combineEvents}},
  \code{\link{locateEvents}},
  \code{\link{locatePolys}},
  \link{LocationSet}.
}
\examples{
local(envir=.PBSmapEnv,expr={
  oldpar = par(no.readonly=TRUE)
  #--- create some EventData: points in a diagonal line
  events <- data.frame(EID=1:11, X=seq(0, 2, length=11),
     Y=seq(0, 2, length=11))
  events <- as.EventData(events, projection=1);
  #--- create a PolySet (a grid)
  polys <- makeGrid (x=seq(0, 2, by=0.50), y=seq(0, 2, by=0.50), projection=1)
  #--- show a picture
  plotPolys(polys, xlim=range(polys$X)+c(-0.1, 0.1),
    ylim=range(polys$Y)+c(-0.1, 0.1), projection=1)
  addPoints(events, col=2)
  #--- run findCells and print the results
  fc <- findCells(events, polys)
  fc <- fc[order(fc$EID, fc$PID, fc$SID), ]
  fc$label <- paste(fc$PID, fc$SID, sep=", ")
  print (fc)
  #--- add labels to the graph
  addLabels(as.PolyData(fc[!duplicated(paste(fc$PID,fc$SID)), ],
    projection=1), placement="CENTROID", 
    polys=as.PolySet(polys, projection=1), col=4)
  par(oldpar)
})
}
\keyword{manip}

