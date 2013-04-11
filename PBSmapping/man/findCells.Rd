\name{findCells}
\alias{findCells}
\title{Find the Grid Cells that Contain Events}
\description{
  Find the grid cells in a \link{PolySet} that contain events specified in
  \link{EventData}.  Similar to \code{\link{findPolys}}, except this
  function requires a \link{PolySet} resulting from
  \code{\link{makeGrid}}.  This restriction allows this function to
  calculate the result with greater efficiency.
}
\usage{
findCells (events, polys)
}
\arguments{
  \item{events}{\link{EventData} to use.}
  \item{polys}{\link{PolySet} to use.}
}
\details{
  The resulting data frame, a \link{LocationSet}, contains the columns
  \code{EID}, \code{PID}, \code{SID} (\emph{if in} \code{polys}), and
  \code{Bdry}, where an event (\code{EID}) occurs in a polygon
  (\code{PID}, \code{SID}). The Boolean variable \code{Bdry} indicates
  whether an event lies on a polygon's edge.  Note that if an event lies
  properly outside of all the polygons, then a record with (\code{EID},
  \code{PID}, \code{SID}) does not occur in the output.  It may happen,
  however, that an event occurs in multiple polygons (i.e., on two or
  more boundaries).  Thus, the same \code{EID} can occur more than once
  in the output.
}
\value{
  \link{LocationSet} that links events with polygons.
}
\seealso{
  \code{\link{combineEvents}},
  \code{\link{findPolys}},
  \code{\link{locateEvents}},
  \code{\link{locatePolys}},
  \link{LocationSet},
  \code{\link{makeGrid}}.
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

