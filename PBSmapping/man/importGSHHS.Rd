\name{importGSHHS}
\alias{importGSHHS}
\title{Import Data from a GSHHS Database}
\description{
  Import data from a GSHHS database and convert data into a \code{PolySet}
  with a \code{PolyData} attribute.
}
\usage{
importGSHHS(gshhsDB, xlim, ylim, level=1, n=0, xoff=-360)
}
\arguments{
  \item{gshhsDB}{filename of binary GSHHS database. If no file is specified,
    look for gshhs\_f.b in the root of the PBSmapping library directory.}
  \item{xlim}{range of X-coordinates to clip. Range should match the transform \code{xoff}.}
  \item{ylim}{range of Y-coordinates to clip.}
  \item{level}{import polygons of level 1 (land), 2 (lakes on land), 3 (islands in lakes),
    4 (ponds on islands). Multiple layers are specified in a vector.}
  \item{n}{retain polygons of at least n vertices.}
  \item{xoff}{transform the X-coordinates by specified amount.}
}
\details{
  This routine requires a binary GSHHS (Global Self-consistent, Hierarchical,
  High-resolution Shoreline) database file. The GSHHS database has been 
  released in the public domain and may be downloaded from
  \url{http://www.soest.hawaii.edu/wessel/gshhs/gshhs.html}.
  At the time of writing, the most recent database is \code{gshhs_1.5.zip}. 
  This version uses binary packing of the header information.
  
  This routine will return a \code{PolySet} object directly to R.
  
  The \code{PolyData} attribute contains four fields: 1) PID which maps poly data
  to a polygon in the \code{PolySet}, 2) LEVEL to indicate the type of polygon 
  (1=land, 2=lake, 3=island, 4=pond), 3) SOURCE to indicate the source of
  the data (1=WVS, not 1=CIA (WDBII)), and 4) GREENWICH (1 if poly crosses
  greenwich, 0 otherwise).
}
\value{
  A \code{PolySet} with a \code{PolyData} attribute.
}
\seealso{
  \code{\link{importEvents}}, \code{\link{importLocs}}, \code{\link{importPolys}}, \code{\link{importShapefile}}
}
\examples{
\dontrun{
  #function to properly plot the 4 levels in order working
  #from 1 (land) to 4 (ponds on islands in lakes on land)
  plotInOrder <- function(polys, ...)
  {
    myCols = c("gold1", "steelblue2", "gold1", "steelblue2")
    projection <- attr(polys, "projection")
    if (is.null(projection))
      stop("projection attribute is null")
    Pdata <- attr(polys, "PolyData")
    if (is.null(Pdata))
      stop("PolyData attribute is null")

    plotMap(NULL, xlim=c(min(polys$X), max(polys$X)), 
            ylim=c(min(polys$Y), max(polys$Y)), projection=projection, ...)

    #extract all polygons of a certain level
    #(could probably be done more efficiently)
    for(i in 1:4) {
      PIDs <- Pdata[Pdata$LEVEL==i,]$PID
      if (length(PIDs)==0) next
      tmp <- NULL
      for (p in PIDs) {
        if (is.null(tmp))
          tmp <- polys[(polys$PID==p),]
        else
          tmp <- rbind(tmp, polys[(polys$PID==p),])
      }
      attr(tmp, "PolyData") <- Pdata
      addPolys(tmp, col=myCols[i])
    }
  }
  
  # You will have to download a copy of `gshhs_f.b'
  # from http://www.soest.hawaii.edu/wessel/gshhs/gshhs.html
  # and place it in the PBSmapping library root.

  # clip out Manitoulin Island area which includes all four levels
  x <- importGSHHS(xlim=c(-84, -81), ylim=c(45.3, 46.5), level=1:4)

  #plot map data and a label
  plotInOrder(x, main="Manitoulin Island, Ontario, Canada")
  text(-82.08, 45.706, "Manitoulin Isl")

}
}
\keyword{file}
