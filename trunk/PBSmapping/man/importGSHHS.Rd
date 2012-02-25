\name{importGSHHS}
\alias{importGSHHS}
\title{Import Data from a GSHHS Database}
\description{
  Import data from a GSHHS database and convert data into a \code{PolySet}
  with a \code{PolyData} attribute.
}
\usage{
importGSHHS(gshhsDB, xlim, ylim, maxLevel=4, n=0)
}
\arguments{
  \item{gshhsDB}{pathname to binary GSHHS database. If unspecified,
    looks for \code{gshhs_f.b} in the root of the PBSmapping library
    directory.}
  \item{xlim}{range of X-coordinates (for clipping).
    The range should be between 0 and 360.}
  \item{ylim}{range of Y-coordinates (for clipping).}
  \item{maxLevel}{maximum level of polygons to import:
    1 (land), 2 (lakes on land), 3 (islands in lakes), or 4 (ponds on
    islands); ignored when importing lines.}
  \item{n}{minimum number of vertices that must exist in a line/polygon
    in order for it to be imported.}
}
\details{
  This routine requires a binary GSHHS (Global Self-consistent,
  Hierarchical, High-resolution Shoreline) database file. The GSHHS
  database has been released in the public domain and may be downloaded
  from \url{http://www.soest.hawaii.edu/wessel/gshhs/}.  At the time of
  writing, the most recent database is \code{gshhs+wdbii_2.2.0.zip}.

  This routine returns a \code{PolySet} object with an associated
  \code{PolyData} attribute. The attribute contains four fields: (a)
  \code{PID}, (b) \code{SID}, (c) \code{Level}, and (d) \code{Source}.
  Each record corresponds to a line/polygon in the \code{PolySet}.  The
  \code{Level} indicates the line's/polygon's level (1=land, 2=lake,
  3=island, 4=pond). The \code{Source} identifies the data source
  (1=WVS, 0=CIA (WDBII)).
}
\value{
  A \code{PolySet} with a \code{PolyData} attribute.
}
\seealso{
  \code{\link{importEvents}}, \code{\link{importLocs}}, \code{\link{importPolys}}, \code{\link{importShapefile}}
}
\examples{
\dontrun{
#--- EXAMPLE 1
#--- set some limits appropriate for a map of Canada
limits <- list(x = c(216.0486, 307.1274), y = c(42.87209, 77.35183))

#--- extract data from the GSHHS binary files; you will need to download
#--- these files from http://www.soest.hawaii.edu/wessel/gshhs/
#--- and place them in an appropriate location
polys <- importGSHHS ("~/gshhs+wdbii_2.2.0/gshhs/gshhs_l.b",
	                 xlim=limits$x, limits$y, maxLevel=4)
rivers <- importGSHHS ("~/gshhs+wdbii_2.2.0/gshhs/wdb_rivers_i.b",
                       xlim=limits$x, limits$y)
borders <- importGSHHS ("~/gshhs+wdbii_2.2.0/gshhs/wdb_borders_i.b",
                        xlim=limits$x, limits$y)

#--- create a PNG for the output
png ("~/plot.png", width=1600, height=1200, pointsize=20)

#--- plot the polygons, river, and then borders
plotMap (polys, col="beige", bg="lightblue")
addLines (rivers, col="lightblue")
addLines (borders, col="red")

#--- close the output file
dev.off ()

#--- EXAMPLE 2
#--- clip out Manitoulin Island area which includes all four levels
polys <- importGSHHS ("~/gshhs+wdbii_2.2.0/gshhs/gshhs_f.b",
                      xlim=c(276, 279), ylim=c(45.3, 46.5), maxLevel=4)
#--- plot the map and add a label
plotMap (polys, col="beige", bg="lightblue");
text (-82.08, 45.706, "Manitoulin Isl")
}
}
\keyword{file}
