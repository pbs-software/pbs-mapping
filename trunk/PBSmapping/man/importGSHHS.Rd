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
  \item{gshhsDB}{path name to binary GSHHS database. If unspecified,
    looks for \code{gshhs_f.b} in the root of the \pkg{PBSmapping} library
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
  from \cr \url{http://www.soest.hawaii.edu/pwessel/gshhg/}. \cr  At the time of
  writing, the most recent database is \code{gshhg-bin-2.2.2.zip}.

  The database \code{gshhg-bin-2.2.2.zip} contains geographical coordinates for 
  shorelines (\code{gshhs}), rivers (\code{wbd_rivers}), and borders (\code{wdb_borders}).
  The latter two come from World DataBank II (WDBII): \cr
  \url{http://meta.wikimedia.org/wiki/Geographical_data#CIA_World_DataBank_II_and_derivates} \cr
  The five resolutions available are: \cr
  full (\code{f}), high (\code{h}), intermediate (\code{i}), low (\code{l}), and coarse (\code{c}).

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
\author{
  Nicholas Boers, Dept. of Computer Science, Grant MacEwan University, Edmonton AB
}
\seealso{
  \code{\link[PBSmapping]{importEvents}}, \code{\link[PBSmapping]{importLocs}}, 
  \code{\link[PBSmapping]{importPolys}},  \code{\link[PBSmapping]{importShapefile}}
}
\examples{
\dontrun{
pbsfun = function(ex=1) {
  switch(ex, {
  #--- EXAMPLE 1
  #--- set some limits appropriate for a map of Canada
  limits <- list(x = c(216.0486, 307.1274), y = c(42.87209, 77.35183))
  #--- extract data from the GSHHS binary files; you will need to download
  #--- these files from http://www.soest.hawaii.edu/pwessel/gshhg/
  #--- and place them in an appropriate location
  polys <- importGSHHS ("./gshhg-bin-2.2.2/gshhs_l.b",
                   xlim=limits$x, limits$y, maxLevel=4)
  rivers <- importGSHHS ("./gshhg-bin-2.2.2/wdb_rivers_i.b",
                         xlim=limits$x, limits$y)
  borders <- importGSHHS ("./gshhg-bin-2.2.2/wdb_borders_i.b",
                          xlim=limits$x, limits$y)
  #--- create a PNG for the output
  png ("./Canada.png", width=1600, height=1200, pointsize=24)
  #--- plot the polygons, river, and then borders
  plotMap (polys, plt=c(.05,.99,.075,.99), col="moccasin", bg="skyblue")
  addLines (rivers, col="lightblue")
  addLines (borders, col="red")
  #--- close the output file
  dev.off ()
  },{
  #--- EXAMPLE 2
  #--- clip out Manitoulin Island area which includes all four levels
  polys <- importGSHHS ("./gshhg-bin-2.2.2/gshhs_f.b",
                        xlim=c(276, 279), ylim=c(45.3, 46.5), maxLevel=4)
  #--- plot the map and add a label
  plotMap (polys, col="beige", bg="lightblue");
  text (-82.08, 45.706, "Manitoulin Isl")
  })
  invisible()
}
pbsfun(1); pbsfun(2)
}
}
\keyword{file}

