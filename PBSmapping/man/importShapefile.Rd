\name{importShapefile}
\alias{importShapefile}
\title{Import an ESRI Shapefile}
\description{
  Import an ESRI shapefile (\code{.shp}) into either a 
  \code{\link{PolySet}} or \code{\link{EventData}}.
}
\usage{
importShapefile (fn, readDBF=TRUE, projection=NULL, zone=NULL, 
   minverts=3, placeholes=FALSE, show.progress=FALSE)
}
\arguments{
  \item{fn}{\code{character} -- file name of the shapefile to import; specifying the
    extension is optional.}
  \item{readDBF}{\code{logical} --  if \code{TRUE}, import the associated
    \code{".dbf"} (database containing feature attributes) and add as an attribute
    to the output \code{PolySet} | \code{EventData} object.}
  \item{projection}{\code{character} -- optional \code{projection} attribute to override the
    internally derived value.}
  \item{zone}{\code{numeric} -- optional \code{zone} attribute to override the default
    value of \code{NULL}.}
  \item{minverts}{\code{numeric} -- minimum number of vertices required for a polygon 
    representing a hole to be retained (does not affect solids).}
  \item{placeholes}{\code{logical} --  if \code{TRUE} then for every \code{PID} 
    identify solids and holes, and place holes under appropriate solids.}
  \item{show.progress}{\code{logical} -- if \code{TRUE}, display on the 
    command console the progress of placing holes under solids.}
}
\details{
  This routine imports an ESRI shapefile (\code{.shp}) into either a
  \code{PolySet} or \code{EventData}, depending on the type of
  shapefile.  It supports types 1 (Point), 3 (PolyLine), and 5 (Polygon)
  and imports type 1 into \code{EventData} and types 3 and 5 into a
  \code{PolySet}.  In addition to the shapefile (\code{.shp}), it
  requires the related index file (\code{.shx}).

  If a database containing feature attributes (\code{.dbf}) exists, it
  also imports this database by default.  For \code{EventData}, it binds
  the database columns to the \code{EventData} object.  For a
  \code{PolySet}, it saves the database in a \code{\link{PolyData}} object and
  attaches that object to the \code{PolySet} in an attribute named
  \dQuote{PolyData}.

  If a \code{.prj} file exists, this information is attached as an attribute.
  If the first 3 characters are \sQuote{GEO}, then a geographic projection 
  is assumed and \code{projection="LL"}. If the first 4 characters are 
  \sQuote{PROJ}, and \sQuote{UTM} occurs elsewhere in the string, then the 
  Universal Transverse Mercator projection is assumed and \code{projection="UTM"}. 
  Otherwise, \code{projection=1}.

  If an \code{.xml} file exists, this information is attached as an attribute.

  Shapes of numeric shape type \code{5} exported from \pkg{ArcView} in geographic 
  projection identify solids as polygons with vertices following a clockwise 
  path and holes as polygons that follow a counter-clockwise path. 
  Unfortuantely, either the export from \pkg{ArcView} or the import using a C-routine 
  from the package \pkg{maptools} often does not report solids followed by their holes. 
  We employ a new R function \code{placeHoles} to do this for us. 
  Ideally, this routine should be rendered in C, but for now we use this 
  function if the user sets the argument \code{placeholes=TRUE}. 
  Depending on the size and complexity of your shapefile, the computation may take a while.
}
\value{
  For points, \code{EventData} with columns \code{EID}, \code{X}, and
  \code{Y}, possibly with other columns from the attribute database. For
  polylines and polygons, a \code{PolySet} with columns \code{PID},
  \code{SID}, \code{POS}, \code{X}, \code{Y} and attribute \code{projection}.
  Other attributes that may or may not be attached: \code{parent.child} 
  (boolean vector from original input), \code{shpType} (numeric shape 
  type: 1, 3, or 5), \code{prj} (projection information from \code{.prj} 
  file, \code{xml} (metadata from an \code{.xml} file), \code{PolyData} 
  (data from the attribute database \code{.dbf}), and \code{zone} (UTM zone).
}
\author{
  \href{https://academic.macewan.ca/boersn/}{Nicholas M. Boers}, Associate Professor -- Computer Science\cr
  MacEwan University, Edmonton AB\cr\cr
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{opus locus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2018-09-07}
}
\seealso{
  \code{\link[PBSmapping]{importGSHHS}},
  \code{\link[PBSmapping]{importEvents}},
  \code{\link[PBSmapping]{importLocs}},
  \code{\link[PBSmapping]{importPolys}},
  \code{\link[PBSmapping]{placeHoles}}
}
\keyword{file}

