\name{importGSHHS}
\alias{importGSHHS}
\title{Import Data from a GSHHS Database}
\description{
  Import data from a GSHHS database and convert data into a \code{PolySet}
  with a \code{PolyData} attribute.
}
\usage{
importGSHHS(gshhsDB, xlim, ylim, maxLevel=4, n=0, useWest=FALSE)
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
  \item{useWest}{logical: if \code{TRUE}, convert the X-coordinates (longitude)
    to \eqn{^\circ}{deg}W (western hemisphere -180 to 0).}
}
\details{
  This routine requires a binary GSHHG (Global Self-consistent, Hierarchical,
  High-resolution Geography) database file. The GSHHG database has been released
  in the public domain and may be downloaded from \cr
  \url{http://www.soest.hawaii.edu/pwessel/gshhg/}.\cr
  At the time of writing, the most recent binary database was the archive
  file called \code{gshhg-bin-2.3.4.zip}.

  The archive contains multiple binary files that contain geographical
  coordinates for shorelines (\code{gshhs}), rivers (\code{wdb_rivers}), and
  borders (\code{wdb_borders}).
  The latter two come from World DataBank II (WDBII): \cr
  \url{http://meta.wikimedia.org/wiki/Geographical_data#CIA_World_DataBank_II_and_derivates} \cr
  The five resolutions available are: \cr
  full (\code{f}), high (\code{h}), intermediate (\code{i}), low (\code{l}), and coarse (\code{c}).

  This routine returns a \code{PolySet} object with an associated
  \code{PolyData} attribute. The attribute contains four fields: (a) \code{PID},
  (b) \code{SID}, (c) \code{Level}, and (d) \code{Source}.
  Each record corresponds to a line/polygon in the \code{PolySet}.  The
  \code{Level} indicates the line's/polygon's level (1=land, 2=lake,
  3=island, 4=pond). The \code{Source} identifies the data source
  (1=WVS, 0=CIA (WDBII)).
}
\value{
  A \code{PolySet} with a \code{PolyData} attribute.
}
\author{
  \href{https://academic.macewan.ca/boersn/}{Nicholas M. Boers}, Associate Professor -- Computer Science\cr
  MacEwan University, Edmonton AB\cr
  Last modified \code{Rd: 2015-02-16}
}
\seealso{
  \code{\link[PBSmapping]{importEvents}}, \code{\link[PBSmapping]{importLocs}}, 
  \code{\link[PBSmapping]{importPolys}},  \code{\link[PBSmapping]{importShapefile}}
}
\note{
  The function calls a C routine, also called \code{importGSHHS}, which returns
  a set of map coordinates that is not always predictably laid out. 
  This issue stems from how the world is divided at the Greenwich meridian and
  at the International Date Line. 
  The unpredictability occurs when user-specified X-limits span either of the 
  longitudinal meridians -- (0\eqn{^\circ}{deg}, 360\eqn{^\circ}{deg}) or 
  (-180\eqn{^\circ}{deg}, 180\eqn{^\circ}{deg}).

  This version of the R function attempts to stitch together the overlapping 
  edges of \code{gshhs} that run from -20\eqn{^\circ}{deg} to 
  360\eqn{^\circ}{deg} (see example map 5 below). 
  At present, no attempt has been made to deal with the overlap 
  at the International Date Line where Russia overlaps the Aleutian Islands of
  Alaska. To some extent, the C-code can deal with this, but not in all cases.
  
  Therefore, the user will likely experience some limitations when using 
  \code{importGSHHS}.
  The solution is to import the whole dataset with this function using
  \code{xlim=c(0,360)}, and then apply the function \code{refocusWorld}
  with user-desired X-limits. 
  The Y-limits are generally not problematic unless the user wants to focus on
  either pole.
}
\examples{
\dontrun{
useWest=FALSE
useVers=c("2.2.0","2.2.3","2.3.0","2.3.4") # GSHHG versions
mapswitch = 5
for (i in c("land","rivers","borders"))
  if (exists(i)) eval(parse(text=paste0("rm(",i,")")))
switch( mapswitch,
 # 1. Canada------------------------------------------------
      {vN=4; useWest=T; xlim=c(-150,-50)+360;ylim=c(40,75)},
 # 2. NW Canada & America-----------------------------------
      {vN=4; useWest=T;xlim=c(-136,-100)+360;ylim=c(40,75)},
 # 3. Black Sea (user Ivailo)-------------------------------
      {vN=4; xlim=c(27.5, 34.3); ylim=c(40.9, 46.7)},
 # 4. W Europe, NW Africa (user Uli)------------------------
      {vN=4; xlim=c(-20,10); ylim=c(20,50)},
 # 5. W Europe + Iceland------------------------------------
      {vN=4; xlim=c(-25, 20); ylim=c(40, 68)},
 # 6. New Zealand-------------------------------------------
      {vN=4; xlim=c(163, 182); ylim=c(-48,-34)},
 # 7. Australia---------------------------------------------
      {vN=4; xlim=c(112,155); ylim=c(-44,-10)},
 # 8. Japan-------------------------------------------------
      {vN=4; xlim=c(127,148); ylim=c(30,47)},
 # 9. Central America---------------------------------------
      {vN=4; useWest=T; xlim=c(-95,-60)+360;ylim=c(-10,25)},
 #10. North Pacific-----------------------------------------
      {vN=4; useWest=T; xlim=c(150,220); ylim=c(45,80)},
 #11. Pacific Ocean-----------------------------------------
      {vN=4; xlim=c(112,240); ylim=c(-48,80)},
 #12. North Atlantic (world coordinates)--------------------
      {vN=4; xlim=c(285,360); ylim=c(40,68)},
 #13. North Atlantic (western hemisphere coordinates)-------
      {vN=4; xlim=c(-75,0); ylim=c(40,68)},
 #14. Atlantic Ocean----------------------------------------
      {vN=4; xlim=c(285,380); ylim=c(-50,68)},
 #15. Northern hemisphere-----------------------------------
      {vN=4; xlim=c(-180,180); ylim=c(0,85)},
 #16. Asia--------------------------------------------------
      {vN=4; xlim=c(0,180); ylim=c(0,80)},
 #17. North America-----------------------------------------
      {vN=4; xlim=c(-180,0); ylim=c(0,80)},
 #18. International date line-------------------------------
      {vN=4; xlim=c(45,315); ylim=c(0,80)},
 #19. Indian Ocean------------------------------------------
      {vN=4; xlim=c(20,130); ylim=c(-40,40)},
 #20. Moose County ("400 miles north of everywhere")--------
      {vN=4; xlim=c(272.5,280.5); ylim=c(43,47.5)}
)
db=paste0("gshhg-bin-",useVers[vN])        # database version folder
gshhg   = paste0("C:/Ruser/GSHHG/",db,"/") # directory with binary files
land    = importGSHHS(paste0(gshhg,"gshhs_i.b"),
          xlim=xlim,ylim=ylim,maxLevel=4,useWest=useWest)
rivers  = importGSHHS(paste0(gshhg,"wdb_rivers_i.b"),
          xlim=xlim,ylim=ylim,useWest=useWest)
borders = importGSHHS(paste0(gshhg,"wdb_borders_i.b"),
          xlim=xlim,ylim=ylim,useWest=useWest,maxLevel=1)
if(exists("land")){
  plotMap(land,xlim=xlim-ifelse(useWest,360,0),ylim=ylim,
    col="lemonchiffon",bg="aliceblue")
  if(!is.null(rivers)) addLines(rivers,col="blue")
  if(!is.null(borders)) addLines(borders,col="red",lwd=2)
}
}
}
\keyword{file}

