\name{placeHoles}
\alias{placeHoles}
\title{
  Place Holes Under Solids
}
\description{
  Place secondary polygons identified as holes (inner contours)
  under primary polygons identified as solids (outer contours)
  if the vertices of the holes lie completely within the vertices of the solids. 
  This operation is performed for each primary polygon until all holes
  have been assigned.
}
\usage{
placeHoles(polyset, minVerts=3, 
   orient=FALSE, show.progress=FALSE, unique.pid=FALSE)
}
\arguments{
  \item{polyset}{a valid \pkg{PBSmapping} PolySet.}
  \item{minVerts}{\code{numeric} -- minimum number of vertices required for a polygon 
    representing a hole to be retained (does not affect solids).}
  \item{orient}{\code{logical} -- if \code{TRUE}, use each polygon's rotation 
    to determine its nature: clockwise = solid (outer contour), counter-clockwise =
    hole (inner contour).}
  \item{show.progress}{\code{logical} -- if \code{TRUE}, display on the 
    command console the progress of placing holes under solids.}
  \item{unique.pid}{\code{logical} -- if \code{TRUE}, ignore the input PIDs
    and redefine them from 1 to the number of solids; this inherently destroys
    the previous organisation that a creator may have intended for the PolySet.}
}
\details{
  The algorithm identifies outer contours (solids) and inner contours (holes)
  using either the default PBSmapping method (solids = increasing \code{"POS"},
  holes = decreasing \code{"POS"}) or the rotational direction of the polygons
  (solids = clockwise, holes = counter-clockwise).

  It then systematically starts matching holes with solids based on their vertices 
  being completely within the boundaries of the solid.
  If a hole happens to match a current solid completley (all vertices on the boundary),
  then the hole is not matched to this solid because it is a hole in another solid
  that creates space for the current solid.

  To facilitate computation, the algorithm assumes that once a 
  hole is located in a solid, it will not occur in any other solid.
  This means that for each successive solid, the number of 
  candidate holes will either decrease or stay the same.
  
  This function makes use of the PBSmapping hidden function \code{".is.in"}
  which uses the C code \code{"findPolys"}. The latter only returns events found
  in a polygon (or on the boundary) but \code{.is.in} evaluates all events and 
  returns a list containing:\cr
  \code{"e.in"} -- events within the polygon,\cr
  \code{"e.out"} -- events outside the polygon,\cr
  \code{"all.in"} -- logical value of whether all events are in the polygon,\cr
  \code{"all.out"} -- logical value of whether all events are outside the polygon,\cr
  \code{"all.bdry"} -- logical value of whether all events occur on the boundary of the polygon
}
\value{
  Returns the input PolySet where holes have been arranged 
  beneath appropriate solids for each \code{PID} (original or redefined).
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Offsite, Vancouver BC\cr
  Last modified \code{Rd: 2023-10-30}
}
\seealso{
In package \pkg{PBSmapping}:\cr
  \code{\link[PBSmapping]{findPolys}}, 
  \code{\link[PBSmapping]{is.PolySet}},
  \code{\link[PBSmapping]{dot-is.in}}
}
\keyword{manip}

