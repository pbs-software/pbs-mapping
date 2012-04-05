\name{placeHoles}
\alias{placeHoles}
\title{
  Place Holes Under Correct Solids
}
\description{
  Place secondary polygons (\code{SID}s) identified as holes 
  (counter-clockwise rotation) under \code{SID}s identified as 
  solids (clockwise rotation) if the vertices of the holes lie 
  completely within the vertices of the solids. 
  This operation is performed for each primary polygon (\code{PID}).
}
\usage{
placeHoles(polyset, minVerts=3)
}
\arguments{
  \item{polyset}{a valid \pkg{PBSmapping} PolySet.}
  \item{minVerts}{minimum number of vertices required for a polygon 
    representing a hole to be retained (does not affect solids).}
}
\details{
  The algorith identifies the rotation of each polygon down to the 
  \code{SID} level using the \pkg{PBSmapping} function \code{.calcOrientation},
  where output values of \code{1} = solids (clockwise rotation) and 
  \code{-1} = holes (counter-clockwise rotation).
  Then for each solid, the function tests whether each hole 
  occurs within the solid. 
  To facilitate computation, the algorithm assumes that once a 
  hole is located in a solid, it will not occur in any other solid.
  This means that for each successive solid, the number of 
  candidate holes will either decrease or stay the same.
  
  This function makes use of the \code{point.in.polygon} function 
  contained in the package \pkg{sp}. For each hole vertex, the latter
  algorithm returns a numeric value: \cr
  \code{0} = hole vertex is strictly exterior to the solid; \cr
  \code{1} = hole vertex is strictly interior to the solid; \cr
  \code{2} = hole vertex lies on the relative interior of an edge of the solid; \cr
  \code{3} = hole vertex coincides with a solid vertex.
}
\value{
  Returns the input PolySet where \code{SID} holes have been arranged 
  beneath appropriate \code{SID} solids for each \code{PID}.
}
\references{
  See copyright notice in \code{\link[sp]{point.in.polygon}}.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC.
}
\seealso{
  \code{\link[PBSmapping]{importShapefile}}, \code{\link[sp]{point.in.polygon}}
}
\keyword{manip}

