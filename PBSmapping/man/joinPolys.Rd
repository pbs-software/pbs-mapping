\name{joinPolys}
\alias{joinPolys}
\title{Join One or Two PolySets using a Logic Operation}
\description{
  Join one or two \link{PolySet}s using a logic operation.
}
\usage{
joinPolys(polysA,polysB=NULL,operation="INT",maxVert=1e+05)
}
\arguments{
  \item{polysA}{\link{PolySet} to join.}
  \item{polysB}{optional second \link{PolySet} with which to join.}
  \item{operation}{one of \code{"DIFF"}, \code{"INT"}, \code{"UNION"},
    or \code{"XOR"}, representing difference, intersection, union, and
    exclusive-or, respectively.}
  \item{maxVert}{estimated maximum number of vertices in the output
    \link{PolySet}.}
}
\details{
  This function interfaces with the General Polygon Clipper library
  (\url{http://www.cs.man.ac.uk/aig/staff/alan/software/}) produced by
  Alan Murta at the University of Manchester.  Consequently, we adopt
  some of his terminology in the details below.
  
  Murta (2004) defines a \emph{generic polygon} (or \emph{polygon set})
  as zero or more disjoint boundaries of arbitrary configuration.  He
  relates a \emph{boundary} to a contour, where each may be convex,
  concave or self-intersecting. In a PolySet, the polygons associated
  with each unique \code{PID} loosely correspond to a generic polygon,
  as they can represent both inner and outer boundaries.  Our use of the
  term \emph{generic polygon} includes the restrictions imposed by a
  PolySet.  For example, the polygons for a given \code{PID} cannot
  be arranged arbitrarily.
  
  If \code{polysB} is \code{NULL}, this function sequentially applies
  the \code{operation} between the generic polygons in \code{polysA}.
  For example, suppose \code{polysA} contains three generic polygons (A,
  B, C).  The function outputs the \link{PolySet} containing ((A op B)
  op C).
  
  If \code{polysB} is not \code{NULL}, this function applies
  \code{operation} between each generic polygon in \code{polysA} and
  each one in \code{polysB}.  For example, suppose \code{polysA}
  contains two generic polygons (A, B) and \code{polysB} contains two
  generic polygons (C, D).  The function's output is the concatenation
  of A \emph{op} C, B \emph{op} C, A \emph{op} D, B \emph{op} D, with
  \code{PID}s 1 to 4, respectively.  Generally there are \emph{n}
  times \emph{m} comparisons, where \emph{n} = number of polygons in
  \code{polysA} and \emph{m} = number of polygons in \code{polysB}.  If
  \code{polysB} contains only one generic polygon, the function
  maintains the \code{PID}s from \code{polysA}.  It also maintains them
  when \code{polysA} contains only one generic polygon and the
  \code{operation} is difference.  Otherwise, if \code{polysA} contains
  only one generic polygon, it maintains the \code{PID}s from
  \code{polysB}.
}
\value{
  If \code{polysB} is \code{NULL}, the resulting \link{PolySet} contains
  a single generic polygon (one \code{PID}), possibly with several
  components (\code{SID}s).  The function recalculates the \code{PID}
  and \code{SID} columns.
  
  If \code{polysB} is not \code{NULL}, the resulting \link{PolySet}
  contains one or more generic polygons (\code{PID}s), each with
  possibly several components (\code{SID}s).  The function recalculates
  the \code{SID} column, and depending on the input, it may recalculate
  the \code{PID} column.
}
\seealso{
  \code{\link{addPolys}},
  \code{\link{appendPolys}},
  \code{\link{clipPolys}},
  \code{\link{closePolys}},
  \code{\link{fixBound}},
  \code{\link{fixPOS}},
  \code{\link{locatePolys}},
  \code{\link{plotMap}},
  \code{\link{plotPoints}},
  \code{\link{thickenPolys}},
  \code{\link{thinPolys}}.
}
\references{
  Murta, A. (2004) \emph{A General Polygon Clipping Library}.
  Accessed: July 29, 2004.\cr
  \url{http://www.cs.man.ac.uk/aig/staff/alan/software/gpc.html}
}
\examples{
#--- load the data (if using R)
if (!is.null(version$language) && (version$language == "R"))
  data(nepacLL)
#--- create a triangle to use in clipping
polysB <- data.frame(PID=rep(1, 3), POS=1:3,
  X=c(-127.5, -124.5, -125.6), Y = c(49.2, 50.3, 48.6))
#--- intersect nepacLL with the single polygon, and plot the result
plotMap(joinPolys(nepacLL, polysB), col=5)
#--- add nepacLL in a different line type to emphasize the intersection
addPolys(nepacLL, border=2, lty=8, density=0)
}
\keyword{manip} % multiple keywords allowed, one per line (this is correct)
\keyword{logic}
