\name{PolySet}
\alias{PolySet}
\alias{as.PolySet}
\alias{is.PolySet}
\title{PolySet Objects}
\description{
  A PolySet object comprises a data frame that defines a collection of
  polygonal contours (i.e., line segments joined at vertices). These contours
  can be open-ended (polylines) or closed (polygons).

  \pkg{PBSmapping} functions that expect PolySet's will accept properly
  formatted data frames in their place (see 'Details').
  
  \code{as.PolySet} attempts to coerce a data frame to an object with
  class PolySet.

  \code{is.PolySet} returns \code{TRUE} if its argument is of class
  PolySet.
}
\usage{
as.PolySet(x, projection = NULL, zone = NULL)
is.PolySet(x, fullValidation = TRUE)
}
\arguments{
  \item{x}{data frame to be coerced or tested.}
  \item{projection}{optional \code{projection} attribute to add to
    the PolySet, possibly overwriting an existing attribute.}
  \item{zone}{optional \code{zone} attribute to add to the PolySet,
    possibly overwriting an existing attribute.}
  \item{fullValidation}{Boolean value; if \code{TRUE}, fully test
    \code{x}.}
}
\details{
In our software, a PolySet data frame defines a collection of polygonal
contours (i.e., line segments joined at vertices), based on four or five
numerical fields:
\itemize{
  \item \code{PID} - the primary identification number for a contour;
  \item \code{SID} - optional, the secondary identification number for a
  contour;
  \item \code{POS} - the position number associated with a vertex;
  \item \code{X} - the horizontal coordinate at a vertex;
  \item \code{Y} - the vertical coordinate at a vertex.
}
The simplest PolySet lacks an \code{SID} column, and each \code{PID}
corresponds to a different contour. By analogy with a child's
\dQuote{follow the dots} game, the \code{POS} field enumerates the
vertices to be connected by straight lines. Coordinates (\code{X},
\code{Y}) specify the location of each vertex. Thus, in familiar
mathematical notation, a contour consists of \eqn{n} points 
(\eqn{x_{i}, y_{i}}{x_i, y_i}) with \eqn{i = 1, ..., n}, where \eqn{i} corresponds to the
\code{POS} index. A PolySet has two potential interpretations. The first
associates a line segment with each successive pair of points from 1 to
\eqn{n}, giving a \emph{polyline} (in GIS terminology) composed of the
sequential segments. The second includes a final line segment joining
points \eqn{n} and 1, thus giving a \emph{polygon}.

The secondary ID field allows us to define regions as composites of
polygons. From this point of view, each primary ID identifies a
collection of polygons distinguished by secondary IDs. For example, a
single management area (\code{PID}) might consist of two fishing areas,
each defined by a unique \code{SID}. A secondary polygon can also
correspond to an inner boundary, like the hole in a doughnut. We adopt
the convention that \code{POS} goes from 1 to \eqn{n} along an outer
boundary, but from \eqn{n} to 1 along an inner boundary, regardless of
rotational direction. This contrasts with other GIS software, such as
ArcView (ESRI 1996), in which outer and inner boundaries correspond to
clockwise and counter-clockwise directions, respectively.

The SID field in a PolySet with secondary IDs must have integer values
that appear in ascending order for a given \code{PID}. Furthermore,
inner boundaries must follow the outer boundary that encloses them. The
\code{POS} field for each contour (\code{PID}, \code{SID}) must
similarly appear as integers in strictly increasing or decreasing order,
for outer and inner boundaries respectively. If the \code{POS} field
erroneously contains floating-point numbers, \code{\link{fixPOS}} can
renumber them as sequential integers, thus simplifying the insertion of
a new point, such as point 3.5 between points 3 and 4.
 
A PolySet can have a \code{projection} attribute, which may be missing,
that specifies a map projection. In the current version of PBS Mapping,
projection can have character values \code{"LL"} or \code{"UTM"},
referring to \dQuote{Longitude-Latitude} and \dQuote{Universal
Transverse Mercator}. We explain these projections more completely
below. If projection is numeric, it specifies the aspect ratio \eqn{r},
the number of \eqn{x} units per \eqn{y} unit. Thus, \eqn{r} units of
\eqn{x} on the graph occupy the same distance as one unit of
\eqn{y}. Another optional attribute \code{zone} specifies the UTM zone
(if \code{projection="UTM"}) or the preferred zone for conversion from
Longitude-Latitude (if \code{projection="LL"}).

A data frame's class attribute by default contains the string
\code{"data.frame"}. Inserting the string \code{"PolySet"} as the class
vector's first element alters the behaviour of some functions. For
example, the \code{\link{summary}} function will print details specific
to a PolySet. Also, when \code{\link{PBSprint}} is \code{TRUE}, the
print function will display a PolySet's summary rather than the contents
of the data frame.
}
\value{
  The \code{as.PolySet} method returns an object with classes
  \code{"PolySet"} and \code{"data.frame"}, in that order.
}
\references{
  Environmental Systems Research Institute (ESRI). (1996) \emph{ArcView GIS:
  The Geographic Information System for Everyone}. ESRI Press, Redlands,
  California. 340 pp.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2022-09-06}
}
\seealso{
  \link[PBSmapping]{PolyData},
  \link[PBSmapping]{EventData},
  \link[PBSmapping]{LocationSet}
}
\keyword{classes}
\keyword{documentation}

