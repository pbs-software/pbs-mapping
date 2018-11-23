\name{rotatePolys}
\alias{rotatePolys}
\alias{rotateEvents}
\title{
  Rotate Polygons and Events
}
\description{
  Rotate a PolySet (or an EventData set) clockwise by a specified angle around a fixed point.
}
\usage{
rotatePolys(polys, angle=40, centroid=c(500,5700),
   proj.out, zone, xlim=c(-135,-121.5), ylim=c(47,56),
   plot=FALSE, keep.extra=FALSE, ...)

rotateEvents(data, angle=40, centroid=c(500,5700),
   proj.out, zone, plot=FALSE, keep.extra=FALSE, ...)
}
\arguments{
  \item{polys}{\code{PolySet} -- data frame with fields \code{'PID'}, \code{'SID'}, \code{'POS'}, \code{'X'}, \code{'Y'},
    and attribute \code{'projection'} set to either \code{"LL"} or \code{"UTM"}.}
  \item{data}{\code{EventData} -- data frame with fields \code{'EID'}, \code{'POS'}, \code{'X'}, \code{'Y'},
    and attribute \code{'projection'} set to either \code{"LL"} or \code{"UTM"}.}
  \item{angle}{\code{numeric} -- angle between 0 and 360 degrees for map rotation in a clockwise direction.}
  \item{centroid}{\code{numeric} -- fixed UTM point, specified in km, around which the map will be rotated.
    Rotation is performed in UTM space regardless of the input and output projections.
    If user specifies \code{'NULL'} or \code{'NA'}, the centroid of the map will be used.}
  \item{proj.out}{\code{character} -- desired output projection, where choices are \code{'LL'} or \code{'UTM'}.
    If \code{'proj.out'} is not specified, the output projection will be the same as the projection of the input object.}
  \item{zone}{\code{numeric} -- UTM zone used for rotation; if not supplied, the zone is set to 9.}
  \item{xlim}{\code{numeric} -- two-element vector specifying the limits of X using units (degrees or km) that match the input projection.}
  \item{ylim}{\code{numeric} -- two-element vector specifying the limits of Y using units (degrees or km) that match the input projection.}
  \item{plot}{\code{logical} -- if \code{TRUE}, plot the results of the rotation.}
  \item{keep.extra}{\code{logical} -- if \code{TRUE}, keep the coordinates of initial and intermediate steps in the rotation process
    in addition to the final rotated coordinates of the desired projection.}
  \item{\dots}{\code{dots} -- additional arguments for \code{'plotMap'} (in \code{'rotatePolys'}) or \code{'plotPoints'} (in \code{'rotateEvents'}).}
}
\details{
  Map rotation returns coordinates that are no longer meaningful with respect to the original coordinate system.
  When displaying rotated maps, the user might wish to turn off axis labels using \code{xaxt="n"} and \code{yaxt="n"}.
}
\value{
  Rotated PolySet or EventData set where \code{'X'} and \code{'Y'} are the rotated coordinates in the projection specified by \code{'proj.out'}.
  The returned object has an attribute list object named \code{'rotation'} that contains:
  \itemize{
    \item \code{angle} -- angle of clockwise rotation in degrees
    \item \code{radian} -- angle of rotation in radians: \code{pi * (-angle)/180}
    \item \code{centroid} -- fixed point in UTM coordinates (km) around which map is rotated in UTM projection
    \item \code{R} -- \href{https://academo.org/demos/rotation-about-point/}{rotation matrix} (2-dimensional)
    \item \code{xylim} -- list object to keep track of \code{'xlim'}, \code{'ylim'} and a bounding box \code{'xybox'}.
    \item \code{projection} -- projection of the rotated PolySet or EventData set
    \item \code{zone} -- zone of the rotated PolySet or EventData set
  }
  When \code{keep.extra=TRUE}, the returned object will contain additional fields calculated by the rotational algorithm:
  \itemize{
    \item \code{(X0,Y0)} -- original coordinates of the input PolySet | EventData set
    \item \code{(uX0,uY0)} -- original coordinates converted to UTM (only if original projection is \code{'LL'})
    \item \code{(aX,aY)} -- UTM coordinates adjusted by subtracting the UTM centroid
    \item \code{(tX,tY)} -- adjusted UTM coordinates transformed by multiplying the rotational matrix
    \item \code{(rX,rY)} -- rotated UTM coordinates re-centered by adding the UTM centroid
  }
  Note:\cr
  If \code{proj.out="UTM"}, the coordinates \code{c(rX, rY)} are used as the final rotated coordinates.
  If \code{proj.out="LL"}, the coordinates \code{c(rX, rY)} are transformed back into projection \code{'LL'} as the final rotated coordinates.

  Additionally, \code{'xylim'} in the \code{'rotation'} list attribute contains intermediary bounding box objects.
  For instance, if the input PolySet | EventData object has projection \code{'LL'}, the \code{'xylim'} object contains:
  \itemize{
    \item \code{LL} -- original \code{(X,Y)} limits (\code{'xlim'}, \code{'ylim'}, \code{'xybox'})
    \item \code{UTM} -- original \code{(X,Y)} limits transformed to UTM coordinates
    \item \code{rot} -- rotated UTM \code{(X,Y)} limits
    \item \code{out} -- final projection \code{(X,Y)} limits
  }
}
\references{
  \href{https://academo.org/demos/rotation-about-point/}{Academo -- 2D Rotation about a point}

  \href{https://en.wikipedia.org/wiki/Rotation_matrix}{Wikipedia -- Rotation matrix}
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2018-11-23}
}
\note{
  The map rotation algorithm is not heavily tested at this time.
  Report any issues to the package maintainer.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[PBSmapping]{as.PolySet}},
\code{\link[PBSmapping]{clipPolys}},
\code{\link[PBSmapping]{nepacLL}},
\code{\link[PBSmapping]{plotMap}},
\code{\link[PBSmapping]{plotPoints}},
\code{\link[PBSmapping]{refocusWorld}},
\code{\link[PBSmapping]{surveyData}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
