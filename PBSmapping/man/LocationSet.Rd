\name{LocationSet}
\alias{LocationSet}
\alias{as.LocationSet}
\alias{is.LocationSet}
\title{LocationSet Objects}
\description{
  A LocationSet comprises a data frame that summarises which EventData points
  (\code{EID}) lie in which PolySet polygons (\code{PID}) or 
  (\code{PID}, \code{SID}).
  Events not located in target polygons are not reported. If an event lies on a
  polygon boundary, an additional LocationSet field called \code{Bdry} is set
  to \code{TRUE}. One event can also occur in multiple polygons.

  \pkg{PBSmapping} functions that expect LocationSet's will accept properly
  formatted data frames in their place (see 'Details').
  
  \code{as.LocationSet} attempts to coerce a data frame to an object with
  class LocationSet.

  \code{is.LocationSet} returns \code{TRUE} if its argument is of class
  LocationSet.
}
\usage{
as.LocationSet(x)
is.LocationSet(x, fullValidation = TRUE)
}
\arguments{
  \item{x}{data frame to be coerced or tested.}
  \item{fullValidation}{Boolean value; if \code{TRUE}, fully test
    \code{x}.}
}
\details{
A \link{PolySet} can define regional boundaries for drawing a map, and
\link{EventData} can give event points on the map. Which events occur in
which regions? Our function \code{\link{findPolys}} resolves this
problem. The output lies in a LocationSet, a data frame with three or
four columns (\code{EID}, \code{PID}, \code{SID}, \code{Bdry}), where
\code{SID} may be missing. One row in a LocationSet means that the event
\code{EID} occurs in the polygon (\code{PID}, \code{SID}). The boundary
(\code{Bdry}) field specifies whether (\code{Bdry=T}) or not
(\code{Bdry=F}) the event lies on the polygon boundary. If \code{SID}
refers to an inner polygon boundary, then \code{EID} occurs in
(\code{PID}, \code{SID}) only if \code{Bdry=T}. An event may occur in
multiple polygons. Thus, the same \code{EID} can occur in multiple
records. If an \code{EID} does not fall in any (\code{PID}, \code{SID}),
or if it falls within a hole, it does not occur in the output
LocationSet. Inserting the string \code{"LocationSet"} as the first
element of a LocationSet's \code{class} attribute alters the behaviour
of some functions, including \code{\link{print}} (if
\code{\link{PBSprint}} is \code{TRUE}) and \code{\link{summary}}.
}
\value{
  The \code{as.LocationSet} method returns an object with classes
  \code{"LocationSet"} and \code{"data.frame"}, in that order.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2015-04-23}
}
\seealso{
  \link[PBSmapping]{PolySet},
  \link[PBSmapping]{PolyData},
  \link[PBSmapping]{EventData}
}
\keyword{classes}
\keyword{documentation}

