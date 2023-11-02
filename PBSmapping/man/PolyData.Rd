\name{PolyData}
\alias{PolyData}
\alias{as.PolyData}
\alias{is.PolyData}
\title{PolyData Objects}
\description{
  A PolyData object comprises a data frame that summarises information
  for each polyline/polygon in a PolySet; each PolyData record is defined by
  a unique \code{PID} or (\code{PID}, \code{SID} combination).

  \pkg{PBSmapping} functions that expect PolyData will accept properly
  formatted data frames in their place (see 'Details').

  \code{as.PolyData} attempts to coerce a data frame to an object with
  class PolyData.

  \code{is.PolyData} returns \code{TRUE} if its argument is of class
  PolyData.
}
\usage{
as.PolyData(x, projection = NULL, zone = NULL)
is.PolyData(x, fullValidation = TRUE)
}
\arguments{
  \item{x}{data frame to be coerced or tested.}
  \item{projection}{optional \code{projection} attribute to add to
    PolyData, possibly overwriting an existing attribute.}
  \item{zone}{optional \code{zone} attribute to add to PolyData,
    possibly overwriting an existing attribute.}
  \item{fullValidation}{Boolean value; if \code{TRUE}, fully test
    \code{x}.}
}
\details{
We define PolyData as a data frame with a first column named \code{PID}
and (optionally) a second column named \code{SID}. Unlike a
\link{PolySet}, where each contour has many records corresponding
to the vertices, a PolyData object must have only one record for each
\code{PID} or each (\code{PID}, \code{SID}) combination. Conceptually,
this object associates data with contours, where the data correspond to
additional fields in the data frame. The R language conveniently
allows data frames to contain fields of various atomic modes
(\code{"logical"}, \code{"numeric"}, \code{"complex"},
\code{"character"}, and \code{"null"}). For example, PolyData with the
fields (\code{PID}, \code{PName}) might assign character names to a set
of primary polygons. Additionally, if fields \code{X} and \code{Y} exist
(perhaps representing locations for placing labels), consider adding
attributes \code{zone} and \code{projection}. Inserting the string
\code{"PolyData"} as the class attribute's first element alters the
behaviour of some functions, including \code{\link{print}} (if
\code{\link{PBSprint}} is \code{TRUE}) and \code{\link{summary}}.

Our software particularly uses PolyData to set various plotting
characteristics. Consistent with graphical parameters used by the R/S
functions \code{\link{lines}} and \code{\link{polygon}}, column names
can specify graphical properties:
\itemize{
  \item \code{lty} - line type in drawing the border and/or shading
  lines;
  \item \code{col} - line or fill colour;
  \item \code{border} - border colour;
  \item \code{density} - density of shading lines;
  \item \code{angle} - angle of shading lines.
}
When drawing polylines (as opposed to closed polygons), only \code{lty}
and \code{col} have meaning.
}
\value{
  The \code{as.PolyData} method returns an object with classes
  \code{"PolyData"} and \code{"data.frame"}, in that order.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2015-04-23}
}
\seealso{
  \link[PBSmapping]{PolySet},
  \link[PBSmapping]{EventData},
  \link[PBSmapping]{LocationSet}
}
\keyword{classes}
\keyword{documentation}

