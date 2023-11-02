\name{EventData}
\alias{EventData}
\alias{as.EventData}
\alias{is.EventData}

\title{EventData Objects}

\description{
  An EventData object comprises a data frame with at least three fields named
  \code{EID}, \code{X}, and \code{Y}; each row specifies an event that occurs
  at a specific point.
  
  \pkg{PBSmapping} functions that expect EventData will accept properly
  formatted data frames in their place (see 'Details').
  
  \code{as.EventData} attempts to coerce a data frame to an object with
  class EventData.

  \code{is.EventData} returns \code{TRUE} if its argument is of class
  EventData.
}
\usage{
as.EventData(x, projection = NULL, zone = NULL)
is.EventData(x, fullValidation = TRUE)
}
\arguments{
  \item{x}{data frame to be coerced or tested.}
  \item{projection}{optional \code{projection} attribute to add to
    EventData, possibly overwriting an existing attribute.}
  \item{zone}{optional \code{zone} attribute to add to EventData,
    possibly overwriting an existing attribute.}
  \item{fullValidation}{Boolean value; if \code{TRUE}, fully test
    \code{x}.}
}
\details{
  Conceptually, an EventData object describes events (EID) that take place at
  specific points (X,Y) in two-dimensional space. Additional fields can specify
  measurements associated with these events. In a fishery context, EventData
  could describe fishing events associated with trawl tows, based on the fields:
  \itemize{
    \item \code{EID} - fishing event (tow) identification number;
    \item \code{X}, \code{Y} - fishing location;
    \item \code{Duration} - length of time for the tow;
    \item \code{Depth} - average depth of the tow;
    \item \code{Catch} - biomass captured.
  }
  Like \link{PolyData}, EventData can have attributes \code{projection}
  and \code{zone}, which may be absent. Inserting the string
  \code{"EventData"} as the class attribute's first element alters the
  behaviour of some functions, including \code{\link{print}} (if
  \code{\link{PBSprint}} is \code{TRUE}) and \code{\link{summary}}.
}
\value{
  The \code{as.EventData} method returns an object with classes
  \code{"EventData"} and \code{"data.frame"}, in that order.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2015-04-23}
}
\seealso{
  \link[PBSmapping]{PolySet},
  \link[PBSmapping]{PolyData}, 
  \link[PBSmapping]{LocationSet}
}
\keyword{classes}
\keyword{documentation}

