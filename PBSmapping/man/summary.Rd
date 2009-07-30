\name{summary}
\alias{summary.EventData}
\alias{summary.LocationSet}
\alias{summary.PolyData}
\alias{summary.PolySet}
\title{Summarize PBS Mapping Objects}
\usage{
\method{summary}{EventData}(object, \dots)
\method{summary}{LocationSet}(object, \dots)
\method{summary}{PolyData}(object, \dots)
\method{summary}{PolySet}(object, \dots)
}
\arguments{
  \item{object}{a PBS Mapping object, such as EventData, a LocationSet,
    PolyData, or a PolySet.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\description{
  \code{summary} method for PBS Mapping classes.
}
\details{
  After creating a list of summary statistics, this function assigns the
  class \code{"summary.PBS"} to the output in order to accomplish
  formatted printing via \code{\link{print.summary.PBS}}.
}
\value{
  A list of summary statistics.
}
\examples{
#--- load the data (if using R)
if (!is.null(version$language) && (version$language == "R"))
  data(surveyData)
print(summary(surveyData))
}
\seealso{
  \link{EventData},
  \link{LocationSet},
  \code{\link{PBSprint}},
  \link{PolyData},
  \link{PolySet}.
}
\keyword{methods}
