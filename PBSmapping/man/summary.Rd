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
  \item{object}{\code{object} -- a PBSmapping object: EventData, LocationSet,
    PolyData, or PolySet.}
  \item{\dots}{\code{dots} -- further arguments passed to or from other methods.}
}
\description{
  \code{summary} method for PBS Mapping classes.
}
\details{
  After creating a list of summary statistics, this function assigns the
  class \code{'summary.PBS'} to the output in order to accomplish
  formatted printing via \code{print.summary.PBS}.
}
\value{
  A list of summary statistics.
}
\examples{
local(envir=.PBSmapEnv,expr={
  #--- load the data (if using R)
  if (!is.null(version$language) && (version$language=="R"))
    data(surveyData,envir=.PBSmapEnv)
  print(summary(surveyData))
})
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2019-03-14}
}
\seealso{
In package \pkg{PBSmapping}:\cr
  \link[PBSmapping]{EventData},
  \link[PBSmapping]{LocationSet},
  \link[PBSmapping]{PolyData},
  \link[PBSmapping]{PolySet},
  \code{\link[PBSmapping]{PBSprint}}
}
\keyword{methods}

