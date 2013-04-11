\name{print}
\alias{print.EventData}
\alias{print.LocationSet}
\alias{print.PolyData}
\alias{print.PolySet}
\alias{print.summary.PBS}
\title{Print PBS Mapping Objects}
\usage{
\method{print}{EventData}(x, \dots)
\method{print}{LocationSet}(x, \dots)
\method{print}{PolyData}(x, \dots)
\method{print}{PolySet}(x, \dots)
\method{print}{summary.PBS}(x, \dots)
}
\arguments{
  \item{x}{a PBS Mapping object of appropriate \code{class}.}
  \item{\dots}{additional arguments to \code{\link{print}}.}
}
\description{
  This function displays information about a PBS Mapping object.

  \code{\link{summary.EventData}}, \code{\link{summary.LocationSet}},
  \code{\link{summary.PolyData}}, and \code{\link{summary.PolySet}}
  produce an object with class \code{summary.PBS}.
}
\examples{
local(envir=.PBSmapEnv,expr={
  #--- load the data (if using R)
  if (!is.null(version$language) && (version$language=="R"))
    data(nepacLL,envir=.PBSmapEnv)
  #--- change to summary printing style
  PBSprint <- TRUE
  #--- print the PolySet
  print(nepacLL)
})
}
\seealso{
  \link{EventData},
  \link{LocationSet},
  \code{\link{PBSprint}},
  \link{PolyData},
  \link{PolySet},
  \code{\link{summary}}.
}
\keyword{IO}

