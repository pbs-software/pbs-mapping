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
  \item{x}{\code{object} -- a PBS Mapping object of appropriate \code{class}.}
  \item{\dots}{\code{dots} -- additional arguments to \code{\link{print}}.}
}
\description{
  This function displays information about a PBS Mapping object.

  Functions \code{'summary.EventData'}, \code{'summary.LocationSet'},
  \code{'summary.PolyData'}, and \code{'summary.PolySet'}
  produce an object with class \code{'summary.PBS'}.
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
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2019-03-14}
}
\seealso{
In package \pkg{PBSmapping}:\cr
  Data structures:
  \link[PBSmapping]{EventData},
  \link[PBSmapping]{LocationSet},
  \link[PBSmapping]{PolyData},
  \link[PBSmapping]{PolySet}\cr
  Functions:
  \code{\link[PBSmapping]{PBSprint}},
  \code{\link[PBSmapping]{summary}}
}
\keyword{IO}

