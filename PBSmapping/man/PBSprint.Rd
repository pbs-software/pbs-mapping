\name{PBSprint}
\alias{PBSprint}
\title{Specify Whether to Print Summaries}
\description{
  Specify whether PBS Mapping should print object summaries or not. If
  not, data objects are displayed as normal.
}
\usage{
PBSprint
}
\details{
  If \code{PBSprint = TRUE}, the mapping software will print summaries
  rather than the data frames for EventData, LocationSet, PolyData, and
  PolySet objects.  If \code{PBSprint = FALSE}, it will print the data
  frames.

  This variable's default value is \code{FALSE}.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2007-06-06}
}
\seealso{
  \code{\link{summary}}.
}
\value{
  \code{TRUE} or \code{FALSE}, depending on the user's preference.
}
\keyword{sysdata}
