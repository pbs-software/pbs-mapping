\name{addPoints}
\alias{addPoints}
\title{Add EventData/PolyData to an Existing Plot as Points}
\description{
  Add \link{EventData}/\link{PolyData} to an existing plot, where each
  unique \code{EID} describes a point.
}
\usage{
addPoints (data, xlim = NULL, ylim = NULL, polyProps = NULL,
           cex = NULL, col = NULL, pch = NULL, \dots)
}
\arguments{
  \item{data}{\link{EventData} or \link{PolyData} to add (\emph{required}).}
  \item{xlim}{range of X-coordinates.}
  \item{ylim}{range of Y-coordinates.}
  \item{polyProps}{\link{PolyData} specifying which points to plot and their
    properties.  \code{\link{par}} parameters passed as direct arguments
    supersede these data.}
  \item{cex}{vector describing character expansion factors (cycled by
    \code{EID} or \code{PID}).}
  \item{col}{vector describing colours (cycled by \code{EID} or
    \code{PID}).}
  \item{pch}{vector describing plotting characters (cycled by \code{EID}
    or \code{PID}).}
  \item{\dots}{additional \code{\link{par}} parameters for the
    \code{\link{points}} function.}
}
\details{
  This function clips \code{data} to \code{xlim} and \code{ylim} before
  plotting.  It only adds \link{PolyData} containing \code{X} and
  \code{Y} columns.

  For additional help on the arguments \code{cex}, \code{col}, and
  \code{pch}, please see \code{\link{par}}.
}
\value{
  \link{PolyData} consisting of the \code{PolyProp}s used to create the plot.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2013-04-10}
}
\seealso{
  \code{\link{combineEvents}},
  \code{\link{convDP}},
  \code{\link{findPolys}},
  \code{\link{locateEvents}},
  \code{\link{plotPoints}}.
}
\examples{
local(envir=.PBSmapEnv,expr={
  oldpar = par(no.readonly=TRUE)
  #--- load the data (if using R)
  if (!is.null(version$language) && (version$language=="R"))
    data(nepacLL,surveyData,envir=.PBSmapEnv)
  #--- plot a map
  plotMap(nepacLL, xlim=c(-136, -125), ylim=c(48, 57))
  #--- add events
  addPoints(surveyData, col=1:7)
  par(oldpar)
})
}
\keyword{aplot}

