\name{addLabels}
\alias{addLabels}
\title{Add Labels to an Existing Plot}
\description{
  Add the \code{label} column of \code{data} to the existing plot.
}
\usage{
addLabels (data, xlim = NULL, ylim = NULL, polyProps = NULL,
           placement = "DATA", polys = NULL, rollup = 3,
           cex = NULL, col = NULL, font = NULL, \dots)
}
\arguments{
  \item{data}{\link{EventData} or \link{PolyData} to add (\emph{required}).}
  \item{xlim}{range of X-coordinates.}
  \item{ylim}{range of Y-coordinates.}
  \item{polyProps}{\link{PolyData} specifying which labels to plot and their
    properties.  \code{\link{par}} parameters passed as direct arguments
    supersede these data.}
  \item{placement}{one of \code{"DATA"}, \code{"CENTROID"},
    \code{"MEAN_RANGE"}, or \code{"MEAN_XY"}.}
  \item{polys}{\link{PolySet} to use for calculating label placement.}
  \item{rollup}{level of detail at which to process \code{polys}, and it
    should match that in \code{data}.  \code{1} = \code{PID}s only,
    \code{2} = outer contours only, and \code{3} = no roll-up.}
  \item{cex}{vector describing character expansion factors (cycled by
    \code{EID} or \code{PID}).}
  \item{col}{vector describing colours (cycled by \code{EID} or
    \code{PID}).}
  \item{font}{vector describing fonts (cycled by \code{EID} or
    \code{PID}).}
  \item{\dots}{additional \code{\link{par}} parameters for the
    \code{\link{text}} function.}
}
\details{
  If \code{data} is \link{EventData}, it must minimally contain the columns
  \code{EID}, \code{X}, \code{Y}, and \code{label}.  Since the
  \code{EID} column does not match a column in \code{polys}, set
  \code{placement = "DATA"}.  The function plots each \code{label} at
  its corresponding \code{X}/\code{Y} coordinate.

  If \code{data} is \link{PolyData}, it must minimally contain the columns
  \code{PID} and \code{label}.  If it also contains \code{X} and
  \code{Y} columns, set \code{placement = "DATA"} to plot labels at
  those coordinates.  Otherwise, set \code{placement} to one of
  \code{"CENTROID"}, \code{"MEAN_RANGE"}, or \code{"MEAN_XY"}.  When
  \code{placement != "DATA"}, supply a \link{PolySet} \code{polys}.  Using this
  \link{PolySet}, the function calculates a centroid, mean range, or mean X/Y
  coordinate for each polygon, and then links those \link{PolyData} with
  \code{data} by \code{PID}/\code{SID} to determine label
  coordinates.

  If \code{data} contains both \code{PID} and \code{EID} columns, the
  function assumes it is \link{PolyData} and ignores the \code{EID}
  column.

  For additional help on the arguments \code{cex}, \code{col}, and
  \code{font}, please see \code{\link{par}}.
}
\value{
  \link{EventData} or \link{PolyData} with \code{X} and \code{Y} columns
  that can subsequently reproduce the labels on the plot.  Modify this
  data frame to tweak label positions.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2013-04-10}
}
\seealso{
  \code{\link{addPoints}},
  \code{\link{calcCentroid}},
  \code{\link{calcMidRange}},
  \code{\link{calcSummary}},
  \link{EventData},
  \code{\link{plotPoints}},
  \link{PolyData}.
}
\examples{
local(envir=.PBSmapEnv,expr={
  oldpar = par(no.readonly=TRUE)
  #--- create sample PolyData to label Vancouver Island
  labelData <- data.frame(PID=33, label="Vancouver Island");
  #--- load data
  if (!is.null(version$language) && (version$language == "R"))
    data(nepacLL,envir=.PBSmapEnv)
  #--- plot the map
  plotMap(nepacLL,xlim=c(-129,-122.6),ylim=c(48,51.1),col="lemonchiffon")
  #--- add the labels
  addLabels(labelData,placement="CENTROID",polys=nepacLL,cex=1.2,col=2,font=2)
  par(oldpar)
})
}
\keyword{aplot}

