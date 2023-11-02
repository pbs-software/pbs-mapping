\name{locateEvents}
\alias{locateEvents}
\title{Locate Events on the Current Plot}
\description{
  Locate events on the current plot (using the \code{\link{locator}}
  function).
}
\usage{
locateEvents (EID, n = 512, type = "p", \dots)
}
\arguments{
  \item{EID}{vector of event IDs (\emph{optional}).}
  \item{n}{maximum number of events to locate.}
  \item{type}{one of \code{"n"}, \code{"p"}, \code{"l"}, or
    \code{"o"}. If \code{"p"} or \code{"o"}, then the points are
    plotted; if \code{"l"} or \code{"o"}, then the points are joined by
    lines.}
  \item{\dots}{additional \code{\link{par}} parameters for the
    \code{\link{locator}} function.}
}
\details{
  This function allows its user to define events with mouse clicks on
  the current plot via the \code{\link{locator}} function.  The
  arguments \code{n} and \code{type} are the usual parameters of the
  \code{\link{locator}} function.  If \code{EID} is not missing, then
  \code{n = length(EID)}.

  On exit from \code{\link{locator}}, suppose the user defined \emph{m}
  events.  If \code{EID} was missing, then the output data frame will
  contain \emph{m} events. However, if \code{EID} exists, then the
  output data frame will contain \code{length(EID)} events, and both
  \code{X} and \code{Y} will be \code{NA} for events
  \code{EID[(}\emph{m}\code{+1):n]}.  The \code{\link{na.omit}} function
  can remove rows with \code{NA}s.
}
\value{
  \link{EventData} with columns \code{EID}, \code{X}, and \code{Y}, and
  \code{projection} attribute equal to the map's projection.  The
  function does not set the \code{zone} attribute.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2007-06-06}
}
\seealso{
  \code{\link{addPoints}},
  \code{\link{combineEvents}},
  \code{\link{convDP}},
  \link{EventData},
  \code{\link{findCells}},
  \code{\link{findPolys}},
  \code{\link{plotPoints}}.
}
\examples{
#--- define five events on the current plot, numbering them 10 to 14
\dontrun{events <- locateEvents(EID = 10:14)}
}
\keyword{iplot}
