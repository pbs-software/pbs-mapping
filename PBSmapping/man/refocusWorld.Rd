\name{refocusWorld}
\alias{refocusWorld}
\title{Refocus the \code{worldLL}/\code{worldLLhigh} Data Sets}
\description{
  Refocus the \code{worldLL}/\code{worldLLhigh} data sets, e.g., refocus
  them so that Eastern Canada appears to the west of Western Europe.
}
\usage{
refocusWorld (polys, xlim = NULL, ylim = NULL)
}
\arguments{
  \item{polys}{\link{PolySet} with one or more polygons; typically
    \code{worldLL} or \code{worldLLhigh} (\emph{required}).}
  \item{xlim}{range of X-coordinates.}
  \item{ylim}{range of Y-coordinates.}
}
\details{
  This function accepts a \link{PolySet} containing one or more polygons
  with X-coordinates that collectively span approximately 360 degrees.
  The function effectively joins the \link{PolySet} into a cylinder and
  then splits it at an arbitrary longitude according to the
  user-specified limits.  Modifications in the resulting \link{PolySet}
  are restricted to shifting X-coordinates by +/- multiples of 360
  degrees, and instead of clipping polygons, the return value simply
  omits out-of-range polygons.
}
\value{
  \link{PolySet}, likely a subset of the input \link{PolySet}, which
  retains the same \code{PID}/\code{SID} values.
}
\seealso{
  \code{\link{joinPolys}}
}
\examples{
#--- load appropriate data
data(worldLL)
#--- set limits
xlim <- c(-100,25)
ylim <- c(0,90)
#--- refocus and plot the world
polys <- refocusWorld(worldLL, xlim, ylim)
plotMap(polys, xlim, ylim)
}
\keyword{manip}
