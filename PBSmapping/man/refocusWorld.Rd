\name{refocusWorld}
\alias{refocusWorld}
\title{Refocus the \code{worldLL}/\code{worldLLhigh} Data Sets}
\description{
  Refocus the \code{worldLL}/\code{worldLLhigh} data sets, e.g., refocus
  them so that Eastern Canada appears to the west of Western Europe.
}
\usage{
refocusWorld (polys, xlim=NULL, ylim=NULL, clip.AN=TRUE)
}
\arguments{
  \item{polys}{\code{PolySet} -- object with one or more polygons; typically
    \code{worldLL} or \code{worldLLhigh} (\emph{required}).}
  \item{xlim}{\code{numeric} -- range of X-coordinates.}
  \item{ylim}{\code{numeric} -- range of Y-coordinates.}
  \item{clip.AN}{\code{logical} -- if \code{TRUE}, clip expanded Antarctica to 
    \code{'xlim'} of refocused polygons other than Antarctica and to user-defined/default \code{'ylim'}.}
}
\details{
  This function accepts a \link[PBSmapping]{PolySet} containing one or more polygons
  with X-coordinates that collectively span approximately 360 degrees.
  The function effectively joins the \link[PBSmapping]{PolySet} into a cylinder and
  then splits it at an arbitrary longitude according to the
  user-specified limits.  Modifications in the resulting \link[PBSmapping]{PolySet}
  are restricted to shifting X-coordinates by +/- multiples of 360
  degrees, and instead of clipping polygons, the return value simply
  omits out-of-range polygons.
}
\value{
  \link[PBSmapping]{PolySet}, likely a subset of the input \link[PBSmapping]{PolySet}, which
  retains the same \code{PID}/\code{SID} values.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr

  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2018-10-26}
}
\note{
  The Antarctic polygon is treated as a special case in that it is expanded longitudinally
  by duplicating it to the West and East of the base polygon. The expanded Antarctica is 
  then clipped to the limits of the plot, or not if \code{'clip.NA=FALSE'}.
}
\seealso{
  \code{\link[PBSmapping]{joinPolys}}
}
\examples{
local(envir=.PBSmapEnv,expr={
  oldpar = par(no.readonly=TRUE)
  #--- load appropriate data
  data(worldLL,envir=.PBSmapEnv)
  #--- set limits
  xlim <- c(-100,25)
  ylim <- c(0,90)
  #--- refocus and plot the world
  polys <- refocusWorld(worldLL, xlim, ylim)
  plotMap(polys, xlim, ylim)
  par(oldpar)
})
}
\keyword{manip}

