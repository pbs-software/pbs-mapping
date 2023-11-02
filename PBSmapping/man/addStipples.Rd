\name{addStipples}
\alias{addStipples}
\title{Add Stipples to an Existing Plot}
\description{
  Add stipples to an existing plot.
}
\usage{
addStipples (polys, xlim=NULL, ylim=NULL, polyProps=NULL,
   side=1, density=1, distance=4, \dots)
}
\arguments{
  \item{polys}{\link{PolySet} that provides the stipple boundaries
    (\emph{required}).}
  \item{xlim}{range of X-coordinates.}
  \item{ylim}{range of Y-coordinates.}
  \item{polyProps}{\link{PolyData} specifying which polygons to stipple
    and their properties.  \code{\link{par}} parameters passed as direct
    arguments supersede these data.}
  \item{side}{one of \code{-1}, \code{0}, or \code{1}, corresponding to
    outside, both sides, or inside, respectively.}
  \item{density}{density of points, relative to the default.}
  \item{distance}{distance to offset points, measured as a percentage of
    the absolute difference in \code{xlim}.}
  \item{\dots}{additional \code{\link{par}} parameters for the
    \code{\link{points}} function.}
}
\details{
  This function locates stipples based on the \link{PolySet}
  \code{polys} and does not stipple degenerate lines.
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
  \code{\link{addPoints}},
  \code{\link{addPolys}},
  \code{\link{plotMap}},
  \code{\link{plotPoints}},
  \code{\link{plotPolys}},
  \code{\link{points}},
  \link{PolySet}.
}
\examples{
local(envir=.PBSmapEnv,expr={
  oldpar = par(no.readonly=TRUE)
  #--- load the data (if using R)
  if (!is.null(version$language) && (version$language=="R"))
    data(nepacLL,envir=.PBSmapEnv)
  #--- plot a map
  plotMap(nepacLL,xlim=c(-128.66,-122.83),ylim=c(48.00,51.16))
  #--- add stippling
  addStipples(nepacLL,col="purple",pch=20,cex=0.25,distance=2)
  par(oldpar)
})
}
\keyword{aplot}

