\name{dot-fix}
\alias{.fixGSHHSWorld}
\title{
  Fix Things
}
\description{
  Fix things like GSHHSWorld.
}
\usage{
.fixGSHHSWorld(world)
}
\arguments{
  \item{world}{\code{data.frame} -- \code{PolySet} of polygons for the global shorelines}
}
\details{
  Determine PID of Antarctica, which is used to extract the current Antarctica.
  The continent is rebuilt from left to right, creating a new, very wide Antarctica.
  It is then clipped and merged into the existing world (with other polygons).
}
\value{
  A revised \code{PolySet} of the world with a peeled version of Antarctica.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers},
  Software Engineer, Jobber, Edmonton AB

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-09-03}
}
\note{
  This function is not called by any PBSmapping functions.
  It can be used after \code{'importGSHHS'} uses \code{ylim=c(-90,90)}.
}
\seealso{
In package \pkg{PBSmapping}:\cr
  \code{\link[PBSmapping]{importGSHHS}},
  \code{\link[PBSmapping]{clipPolys}}
}
\examples{
\dontrun{
worldLL =
importGSHHS("gshhs_l.b",xlim=c(-20,360),ylim=c(-90,90),level=1,n=15,xoff=0)
worldLL = .fixGSHHSWorld(worldLL)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{manip}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{dotFuns.r}
% \concept{ ~cpt2 }
% Use only one concept per line.
