\name{dot-calc}
\alias{.calcDist}
\alias{.calcOrientation}
\title{
  Calculate Things
}
\description{
  Calculate distance and/or orientation.
}
\usage{
.calcDist(polys)
.calcOrientation(polys)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{polys}{\code{data.frame} -- valid \code{PolySet} with \code{'projection'} attribute}
}
\details{
  \code{.calcdist} : Equatorial radius 6,378.14 km; Polar radius 6,356.78 km; Mean radius 6,371.3 km

  \code{.calcOrientation} : Calls C code \code{'calcOrientation'} using \code{.C()}.
}
\value{
  \code{.calcdist} : distance vector (distances between each point)

  \code{.calcOrientation} : data frame with \code{'orientation'} column
  (-1 when counter-clockwise; 0 when N/A; +1 when clockwise) or
  NULL if no rows in output
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers},
  Software Engineer, Jobber, Edmonton AB

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-09-03}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
In package \pkg{PBSmapping}:\cr
  \code{\link[PBSmapping]{calcLength}},
  \code{\link[PBSmapping]{placeHoles}},
  \code{\link[PBSmapping]{thickenPolys}}
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
