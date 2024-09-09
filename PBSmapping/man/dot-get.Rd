\name{dot-get}
\alias{.getBasename}
\alias{.getGridPars}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Get Stuff
}
\description{
  Routines to get various attributes from \code{PBmapping} objects.
}
\usage{
.getBasename(fn, ext)
.getGridPars(polys, fullValidation=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fn}{\code{character} -- file name (primarily used for shapefiles), with or without extension}
  \item{ext}{\code{character} -- extension text (perhaps only applicable to Windows OS)}
  \item{polys}{\code{numeric} -- \code{PolySet} data frame}
  \item{fullValidation}{\code{logical} -- if \code{TRUE}, apply the full validation routine on the \code{PolySet}}
}
\details{
  Auxiliary dot functions to facilitate the machinations of \code{PBSmapping}.
}
\value{
	\code{.getBasename} : base name of a shapefile set (usually with multiple extensions).\cr
	\code{.getGridPars} : \code{PBSmapping} grid object attributes.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers},
  Software Engineer, Jobber, Edmonton AB

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-09-03}
}
\seealso{
In package \pkg{PBSmapping}:\cr
  \code{importShapefile} (temporarily unavailable),
  \code{\link[PBSmapping]{makeGrid}},
  \code{\link[PBSmapping]{is.PolySet}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{manip}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{dotFuns}
% \concept{ ~cpt2 }
% Use only one concept per line.
