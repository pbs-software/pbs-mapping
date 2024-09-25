\name{dot-create}
\alias{.createIDs}
\alias{.createFastIDdig}
\alias{.createGridIDs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Create Indices for PolySets
}
\description{
  Create indices for indexing map data structures (PolySets).
}
\usage{
.createIDs(x, cols, fastIDdig=NULL)
.createFastIDdig(polysA, polysB=NULL, cols)
.createGridIDs(d, addSID, byrow)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{\code{data.frame} -- PolySet with one or more ID columns}
  \item{cols}{\code{character} -- field (or column) names (e.g., \code{('PID','SID')})}
  \item{fastIDdig}{\code{numeric} -- maximum number of digits in the second column;
    often the output from \code{'.createFastIDdig'} is passed to \code{'fastIDdig'}
    when creating (some) matching indices for two different data frames}
  \item{polysA}{\code{data.frame} -- first PolySet data frame}
  \item{polysB}{\code{data.frame} -- second PolySet data frame, which may be missing one or both \code{'cols'}}
  \item{d}{\code{data.frame} -- grid Polyset created using \code{addSID=T} and \code{byrow=T}}
  \item{addSID}{\code{logical} -- if \code{TRUE}, include a \code{'SID'} column}
  \item{byrow}{\code{logical} -- if \code{TRUE}, increment \code{'PID'} along \code{'X'}}
}
\details{
  \code{.createIDs} : create IDs (or IDX) column from its input.

  \code{.createFastIDdig} : determine the maximum number of digits in
    the second column of a data frame.
    If given two data frames (\code{'polysA'} and \code{'polysB'}), 
    determine the maximum between the two data frames.

  \code{.createGridIDs} : Create IDs for a grid according to the 
    \code{addSID} and \code{byrow} arguments.
}
\value{
  \code{.createIDs} : a vector of integer or real-number indices.

  \code{.createFastIDdig} : maximum number of digits to use in real-number index.

  \code{.createGridIDs} : a modified grid PolySet.
}
\author{
  Nicholas M. Boers, Software Engineer, Jobber, Edmonton AB

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-09-25}
}
\seealso{
In package \pkg{PBSmapping}:\cr
  \code{\link[PBSmapping]{plotMap}},
  \code{\link[PBSmapping]{addBubbles}},
  \code{\link[PBSmapping]{addPolys}},
  \code{\link[PBSmapping]{addLabels}},
  \code{\link[PBSmapping]{addStipples}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{aplot}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{dotFuns}
% \concept{ ~cpt2 }
% Use only one concept per line.
