\name{dot-mat2df}
\alias{.mat2df}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Convert Matrix to Data Frame
}
\description{
  Convert PBS matrices to data frames, preserving certain attributes.
}
\usage{
.mat2df(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{\code{numeric} -- matrix, usually numeric but could be all character
    (mixing data modes usually not allowed in matrices/arrays)}
}
\details{
  Converts a PBS matrix object to a PBS data frame.
  Preserves PBS class objects \code{'EventData'}, \code{'LocationSet'},
  \code{'PolyData'}, \code{'PolySet'}, if any of them exist in the matrix.
}
\value{
  Transformed data object (from matrix to data frame).
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
  \code{\link[PBSmapping]{addPoints}},
  \code{\link[PBSmapping]{.addFeature}},
  \code{\link[PBSmapping]{.validateData}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{manip}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{dotFuns.r}
%\concept{ ~cpt2 }
% Use only one concept per line.
