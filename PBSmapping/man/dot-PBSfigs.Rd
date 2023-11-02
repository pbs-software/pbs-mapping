\name{dot-PBSfigs}
\alias{.PBSfigs}
\alias{.PBSfig01}
\alias{.PBSfig02}
\alias{.PBSfig03}
\alias{.PBSfig04}
\alias{.PBSfig05}
\alias{.PBSfig06}
\alias{.PBSfig07}
\alias{.PBSfig08}
\alias{.PBSfig09}
\alias{.PBSfig10}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Run PBSmapping Example Figures
}
\description{
	Run built-in example figures for PBSmapping.
}
\usage{
.PBSfigs(nfigs=1:10, wait=TRUE)
.PBSfig01()
.PBSfig02()
.PBSfig03()
.PBSfig04()
.PBSfig05()
.PBSfig06()
.PBSfig07()
.PBSfig08()
.PBSfig09()
.PBSfig10()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nfigs}{\code{numeric} -- figure number(s) to run}
  \item{wait}{\code{logical} -- if \code{TRUE}, figure pauses until user hits
    <return> for the next figure to display.}
}
\details{
	The wrapper function \code{'.PBSfigs'} displays pre-defined figures
	from 1 to 10, depending on the numbers that the user specifies.
	User can also call each figure directly, e.g., \code{'.PBSfig05()'}.
}
\value{NULL}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Software Engineer, Jobber, Edmonton AB\cr
  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Offsite, Vancouver BC\cr
  Last modified \code{Rd: 2023-10-30}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
.PBSfigs(nfigs=1:3, wait=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{hplot}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
