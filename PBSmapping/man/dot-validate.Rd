\name{dot-validate}
\alias{.validateData}
\alias{.validateEventData}
\alias{.validateLocationSet}
\alias{.validatePolyData}
\alias{.validatePolyProps}
\alias{.validatePolySet}
\alias{.validateXYData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Validate PBSmapping Data Structures
}
\description{
  Routines to validate data structures (e.g., required field names, attributes)
  that are passed to package functions.
}
\usage{
.validateData(data, className, requiredCols=NULL, requiredAttr=NULL, 
   noFactorCols=NULL, noNACols=NULL, keyCols=NULL, numericCols=NULL)
.validateEventData(EventData)
.validateLocationSet(LocationSet)
.validatePolyData(PolyData)
.validatePolyProps(polyProps, parCols=NULL)
.validatePolySet(polys)
.validateXYData(xyData)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{\code{data.frame|matrix} -- data object for use in \pkg{PBSmapping} functions}
  \item{className}{\code{character} -- class name of \code{data}, usually \code{'data.frame'}}
  \item{requiredCols}{\code{character} -- vector of required field names in \code{data}}
  \item{requiredAttr}{\code{character} -- names of attributes to check}
  \item{noFactorCols}{\code{character} -- vector of field names that cannot be factors}
  \item{noNACols}{\code{character} -- vector of field names that cannot comtain \code{NA} values}
  \item{keyCols}{\code{character} -- vector of field names that must contain unique values}
  \item{numericCols}{\code{character} -- vector of field names that must be numeric}
  \item{EventData}{\code{data.frame} -- data frame with at least three fields named
    (\code{'EID'}, \code{'X'}, \code{'Y'})}
  \item{LocationSet}{\code{data.frame} -- data frame with three or four columns 
    (\code{'EID'}, \code{'PID'}, \code{'SID'}, \code{'Bdry'}), where \code{'SID'} may be missing}
  \item{PolyData}{\code{data.frame} -- data frame with a first column named \code{'PID'} and 
    (optionally) a second column named \code{'SID'}}
  \item{polyProps}{\code{data.frame} -- support \code{PolyData} object,
    usually containing fields that facilitate plotting,
    either derived from a \code{PolySet} summary or given as an explicit \code{PolyData} object}
  \item{parCols}{\code{character} -- vector of field names that cannot be factors}
  \item{polys}{\code{data.frame} -- a collection of polygonal contours (\code{PolySet})
     based on four or five numerical fields:\cr
    \code{'PID'} the primary identification number for a contour\cr
    \code{'SID'} (optional) the secondary identification number for a contour\cr
    \code{'POS'} the position number associated with a vertex\cr
    \code{'X'} the horizontal coordinate at a vertex\cr
    \code{'Y'} the vertical coordinate at a vertex}
  \item{xyData}{\code{data.frame} -- a collection of events that contain fields \code{'X'} and \code{'Y'}}
}
\details{
  Internal functions check the validity of data objects used by \pkg{PBSmapping}.
  The primary function is \code{'.validateData'}; other dot \code{validate} functions
  are wrappers for the four main data structures:
  \code{'PolySet'}, \code{'PolyData'}, \code{'EventData'}, and \code{'LocationSet'}.
}
\value{
	The data object that is validated.
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
  \code{\link[PBSmapping]{PolySet}},
  \code{\link[PBSmapping]{PolyData}},
  \code{\link[PBSmapping]{EventData}},
  \code{\link[PBSmapping]{LocationSet}}
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
