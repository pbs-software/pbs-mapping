\name{appendPolys}
\alias{appendPolys}
\title{Append a Two-Column Matrix to a PolySet}
\description{
  Append a two-column matrix to a \link{PolySet}, assigning \code{PID} and
  possibly \code{SID} values automatically or as specified in its
  arguments.
}
\usage{
appendPolys (polys, mat, PID = NULL, SID = NULL, isHole = FALSE)
}
\arguments{
  \item{polys}{existing \link{PolySet}; if \code{NULL}, creates a new
    \link{PolySet} (\emph{required}).}
  \item{mat}{two-column matrix to append (\emph{required}).}
  \item{PID}{new polygon's \code{PID}.}
  \item{SID}{new polygon's \code{SID}.}
  \item{isHole}{Boolean value; if \code{TRUE}, \code{mat} represents a
    hole.}
}
\details{
  If the \code{PID} argument is \code{NULL}, the appended polygon's
  \code{PID} will be one greater than the maximum within \code{polys}
  (if defined); otherwise, it will be 1.
  
  If \code{polys} contains an \code{SID} column and the \code{SID}
  argument equals \code{NULL}, this function uses the next available
  \code{SID} for the corresponding \code{PID}.

  If \code{polys} does not contain an \code{SID} column and the
  caller passes an \code{SID} argument, all existing polygons will
  receive an \code{SID} of 1.  The new polygon's \code{SID} will
  match the \code{SID} argument.
  
  If \code{isHole = TRUE}, the polygon's \code{POS} values will
  appropriately represent a hole (reverse order of POS).

  If (\code{PID}, \code{SID}) already exists in the \link{PolySet}, the
  function will issue a warning and duplicate those identifiers.
}
\value{
  \link{PolySet} containing \code{mat} appended to \code{polys}.  The
  function retains attributes from \code{polys}.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2013-04-10}
}
\seealso{
  \code{\link{addPolys}},
  \code{\link{clipPolys}},
  \code{\link{closePolys}},
  \code{\link{convLP}},
  \code{\link{fixBound}},
  \code{\link{fixPOS}},
  \code{\link{joinPolys}},
  \code{\link{plotMap}},
  \code{\link{plotPolys}}.
}
\examples{
local(envir=.PBSmapEnv,expr={
  #--- create two simple matrices
  a <- matrix(data=c(0,0,1,0,1,1,0,1),ncol=2,byrow=TRUE);
  b <- matrix(data=c(2,2,3,2,3,3,2,3), ncol=2,byrow=TRUE);
  #--- build a PolySet from them
  polys <- appendPolys(NULL, a);
  polys <- appendPolys(polys, b);
  #--- print the result
  print (polys);
})
}
\keyword{manip}
