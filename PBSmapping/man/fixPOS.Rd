\name{fixPOS}
\alias{fixPOS}
\title{Fix the POS Column of a PolySet}
\description{
  Fix the \code{POS} column of a \link{PolySet} by recalculating it
  using sequential integers.
}
\usage{
fixPOS (polys, exteriorCCW = NA)
}
\arguments{
  \item{polys}{\link{PolySet} to fix.}
  \item{exteriorCCW}{Boolean value; if \code{TRUE}, orders exterior
    polygon vertices in a counter-clockwise direction.  If \code{FALSE},
    orders them in a clockwise direction.  If \code{NA}, maintains their
    original order.}
}
\details{
  This function recalculates the \code{POS} values of each (\code{PID},
  \code{SID}) as either 1 to N or N to 1, depending on the order of
  \code{POS} (ascending or descending) in the input data.  \code{POS}
  values in the input must be properly ordered (ascending or
  descending), but they may contain fractional values.  For example,
  \code{POS = 2.5} might correspond to a point manually added between
  \code{POS = 2} and \code{POS = 3}.  If \code{exteriorCCW = NA}, all
  other columns remain unchanged.  Otherwise, it orders the \code{X} and
  \code{Y} columns according to \code{exteriorCCW}.
}
\value{
  \link{PolySet} with the same columns as the input, except for possible
  changes to the \code{POS}, \code{X}, and \code{Y} columns.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2013-04-10}
}
\seealso{
  \code{\link{closePolys}},
  \code{\link{fixBound}},
  \code{\link{isConvex}},
  \code{\link{isIntersecting}},
  \link{PolySet}.
}
\examples{
local(envir=.PBSmapEnv,expr={
  #--- create a PolySet with broken POS numbering
  polys <- data.frame(PID = c(rep(1, 10), rep(2, 10)),
    POS = c(seq(2, 10, length = 10), seq(10, 2, length = 10)),
    X = c(rep(1, 10), rep(1, 10)),
    Y = c(rep(1, 10), rep(1, 10)))
  #--- fix the POS numbering
  polys <- fixPOS(polys)
  #--- print the results
  print(polys)
})
}
\keyword{manip}

