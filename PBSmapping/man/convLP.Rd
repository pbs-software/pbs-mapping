\name{convLP}
\alias{convLP}
\title{Convert Polylines into a Polygon}
\description{
  Convert two polylines into a polygon.
}
\usage{
convLP (polyA, polyB, reverse = TRUE)
}
\arguments{
  \item{polyA}{\link{PolySet} containing a polyline.}
  \item{polyB}{\link{PolySet} containing a polyline.}
  \item{reverse}{Boolean value; if \code{TRUE}, reverse \code{polyB}'s
    vertices.}
}
\details{
  The resulting \link{PolySet} contains all the vertices from
  \code{polyA} in their original order.  If \code{reverse = TRUE}, this
  function appends the vertices from \code{polyB} in the reverse order
  (\code{nrow(polyB):1}).  Otherwise, it appends them in their original
  order.  The \code{PID} column equals the \code{PID} of \code{polyA}.
  No \code{SID} column appears in the result.  The resulting polygon is
  an exterior boundary.
}
\value{
  \link{PolySet} with a single \code{PID} that is the same as
  \code{polyA}. The result contains all the vertices in \code{polyA} and
  \code{polyB}. It has the same \code{projection} and \code{zone}
  attributes as those in the input PolySets.  If an input PolySet's
  attributes equal \code{NULL}, the function uses the other
  PolySet's. If the PolySet attributes conflict, the result's attribute
  equals \code{NULL}.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2013-04-10}
}
\seealso{
  \code{\link{addLines}},
  \code{\link{appendPolys}},
  \code{\link{closePolys}},
  \code{\link{convCP}},
  \code{\link{joinPolys}},
  \code{\link{plotLines}}.
}
\examples{
local(envir=.PBSmapEnv,expr={
  oldpar = par(no.readonly=TRUE)
  #--- create two polylines
  polyline1 <- data.frame(PID=rep(1,2),POS=1:2,X=c(1,4),Y=c(1,4))
  polyline2 <- data.frame(PID=rep(1,2),POS=1:2,X=c(2,5),Y=c(1,4))
  #--- create two plots to demonstrate the effect of `reverse'
  par(mfrow=c(2, 1))
  plotPolys(convLP(polyline1, polyline2, reverse=TRUE), col=2)
  plotPolys(convLP(polyline1, polyline2, reverse=FALSE), col=3)
  par(oldpar)
})
}
\keyword{manip}

