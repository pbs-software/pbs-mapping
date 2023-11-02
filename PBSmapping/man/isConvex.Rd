\name{isConvex}
\alias{isConvex}
\title{Determine Whether Polygons are Convex}
\description{
  Determine whether polygons found in a \link{PolySet} are convex.
}
\usage{
isConvex (polys)
}
\arguments{
  \item{polys}{\link{PolySet} to use.}
}
\details{
  Convex polygons do not self-intersect.  In a convex polygon, only the
  first and last vertices may share the same coordinates (i.e., the
  polygons are optionally closed).

  The function does not give special consideration to holes.  It returns
  a value for each unique (\code{PID}, \code{SID}), regardless of
  whether a contour represents a hole.
}
\value{
  \link{PolyData} with columns \code{PID}, \code{SID} (\emph{may be missing}),
  and \code{convex}.  Column \code{convex} contains Boolean values.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2013-04-10}
}
\seealso{
  \code{\link{isIntersecting}},
  \link{PolySet}.
}
\examples{
local(envir=.PBSmapEnv,expr={
  #--- load the data (if using R)
  if (!is.null(version$language) && (version$language=="R"))
    data(nepacLL,envir=.PBSmapEnv)
  #--- calculate then print the polygons that are convex
  p <- isConvex(nepacLL);
  #--- nepacLL actually contains no convex polygons
  print(p[p$convex,])
})
}
\keyword{manip}

