\name{pythagoras}
\docType{data}
\alias{pythagoras}
\title{Data: Pythagoras' Theorem Diagram PolySet}
\description{
  \link{PolySet} of shapes to prove Pythagoras' Theorem:
  \eqn{a^2 + b^2 = c^2}{a*a + b*b = c*c}.
}
\usage{data(pythagoras)}
\format{
  4 column data frame: \code{PID} = primary polygon ID,
  \code{POS} = position of each vertex within a given polyline, \code{X}
  = X-coordinate, and \code{Y} = Y-coordinate. Attributes:
  \code{projection = 1}.
}
\note{
  In R, the data must be loaded using the \code{\link{data}} function.
}
\source{
 An artificial construct to illustrate the proof of Pythagoras' Theorem
 using trigonometry.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2022-09-06}
}
\seealso{
  \code{\link{addPolys}},
  \code{\link{plotPolys}},
  \code{\link{plotMap}},
  \link{PolySet}.
}
\keyword{datasets}
