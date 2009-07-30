\name{bcBathymetry}
\docType{data}
\alias{bcBathymetry}
\title{Data: Bathymetry Spanning British Columbia's Coast}
\description{
  Bathymetry data spanning British Columbia's coast.
}
\usage{data(bcBathymetry)}
\format{
  Three-element list: \code{x} = vector of horizontal grid line
  locations, \code{y} = vector of vertical grid line locations, \code{z}
  = (\code{x} by \code{y}) matrix containing water depths measured in
  meters.  Positive values indicate distance below sea level and
  negative values above it.

  \code{\link{contour}} and \code{\link{contourLines}} expect data in
  this format.  \code{\link{convCP}} converts the output from
  \code{\link{contourLines}} into a \link{PolySet}.
}
\note{
  In R, the data must be loaded using the \code{\link{data}} function.
}
\source{
  Bathymetry data acquired from the Scripps Institution of Oceanography
  at the University of San Diego.
  
  Using their online form, we requested bathymetry data for the complete
  \code{\link{nepacLL}} region.  At forty megabytes, the data were not
  suitable for distribution in our mapping package.  Therefore, we
  reduced the data to the range \eqn{-140^\circ \le x \le -122^\circ}{-140
  <= x <= -122 degrees} and \eqn{47^\circ \le y \le 61^\circ}{47 <= y <=
  61 degrees}.
}
\references{
  Smith, W.H.F. and Sandwell, D.T. (1997) Global seafloor topography from
  satellite altimetry and ship depth soundings. \emph{Science} \bold{277},
  1957--1962. \cr
  \url{http://topex.ucsd.edu/WWW_html/mar_topo.html}
}
\seealso{
  \code{\link{contour}},
  \code{\link{contourLines}},
  \code{\link{convCP}},
  \code{\link{nepacLL}},
  \code{\link{nepacLLhigh}}.
}
\keyword{datasets}
