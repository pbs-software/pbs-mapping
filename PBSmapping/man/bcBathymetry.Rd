\name{bcBathymetry}
\docType{data}
\alias{bcBathymetry}
\title{Data: Bathymetry Spanning BC Coast}
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

  The functions \code{'graphics::contour'} and \code{'grDevices::contourLines'} expect data in this format.
  Function \code{\link[PBSmapping]{convCP}} converts the output from \code{'grDevices::contourLines'} into a \link[PBSmapping]{PolySet}.
}
\note{
  In R, the data must be loaded using the \code{'utils::data'} function.
}
\source{
  Bathymetry data acquired from the Scripps Institution of Oceanography
  at the University of San Diego.
  
  Using their online form, we requested bathymetry data for the complete
  \code{\link[PBSmapping]{nepacLL}} region.
  At forty megabytes, the data were not suitable for distribution in our mapping package.
  Therefore, we reduced the data to the range \eqn{-140^\circ \le x \le -122^\circ}{-140 <= x <= -122 degrees}
  and \eqn{47^\circ \le y \le 61^\circ}{47 <= y <= 61 degrees}.
}
\references{
  Smith, W.H.F. and Sandwell, D.T. (1997)
  \href{https://www.science.org/doi/10.1126/science.277.5334.1956}{Global seafloor topography from satellite altimetry and ship depth soundings}.
  \emph{Science} \bold{277}, 1957-1962. \cr
  Website: \url{https://topex.ucsd.edu/WWW_html/mar_topo.html}
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Regional Headquarters, Vancouver BC\cr
  Last modified \code{Rd: 2022-07-07}
}
\seealso{
In package \pkg{graphics}:\cr
  \code{\link[graphics]{contour}}\cr
In package \pkg{grDevices}:\cr
  \code{\link[grDevices]{contourLines}}\cr
In package \pkg{PBSmapping}:\cr
  \code{\link[PBSmapping]{convCP}},
  \code{\link[PBSmapping]{nepacLL}},
  \code{\link[PBSmapping]{nepacLLhigh}}
}
\keyword{datasets}
