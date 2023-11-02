\name{makeGrid}
\alias{makeGrid}
\title{Make a Grid of Polygons}
\description{
  Make a grid of polygons, using \code{PID}s and \code{SID}s according
  to the input arguments.
}
\usage{
makeGrid(x, y, byrow=TRUE, addSID=TRUE, 
   projection=NULL, zone = NULL, type="rectangle")
}
\arguments{
  \item{x}{\code{numeric} -- vector of X-coordinates (of length \eqn{m}).}
  \item{y}{\code{numeric} -- vector of Y-coordinates (of length \eqn{n}).}
  \item{byrow}{\code{logical} -- if \code{TRUE} \emph{and} \code{type='rectangle'}, increment \code{PID} along X (column-wise);\cr
    -- if \code{TRUE} \emph{and} \code{type='hexagon'}, create flat-topped hexagons contiguous by column and increment \code{PID} by column;\cr
    -- if \code{FALSE} \emph{and} \code{type='hexagon'}, create pointy-topped hexagons contiguous by row and increment \code{PID} by row.}
  \item{addSID}{\code{logical} -- if \code{TRUE}, include an \code{SID} field in
    the resulting \link{PolySet}, incremented by the alternative dimension used by \code{PID}.}
  \item{projection}{\code{character} -- optional \code{projection} attribute to add to
    the PolySet.}
  \item{zone}{\code{numeric} -- optional \code{zone} attribute to add to the PolySet.}
  \item{type}{\code{character} -- type of regular tesselation; choices: \code{"rectangle"} or \code{"hexagon"}.}
}
\details{
  This function makes a grid of polygons, labeling them according to
  \code{byrow} and \code{addSID}.

  For rectangular tesselations (grid cells), the variables \eqn{i} and \eqn{j}
  indicate column and row numbers, respectively, where the lower-left cell
  of the grid is (1, 1):
  \itemize{
    \item \code{byrow} \eqn{=} \code{TRUE} and \code{addSID} \eqn{=}
      \code{FALSE} implies \code{PID} \eqn{= i + (j - 1) \times (m -
        1)}{= i + (j - 1) * (m - 1)}
    \item \code{byrow} \eqn{=} \code{FALSE} and \code{addSID} \eqn{=}
      \code{FALSE} implies \code{PID} \eqn{= j + (i - 1) \times (n -
        1)}{= j + (i - 1) * (n - 1)}
    \item \code{byrow} \eqn{=} \code{TRUE} and \code{addSID} \eqn{=}
      \code{TRUE} implies \code{PID} \eqn{= i}, \code{SID} \eqn{= j}
    \item \code{byrow} \eqn{=} \code{FALSE} and \code{addSID} \eqn{=}
      \code{TRUE} implies \code{PID} \eqn{= j}, \code{SID} \eqn{= i}
  }
  For hexagonal tesselations (grid cells), \eqn{i} indicates columns for flat-topped
  hexagons and rows for pointy-topped hexagons. The reverse is true for \eqn{j}.
  Stemming from their six-sided nature, hexagons will adjoin along a long-edge by row when
  their orientation is such that one vertex is higher than all the others.
  Hexagons will adjoin along a long-edge by column when their orientation shows two
  uppermost vertices.
}
\value{
  \link[PBSmapping]{PolySet} with columns \code{PID}, \code{SID}
  (\emph{if} \code{addSID=TRUE}), \code{POS}, \code{X}, and \code{Y}.\cr
  The PolySet is a set of rectangular grid cells when \code{type='rectangle'}, with
  vertices:\cr
  \eqn{(x_{i}, y_{j}), (x_{i+1}, y_{j}), (x_{i+1}, y_{j+1}), (x_{i},
    y_{j+1})}{(x_i, y_j), (x_(i+1), y_j), (x_(i+1), y_(j+1)),
    (x_i, y_(j+1))}.\cr
  The PolySet is a set of hexagonal grid cells when \code{type='hexagon'}.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB

  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2019-01-04}
}
\seealso{
  \code{\link{addPolys}},
  \code{\link{clipPolys}},
  \code{\link{combineEvents}},
  \code{\link{findCells}},
  \code{\link{findPolys}},
  \code{\link{thickenPolys}}.
}
\examples{
local(envir=.PBSmapEnv,expr={
  oldpar = par(no.readonly=TRUE)
  ##--- make a 10 x 10 grid
  polyGrid <- makeGrid(x=0:10, y=0:10)
  ##--- plot the grid
  plotPolys(polyGrid, density=0, projection=1)
  par(oldpar)
})
}
\keyword{manip}

