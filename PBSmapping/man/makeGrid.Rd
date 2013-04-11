\name{makeGrid}
\alias{makeGrid}
\title{Make a Grid of Polygons}
\description{
  Make a grid of polygons, using \code{PID}s and \code{SID}s according
  to the input arguments.
}
\usage{
makeGrid(x,y,byrow=TRUE,addSID=TRUE,projection=NULL,zone=NULL)
}
\arguments{
  \item{x}{vector of X-coordinates (of length \eqn{m}).}
  \item{y}{vector of Y-coordinates (of length \eqn{n}).}
  \item{byrow}{Boolean value; if \code{TRUE}, increment \code{PID} along X.}
  \item{addSID}{Boolean value; if \code{TRUE}, include an \code{SID} column in
    the resulting \link{PolySet}.}
  \item{projection}{optional \code{projection} attribute to add to
    the PolySet.}
  \item{zone}{optional \code{zone} attribute to add to the PolySet.}
}
\details{
  This function makes a grid of polygons, labeling them according to
  \code{byrow} and \code{addSID}.  In the following description, the
  variables \eqn{i} and \eqn{j} indicate column and row numbers,
  respectively, where the lower-left cell of the grid is (1, 1).
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
}
\value{
  \link{PolySet} with columns \code{PID}, \code{SID}
  (\emph{if} \code{addSID = TRUE}), \code{POS}, \code{X}, and \code{Y}.
  The \link{PolySet} is a set of rectangular grid cells with
  vertices:\cr
  \eqn{(x_{i}, y_{j}), (x_{i+1}, y_{j}), (x_{i+1}, y_{j+1}), (x_{i},
    y_{j+1})}{(x_i, y_j), (x_(i+1), y_j), (x_(i+1), y_(j+1)),
    (x_i, y_(j+1))}.
}
\seealso{
  \code{\link{addPolys}},
  \code{\link{clipPolys}},
  \code{\link{combineEvents}},
  \code{\link{findCells}},
  \code{\link{findPolys}},
  \link{PolySet},
  \code{\link{thickenPolys}}.
}
\examples{
local(envir=.PBSmapEnv,expr={
  oldpar = par(no.readonly=TRUE)
  #--- make a 10 x 10 grid
  polyGrid <- makeGrid(x=0:10, y=0:10)
  #--- plot the grid
  plotPolys(polyGrid, density=0, projection=1)
  par(oldpar)
})
}
\keyword{manip}

