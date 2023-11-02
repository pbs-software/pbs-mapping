\name{joinPolys}
\alias{joinPolys}
\title{Join One or Two PolySets using a Logic Operation}
\description{
  Join one or two PolySets using a logic operation.
}
\usage{
joinPolys(polysA, polysB=NULL, operation="INT")
}
\arguments{
  \item{polysA}{\link[PBSmapping]{PolySet} to join.}
  \item{polysB}{optional second \link[PBSmapping]{PolySet} with which to join.}
  \item{operation}{one of \code{"DIFF"}, \code{"INT"}, \code{"UNION"},
    or \code{"XOR"}, representing difference, intersection, union, and
    exclusive-or, respectively.}
}
\details{
  This function interfaces with the \href{https://www.angusj.com/delphi/clipper.php}{Clipper library},
  specifically version 6.2.1 released 2014-10-31, developed by Angus Johnson.
  Prior to 2013-03-23, \code{'joinPolys'} used the General Polygon Clipper library
  by Alan Murta at the University of Manchester.
  We keep this historic reference to GPC because \code{'joinPolys'} remains faithful
  to Murta's definition of a generic polygon, which we describe below.
  
  Murta (2004) defines a \emph{generic polygon} (or \emph{polygon set})
  as zero or more disjoint boundaries of arbitrary configuration.  He
  relates a \emph{boundary} to a contour, where each may be convex,
  concave or self-intersecting. In a PolySet, the polygons associated
  with each unique \code{PID} loosely correspond to a generic polygon,
  as they can represent both inner and outer boundaries.  Our use of the
  term \emph{generic polygon} includes the restrictions imposed by a
  PolySet.  For example, the polygons for a given \code{PID} cannot
  be arranged arbitrarily.
  
  If \code{'polysB'} is \code{NULL}, this function sequentially applies
  the logic \code{'operation'} between the generic polygons in \code{'polysA'}.
  For example, suppose \code{'polysA'} contains three generic polygons (A, B, C).
  The function outputs the PolySet containing ((A \emph{op} B) \emph{op} C).
  
  If \code{'polysB'} is not \code{NULL}, this function applies the logic \code{'operation'}
  between each generic polygon in \code{'polysA'} and each one in \code{'polysB'}.
  For example, suppose \code{'polysA'} contains two generic polygons \code{(A, B)}
  and \code{'polysB'} contains two generic polygons \code{(C, D)}.
  The function's output is the concatenation of
  A \emph{op} C, B \emph{op} C, A \emph{op} D, B \emph{op} D, with \code{PID}s 1 to 4, respectively.
  Generally there are \emph{n} times \emph{m} comparisons, where \emph{n} = number of polygons
  in \code{'polysA'} and \emph{m} = number of polygons in \code{'polysB'}.
  If \code{'polysB'} contains only one generic polygon, the function
  maintains the \code{PID}s from \code{'polysA'}.
  It also maintains them when \code{'polysA'} contains only one generic polygon and
  the \code{'operation'} is \code{"DIFF"} (difference).
  Otherwise, if \code{'polysA'} contains only one generic polygon, it maintains
  the \code{PID}s from \code{'polysB'}.
}
\value{
  If \code{'polysB'} is \code{NULL}, the resulting PolySet contains
  a single generic polygon (one \code{PID}), possibly with several
  components (\code{SID}s).
  The function recalculates the \code{PID} and \code{SID} columns.
  
  If \code{'polysB'} is not \code{NULL}, the resulting PolySet contains one or more
  generic polygons (\code{PID}s), each with possibly several components (\code{SID}s).
  The function recalculates the \code{SID} column, and depending on the input,
  it may recalculate the \code{PID} column.
}
\author{
  C code: \href{https://www.angusj.com/delphi/clipper.php}{Angus Johnson}, Computer Programmer

  Implementation: \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Offsite, Vancouver BC\cr
  Last modified \code{Rd: 2023-10-30}
}
\seealso{
In package \pkg{PBSmapping}:\cr
  \code{\link[PBSmapping]{addPolys}},
  \code{\link[PBSmapping]{appendPolys}},
  \code{\link[PBSmapping]{clipPolys}},
  \code{\link[PBSmapping]{closePolys}},
  \code{\link[PBSmapping]{fixBound}},
  \code{\link[PBSmapping]{fixPOS}},
  \code{\link[PBSmapping]{locatePolys}},
  \code{\link[PBSmapping]{plotMap}},
  \code{\link[PBSmapping]{plotPoints}},
  \code{\link[PBSmapping]{thickenPolys}},
  \code{\link[PBSmapping]{thinPolys}}
}
\references{
  Murta, A. (2004) \emph{A General Polygon Clipping Library}. Accessed: Jul 29, 2004.

  Johnson, A. (2014) \emph{Clipper} -- an open source freeware library for clipping and offsetting lines and polygons. Accessed: Oct 31, 2014.
}
\examples{
local(envir=.PBSmapEnv,expr={
  oldpar = par(no.readonly=TRUE)
  #--- load the data (if using R)
  if (!is.null(version$language) && (version$language=="R"))
    data(nepacLL,envir=.PBSmapEnv)

  ### Example 1. Cut a triangle out of Vancouver Island
  par(mfrow=c(1,1))
  #--- create a triangle to use in clipping
  polysB <- data.frame(PID=rep(1, 3), POS=1:3,
    X=c(-127.5, -124.5, -125.6), Y = c(49.2, 50.3, 48.6))
  #--- intersect nepacLL with the single polygon, and plot the result
  plotMap(joinPolys(nepacLL, polysB), col="cyan")
  #--- add nepacLL in a different line type to emphasize the intersection
  addPolys(nepacLL, border="purple", lty=3, density=0)
  box()

  ### Example 2. Cut Texada and Lasqueti Islands out of Boxes
  xlim = list(box1=c(-124.8,-124),box2=c(-124,-123.9))
  ylim = list(box1=c(49.4,49.85), box2=c(49.85,49.9))
  Xlim = extendrange(xlim); Ylim=extendrange(ylim)
  polyA = as.PolySet(data.frame(
    PID = rep(1:2,each=4), POS = rep(1:4,2),
    X = as.vector(sapply(xlim,function(x){x[c(1,1,2,2)]})),
    Y = as.vector(sapply(ylim,function(x){x[c(1,2,2,1)]}))
    ), projection="LL")
  data(nepacLLhigh,envir=.PBSmapEnv)
  polyB = nepacLLhigh[is.element(nepacLLhigh$PID,c(736,1912)),]
  polyC = joinPolys(polyA, polyB, "DIFF")
  par(mfrow=c(2,2),cex=1,mgp=c(2,0.5,0))
  plotMap(polyA,col="lightblue",xlim=Xlim,ylim=Ylim)
  addPolys(polyB,col="gold");
  text(mean(Xlim)-0.05,Ylim-0.04,"Boxes (A,B)  and  Isles (C,D)")
  labs = calcCentroid(polyA)
  labs[1,c("X","Y")] = labs[2,c("X","Y")]+c(-0.1,-0.05)
  text(labs[,"X"],labs[,"Y"],c("A","B"),font=2)
  plotMap(polyC[is.element(polyC$PID,1),],col="pink",xlim=Xlim,ylim=Ylim)
  text(mean(Xlim)-0.05,Ylim-0.04,"Box A  \"DIFF\"  Isle C")
  plotMap(polyC[is.element(polyC$PID,3),],col="green",xlim=Xlim,ylim=Ylim)
  text(mean(Xlim)-0.05,Ylim-0.04,"Box A  \"DIFF\"  Isle D")
  plotMap(polyC[is.element(polyC$PID,c(1,3)),],col="cyan",xlim=Xlim,ylim=Ylim)
  text(mean(Xlim)-0.05,Ylim-0.04,"Box A  \"DIFF\"  Isles (C,D)")
  par(oldpar)
})
}
\keyword{manip} % multiple keywords allowed, one per line (this is correct)
\keyword{logic}

