\name{plotPoints}
\alias{plotPoints}
\title{Plot EventData/PolyData as Points}
\description{
  Plot \link{EventData}/\link{PolyData}, where each unique \code{EID} or
  (\code{PID}, \code{SID}) describes a point.
}
\usage{
plotPoints (data, xlim = NULL, ylim = NULL, projection = FALSE,
            plt = c(0.11, 0.98, 0.12, 0.88), polyProps = NULL,
            cex = NULL, col = NULL, pch = NULL, axes = TRUE,
            tckLab = TRUE, tck = 0.014, tckMinor = 0.5 * tck, \dots)
}
\arguments{
  \item{data}{\link{EventData} or \link{PolyData} to plot (\emph{required}).}
  \item{xlim}{range of X-coordinates.}
  \item{ylim}{range of Y-coordinates.}
  \item{projection}{desired projection when \link{PolySet} lacks a
    \code{projection} attribute; one of \code{"LL"}, \code{"UTM"},
    or a numeric value.  If Boolean, specifies whether to check
    \code{polys} for a \code{projection} attribute.}
  \item{plt}{four element numeric vector \code{(x1, x2, y1, y2)} giving
    the coordinates of the plot region measured as a fraction of the
    figure region. Set to \code{NULL} if \code{mai} in \code{par} is
    desired.}
  \item{polyProps}{\link{PolyData} specifying which points to plot and their
    properties.  \code{\link{par}} parameters passed as direct arguments
    supersede these data.}
  \item{cex}{vector describing character expansion factors (cycled by
    \code{EID} or \code{PID}).}
  \item{col}{vector describing colours (cycled by \code{EID} or
    \code{PID}).}
  \item{pch}{vector describing plotting characters (cycled by \code{EID}
    or \code{PID}).}
  \item{axes}{Boolean value; if \code{TRUE}, plot axes.}
  \item{tckLab}{Boolean vector (length 1 or 2); if \code{TRUE},
    label the major tick marks.  If given a two-element
    vector, the first element describes the tick marks on the
    x-axis and the second element describes those on the y-axis.}
  \item{tck}{numeric vector (length 1 or 2) describing the length
    of tick marks as a fraction of the smallest dimension. If
    \code{tckLab = TRUE}, these tick marks will be automatically
    labelled.  If given a two-element vector, the first element
    describes the tick marks on the x-axis and the second element
    describes those on the y-axis.}
  \item{tckMinor}{numeric vector (length 1 or 2) describing the length
    of tick marks as a fraction of the smallest dimension.  These tick
    marks can not be automatically labelled.  If given a two-element vector,
    the first element describes the tick marks on the x-axis and
    the second element describes those on the y-axis.}
  \item{\dots}{additional \code{\link{par}} parameters, or the arguments
    \code{main}, \code{sub}, \code{xlab}, or \code{ylab} for the
    \code{\link{title}} function.}
}
\details{
  This function clips \code{data} to \code{xlim} and \code{ylim} before
  plotting.  It only adds \link{PolyData} containing \code{X} and
  \code{Y} columns.

  The function creates a blank plot when \code{polys} equals
  \code{NULL}.  In this case, the user must supply both \code{xlim} and
  \code{ylim} arguments.  Alternatively, it accepts the argument
  \code{type = "n"} as part of \dots, which is equivalent to specifying
  \code{polys = NULL}, but requires a \link{PolySet}.  In both cases,
  the function's behaviour changes slightly.  To resemble the
  \code{\link{plot}} function, it plots the border, labels, and other
  parts according to \code{\link{par}} parameters such as \code{col}.

  For additional help on the arguments \code{cex}, \code{col}, and
  \code{pch}, please see \code{\link{par}}.
}
\value{
  \link{PolyData} consisting of the \code{PolyProp}s used to create the plot.
}
\note{
  To satisfy the aspect ratio, this plotting routine resizes the plot
  region.  Consequently, \code{\link{par}} parameters such as
  \code{plt}, \code{mai}, and \code{mar} will change.  When the function
  terminates, these changes persist to allow for additions to the plot.
}
\author{
  \href{https://github.com/boersn/}{Nicholas M. Boers}, Staff Software Engineer\cr
  Jobber, Edmonton AB\cr
  Last modified \code{Rd: 2013-04-10}
}
\seealso{
  \code{\link{addPoints}},
  \code{\link{combineEvents}},
  \code{\link{convDP}},
  \code{\link{findPolys}},
  \code{\link{locateEvents}}.
}
\examples{
local(envir=.PBSmapEnv,expr={
  oldpar = par(no.readonly=TRUE)
  #--- load the data (if using R)
  if (!is.null(version$language) && (version$language=="R"))
    data(nepacLL,surveyData,envir=.PBSmapEnv)
  #--- plot a map
  plotMap(nepacLL, xlim=c(-136, -125), ylim=c(48, 57))
  #--- add events
  addPoints(surveyData, col=1:7)
  par(oldpar)
})
}
\keyword{aplot}

