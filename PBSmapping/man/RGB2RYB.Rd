\name{RGB2RYB}
\alias{RGB2RYB}
\alias{RYB2RGB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Convert RGB to RYB and RYB to RGB
}
\description{
  Convert RGB (red-green-blue) colours to RYB (red-yellow-blue) colours, and vice versa.
  Algorithm based on Sugita and Takahashi (2015, 2017)
}
\usage{
RGB2RYB(RGBmat)
RYB2RGB(RYBmat)
}
\arguments{
  \item{RGBmat}{\code{numeric} -- matrix of red-green-blue primary colors by column, where rows are individual records of the three primary (RGB) colours. User can specify RGB in terms of 0-1 or 0-255; however, the algorith converts the latter to 0-1.}
  \item{RYBmat}{\code{numeric} -- matrix of red-yellow-blue primary colors by column, where rows are individual records of the three primary (RYB) colours. User can specify RYB in terms of 0-1 or 0-255; however, the algorith converts the latter to 0-1.}
}
\details{
  The RYB colour wheel is more commonly used by artists, and provides a more intuitive system when blending colours -- red and yellow makes orange, yellow and blue makes green, blue and red makes purple. On the RYB colour wheel, red lies opposite green, but on the RGB colour wheel, red lies opposite cyan.
}
\value{
  Matrix of RGB or RYB primary colour intensities, where rows are records and columns are primary colours.
}
\references{
Sugita, J. and Takahashi, T. (2017)
\href{https://www.jstage.jst.go.jp/article/tievciieej/5/2/5_110/_article/-char/ja}{Computational RYB Color Model and its Applications}.
IIEEJ Transactions on Image Electronics and Visual Computing 5(2): 110-122.

Sugita, J. and Takahashi, T. (2015)
\href{http://nishitalab.org/user/UEI/publication/Sugita_IWAIT2015.pdf}{RYB Color Compositing}.
International Workshop on Applications in Information Technology, October 8-10, 2015.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Regional Headquarters, Vancouver BC\cr
  Last modified \code{Rd: 2022-07-06}
}
\note{
  Opposite colours calculated in RYB space (1-RYB) are not always what one expects.
  For example the colour \code{"purple"}, RGB \{160, 32, 240\}, might better be specified
  as RGB \{126, 0, 255\} before converting to RYB and inverting.
}
\seealso{
In package \pkg{PBSmapping}:\cr
  \code{\link[PBSmapping]{addBubbles}}

In package \pkg{grDevices}:\cr
  \code{\link[grDevices]{col2rgb}},
  \code{\link[grDevices]{rgb}},
  \code{\link[grDevices]{rgb2hsv}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{color}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
