\name{Set Base Plot Regions}
\alias{gridOMI}
\alias{gridFIG}
\alias{gridPLT}
\alias{gridPAR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Set Base Plotting Regions from Grid Viewport }
\description{
  These functions can be used to align base plotting regions
  with the current grid
  viewport.  This can be used to draw base plots within a grid viewport.
}
\usage{
gridOMI()
gridFIG()
gridPLT()
gridPAR()
}
\details{
  For this to be useful, you will have to make liberal use of
  \code{par(new=TRUE)} to prevent base from moving to a new page.

  With care, these can even be used to draw multiple base plots within
  a grid viewport (see the examples below), but in general, base
  plotting functions that draw multiple panels (e.g., \code{coplot})
  should not be
  expected to work.
}
%- maybe also 'usage' for other objects documented here.
\value{
  \code{gridOMI} returns a value that can be used to set the
  \code{par(omi)} parameter.

  \code{gridFIG} returns a value that can be used to set the
  \code{par(fig)} parameter.

  \code{gridPLT} returns a value that can be used to set the
  \code{par(plt)} parameter.

  \code{gridPAR} returns a value that can be used to set
  some graphical parameters (currently, \code{lwd}, \code{lty},
  and \code{col}).
}
\author{ Paul Murrell }

\section{Warning}{ If you resize the device, all bets are off! }
\seealso{ \link{Grid}, \code{\link{viewport}} }
\examples{
opar <- par(no.readonly=TRUE)
# gridFIG
grid.newpage()
pushViewport(viewport(width=0.5, height=0.5))
grid.rect(gp=gpar(col="grey", lty="dashed"))
par(fig=gridFIG())
par(new=TRUE)
plot(1:10)
# multiple plots
# NOTE the use of par(mfg)
# gridOMI
par(opar)
grid.newpage()
pushViewport(viewport(width=0.5, height=0.5))
grid.rect(gp=gpar(col="grey", lty="dashed"))
par(omi=gridOMI())
par(mfrow=c(2, 2), mfg=c(1, 1), mar=c(3, 3, 1, 0))
for (i in 1:4) {
  plot(i)
}
# gridPLT
par(opar)
grid.newpage()
pushViewport(viewport(width=0.5, height=0.5))
grid.rect(gp=gpar(col="grey", lwd=5))
par(plt=gridPLT())
par(new=TRUE)
plot(1:10)
# gridFIG with par(omi) set
par(opar)
grid.newpage()
par(omi=rep(1, 4))
pushViewport(viewport(width=0.5, height=0.5))
grid.rect(gp=gpar(col="grey", lwd=5))
par(fig=gridFIG())
par(new=TRUE)
plot(1:10)
# gridPLT with par(omi) set
par(opar)
grid.newpage()
par(omi=rep(1, 4))
pushViewport(viewport(width=0.5, height=0.5))
grid.rect(gp=gpar(col="grey", lwd=5))
par(plt=gridPLT())
par(new=TRUE)
plot(1:10)
# gridPAR
par(opar)
grid.newpage()
pushViewport(viewport(width=0.5, height=0.5,
  gp=gpar(col="red", lwd=3, lty="dotted")))
grid.rect(gp=gpar(col="grey", lwd=5))
par(fig=gridFIG())
par(gridPAR())
par(new=TRUE)
plot(1:10, type="b")
}
\keyword{ dplot }% at least one, from doc/KEYWORDS
