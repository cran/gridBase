\name{baseViewports}
\alias{baseViewports}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Generate grid Viewports from Base Plot }
\description{
  This will generate a list of grid viewports which correspond
  to the current inner, figure, and plot regions of the current
  base plot.
}
\usage{
baseViewports()
}
\details{
  The figure region is relative to the inner region so you
  must push the inner region before pushing the figure region.
  Similarly, the plot region is relative to the figure region
  so this should only be pushed after the previous two.
  }
%- maybe also 'usage' for other objects documented here.
\value{
  A list with three elements:
  \item{inner }{A viewport corresponding to the inner region
    of the current plot.}
  \item{figure }{A viewport corresponding to the figure region
    of the current plot.}
  \item{plot }{A viewport corresponding to the plot region
    of the current plot.}
}
\author{ Paul Murrell }

\section{Warning}{ If you resize the device, all bets are off! }
\seealso{ \link{Grid}, \code{\link{viewport}} }
\examples{
par(oma=rep(1, 4), mfrow=c(1, 2), xpd=NA)
plot(1:10)
vps <- baseViewports()
pushViewport(vps$inner)
grid.rect(gp=gpar(lwd=3, col="red"))
pushViewport(vps$figure)
grid.rect(gp=gpar(lwd=3, col="green"))
pushViewport(vps$plot)
grid.rect(gp=gpar(lwd=3, col="blue"))
grid.points(1:10, 10:1)
}
\keyword{ dplot }% at least one, from doc/KEYWORDS
