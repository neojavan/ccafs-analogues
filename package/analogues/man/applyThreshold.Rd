\name{applyThreshold}

\alias{applyThreshold}

\title{Apply thresholding to a raster...}

\usage{
  applyThreshold(results, range, best)
}

\description{Apply thresholding to a raster, based on either a \code{range} of values or on the closest or \code{best} x fraction of pixels}

\value{
\code{RasterLayer} where anything furthest than \code{best} fraction
}

\arguments{
  \item{results}{RasterLayer or RasterStack}
  \item{range}{Vector of length 2. If the raster should be thresholder on a range then provide a range consisting of a lower limit and an upper limit.}
  \item{best}{Numeric. Probablity values at which threshold should be cut, i.e. fraction of values to be retained in the output grid}
}

\examples{
#being rs a RasterLayer
#threshold by range between 0 and 1
#rs.range <- applyThreshold(rs,range=c(0,1))
#
#threshold by best 10 percent
#rs.best <- applyThreshold(rs,best=0.1)
#
#now plot this
#plot(stack(rs.best,rs.range))
}

\alias{applyThreshold}


