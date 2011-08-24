\name{loadWeights}

\alias{loadWeights}

\title{Load weights for dissimilarity...}

\usage{
  loadWeights(params)
}

\description{Load weights for raster based dissimilarity calculations. Data will be loaded as objects of class \code{RasterLayer} 
or \code{numeric}, depending on the variables and scenarios specified. For point-based calculations see \code{loadWeightsPoints()}}

\details{This functions loads weight grids as \code{Raster*}, if they exist. Otherwise weights are set to 1 or the value specified. 
Ensure that the path to your climate data is properly specified and that your climate data is properly named and is geographically consistent.}

\value{
A list of objects, with a combination of \code{raster} objects and \code{numeric} objects, though it depends
on the set up of the \code{params} variable.
}

\arguments{
  \item{params}{List. An object of the class \code{list} created with the function \code{createParameters()}}
}

\examples{
#No examples at the moment
}

\keyword{loadWeights}
\keyword{loadWeightsPoints}


