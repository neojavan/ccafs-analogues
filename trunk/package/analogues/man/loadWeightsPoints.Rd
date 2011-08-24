\name{loadWeightsPoints}

\alias{loadWeightsPoints}

\title{Load weights for dissimilarity...}

\usage{
  loadWeightsPoints(params)
}

\description{Load weights for point-based dissimilarity calculations. Data will be loaded as objects of class \code{matrix} 
or \code{numeric}, depending on the variables and scenarios specified. For raster-based calculations see \code{loadWeights()}}

\details{This functions loads weight data as \code{matrix}, if they exist. Otherwise weights are set to 1 or the value specified in \code{params}. 
Ensure that the path to your climate data is properly specified and that your climate data is properly named and is geographically consistent.}

\value{
A list of objects, with a combination of \code{matrix} objects and \code{numeric} objects, though it depends on the set up of the \code{params} variable.
}

\arguments{
  \item{params}{List. An object of the class \code{list} created with the function \code{createParameters()}}
}

\examples{
#No examples at the moment
}

\keyword{loadWeights}
\keyword{loadWeightsPoints}

