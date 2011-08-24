\name{loadData}

\alias{loadData}

\title{Load data for dissimilarity calculations...}

\usage{
  loadData(params)
}

\description{Load data for the dissimilarity calculations on a grid or \code{raster} basis. Data will be loaded as objects 
of type \code{RasterLayer} and will be stored onto a list, depending on the variables and scenarios specified. For point-based calculations see \code{loadDataPoints()}.}

\details{This function loads the dissimilarity data, based on the specified configuration on the existing object \code{params}. Ensure 
that the path to your climate data is properly specified and that your climate data is properly named and is geographically consistent.
}


\keyword{loadData}
\keyword{loadDataPoints}
\keyword{raster}

\value{
A list of objects of class \code{Raster*}, normally \code{RasterStack}, though it depends on the set up of the \code{params} variable.
}

\arguments{
  \item{params}{List. An object of the class \code{list} created with the function \code{createParameters()}}
}

\examples{
#No examples at the moment
}


