\name{loadDataPoints}

\alias{loadDataPoints}

\title{Load data for dissimilarity calculations...}

\usage{
  loadDataPoints(params)
}

\description{Load data for the dissimilarity calculations point basis. Data will be loaded as objects 
of the type \code{matrix} and will be stored into a list, depending on the variables and scenarios specified. 
For raster-based calculations see \code{loadData()}.}

\details{This functions loads the dissimilarity data, based on a the specified configuration of the object \code{params}.
Ensure that your data is loaded, as this would look for objects named exactly as the variables and scenarios or GCMs specified.
}


\keyword{loadDataPoints}
\keyword{loadData}
\keyword{raster}

\value{
An object of class \code{list} containing matrices corresponding to the values of each data point at each month or step as specified in the \code{params} object.
}

\arguments{
  \item{params}{List. An object of the class \code{list} created with the function \code{createParameters()}}
}

\examples{
#No examples at the moment
}

