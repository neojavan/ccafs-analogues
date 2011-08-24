\name{dissimilarityPoints}

\alias{dissimilarityPoints}

\title{dissimilarity calculation for points}

\usage{
  dissimilarityPoints(params, training, weights)
}

\description{Computes distance between many points based on an object of class \code{list} created with the function 
\code{createParameters()} of the package \code{analogues}, and other two \code{list}s created with the functions
\code{loadDataPoints()} and \code{loadWeightsPoints()}.}

\arguments{
  \item{params}{List. An object of class \code{list}, created with the function \code{createParameters()}}
  \item{training}{List. A list containing all training data, created
with \code{loadDataPoints()}.}
  \item{weights}{List. A list containing all weight data, created
with \code{loadWeightsPoints()}.}
}

\alias{dissimilarityPoints}
