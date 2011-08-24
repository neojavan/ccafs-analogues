\name{dissimilarity}

\alias{dissimilarity}

\title{dissimilarity calculation for rasters}

\usage{
  dissimilarity(params, training, weights)
}

\description{Computes distance between one reference point and other points based on an object of class \code{list} created with 
the function \code{createParameters()}, and other two \code{list}s created with the functions \code{loadData()} 
and \code{loadWeights()}.}

\arguments{
  \item{params}{List. An object of class \code{list}}
  \item{training}{List. A list containing all training \code{RasterLayers}, created
with \code{loadData()}.}
  \item{weights}{List. A list containing all weight \code{RasterLayers} or values, created
with \code{loadWeights()}.}
}

\alias{dissimilarity}

