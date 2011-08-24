\name{ccafsMPointsPercentDiff}

\alias{ccafsMPointsPercentDiff}

\title{Calculate ccafs dissimilarity between points with climate information...}

\usage{
ccafsMPointsPercentDiff(ref.t, poi.t, ref.w, poi.w, z=2)
}

\description{Calculate CCAFS dissimilarity between points with climate information. Standard users will not need to go 
into much detail on this, as it is  wrapped into \code{dissimilarity()} and \code{dissimilarityPoints()}}

\value{
A vector of length nrow(poi.t), containing dissimilarity
}

\arguments{
  \item{ref.t:}{vector with values of vars at reference point}
  \item{poi.t:}{matrix with values of vars at target points}
  \item{ref.w:}{vector with values of weights at reference point}
  \item{poi.w:}{matrix with values of weights at target points}
  \item{z:}{number parameter. Default is 2, equal to euclidean distance}
}

\alias{ccafsMPointsPercentDiff}

