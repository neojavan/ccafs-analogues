\name{summariseRuns}

\alias{summariseRuns}

\title{Summarise results of various comparisons...}

\usage{
summariseRuns(results, do_mean=TRUE, do_std=TRUE, do_cv=TRUE)
}

\description{
Create a report
}

\value{
prints a report and save it as pdf
}
\arguments{
\item{results}{A list with rasters, they all need to have the same extent}
\item{do.mean}{Logical. Calculate the mean over all rasters in the list}
\item{do.std}{Logical. Calcualte the sd over all rasters in the list}
\item{do.cv}{Logical. Calculate the coefficient of variance over all rasters in the list}
}
\examples{
#assoming results is a list with various dissimilarity calculations
#output <- summary(list(results[[1]], results[[3]],results[[2]]),do.mean=T,do.std=T,do.cv=T)
}

\alias{summariseRuns}
