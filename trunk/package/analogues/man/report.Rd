\name{report}

\alias{report}

\title{Create a PDF report...}

\usage{
report(models=list(list(params, training, results, r.lab, m.lab)), pdf.name="test.pdf")
}

\description{
Create a PDF report with the results of a given dissimilarity analysis.
}

\value{
Prints a report and saves it as pdf

}

\arguments{
  \item{models}{A list of a series of analogue specifications}
  \item{params}{A list created with \code{createParameters()}}
  \item{training}{A list with the input data used for the calculation}
  \item{results}{A list of rasters to be included in the PDF plots}
  \item{m.lab}{General label of the pdf, i.e. 2030}
  \item{m.lab}{A vector of labels corresponding to \code{results}. Must be of equal length as \code{results}}
  \item{pdf.name}{path and name to output pdf}
}

\examples{
#Nothing at the moment
}

\alias{report}


