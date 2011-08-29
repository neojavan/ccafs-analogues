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
  \item{m.lab}{General label for the models. Must be of equal length to the variable \code{models}}
  \item{r.lab}{A vector of labels corresponding to \code{results}. Must be of equal length as \code{results}}
  \item{pdf.name}{path and name to output pdf}
}

\examples{
##assuming params contains a given configuration, training contains variable data
##and we want to plot rasters in an object named result, outcome of the,
##dissimilarity() function. The result object contains two model outcomes,
## we will use the year (2030) as model.lab
##
##we first configure the model list
##
#mod <- list(params=params,
#            training=training,
#            results=result,
#            r.lab=c("ukmo","cccma"),
#            m.lab="2030")
#
##now we create the report
##
#report(models=list(mod),pdf.name="report.pdf")
}

\alias{report}


