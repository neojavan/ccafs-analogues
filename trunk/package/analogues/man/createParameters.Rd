\name{createParameters}

\alias{createParameters}

\title{Initiate parameters for analogue dissimilarity analysis.}

\description{Initiate parameters for analogue dissimilarity analysis.}

\usage{createParameters(x=10, y=48, to=NA, method="ccafs", hal.rad=c(NA, 0.15), hal.mad=c(NA,0.3), hal.mrd=c(1, NA), 
z=2, gcms=c("current","gcm1","..."), vars=c("tmean","prec"), weights=c("dtr", 1), ndivisions=12, climate.data="./MyFolder", 
ext="asc",direction="backwd", growing.season=1:12, across.year=T, keep.lag=F, normalise=F)}

\details{
This function is the first step to find analogue climate sites. Here all parameters for the later analysis are set. 
The initial parameters can be changed later. Make sure climate data are name properly.

For point-based calculations, \code{x} should be a vector of longitude values, and \code{y} should be a vector of latitude values.
In that case, you do not need to specify the climate data folder, so that means \code{climate.data=NA.}

Here all parameters for the later analysis are set. The initial parameters can be changed later. Make sure climate data are name properly. 
Format of climate data should be: 
\code{scenario}_\code{variable}_\code{divison}.\code{ext}, where scenario can be either current or a combination of any SRES, year and GCM; 
division is a consecutive number specifying the time step of your data. So, for monthly it would be from 1 to 12; and ext is 
the extension of the \code{raster} data.
}

\value{
An object of AnalogueParameters, i.e. a \code{list}
}

\arguments{
  \item{x}{Numeric. Longitude or coordinate X of the site under analysis. Can also be a vector of \code{n} values corresponding 
  to the values. See \code{dissimilarityPoints()} for details on this.}
  \item{y}{Numeric. Latitude or coordinate Y of the site under analysis. Can also be a vector of \code{n} values corresponding 
  to the values. See \code{dissimilarityPoints()} for details on this.}
  \item{to}{Matrix. Target point to which dissimilarity should be calculated. A \code{matrix} with two columns: x and y. If is set to \code{NA} dissimilarities are calculated for the whole \code{RasterLayer}.}
  \item{method}{Character. Method to be used, either CCAFS \code{ccafs} or Hallegate \code{hal}}
  \item{hal.rad}{Vector. Maximum tolerable relative annual difference for Hallegate method. Should be of length equal 
  to \code{length(vars)}. If this condition should not be considered for a given \code{var}, then substitute 
  the corresponding position in the \code{hal.rad} vector with \code{NA}.}
  \item{hal.mad}{Vector. Maximum tolerable mean absolute difference. Should be of length equal to \code{length(vars)}. If this condition should not be considered for a given \code{var}, then substitute 
  the corresponding position in the \code{hal.rad} vector with \code{NA}.}
  \item{hal.mrd}{Vector. Mean absolute relative difference between months. Should be of length equal to \code{length(vars)}. If this condition should not be considered for a given \code{var}, then substitute the corresponding position in the \code{hal.rad} vector with \code{NA}.}
  \item{z}{exponent for ccafs dissimilarity, default is 2.}
  \item{scenario}{Character vector. List of climate scenarios to calculate dissimilarity. The first value in this vector 
  is always the reference scenario, hence it is usually the present day, others are a combination of SRES, period and GCM.}
  \item{vars}{Character vector. Specify which variables are being used in the calculation (e.g. tmean, precipitation). Names should be provided according to your the \code{analogues} standard file naming}
  \item{weights}{Character or numeric vector. Specify how the variables are weighted. This can either be a \code{RasterLayer}
  or a single number. Provide a vector with the name of the raster in quotes or a numeric value for weighting.}
  \item{ndivisions}{Numeric. Define how many division the variables have per year (e.g. for monthly data use 12).}
  \item{env.data}{Character. Define directory where the environmental data is located.}
  \item{ext}{Character. Extension of raster format (must be supported by gdal).}
  \item{direction}{Character. Which direction should the dissimilarity be calculated. Available options are \code{forward}, \code{backward} and \code{none}.}
  \item{growing.season}{Numeric vector. Define the growing season of the crop that is to be modelled as the consecutive series of numeric months. 
  For instance, for January to March it would be \code{c(1:3)}.}
  \item{accross.year}{Logical. Should the analogue method be looked accross years, i.e. should time lag be used to account 
  for differences in seasons.}
  \item{normalise}{Logical. Should the rasater be normalised to a mean of 0 and sd of 1 when being loaded.}
  \item{keep.lag}{Logical. Specify whether or not grids for each lag should be kept. Default is false. 
  Keep it \code{FALSE} to avoid memory overload.}
  \item{...}{Expressions evaluated in the context of \code{df} and 
  then fed to \code{\link{order}}}
}

\examples{
#ccafs_params <- createParams(x, z, )
}

\keyword{manip}