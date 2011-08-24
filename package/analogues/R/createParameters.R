#' Initiate parameters for analogue dissimilarity analysis.
#'
#' This function is the first step too find analogue climate sites.
#' Here all parameters for the later analysis are set. The initial 
#' parameters can be changed later. Make sure clmate data are name properly. 
#'
#' @param x coordinate of point under analysis.
#' @param y coodrinate of point under analysis
#' @param to coordinates for which dissimilarity should be caluclated as matrix with. A \code{matrix} 
#' with two columns: x and y. If is set to \code{NA} dissimilarities are calculated for the whole raster. 
#' @param method select the method that is going to be use, either ccafs or hal for hallegate
#' @param z exponent for ccafs dissimilarity, default is 2.
#' @param hal.rad maximum tolerable relative annual difference for hellegate method. A vector of \code{length(vars)}, 
#' if variables should not be considered, substitute the number with \code{NA}. 
#' @param hal.mad maximum tollerable difference in annual precipitation. A vector of \code{length(vars)}, 
#' if variables should not be considered, substitute the number with \code{NA}.
#' @param hal.mrd mean absolute difference between months difference. A vector of \code{length(vars)}, 
#' if variables should not be considered, substitute the number with \code{NA}.
#' @param hal.ncond specify how many of the above condition need to be true in order that a sites is similiar.
#' @param gcms which gcm model should be used. If comparisions are made with the current climate, the first
#  scenario needs to be the current one (e.g. current,a1b_2030_bccr_bcm2_0,a1b_2030_cccma_cgcm3_1_t47).
#' @param vars specify which variables are being used in the calculation (e.g. tmean, precipitation)
#' @param weights specify how the variables are weighted. This can either be a \code{rasterLayer}
#' or a single number. Provide a list with the name of the raster or a value for weighting.
#' @param ndivision define how many devision all variables have per year (e.g. for monthly data use 12).
#' @param climate_data define directory where the climate data is located.
#' @param ext Extension of raster format (must be supported by gdal).
#' @param direction which direction should the dissimilarity be calculated. Available 
#' options are forward, backward and none (to calculate dissimilarity within the first gcm).
#' @param growing_season define the growing season of the crop that is to be modelled, provide the number 
#' for each division the plant is growing as a \code{vector}.
#' @param accross.year should the analogue method be looked accross years (i.e. should time lag be 
#' used to account for differences in seasons
#' @param normalise should the rasater be normalised to a mean of 0 and sd of 1 when being loaded.
#' @param keepllag specify whether or not grids for each lag should be kept
#' @return an object of AnalogueParameters
#' @export

createParameters <- function(x=10,   
  y=48, 
  to=NA,                       
  method="ccafs",                
  hal.rad=c(NA,0.15),                    
  hal.mad=c(NA,0.3),            
  hal.mrd=c(1,NA),           
  # hal.ncond=2,  # commented out by jsigner, for simplicity, if it works ok, remove
  z=2,
  scenario=c("current"),
  vars=c("tmean", "prec"),        
  weights=c("dtr",1),
  ndivisions=12,                  
  env.data=".",              
  ext="asc",
  direction="backwd",             
  growing.season=1:12,           
  across.year=T,
  keep.lag=F,
  normalise=F){

  # required packages
  require(raster)
  require(stringr)
  require(maptools)
  require(maps)
  require(spgrass6)
  require(akima)
  require(grid)
  require(rimage)

  
  # check wether point is terrestrial or not
  
  # growing season needs to be <= ndivisions
  if (length(growing.season) > ndivisions) {
    stop("Growing season > ndivision")
  }
  
  # each variable needs a weight
  if (length(weights) != length(vars)) {
    stop("Variables and weights don't match")
  }
  
  #check method is correct
  if (!method %in% c("ccafs","hal")) {
    stop("Available methods are only ccafs and hal")
  }
  
  # Make a list with all parameters
  params <- list(x=x,
                  y=y,
                  to=to,
                  method=method,
                  scenario=gcms,
                  hal.rad=hal.rad,
                  hal.mad=hal.mad,
                  hal.mrd=hal.mrd,
                  hal.ncond=sum(!is.na(c(hal.rad, hal.mad, hal.mrd))),  # of user defined just take the maximum
                  z=z,
                  direction=tolower(direction),
                  across.year=across.year,
                  growing.season=growing.season,
                  keep.lag=keep.lag,
                  env.data=climate.data,
                  vars=vars,
                  weights=weights,
                  normalise=normalise,
                  ext=ext,
                  ndivisions=ndivisions)
                  
  # add idxs
  # indexing vars, ie saying to which gcm each variable belongs
  params$idx.gcms <- rep(1:(length(params$gcms)),each=length(params$vars)) 

  params$idx.vars <- rep(1:(length(params$vars)),length(params$gcms)) 
  
  
  # load logos for plots
  data(logos)
    
  return(params)
}

