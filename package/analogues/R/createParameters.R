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
  to=NA, #probably no longer necessary a code update is needed to remove useless code
  method="ccafs",                
  hal.rad=c(NA,0.15),                    
  hal.mad=c(NA,0.3),            
  hal.mrd=c(1,NA),           
  # hal.ncond=2,  # commented out by jsigner, for simplicity, if it works ok, remove
  z=2,
  scenario=c("current","any"),
  vars=c("tmean", "prec"),        
  weights=c("dtr",1),
  ndivisions=12,                  
  env.data=".",              
  ext=".asc",
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
  
  # ----- massive verification of parameters
  #check if x and y have the same length
  if (length(x) != length(y)) {
    stop("analogues: length of your x's and y's is not equal")
  }
  
  #across.year must be F when ndivisions=1, or when length(growing.season=1)
  if (ndivisions == 1 | length(growing.season) == 1) {
    if (across.year) {
      stop("analogues: across.year must be FALSE when ndivisions=1 or length of growing.season=1")
    }
  }
  
  #if ext is not NA then it must be character and have a point
  if (!is.na(ext)) {
    if (!is.character(ext)) {
      stop("analogues: ext must be a character variable")
    }
    
    #looking for point
    if (length(grep(".",ext,fixed=T)) == 0) {
      stop("analogues: ext must contain a prefixing dot")
    }
  }
  
  #check if x and y are numeric
  if (!is.numeric(x)) {
    stop("analogues: x must be numeric")
  } else if (!is.numeric(y)) {
    stop("analogues: y must be numeric")
  }
  
  # ---- growing.season verifications
  #growing season must not have any NAs
  if (length(which(is.na(growing.season))) > 0) {
    stop("analogues: growing.season cannot have missing values")
  }
  
  #growing.season must be numeric
  if (!is.numeric(growing.season)) {
    stop("analogues: growing.season must be numeric")
  }
  
  # growing season needs to be <= ndivisions
  if (length(growing.season) > ndivisions) {
    stop("analogues: length of growing.season > ndivisions")
  }
  
  #growing.season must be within 1 and ndivisions
  ndv <- c(1:ndivisions)
  gs.no <- length(which(!growing.season %in% ndv))
  if (gs.no != 0) {
    stop("analogues: your growing season must be between 1 and ndivisions")
  }
  
  #growing.season must be continuous
  gs.fake <- c(min(growing.season):max(growing.season))
  if (length(gs.fake) != length(growing.season)) {
    #if they dont match it means there is a value in growing.season == ndivisions
    #(otherwise it means there is an error. We find that value
    where.ndv <- which(growing.season == ndivisions)
    #if there is no such value then error
    if (length(where.ndv) != 1) {
      stop("analogues: growing.season must be a continuous series")
    }
    
    #now, take all values one position after where.ndv and sum ndivisions to them
    gs.fake2 <- growing.season
    gs.fake2[(where.ndv+1):length(gs.fake2)] <- gs.fake2[(where.ndv+1):length(gs.fake2)] + ndivisions
    
    #now create a continous series and see if it matches with the 
    #length of the growing.season selected
    gs.fake3 <- c(min(gs.fake2):max(gs.fake2))
    if (length(gs.fake3) != length(growing.season)) {
      stop("analogues: growing.season must be a continuous series")
    }
  }
  
  #----------direction verifications
  #direction must be either forward, forwd, backward, backwd, none, or no
  if (!direction %in% c("forward","forwd","backward","backwd","none","no")) {
    stop("analogues: direction not recognised")
  }
  
  #when direction is backward or forward length(scenario) > 1
  if (direction == "backwd" | direction == "backward" | direction == "forward" | direction == "forwd") {
    if (length(scenario) < 2) {
      stop("analogues: forward or backward direction require at least two scenarios")
    }
  }
  
  #z must be numeric
  if (!is.numeric(z)) {
    stop("analogues: z parameter must be numeric")
  }
  
  # each variable needs a weight
  if (length(weights) != length(vars)) {
    stop("analogues: variables and weights don't match")
  }
  
  #check method is correct
  if (!method %in% c("ccafs","hal")) {
    stop("analogues: available methods are only ccafs and hal")
  }
  
  #hal checks
  if (method == "hal") {
    #conditions must be at least 1
    hal.nc <- length(which(!is.na(c(hal.mad,hal.mrd,hal.rad))))
    if (hal.nc == 0) {
      stop("analogues: at least one of Hallegatte thresholds needs to be switched on")
    }
    
    #none of the hal conditions can be zero
    hal.nz <- length(which(c(hal.mad,hal.mrd,hal.rad) == 0))
    if (hal.nz != 0) {
      stop("analogues: none of Hallegatte conditions should equal zero")
    }
    
    #there must be one condition per variable
    if (length(vars) != length(hal.mad)) {
      stop("analogues: hal conditions and variables don't match")
    } else if (length(vars) != length(hal.mrd)) {
      stop("analogues: hal conditions and variables don't match")
    } else if (length(vars) != length(hal.rad)) {
      stop("analogues: hal conditions and variables don't match")
    }
    
    #non-numeric weights make no sense for hal
    if (!is.numeric(weights)) {
      stop("analogues: non-numeric weights not necessary when method=hal")
    }
  }
  
  #normalise, across.year, keep.lag are logical
  if (!is.logical(across.year)) {
    stop("analogues: across.year must be logical")
  } else if (!is.logical(keep.lag)) {
    stop("analogues: keep.lag must be logical")
  } else if (!is.logical(normalise)) {
    stop("analogues: normalise must be logical")
  }
  
  #normalise=F when method="hal"
  if (method == "hal") {
    if (normalise) {
      stop("analogues: normalise should be FALSE when method=hal")
    }
  }
  
  # Make a list with all parameters
  params <- list(x=x,
                  y=y,
                  to=to,
                  method=method,
                  scenario=scenario,
                  hal.rad=hal.rad,
                  hal.mad=hal.mad,
                  hal.mrd=hal.mrd,
                  hal.ncond=sum(!is.na(c(hal.rad, hal.mad, hal.mrd))),  # of user defined just take the maximum
                  z=z,
                  direction=tolower(direction),
                  across.year=across.year,
                  growing.season=growing.season,
                  keep.lag=keep.lag,
                  env.data=env.data,
                  vars=vars,
                  weights=weights,
                  normalise=normalise,
                  ext=ext,
                  ndivisions=ndivisions)
                  
  # add idxs
  # indexing vars, ie saying to which gcm each variable belongs
  params$idx.gcms <- rep(1:(length(params$scenario)),each=length(params$vars)) 

  params$idx.vars <- rep(1:(length(params$vars)),length(params$scenario)) 
  
  # load logos for plots in PDF reports
  #data(logos)
    
  return(params)
}

