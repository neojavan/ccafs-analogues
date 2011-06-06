dissimilarity <- function(params,training, weights) {
  # Computes distance between one reference point and one other points
  #
  # Args:
  #  params: list specifying all parameters (createParams)
  #  training: list of vectors and matrices with the data at ref and poi.
  #  weights: list of vectors and matrices with the data at ref and poi.
  #  .tmpRast: a rasterlayer, to create raster layers again
  #
  # Returns:
  #  A list of RasterLayers or RasterStacks, depending if lags are kept or not
  
  # constants
  ngcms <- length(params$gcms)  # number of gcms
  res.all <- list()  # list with results
  xmn <- xmin(training[[1]])
  xmx <- xmax(training[[1]])
  ymn <- ymin(training[[1]])
  ymx <- ymax(training[[1]])
  nrow <- nrow(training[[1]])
  ncell <- ncell(training[[1]])
  nlayers <- nlayers(training[[1]])
  ref.where <- cellFromXY(training[[1]], cbind(params$x, params$y))
  
  # -------------------------------------------------------------------------- #
  # Create roll for lags
  roll.v <- c()
  months <- 1:params$ndivisions
  for (i in 1:length(months)) {
    roll.v <- c(roll.v,(months[c(i:length(months),0:(i-1))]))  
  }
  roll <- matrix(data=roll.v, ncol=length(months), byrow=TRUE)
  
  # cut roll to the actual growin period
  roll <- roll[ , params$growing.season]
  
  # only keep first row, if accross the years is false
  if (!params$across.year & length(roll)>1) {
    roll <- roll[1, , drop=FALSE]
  }
  
  if (length(roll)==1) {
    roll <- matrix(roll, nrow=1)
  }
  
  # -------------------------------------------------------------------------- #
  # get values at poi
  
  # check wich cells hold the information we are interested in 
  cat("preparing training data \n")
  poi.t <- lapply(training, getValues)
  
  cat("preparing weights \n")
  poi.w <- list()

  for (i in 1:length(weights)) {
    if (is.numeric(weights[[i]]) | is.character(weights[[i]])) {
      poi.w[[i]] <- matrix(rep(as.numeric(weights[[i]]), (ncell*nlayers)), 
        ncol=nlayers)
    } else {
      poi.w[[i]] <- getValues(weights[[i]])
  }}


if (params$normalise) {
  cat("Normalising training data \n")
  poi.t <- lapply(poi.t, scale)
  
  cat("Normalising weight \n")
  poi.w <- lapply(poi.w, scale)
}

  # -------------------------------------------------------------------------- #
  # Call function
  
  if (params$direction=="backwd" | params$direction=="backward") {
    # projecting all future gcms back to the first one, which is current
    
    # start with with two because we are always projection to 1
    for (gcm in 2:ngcms) {  
      
      # project from gcm to 1 (ie current)
      res.all[[(gcm - 1)]] <- callDissimilarity(params, ref.where,
        poi.t, poi.w, from=gcm, to=1, roll)
    }


  } else if (params$direction=="forwd" | params$direction=="forward"){
    # projecting from the current (first grid) to all futur gcms 
    
    for (gcm in 2:ngcms) {
     res.all[[(gcm - 1)]] <-  callDissimilarity(params, ref.where, 
        poi.t, poi.w, from=1, to=gcm, roll)
    }
    
  } else if (params$direction=="current") {
      
      res.all[[1]] <- callDissimilarity(params, ref.where, 
        poi.t, poi.w, from=1, to=1, roll)
    
  } else { 
      stop("no directions was chosen") 
  }
  
# ---------------------------------------------------------------------------- #  
  
  if (params$method == "ccafs") {
    if (params$keep.lag) {
      # make rasters again3
      cat("creating rasters \n")
      res.all <- lapply(res.all, function(x) setValues(training[[1]],x))
      
    } else {
      # TODO make function dynamic
      cat("looking for min dissimilarity \n")
      res.all <- lapply(res.all, function(x) apply(x,1,min))
      
      # make rasters again
      cat("creating rasters\n")
      res.all <- lapply(res.all, function(x) setValues(training[[1]],x))
    }
  } else if (params$method == "hal") {
    if (params$keep.lag) {
      # make rasters again3
      cat("creating rasters\n")
      res.all <- lapply(res.all, function(x) setValues(training[[1]],x))      
    } else {
      # TODO make function dynamic
      cat("aggregating lag\n")
      res.all <- lapply(res.all, function(x) apply(x,1,sum))
      res.all <- lapply(res.all, function(x) ifelse(x > 0,1,0))
      # make rasters again
      cat("creating rasters\n")
      res.all <- lapply(res.all, function(x) setValues(training[[1]],x))
    }
  }
  return(res.all)
}

# ---------------------------------------------------------------------------- #
callDissimilarity <- function(params, ref.where, poi.t, poi.w, 
from, to, roll) {
        
      this.res <- c()
      nvars <- length(params$vars)
      
      this.ref.t <- lapply(poi.t[which(params$idx.gcms==to)], function(x) {
        as.vector(x[ref.where,])
      })
      
      this.poi.t <- poi.t[which(params$idx.gcms==from)]
      
      # Weights are only needed for ccafs method
      if (params$method == "ccafs") {
         
        this.ref.w <- lapply(poi.w[which(params$idx.gcms==to)], function(x) {
          as.vector(x[ref.where,])
        })
      
        this.poi.w <- poi.w[which(params$idx.gcms==from)]
      
        this.z <- params$z
        
        cat("calc dissimilarity starting with ")
        
        this.res <- apply(roll, 1, function(x) {
          cat(x[1]," ")
          ccafsMPoints(ref.t=lapply(this.ref.t, function(y) y[x]), 
                poi.t=lapply(this.poi.t, function(y) y[,params$growing.season]), 
                ref.w=lapply(this.ref.w, function(y) y[x]), 
                poi.w=lapply(this.poi.w, function(y) y[,params$growing.season]), 
                params$z) 
        })
        
         cat("\n")
        
      } else if (params$method == "hal") {
        
        cat("calculating hal starting with: ")
        for (i in 1:nrow(roll)) {
          cat(roll[i,1], " ")
          mad <- applyThreshold(madMPoints(
            lapply(1:nvars, function(x) this.ref.t[[x]][roll[i,]]), this.poi.t), 
            params$hal.mad)
          mrd <- applyThreshold(mrdMPoints(
            lapply(1:nvars, function(x) this.ref.t[[x]][roll[i,]]), this.poi.t), 
            params$hal.mrd)
          rad <- applyThreshold(radMPoints(
            lapply(1:nvars, function(x) this.ref.t[[x]][roll[i,]]), this.poi.t), 
            params$hal.rad)
          
          this.res.tmp <- do.call('+',mad) + do.call('+',mrd) + do.call('+',rad)
          this.res <- cbind(this.res,ifelse(this.res.tmp >= params$hal.ncond, 1,0))
        }
      cat("\n")
      }
      return(this.res)    
}

# ---------------------------------------------------------------------------- #

applyThreshold <- function(obj, th) {
  for (i in 1:length(obj)) {
    if (!is.na(th[[i]])) {
      obj[[i]] <- obj[[i]] <= th[[i]]
    } else {
      obj[[i]] <- 0
    }
  }
  return(obj)
}
