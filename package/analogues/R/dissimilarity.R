#' Computes distance between one reference point and one other points
#'
#' @param params an object of class AnaloguesParameters
#' @param training a list containing all training rasters, created
#' with \code{loadData()}. 
#' @param training a list containing all weight rasters or values, created
#' with \code{loadWeights()}. 
#' @value A list of RasterLayers or RasterStacks, depending if lags are kept or not
#' @export

dissimilarity <- function(params,training, weights) {
  
  # constants
  ngcms <- length(params$scenario)  # number of gcms
  res.all <- list()  # list with results
  xmn <- xmin(training[[1]])
  xmx <- xmax(training[[1]])
  ymn <- ymin(training[[1]])
  ymx <- ymax(training[[1]])
  nrow <- nrow(training[[1]])
  ncell <- ncell(training[[1]])
  nlayers <- nlayers(training[[1]])
  ref.where <- cellFromXY(training[[1]], cbind(params$x, params$y))
  
  #x and y must be within the analysis extent
  an.ext <- extent(training[[1]])
  if (params$x > an.ext@xmax | params$x < an.ext@xmin) {
    stop("analogues: x must be within your geographic extent")
  } else if (params$y > an.ext@ymax | params$y < an.ext@ymin) {
    stop("analogues: y must be within your geographic extent")
  }
  
  #check whether we are dealing with only 1 point
  if (length(params$x) > 1) {
    stop("analogues: for grid-based analyses only one point is needed")
  }
  
  #check whether the point is terrestrial or not
  ver.vals <- extract(training[[1]],data.frame(x=params$x,y=params$y))
  nas <- which(is.na(ver.vals))
  if (length(nas) > 0) {
    stop("analogues: NAs found when extracting data, check your point is located in-land")
  }
  
  #if there is a to, then focus on that one, deprecated from version 0.0.5 onwards
  if (is.matrix(params$to)) {
    poi.where <- cellFromXY(training[[1]], params$to)
  }
  
  # -------------------------------------------------------------------------- #
  # Create roll for lags
  roll.v <- c()
  months <- 1:params$ndivisions
  for (i in 1:length(months)) {
    roll.v <- c(roll.v,(months[c(i:length(months),0:(i-1))]))  
  }
  roll <- matrix(data=roll.v, ncol=length(months), byrow=TRUE)
  
  # cut roll to the actual growin period
  roll <- roll[ , params$growing.season, drop=FALSE]
  
  # only keep first row, if accross the years is false
  if (!params$across.year & length(roll)>1) {
    roll <- roll[1, , drop=FALSE]
  }
  
  #verify that roll is a matrix
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
    # poi.t <- lapply(poi.t, function(x) matrix(scale(as.vector(x)), ncol=params$ndivision, byrow=FALSE))

    poi.tt <- list()    
        
    for (i in 1:length(params$vars)) {
      a <- scale(unlist(poi.t[which(params$idx.vars==i)]))
      
      which.put <- which(params$idx.vars==i)
      where.start <- seq(1,by=length(a)/length(which.put), length.out=length(which.put)*params$ndivisions)

      for (j in 1:length(which.put)) {
        poi.tt[[which.put[j]]] <- matrix(a[where.start[j]:(where.start[j]+(length(a)/length(which.put)-1)), ], ncol=params$ndivisions, byrow=FALSE)
      }
    }
    
    poi.t <- poi.tt  
    # if all weights are the same, standardisation will result in NaN
    # cat("Normalising weight \n")    
    # poi.w <- lapply(poi.w, function(x) {
    #  if (all(x[1,1] == x)) {
    #    x[,] <- x[1,1]
    #    return(x)
    #  } else {
    #    return(scale(x) + 10)
    #  }
    #})
  }

  # -------------------------------------------------------------------------- #
  # Call function
  
  if (params$direction=="backwd" | params$direction=="backward") {
    # projecting all future gcms back to the first one, which is current
    
    # start with with two because we are always projection to 1
    for (gcm in 2:ngcms) {  
      
      # project from gcm to 1 (ie current)
      res.all[[(gcm - 1)]] <- callDissimilarity(params, ref.where,
        poi.t, poi.w, from=gcm, to=1, roll, poi.where)
    }

  } else if (params$direction=="forwd" | params$direction=="forward"){
    # projecting from the current (first grid) to all futur gcms 
    
    for (gcm in 2:ngcms) {
     res.all[[(gcm - 1)]] <- callDissimilarity(params, ref.where, 
        poi.t, poi.w, from=1, to=gcm, roll,poi.where)
    }
    
  } else if (params$direction=="no" | params$direction=="none" | params$direction==NA) {
      
      res.all[[1]] <- callDissimilarity(params, ref.where, 
        poi.t, poi.w, from=1, to=1, roll, poi.where)
    
  } else { 
      stop("no directions was chosen") 
  }
  
# ---------------------------------------------------------------------------- #  
  if (!is.matrix(params$to)) {
    if (params$method == "ccafs" | params$method == "ccafsp1") {
      if (params$keep.lag) {
        # make rasters again3
        cat("creating rasters \n")
        res.all <- lapply(res.all, function(x) setValues(training[[1]][[1]],x))
        
      } else {
        # TODO make function dynamic
        
        cat("looking for min dissimilarity \n")
        if (length(params$growing.season) > 1) {
          res.all <- lapply(res.all, function(x) apply(x,1,min))
        } else {
          res.all <- lapply(res.all, function(x) x[,1])
        }
        
        # make rasters again
        cat("creating rasters\n")
        res.all <- lapply(res.all, function(x) setValues(training[[1]][[1]],x))
      }
    } else if (params$method == "hal") {
      if (params$keep.lag) {
        # make rasters again3
        cat("creating rasters\n")
        res.all <- lapply(res.all, function(x) setValues(training[[1]][[1]],x))      
      } else {
        # TODO make function dynamic
        if (nrow(roll) > 1) {
          cat("aggregating lag\n")
          res.all <- lapply(res.all, function(x) apply(x,1,sum))
          res.all <- lapply(res.all, function(x) ifelse(x > 0,1,0))
        }
        # make rasters again
        cat("creating rasters\n")
        res.all <- lapply(res.all, function(x) setValues(training[[1]][[1]],x))
      }
    }
  } else {
    if (params$method == "ccafs") {
        if (params$keep.lag) {
        
          return(res.all)
          
        } else {
          # TODO make function dynamic
          cat("looking for min dissimilarity \n")
          res.all <- lapply(res.all, function(x) apply(x,1,min))
          
          return(res.all)
          
        }
      } else if (params$method == "hal") {
        if (params$keep.lag) {
          # make rasters again3
          return(res.all)
        } else {
          # TODO make function dynamic
          cat("aggregating lag\n")
          if (nrow(roll) > 1) {
             res.all <- lapply(res.all, function(x) apply(x,1,sum))
             res.all <- lapply(res.all, function(x) ifelse(x > 0,1,0))
          }
          return(res.all)
        }
      }
    }
  return(res.all)
}

# ---------------------------------------------------------------------------- #
callDissimilarity <- function(params, ref.where, poi.t, poi.w, 
from, to, roll, poi.where=NA) {
      
      #this.res <- matrix(rep(NA, (params$ndivisions * nrow(this.poi.t[[1]]))), ncol=params$ndivisions)
      this.res <- matrix(NA, ncol=nrow(roll), nrow=nrow(poi.t[[1]]))
      nvars <- length(params$vars)
      
      #get the data for the reference point (it'd be only one vector)
      this.ref.t <- lapply(poi.t[which(params$idx.gcms==to)], function(x) {
        as.vector(x[ref.where,])
      })
      
      #get the data for the target points. All pixels within a given study area.
      this.poi.t <- poi.t[which(params$idx.gcms==from)]
      
      #if we have a params$to point, then discard all other pixels and keep only that one
      if (is.matrix(params$to)) {
        this.poi.t <- lapply(poi.t[which(params$idx.gcms==to)], function(x) x[poi.where, , drop=FALSE])
      }
      
      # Weights are only needed for ccafs method
      if (params$method == "ccafs" | params$method == "ccafsp1") {
        
        #get weights for the reference point
        this.ref.w <- lapply(poi.w[which(params$idx.gcms==to)], function(x) {
          as.vector(x[ref.where,])
        })
        
        #get weights for the target points
        this.poi.w <- poi.w[which(params$idx.gcms==from)]
        
        #if we have a params$to point, then discard all other pixels and keep only that one
        if (is.matrix(params$to)) {
          this.poi.w <- lapply(poi.w[which(params$idx.gcms==to)], function(x) x[poi.where, , drop=FALSE])
        }
        
        #get z value from params
        this.z <- params$z
        
        cat("calc dissimilarity starting with ")
        
        #empty matrix to store outputs, filled all with NA, with params$ndivisions columns
        #and nrows=number of pixels
        #this.res <- matrix(rep(NA, (params$ndivisions * nrow(this.poi.t[[1]]))), ncol=params$ndivisions)
        
        #looping through the lagging roll
	      for (i in 1:nrow(roll)) { 
	        
          cat(roll[i, 1], " ")
          
          #get this particular roll
          this.roll <- roll[i,]
          
          if (params$method == "ccafs") {
          
          #Calling point-based dissimilarity calculation. Keep in mind that
          #the reference point's growing seasons MUST NOT VARY at all
          #it is the target point's growing season the one that should rotate to match
          #the other!!!!
  	      this.res[,i] <- ccafsMPoints(ref.t=lapply(this.ref.t, function(y) y[params$growing.season]), 
                poi.t=lapply(this.poi.t, function(y) y[,this.roll]), 
                ref.w=lapply(this.ref.w, function(y) y[params$growing.season]), 
                poi.w=lapply(this.poi.w, function(y) y[,this.roll]), 
                params$z)
          } else {
            this.res[,i] <- ccafsMPointsPercentDiff(ref.t=lapply(this.ref.t, function(y) y[params$growing.season]), 
                poi.t=lapply(this.poi.t, function(y) y[,this.roll]), 
                ref.w=lapply(this.ref.w, function(y) y[params$growing.season]), 
                poi.w=lapply(this.poi.w, function(y) y[,this.roll]), 
                params$z) 
                
          }
        }
        
         cat("\n")
        
      } else if (params$method == "hal") {
        
        cat("calculating hal starting with: ")
        for (i in 1:nrow(roll)) {
          cat(roll[i, 1], " ")
          mad <- applyHalThreshold(madMPoints(
            ref.t=lapply(1:nvars, function(x) this.ref.t[[x]][params$growing.season]), 
            poi.t=lapply(1:nvars, function(x) this.poi.t[[x]][,roll[i,]])),
            params$hal.mad)
          mrd <- applyHalThreshold(mrdMPoints(
            ref.t=lapply(1:nvars, function(x) this.ref.t[[x]][params$growing.season]), 
            poi.t=lapply(1:nvars, function(x) this.poi.t[[x]][,roll[i,]])), 
            params$hal.mrd)
          rad <- applyHalThreshold(radMPoints(
            ref.t=lapply(1:nvars, function(x) this.ref.t[[x]][params$growing.season]), 
            poi.t=lapply(1:nvars, function(x) this.poi.t[[x]][,roll[i,]])), 
            params$hal.rad)
          
          # Sum everything up
          this.res.tmp <- do.call('+', mad) + do.call('+', mrd) + do.call('+', rad)
          # cbind each step to the total
          if (i > 1) {
            this.res <- cbind(this.res,ifelse(this.res.tmp >= params$hal.ncond, 1,0))
          } else {
            this.res <- ifelse(this.res.tmp >= params$hal.ncond, 1,0)
          }

        }
      cat("\n")
      } 
      
      return(this.res)
}

# ---------------------------------------------------------------------------- #

applyHalThreshold <- function(obj, th) {
  for (i in 1:length(obj)) {
    if (!is.na(th[[i]])) {
      obj[[i]] <- ifelse(obj[[i]] <= th[[i]], 1, 0)
    } else {
      obj[[i]] <- ifelse(is.na(obj[[i]]), NA, 0)
    }
  }
  return(obj)

}
