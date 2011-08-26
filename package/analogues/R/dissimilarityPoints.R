#Julian Ramirez-Villegas
#University of Leeds / CIAT / CCAFS
#August 2011

#Calculates dissimilarity for given points, based on a set of parameters

#needs: training.p and weights.p, as explained below
#
#       *training.p is a list of length equal to the number of gcms in params$scenario
#        each element is a matrix of nrow=number of sites, and ncol=params$ndivisions
#        and contains the values of each gcm and variable. So, training.p[[1]] would
#        be the first gcm and first variable (commonly current and tmean)
#
#       *weights.p is a list of length equal to the number of gcms in params$scenario
#        each element is a matrix of nrow=number of sites, and ncol=params$ndivisions
#        and contains the values of each gcm and weight. So, weights.p[[1]] would
#        be the first gcm and first weight (commonly current and dtr)

#from: Reference! if backwd this is GCMs, if fwd this would be current
#to: Target! if backwd this is current, if fwd this is GCMs

dissimilarityPoints <- function(params,training.p,weights.p=NA) {
  #Normalise if neeeded
  if (params$normalise) {
    training.pn <- list()
    #cat("normalising \n")
    #first normalise 
    for (i in 1:length(params$vars)) {
      #This would scale the variable for all the scenarios, and all months
      a <- scale(unlist(training.p[which(params$idx.vars==i)]))
      #This would know where to put the new data (position in the list)
      which.put <- which(params$idx.vars==i)
      where.start <- seq(1,by=length(a)/length(which.put), length.out=length(which.put)*params$ndivisions)
      
      for (j in 1:length(which.put)) {
        training.pn[[which.put[j]]] <- matrix(a[where.start[j]:(where.start[j]+(length(a)/length(which.put)-1)), ], ncol=params$ndivisions, byrow=FALSE)
      }
    }
    training.p <- training.pn; rm(training.pn)
  }
  #Now proceed with the dissimilarity calculation
  if (params$direction == "none" | params$direction == "no" | is.na(params$direction)) {
    to.varlist <- which(params$idx.gcms == 1)
    from.varlist <- which(params$idx.gcms == 1)
    #Calling function over points (avoid user doing all this painful *apply stuff)
    all.results <- callDissimilarityPoints(params,from=1,to=1,training.p,weights.p)
  } else {
    if (params$direction == "backward" | params$direction == "backwd" | params$direction == "b") {
      times <- length(params$scenario)-1
      #Looping through the pairs of comparisons to be done
      for (iter in 1:times) {
        to <- 1
        from <- iter+1
        #Calling function over points (avoid user doing all this painful *apply stuff)
        result <- callDissimilarityPoints(params,from=from,to=to,training.p,weights.p)
        #Getting each comparison as a column into a broad results list, in which
        #each of the elements is a data frame with nrows=number of points, and
        #each column is each of the comparisons
        if (iter == 1) {
          all.results <- result
        } else {
          #When iter>1 loop through points to get into data frames and assign the 
          #new result to a new column in each of the elements of the list
          for (i in 1:length(all.results)) {
            all.results[[i]] <- data.frame(all.results[[i]],result[[i]])
          }
        }
      }
      #Naming all the individual comparisons in the data frames of the output list
      if (times > 1) {
        for (i in 1:length(all.results)) {
          names(all.results[[i]]) <- params$scenario[2:length(params$scenario)]
        }
      }
    } else if (params$direction == "forward" | params$direction == "forwd" | params$direction == "f") {
      times <- length(params$scenario)-1
      #Looping through the pairs of comparisons to be done
      for (iter in 1:times) {
        to <- i+1
        from <- 1
        #Calling function over points (avoid user doing all this painful *apply stuff)
        result <- callDissimilarityPoints(params,from=from,to=to,training.p,weights.p)
        #Getting each comparison as a column into a broad results list, in which
        #each of the elements is a data frame with nrows=number of points, and
        #each column is each of the comparisons
        if (iter == 1) {
          all.results <- result
        } else {
          #When iter>1 loop through points to get into data frames and assign the 
          #new result to a new column in each of the elements of the list
          for (i in 1:length(all.results)) {
            all.results[[i]] <- data.frame(all.results[[i]],result[[i]])
          }
        }
      }
      #Naming all the individual comparisons in the data frames of the output list
      if (times > 1) {
        for (i in 1:length(all.results)) {
          names(all.results[[i]]) <- params$scenario[2:length(params$scenario)]
        }
      }
    }
  }
  return(all.results)
}
################################################################################


################################################################################
#Function to apply dissimilarity over a number of points
callDissimilarityPoints <- function(params,from,to,training.p,weights.p=NA)  {
  #Required input: xy, params, from, to
  #create matrix from x and y in params object
  xy <- data.frame(x=params$x,y=params$y)
  res.dis <- list()
  #Locations in the lists of data
  to.varlist <- which(params$idx.gcms == to)
  from.varlist <- which(params$idx.gcms == from)
  #Create roll (it would take into account whether lag is being accounted)
  roll <- rollCreate(params)
  #Loop through points
  for (p in 1:nrow(xy)) {
    #Apply over the roll possiblities
    for (r in 1:nrow(roll)) {
      #Get the roll
      this.roll <- roll[r,]
      #Get stuff ready for ccafsMPoints
      #Reference point training data
      ref.training <- list() #1:2 in training or weights refer to current
      k <- 1
      for (i in from.varlist) {
        ref.training[[k]] <- training.p[[i]][p,params$growing.season]
        k <- k+1
      }
      #Target points training data
      poi.training <- list()
      k <- 1
      for (i in to.varlist) {
        poi.training[[i]] <- training.p[[i]][,this.roll]
        k <- k+1
      }
      #only get weights for the ccafs method
      if (params$method == "ccafs") {
        #Reference point weights
        ref.weights <- list()
        k <- 1
        for (i in from.varlist) {
          ref.weights[[i]] <- weights.p[[i]][p,params$growing.season] #1 refers to the first point (n)
          k <- k+1
        }
        #Target points weights
        poi.weights <- list()
        k <- 1
        for (i in to.varlist) {
          poi.weights[[i]] <- weights.p[[i]][,this.roll]
          k <- k+1
        }
      }
      #calculating dissimilarities
      if (params$method == "ccafs") {
        this.dis <- ccafsMPoints(ref.t=ref.training,poi.t=poi.training,
                                 ref.w=ref.weights,poi.w=poi.weights,z=2)
      } else if (params$method == "hal") {
        rad <- radMPoints(ref.t=ref.training,poi.t=poi.training)
        mrd <- mrdMPoints(ref.t=ref.training,poi.t=poi.training)
        mad <- madMPoints(ref.t=ref.training,poi.t=poi.training)
        
        #now apply Hallegatte's threshold for relative annual dif. (rad)
        rad <- applyHalThresholdPoints(rad,params$hal.rad)
        #now apply Hallegatte's threshold for mean monthly rel. dif. (mrd)
        mrd <- applyHalThresholdPoints(mrd,params$hal.mrd)
        #now apply Hallegatte's threshold for mean absolute difference (mad)
        mad <- applyHalThresholdPoints(mad,params$hal.mad)
        
        this.dis <- do.call('+', mad) + do.call('+', mrd) + do.call('+', rad)
      }
      if (r == 1) {
        this.rolled <- matrix(this.dis,nrow=nrow(xy))
      } else {
        this.rolled <- cbind(this.rolled,this.dis)
      }
    }
    if (params$method == "ccafs") {
      res.dis[[p]] <- apply(this.rolled,1,min)
    } else if (params$method == "hal") {
      this.rolled <- apply(this.rolled,1,sum)
      res.dis[[p]] <- ifelse(this.rolled > 0,1,0)
    }
  }
  return(res.dis)
}
################################################################################

applyHalThresholdPoints <- function(obj, th) {
  for (i in 1:length(obj)) {
    if (!is.na(th[[i]])) {
      obj[[i]] <- ifelse(obj[[i]] <= th[[i]], 1, 0)
    } else {
      obj[[i]] <- ifelse(is.na(obj[[i]]), NA, 0)
    }
  }
  return(obj)
}


################################################################################
#Create roll by default
rollCreate <- function(params) {
  if (params$across.year) {
    roll.v <- c()
    months <- 1:params$ndivisions
    for (i in 1:length(months)) {
       roll.v <- c(roll.v,(months[c(i:length(months),0:(i-1))]))  
    }
    roll <- matrix(data=roll.v, ncol=length(months), byrow=TRUE)
    # cut roll to the actual growin period
    roll <- roll[ , params$growing.season]
  } else {
    roll <- matrix(1:12,ncol=params$ndivisions,nrow=1)
  }
  return(roll)
}
################################################################################
