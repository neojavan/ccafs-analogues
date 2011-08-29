#Julian Ramirez-Villegas
#University of Leeds / CIAT / CCAFS
#August 2011

#Uses the params variable to create the needed lists if the users supplies only data 
#for their given points

##################################################
#Creating the list for weights
loadWeightsPoints <- function(params) {
  xy <- data.frame(x=params$x,y=params$y)
  weights.p <- list()
  k <- 1
  for (g in 1:length(params$scenario)) {
    for (w in 1:length(params$weights)) {
      #First verify what to do
      options(warn=2)
      err <- try(as.numeric(params$weights[w]),silent=T)
      #Get the data
      if (class(err) == "try-error") {
        #if it cannot be converted to a number then load it from existing objects
        to.load <- get(paste(params$weights[w],".",params$scenario[g],sep=""))
      } else {
        #if it can be converted then create a matrix with that particular value
        to.load <- matrix(nrow=nrow(xy),ncol=params$ndivisions)
        to.load[] <- as.numeric(params$weights[w])
      }
      options(warn=0)
      #Check for NAs
      nas <- which(is.na(to.load))
      if (length(nas) > 0) {
        stop("analogues: NAs found in ",paste(params$vars[w],".",params$scenario[g],sep=""),". Please check your input data.")
      }
      #get the object into the list
      weights.p[[k]] <- to.load
      k <- k+1
    }
  }
  #Check if length(x) = ncol(input variables)
  if (nrow(weights.p[[1]]) != length(params$x)) {
    stop("analogues: length of x and y in parameters does not coincide with your weights matrices")
  }
  return(weights.p)
}
