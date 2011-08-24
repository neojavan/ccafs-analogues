#Julian Ramirez-Villegas
#University of Leeds / CIAT / CCAFS
#August 2011

#Uses the params variable to create the needed lists if the users supplies only data 
#for their given points

##################################################
loadDataPoints <- function(params) {
  #Creating the list for training
  training.p=list()
  k <- 1
  for (g in 1:length(params$scenario)) {
    for (v in 1:length(params$vars)) {
      #Load the data
      to.load <- get(paste(params$vars[v],".",params$scenario[g],sep=""))
      #Check for NAs
      nas <- which(is.na(to.load))
      if (length(nas) > 0) {
        stop("NAs found in ",paste(params$vars[v],".",params$scenario[g],sep=""),". Please check your input data.")
      }
      #get the object into the list
      training.p[[k]] <- to.load
      k <- k+1
    }
  }
  return(training.p)
}
