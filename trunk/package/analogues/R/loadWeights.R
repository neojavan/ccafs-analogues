#' Load weights for grids
#'
#' This functions loads weight grids, if they exist. Otherwise
#' weights are set to 1 or the value specified.
#'
#' @param params an object of the class AnalogueParameters
#' @return an object of class AnalogueWeights
#' @export


loadWeights <- function(params) {
  
  # list with the return values
  rweights <- list()
  
  # path to grids, only division (e.g. month is added later)
  paths <- as.vector(t(outer(str_c(params$climate.data,"/",params$gcms,"_"),
    str_c(params$weights,"_"), FUN="str_c")))
  
  
  for (i in 1:length(paths)) {
    
    # Assuming that if the first file exists, all the others do too
    if (file.exists(str_c(paths[i],"1.asc"))) {
      rweights[[i]] <- loadGridsFromFiles(paths[i],params)
    } else {
      # check for error
      if (i <= length(params$weights)) {
        rweights[[i]] <- params$weights[i]
      } else {
        rweights[[i]] <- params$weights[params$idx.var[i]]
      }
    }
  }
 
 
  return(rweights)
}

