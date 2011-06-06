#' Load data for the dissimilarity calculations
#'
#' This functions loads the dissimilarity data.
#'
#' @param params an object of the class AnalogueParameters
#' @keywords manip
#' @return an object of AnalogueTraining
#' @export
#' @examples
#' ccafs_params <- createParams(x, z, )

loadData <- function(params) {
  
  # list to hold all the data
  training <- list() 
  
  # get paths for rasters, if there are gcms as well
  # add the ndivisions in the next (level loadGridsFromFiles function, so 
  # that its easy to build stacks
    
  paths <- as.vector(t(outer(str_c(params$climate.data,"/",params$gcms,"_"),
    str_c(params$vars,"_"), FUN="str_c")))
      
  # load data
  training <- lapply(paths, function(x) loadGridsFromFiles(x,params))
  training <- lapply(training, readAll)
        
  return(training)
}
