#' Load seperate grids 
#'
#' This functions seperate grids and returns a raster stack
#'
#' @param params an object of the class AnalogueParameters
#' @param path path to the file
#' @keywords manip
#' @return an object of class RasterStack
#' @export

loadGridsFromFiles <- function(path,params) {

  cat(str_c("loading ",str_sub(path,1,-2)," \n"))
  grid <- do.call(stack,lapply(str_c(path, 1:params$ndivisions, ".",params$ext),
    raster))
  
  return(grid)
}
