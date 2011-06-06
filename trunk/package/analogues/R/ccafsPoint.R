ccafsPoint <- function(ref.t, poi.t, ref.w, poi.w, z=2) {
  # Computes distance between one reference point and one other points
  #
  # Args:
  #  ref.t: vector with values of vars at reference point
  #  poi.t: vector with values of vars at poi
  #  ref.w: vector with values of weights at reference point
  #  poi.w: vector with values of weights at poi
  #  z: number parameter (default 2, equal to euclidean distance)
  #
  # Returns:
  #  A vector of length 1, containing dissimilarity
  
  # Packages
  require(stringr)  # for enhance string operations
  
  # Number of variables
  nvars <- length(ref.t)
  
  # Validity testing
  if (length(ref.t) != length(poi.t)) {
    stop(str_c("ref has ", length(ref.t), " elements and poi has ", 
      length(poi.t), " elements, they need to be equal"))
  }
  
  # TODO list validity
   
  # dissimilarity
  # 1. substract
  tmp <- lapply(1:nvars, function(x) poi.t[[x]] - ref.t[[x]])
   
  # 2. to the power z
  tmp <- lapply(tmp, function(x) x^z)
   
  # 3. devide weights
  wei <- lapply(1:nvars, function(x) ref.w[[x]] / poi.w[[x]])
   
  # 4. multiply by weights
  tmp <- lapply(1:nvars, function(x) wei[[x]] * tmp[[x]])
   
  # 5. sum accross devisions
  tmp <- do.call(sum, tmp)
   
  # 6. take the zth root
  tmp <- tmp^(1/z)
  
  return(tmp)
}