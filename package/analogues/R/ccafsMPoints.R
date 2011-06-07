#' Calculate ccafs dissimilarity between points with climate information
#'
#' @param ref.t: vector with values of vars at reference point
#' @param poi.t: matrix with values of vars at poi
#' @param ref.w: vector with values of weights at reference point
#' @param poi.w: matrix with values of weights at poi
#' @param z: number parameter (default 2, equal to euclidean distance)
#' @return A vector of length nrow(poi.t), containing dissimilarity
#' @export

ccafsMPoints <- function(ref.t, poi.t, ref.w, poi.w, z=2) {
  # Packages
  require(stringr)  # for enhance string operations
  
  # Number of variables
  nvars <- length(ref.t)
  ndivisions <- ncol(poi.t[[1]])
  
  # Validity testing
  if (length(ref.t) != length(poi.t)) {
    stop(str_c("ref has ", length(ref.t), " elements and poi has ", 
      length(poi.t), " elements, they need to be equal"))
  }
  
  # TODO list validity
   
  # dissimilarity
  # 1. substract, from each column of the poi, substract the correspongind
  #    reference vale
  tmp <- lapply(1:nvars, function(x) (t(poi.t[[x]]) - ref.t[[x]]))

  # 2. to the power z
  tmp <- lapply(tmp, function(x) x^z)

  # 3. devide weights
  wei <- lapply(1:nvars, function(x) t(poi.w[[x]]) / ref.w[[x]])

  # 4. multiply by weights
  tmp <- lapply(1:nvars, function(x) tmp[[x]] * wei[[x]])

  # 5. sum accross devisions, transpose back
  tmp <- rowSums(do.call(cbind, lapply(tmp,t)))

  # 6. take the zth root
  tmp <- tmp^(1/z)
  
  return(tmp)
}
