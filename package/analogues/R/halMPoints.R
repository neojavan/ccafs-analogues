#' Method after Hallegatte
#'
#' @param params an object of the class AnalogueParameters
#' @param ... more to come
#' @export
#' @examples
#' ccafs_params <- dissimilarity(x, z, )


# 1. relative annual difference (rad)

radMPoints <- function(ref.t, poi.t) {
  nvars <- length(ref.t)
 
  # sum refernce values
  ref.sum <- lapply(ref.t,sum)   

  # sum projecting values and substract reference values
  rad <- lapply(1:nvars, function(x) abs(rowSums(poi.t[[x]]) - ref.sum[[x]]))

  # devide by summed reference values
  ref.sum <- lapply(ref.sum, function(x) ifelse(x==0,1,x))
  rad <- lapply(1:nvars, function(x) rad[[x]] / ref.sum[[x]]) 
  
  return(rad)
}
 
#' Method after Hallegatte
#'
#' @param params an object of the class AnalogueParameters
#' @param ... more to come
#' @export
#' @examples
#' ccafs_params <- dissimilarity(x, z, )
mrdMPoints <- function(ref.t, poi.t) {
  nvars <- length(ref.t)
  # 2. Mean relative difference between months
  # poi - ref
  mrd <- lapply(1:nvars, function(x) t(poi.t[[x]]) - ref.t[[x]])

  # abs
  mrd <- lapply(mrd, abs)
 
  # account for zeros with devision
  ref.t.corr <- lapply(ref.t, function(x) abs(ifelse(x==0,1,x)))
  
  # devide by ref
  mrd <- lapply(1:nvars, function(x) mrd[[x]] / ref.t.corr[[x]])
 
  # sum over a time period
  mrd <- lapply(mrd, function(x) rowSums(t(x)))

  # devide by number of time periods
  mrd <- lapply(mrd, function(x) x/length(ref.t[[1]]))
  
  return(mrd)
}

#' Method after Hallegatte
#'
#' @param params an object of the class AnalogueParameters
#' @param ... more to come
#' @export
#' @examples
#' ccafs_params <- dissimilarity(x, z, )
madMPoints <- function(ref.t, poi.t) {
  
  # mean absolute difference
  nvars <- length(ref.t)
 
  # subtract ref from poi
  mad <- lapply(1:nvars, function(x) t(poi.t[[x]]) - ref.t[[x]])
      
  # take abs
  mad <- lapply(mad,abs)

  # sum up periods
  mad <- lapply(mad,function(x) rowSums(t(x)))

  # devide by n divisions
  mad <- lapply(mad, function(x) x/length(ref.t[[1]]))
  
  return(mad)
}
