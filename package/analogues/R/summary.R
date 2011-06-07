#' Summarises Results
#'
#' @param results a list with rasters, they all need to have the same extent
#' @param do.mean calculate the mean over all rasters in the list
#' @param do.std calcualte the sd over all rasters in the list
#' @param do.cv calculate the coefficient of variance over all rasters in the list
#' @return prints a report and save it as pdf
#' @value a list
#' @export
#' @examples
#' summary(list(restuls[[1]], results[[3]], results[[2]]), TRUE,TRUE,TRUE)


summariseRuns <- function(results, do.mean=TRUE,do.std=TRUE,do.cv=TRUE) {
  
  # get predicted dissimilarities
  results.sum <- list()
  
  results <- do.call(stack, results)
        
  if (do.mean | do.cv) {
    cat("calculating mean dissimilarity \n")
    results.sum$mean <- stackApply(results, indices=rep(1,nlayers(results)),fun=mean)
  }
     
  if (do.std | do.cv) {
    cat("calculating sd of dissimilarity \n")
    results.sum$std <- stackApply(results,indices=rep(1,nlayers(results)),fun=sd)
  }
  
  if (do.cv) {
    cat("calculating coefficient of variance \n")
    results.sum$cv <- results.sum$std / results.sum$mean * 100
  }
  
  return(results.sum)
}
