#' Apply thresholding to araster
#'
#' @param r RasterLayer or RasterStack
#' @param range if threshold should be applied to a range
#' @param best probablity values at which threshold should be cut
#' @return RasterLayer
#' @export
#' @examples
#' ccafs_params <- dissimilarity(x, z, )


applyThreshold <- function(results,range,best) {
  
  if (!missing(range) & !missing(best))
    exit("range and best are mutually exclusive, one needs to be NA")
  
  results.v <- getValues(results)
  
  if (!missing(range)) {
    results.n <- ifelse(results.v >= range[1] & results.v <= range[2], results.v,NA)
  }
  
  if (!missing(best)) {

    if (best > 1) {
      cat("Fraction was probably provided in %, I will devide it by 100\n")
      
      best <- best/100
    }    

    # order the results
    results.v.o <- results.v[!is.na(results.v)]    
    results.v.o <- results.v.o[order(results.v.o)]
    
    # figure out where to break
    where.break	<- ceiling(length(results.v.o) * best)
    threshold <- results.v.o[where.break]

    results.n <- ifelse(results.v >= 0 & results.v <= threshold, results.v,NA)
  }
  
  results <- setValues(results,results.n)
  
  return(results)
  
}

