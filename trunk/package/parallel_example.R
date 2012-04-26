library(snowfall)

sfInit(parallel=T,cpus=3)

constant <- 0.001
sfExport("constant")

library(raster)

rs <- raster(nrow=2160,ncol=4320)
rs[] <- rnorm(ncell(rs),0,1)


fun <- function(x) {
  if (is.na(x)) {
    y <- NA
  } else {
    y <- x*constant
  }
  return(y)
}

b <- sfSapply(as.vector(rs[]),fun)
