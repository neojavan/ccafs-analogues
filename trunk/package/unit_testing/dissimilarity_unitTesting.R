# Calcualte Dissimilarity

params <- list(x=10, y=23, to=NA, idx.gcms=rep(1:3,each=2), ndivisions=12,
  z=2, across.year=TRUE, keep.lag=FALSE, direction="backward", 
  gcms=c("cur", "a1", "a2"), growing.season=1:12)

training <- replicate(6,rstack(12))
weights <- replicate(6,rstack(12))

a <- dissimilarity(params,training, weights)

# alter params
params <- list(x=10, y=23, to=NA, idx.gcms=rep(1:2,each=2), ndivisions=12,
  z=2, across.year=TRUE, keep.lag=FALSE, direction="forward", 
  gcms=c("cur", "a1"), growing.season=8:2)
  
a <- dissimilarity(params,training, weights)  

# CalcCcafsDissimilarity
# create data
params <- list(x=10, y=23, to=NA, idx.gcms=rep(1:2,each=2), ndivisions=12,
  z=2, across.year=TRUE, keep.lag=FALSE, direction="current")

training <- replicate(4,rstack(12))
weights <- replicate(4,rstack(12))

poi.t <- lapply(training, getValues)

poi.w <- lapply(weights, 
      function(x) {
        if (is.numeric(x)) {
          # we can make use of training[[1]]
          return(setValues(training[[1]],rep(x,ncell(training[[1]]))))
        } else {
          return(getValues(x))
        }
    })

callCcafsMpoints(params, training, weights, poi.t, poi.w, 1, 1, matrix(1:12, ncol=12))

# helper
rstack <- function(n) {
  do.call(stack, list(replicate(n,setValues(raster(), abs(rnorm(64800))))))
}