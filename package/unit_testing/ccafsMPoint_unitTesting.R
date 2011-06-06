ref.t <- list(rain=c(10,20,30,40,50))
poi.t <- list(rain=matrix(rnorm(100), ncol=5))

ref.w <- list(rain.w=1)
poi.w <- list(rain.w=rnorm(20))

ccafsMPoints(ref.t, poi.t, ref.w, poi.w, z=2)

# several variables 

ref.t <- list(rain=0.4, tmean=0.24)
poi.t <- list(rain=matrix(rnorm(20), ncol=1), tmean=matrix(rnorm(20), ncol=1))

ref.w <- list(rain.w=1,tmean.w=1)
poi.w <- list(rain.w=matrix(abs(rnorm(20)), ncol=1),tmean.w=matrix(abs(rnorm(20)), ncol=1))

ccafsMPoints(ref.t, poi.t, ref.w, poi.w, z=2)

ref.t <- list(rain=rnorm(12), tmean=rnorm(12), soil=rnorm(12))
poi.t <- list(rain=matrix(rnorm(240), ncol=12), tmean=matrix(rnorm(240), 
  ncol=12), soil=matrix(rnorm(240), ncol=12))

ref.w <- list(rain.w=rep(1,12),tmean.w=rep(1,12), soil=rep(1,12))
poi.w <- list(rain.w=rep(1,12),tmean.w=rep(1,12), soil=rep(1,12))

ccafsMPoints(ref.t, poi.t, ref.w, poi.w, z=2)

# test for 100.000 points
ref.t <- list(rain=rnorm(12), tmean=rnorm(12), soil=rnorm(12))
poi.t <- list(rain=matrix(rnorm(1200000), ncol=12), tmean=matrix(rnorm(1200000), 
  ncol=12), soil=matrix(rnorm(1200000), ncol=12))

ref.w <- list(rain.w=rep(1,12),tmean.w=rep(1,12), soil=rep(1,12))
poi.w <- list(rain.w=rep(1,12),tmean.w=rep(1,12), soil=rep(1,12))

# test for 100.000 points and each point has a weight
ref.t <- list(rain=rnorm(12), tmean=rnorm(12), soil=rnorm(12))
poi.t <- list(rain=matrix(rnorm(1200000), ncol=12), tmean=matrix(rnorm(1200000), 
  ncol=12), soil=matrix(rnorm(1200000), ncol=12))

ref.w <- list(rain.w=rep(1,12),tmean.w=rep(1,12), soil=rep(1,12))
poi.w <- list(rain.w=matrix(rnorm(1200000)),tmean.w=matrix(rnorm(1200000),ncol=12),
  soil=matrix(rnorm(1200000),ncol=12))

system.time(ccafsMPoints(ref.t, poi.t, ref.w, poi.w, z=2))

# test for 1.000.000 points
ref.t <- list(rain=rnorm(12), tmean=rnorm(12), soil=rnorm(12))
poi.t <- list(rain=matrix(rnorm(12000000), ncol=12), 
  tmean=matrix(rnorm(12000000), ncol=12), soil=matrix(rnorm(12000000), ncol=12))

ref.w <- list(rain.w=rep(1,12),tmean.w=rep(1,12), soil=rep(1,12))
poi.w <- list(rain.w=matrix(abs(rnorm(12000000)), ncol=12),
  tmean.w=matrix(abs(rnorm(12000000)), ncol=12), 
  soil=matrix(abs(rnorm(12000000)), ncol=12))

system.time(result.oo <- ccafsMPoints(ref.t, poi.t, ref.w, poi.w, z=2))

# test for 10.000.000 points, could allocate vector of size 915 mb on lenovo laptop
# dell desktop, took a couple of mins to generate matrix, but could not allocateresults

ref.t <- list(rain=rnorm(12), tmean=rnorm(12), soil=rnorm(12))
poi.t <- list(rain=matrix(rnorm(120000000), ncol=12), 
  tmean=matrix(rnorm(120000000), ncol=12), soil=matrix(rnorm(120000000), ncol=12))

ref.w <- list(rain.w=rep(1,12),tmean.w=rep(1,12), soil=rep(1,12))
poi.w <- list(rain.w=rep(1,12),tmean.w=rep(1,12), soil=rep(1,12))

system.time(result <- ccafsMPoints(ref.t, poi.t, ref.w, poi.w, z=2))
