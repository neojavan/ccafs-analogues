ref.t <- list(rain=0.2)
poi.t <- list(rain=matrix(rnorm(20), ncol=1))

ref.w <- list(rain.w=1)
poi.w <- list(rain.w=1)

ccafsMPoints(ref.t, poi.t, ref.w, poi.w, z=2)

ref.t <- list(rain=0.4, tmean=0.24)
poi.t <- list(rain=matrix(rnorm(20), ncol=1), tmean=matrix(rnorm(20), ncol=1))

ref.w <- list(rain.w=1,tmean.w=1)
poi.w <- list(rain.w=1,tmean.w=1)

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

system.time(ccafsMPoint(ref.t, poi.t, ref.w, poi.w, z=2))

# test for 1.000.000 points
ref.t <- list(rain=rnorm(12), tmean=rnorm(12), soil=rnorm(12))
poi.t <- list(rain=matrix(rnorm(12000000), ncol=12), 
  tmean=matrix(rnorm(12000000), ncol=12), soil=matrix(rnorm(12000000), ncol=12))

ref.w <- list(rain.w=rep(1,12),tmean.w=rep(1,12), soil=rep(1,12))
poi.w <- list(rain.w=rep(1,12),tmean.w=rep(1,12), soil=rep(1,12))

system.time(result <- ccafsMPoints(ref.t, poi.t, ref.w, poi.w, z=2))

# test for 10.000.000 points, could allocate vector of size 915 mb
