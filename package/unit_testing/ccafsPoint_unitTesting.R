ref.t <- list(rain=10)
poi.t <- list(rain=23)

ref.w <- list(rain.w=1)
poi.w <- list(rain.w=1)

ccafsPoint(ref.t, poi.t, ref.w, poi.w, z=2)

ref.t <- list(rain=10, tmean=12)
poi.t <- list(rain=23, tmean=24)

ref.w <- list(rain.w=1,tmean.w=1)
poi.w <- list(rain.w=1,tmean.w=1)

ccafsPoint(ref.t, poi.t, ref.w, poi.w, z=2)

ref.t <- list(rain=rnorm(12), tmean=rnorm(12), soil=rnorm(12))
poi.t <- list(rain=rnorm(12), tmean=rnorm(12), soil=rnorm(12))

ref.w <- list(rain.w=rep(1,12),tmean.w=rep(1,12), soil=rep(1,12))
poi.w <- list(rain.w=rep(1,12),tmean.w=rep(1,12), soil=rep(1,12))

ccafsPoint(ref.t, poi.t, ref.w, poi.w, z=2)