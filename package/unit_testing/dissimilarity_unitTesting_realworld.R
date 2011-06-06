params <- list(x=10, y=53, to=NA, idx.gcms=rep(1:3,each=2), ndivisions=12,
  z=2, across.year=TRUE, keep.lag=FALSE, direction="backward", 
  gcms=c("current", "a1b_2020_2049_miroc3_2_hires", "a1b_2040_2069_gfdl_cm2_1"), 
  growing.season=1:12, method="hal",
  climate.data="/media/DATA/analogues_data/current_a1b_2030_a1b_2050", 
  vars=c("tmean", "prec"), weights=c("dtr",1), ext="asc",
  hal.mad=list(5,NA,NA), hal.rad=list(12,12,12), hal.mrd=list(12,13,12), hal.ncond=5)
  
training <- loadData(params)
weights <- list(1,1,1,1,1,1)

system.time(a <- dissimilarity(params,training, weights))

ref.t <- this.ref.t
ref.w <- this.ref.w
poi.t <- this.poi.t
poi.w <- this.poi.w

# ---------------------------------------------------------------------------- #

rm(list=ls())

source("R/applyThreshold.R")
source("R/ccafsMPoints.R")
source("R/createParameters.R")
source("R/cropInteractive.R")
source("R/dissimilarity.R")
source("R/halMPoints.R")
source("R/loadData.R")
source("R/loadGridsFromFile.R")
source("R/loadWeights.R")
source("R/report.R")
source("R/summary.R")
source("R/util.R")

params <- createParameters(x=10,   
  y=48, 
  to=NA,                       
  method="ccafs",                
  hal.rad=c(NA,0.15),                    
  hal.mad=c(NA,0.3),            
  hal.mrd=c(1,NA),           
  hal.ncond=2,
  z=2,
  gcms=c("current", "a1b_2020_2049_miroc3_2_hires", "a1b_2040_2069_gfdl_cm2_1"),
  vars=c("tmean", "prec"),        
  weights=c("dtr",1),
  ndivisions=12,                  
  climate.data="current_a1b_2030_a1b_2050",              
  ext="asc",
  direction="backwd",             
  growing.season=c(8:12,1:2),           
  across.year=T,
  keep.lag=F,
  normalise=F)
  
# Load data
system.time(training <- loadData(params))
weights <- loadWeights(params)

system.time(res1 <- dissimilarity(params,training, weights))

params$method <- "hal"
res2 <- dissimilarity(params,training, weights)

# summarise different scenarios
res1_sum <- summariseRuns(res1)

# Crop to Area of interest
res1_sum_mean_cropped <- cropInteractive(res1_sum$mean)

res1_sum_mean_cropped_th5p <- applyThreshold(res1_sum_mean_cropped, best=0.05)
res1_sum_mean_cropped_th100 <- applyThreshold(res1_sum_mean_cropped,
range=c(0,100))

# create a report
report(params, list(res1[[1]],
res1[[1]],res1_sum$mean,res1_sum$cv,res1_sum_mean_cropped,res1_sum_mean_cropped_th5p),
c("current", "2040_2069_giss_aom", "mean over 2 gcms", "cv", "mean in
area of interest", "best 5%", "0 - 100"), "run2.pdf")
