# Issue 3

library(analogues)

params <- createParameters(x=10,   
  y=48, 
  to=NA,                       
  method="ccafs",                
  hal.rad=c(NA,0.15),                    
  hal.mad=c(NA,0.3),            
  hal.mrd=c(1,NA),           
  hal.ncond=2,
  z=2,
  gcms=c("current"),
  vars=c("bio1"),        
  weights=c(1),
  ndivisions=1,                  
  climate.data="/home/johannes/12_work/analogue_rewrite/current_a1b_2030_a1b_2050",              
  ext="asc",
  direction="no",             
  growing.season=1,           
  across.year=T,
  keep.lag=F,
  normalise=F)
  
# Load data
system.time(training <- loadData(params))
weights <- loadWeights(params)

system.time(res1 <- dissimilarity(params,training, weights))

# Issue 4
library(analogues)

params <- createParameters(x=10,   
  y=48, 
  to=matrix(c(10,20,30,50,51,52), ncol=2,byrow=F),                       
  method="ccafs",                
  hal.rad=c(NA,0.15),                    
  hal.mad=c(NA,0.3),            
  hal.mrd=c(1,NA),           
  hal.ncond=2,
  z=2,
  gcms=c("current"),
  vars=c("bio1"),        
  weights=c(1),
  ndivisions=1,                  
  climate.data="/home/johannes/12_work/analogue_rewrite/current_a1b_2030_a1b_2050",              
  ext="asc",
  direction="no",             
  growing.season=1,           
  across.year=T,
  keep.lag=F,
  normalise=F)
  
# Load data
system.time(training <- loadData(params))
weights <- loadWeights(params)

system.time(res1 <- dissimilarity(params,training, weights))
