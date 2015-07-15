# Exercise 3.6: #
## Understanding the effects of using different variables ##

In this tutorial you will learn:
  1. how to perform analogue runs using different variables in the CCAFS measure,
  1. how to perform analogue runs using different variables in the Hallegatte measure,
  1. the differences between dissimilarities for two different locations with different seasonal patterns,

What do you need to complete this tutorial?
  1. Either a Windows or Linux operating system,
  1. the latest version of R installed on your computer,
  1. to have completed tutorial 3.1 to 3.3 successfully,
  1. to understand the concept of analogues and the underlying principles and equations,


## 1. Perform analogue runs using different variables with the CCAFS measure ##

Sometimes dissimilarities might be more influenced by certain variables than others for certain sites, and this might also alter the degree at which these dissimilarities are relevant to agricultural systems. To test for the influence of using different variables when calculating dissimilarities we will calculate dissimilarities using three different types of variables: monthly temperature (weighted by temperature range), monthly rainfall, and 19 bioclimatic indices. We will use the `"backward"` direction and will calculate dissimilarities.

First, you will now configure an analogue run using the CCAFS method, only with temperature as variable, weighted by diurnal temperature range, with 12 time steps (monthly), no lag, no standardization, using two climate scenarios (current climate and one GCM) and using the same reference site as in exercise 3.4.

```
params <- createParameters(x=87.224,
                           y=26.62,
                           method="ccafs",
                           hal.rad=NA,
                           hal.mad=NA,
                           hal.mrd=NA,
                           z=2,
                           scenario=c("current","a1b_2020_2049_bccr_bcm2_0"),
                           vars=c("tmean"),
                           weights=c("dtr"),
                           ndivisions=12,
                           env.data="C:/analogues/climate-data/current_a1b_2030",
                           ext=".asc",
                           direction="backwd",
                           growing.season=c(1:12),
                           across.year=F,
                           normalise=F,
                           keep.lag=F)

training <- loadData(params)
weights <- loadWeights(params)
```

Note that this time we have put only `"tmean"` in the parameter vars and only `"dtr"` in the parameter weights, whereas all the other parameters keep the same. Now we run the `dissimilarity()` function

```
res1ccafs <- dissimilarity(params,training,weights)
plot(res1ccafs[[1]],col=rainbow(20))
writeRaster(res1ccafs[[1]],
            "C:/analogues/results/3.6.ccafs.temp.asc",
            format='ascii')
```

Now initialize and run for precipitation, with a weight equal to 1,

```
params <- createParameters(x=87.224,
                           y=26.62,
                           method="ccafs",
                           hal.rad=NA,
                           hal.mad=NA,
                           hal.mrd=NA,
                           z=2,
                           scenario=c("current","a1b_2020_2049_bccr_bcm2_0"),
                           vars=c("prec"),
                           weights=c(1),
                           ndivisions=12,
                           env.data="C:/analogues/climate-data/current_a1b_2030",
                           ext=".asc",
                           direction="backwd",
                           growing.season=c(1:12),
                           across.year=F,
                           normalise=F,
                           keep.lag=F)

training <- loadData(params)
weights <- loadWeights(params)

res2ccafs <- dissimilarity(params,training,weights)
plot(res2ccafs[[1]],col=rainbow(20))

writeRaster(res2ccafs[[1]],
            "C:/analogues/results/3.6.ccafs.prec.asc",
            format='ascii')
```

Now we will change our variables to the 19 bioclimatic indices. These variables are only of 1 time step and are 19 variables, as opposed to precipitation or temperature that are only one variable with many time steps. To configure a run with these we will first list the variables, by typing in the R console:

```
bio.list <- paste("bio_",c(1:19),sep="")
```

That will generate a vector of 19 variables, from bio\_1 to bio\_19, as you have for all of your climate scenarios. These variables will be weighted all by 1, so we will create a vector of length equal to 19, and with all elements being 1 by typing

```
wei.list <- rep(1,times=19)
```

Now we will configure our run, by calling the `createParameters()` function. Note that instead of listing manually the variables in the parameter vars we will use our variable `bio.list`, and instead of listing the weights in the parameter weights, we will use our variable `wei.list`.

Note that we use `normalise=T`, since all these variables have different scales, and that we will use `ndivisions=1`, as each bioclimatic index is a variable with only 1 time step (a yearly indicator). To match `ndivisions`, we also need to specify `growing.season=1`, otherwise it would make no sense,

```
params <- createParameters(x=87.224,
                           y=26.62,
                           method="ccafs",
                           hal.rad=NA,
                           hal.mad=NA,
                           hal.mrd=NA,
                           z=2,
                           scenario=c("current","a1b_2020_2049_bccr_bcm2_0"),
                           vars=bio.list,
                           weights=wei.list,
                           ndivisions=1,
                           env.data="C:/analogues/climate-data/current_a1b_2030",
                           ext=".asc",
                           direction="backwd",
                           growing.season=1,
                           across.year=F,
                           normalise=T,
                           keep.lag=F)
```

Now we will load the data and weights, and will run the function `dissimilarity()`, to then plot and store our results,

```
training <- loadData(params)
weights <- loadWeights(params)

res3ccafs <- dissimilarity(params,training,weights)
plot(res3ccafs[[1]],col=rainbow(20))
writeRaster(res3ccafs[[1]],
            "C:/analogues/results/3.6.ccafs.bioclimatic.asc",
            format='ascii')
```

You will notice important differences in the outputs when using different variables (Figure 1).

http://ccafs-analogues.googlecode.com/svn/wiki/img/fig1-3.6.PNG

_**Figure 1** Results of analogue runs with different variables: monthly temperature weighted by diurnal temperature range (top), monthly rainfall (middle) and 19 bioclimatic indices (bottom)_

## 2. Perform analogue runs using different variables with the Hallegatte measure ##

With the Hallegatte measure you cannot include variables with less than 2 time steps and hence you cannot use the bioclimatic indices. Nonetheless, you could separate the effects of rainfall and temperature thresholds. In this case study we will run two dissimilarity analyses, using only the first condition (relative annual difference), but against each of the two variables.

```
params <- createParameters(x=87.224,
                           y=26.62,
                           method="hal",
                           hal.rad=c(NA,0.15),
                           hal.mad=c(NA,NA),
                           hal.mrd=c(NA,NA),
                           z=NA,
                           scenario=c("current","a1b_2020_2049_bccr_bcm2_0"),
                           vars=c("tmean","prec"),
                           weights=c(1,1),
                           ndivisions=12,
                           env.data="C:/analogues/climate-data/current_a1b_2030",
                           ext=".asc",
                           direction="backwd",
                           growing.season=c(1:12),
                           across.year=F,
                           normalise=F,
                           keep.lag=F)

training <- loadData(params)
weights <- loadWeights(params)

res1hal <- dissimilarity(params,training,weights)
writeRaster(res1hal[[1]],
           "C:/analogues/results/3.6.hal.prec.asc",
           format='ascii')
plot(res1hal[[1]])
```

And we now do the same with temperature, we will change the hal.rad parameter in our `params` object, by typing:

```
params$hal.rad <- c(0.15,NA)
```

**Note** that if you were going to switch more conditions ON, that is, increasing the number of Hallegatte conditions, you would need to re-configure the `params` object. Now we run the function (we do not have to re-load the variables data or the weights):

```
res2hal <- dissimilarity(params,training,weights)
plot(res2hal[[1]])
writeRaster(res2hal[[1]],
            "C:/analogues/results/3.6.hal.temp.asc",
            format='ascii')
```

http://ccafs-analogues.googlecode.com/svn/wiki/img/fig2-3.6.PNG

_**Figure 2** Results of analogue runs with different variables with the Hallegatte method: monthly rainfall (top), monthly mean temperature (bottom)_

You will notice that both results have in fact a number of similarities, but also some discrepancies. This all suggest that one should calibrate both methods so that dissimilarities are analyzed with the relevant variables for different sites.
