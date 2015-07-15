# Exercise 3.5: #
## Understanding the effects of location ##

In this tutorial you will learn:
  1. how to perform analogue runs using different reference points in the CCAFS measure,
  1. how to perform analogue runs using different reference points in the Hallegatte measure,
  1. the differences between dissimilarities for two different locations with different seasonal patterns,

What do you need to complete this tutorial?
  1. Either a Windows or Linux operating system,
  1. the latest version of R installed on your computer,
  1. to have completed tutorial 3.1 to 3.3 successfully,
  1. to understand the concept of analogues and the underlying principles and equations,


## 1. Perform analogue runs for two locations with different seasonal patterns using the CCAFS measure ##

To the moment, you have learnt how to run analogues in different directions, using a target site and a pair of climate scenarios; however, it is also interesting to analyze dissimilarities in different climate types. To do that we will take the same site as in exercise 3.3 (Himalaya), but we will also perform the analysis with another site in the very core of the Amazon forest (longitude= -67.1 and latitude= -1.7).

We will use mean temperatures (weighted by diurnal temperature range) and total rainfall at a monthly level, and perform a `"backward"` analysis based on current climate and only 1 GCM. We will use the CCAFS measure.

First, we will create our `params` object, we will then load the variables data and the weights data for our Himalayan location by typing:

```
params.him <- createParameters(x=87.224,
                               y=26.62,
                               method="ccafs",
                               hal.rad=NA,
                               hal.mad=NA,
                               hal.mrd=NA,
                               z=2,
                               scenario=c("current","a1b_2020_2049_bccr_bcm2_0"),
                               vars=c("tmean","prec"),
                               weights=c("dtr",1),
                               ndivisions=12,
                               env.data="C:/analogues/climate-data/current_a1b_2030",
                               ext=".asc",
                               direction="backwd",
                               growing.season=c(1:12),
                               across.year=F,
                               normalise=F,
                               keep.lag=F)

training <- loadData(params.him)
weights <- loadWeights(params.him)
```

Note that the name of the usual `params` object was changed by `params.him`; this was done to differentiate the Himalayas run from the Amazon run. Now we will perform the `dissimilarity()` function, plot and save our resultant raster using this configuration,

```
him <- dissimilarity(params.him,training,weights)
plot(him[[1]],col=rainbow(20),horizontal=T)
points(params.him$x,params.him$y)
writeRaster(him[[1]],
           "C:/analogues/results/3.5.ccafs.himalaya.asc",
           format='ascii')
```

To perform our Amazon run we will first copy the `params.him` into an object named `params.ama`, by typing in the R console:

```
params.ama <- params.him
```

Now we need to replace the Himalayas coordinates (`x` and `y` parameters in `params.ama`) by the actual coordinates of our site in the Amazon by:

```
params.ama$x <- -67.1
params.ama$y <- -1.7
```

And now we will perform the `dissimilarity()` function, store the results and plot them,

```
ama <- dissimilarity(params.ama,training,weights)
plot(ama[[1]],col=rainbow(20),horizontal=T)
points(params.ama$x,params.ama$y)
writeRaster(ama[[1]],
           "C:/analogues/results/3.5.ccafs.amazon.asc",
           format='ascii')
```

You will note the differences in the outputs (Figure 1). Reddish areas in each figure are areas very analogous to the target points in the Himalayas (Figure 1 top) and the Amazon (Figure 1 bottom), respectively.

http://ccafs-analogues.googlecode.com/svn/wiki/img/fig1-3.5.PNG

_**Figure 1** Dissimilarity result for a location in the Himalayas (top figure) and the Amazon (bottom) using the CCAFS measure. Reference points are marked with black circles_

## 2. Perform analogue runs for two locations with different seasonal patterns using the Hallegatte measure ##

Now we can do the same as we just, but this time using the Hallegatte measure. We will use the same reference sites (Himalayas, Amazon), and we will use only one of the Hallegatte conditions, namely, the relative annual difference (`hal.rad`), and we will set its threshold to 0.15, and we will use it only for rainfall. We first initialize the parameters object, and load the variables data and weights,

```
params.him <- createParameters(x=87.224,
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

training <- loadData(params.him)
weights <- loadWeights(params.him)
```

Now we run the `dissimilarity()` function, store and plot the results,

```
him <- dissimilarity(params.him,training,weights)
plot(him[[1]],horizontal=T)
points(params.him$x,params.him$y)
writeRaster(him[[1]],
           "C:/analogues/results/3.5.hallegatte.himalaya.asc",
           format='ascii')
```

Now we copy the parameters into the `params.ama` object and we do the same as before, but using the Hallegatte measure, for the Amazon

```
params.ama <- params.him
params.ama$x <- -67.1
params.ama$y <- -1.7
```

Now we run the `dissimilarity()` function with this configuration, plot the results and store them,

```
ama <- dissimilarity(params.ama,training,weights)
plot(ama[[1]])
points(params.ama$x,params.ama$y)
writeRaster(ama[[1]],
            "C:/analogues/results/3.5.hallegatte.amazon.asc",
			format='ascii')
```

These two results can be seen in Figure 2.

http://ccafs-analogues.googlecode.com/svn/wiki/img/fig2-3.5.PNG

_**Figure 2** Dissimilarity result for a location in the Himalayas (top figure) and the Amazon (bottom) using the Hallegatte measure. Reference points are marked with black circles_


You can note the difference between the two results, but also some similarities, mostly found in some areas of the Pacific Islands.
