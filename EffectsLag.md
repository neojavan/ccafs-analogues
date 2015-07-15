# Exercise 3.7: #
## Understanding the effects of lag ##

In this tutorial you will learn:
  1. how to account for lag and not when performing analogue runs with the CCAFS method,
  1. how to account for lag and not when performing analogue runs with the Hallegatte method,
  1. the differences between lagged and non-lagged dissimilarities and the importance of lag,

What do you need to complete this tutorial?
  1. Either a Windows or Linux operating system,
  1. the latest version of R installed on your computer,
  1. to have completed tutorial 3.1 to 3.3 successfully,
  1. to understand the concept of analogues and the underlying principles and equations,


## 1. Perform analogue runs using lagged and non-lagged configurations for the CCAFS measure ##

As you might know, we have created a method that is capable of surpassing the difficulties when comparing sites with different seasonal patterns. This is done by means of a strategy named "lagging". When accounting to lag you rotate the target climate in order to match your reference climate. In our method, we calculate dissimilarity by rotating the target climate `ndivisions` times and then take the minimum dissimilarity as the result.

We will test the differences when accounting or not to lag. We will use the same site as in our last exercise, and will use mean temperature (weighted by diurnal temperature range), and rainfall (weighted by 1) for 12 months, with a growing season of 1 to 12. We will not normalize our input data, and will run a `"backward"` analysis with current climate and 1 GCM. First we create the `params` object with `across.year=F`, and calculate dissimilarities using that object,

```
params <- createParameters(x=87.224,
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

training <- loadData(params)
weights <- loadWeights(params)

nolag <- dissimilarity(params,training,weights)
plot(nolag[[1]],col=rainbow(20))
writeRaster(nolag[[1]],
            "C:/analogues/results/3.7.ccafs.nolag.asc",
            format='ascii')
```

We now do the same with but this time we will take climate lag into account, we will switch the `across.year` parameter on in our `params` object, by typing:

```
params$across.year <- T
```

```
lagged <- dissimilarity(params,training,weights)
plot(lagged[[1]],col=rainbow(20))
writeRaster(lagged[[1]],
            "C:/analogues/results/3.7.ccafs.lagged.asc",
            format='ascii')
```

You can see the differences in the maps when plotting them (Figure 1). You will notice that now areas in the southern hemisphere have become reddish when lag is accounted. This is because they have similar climates but at different time of the year (i.e. different seasonal pattern).

http://ccafs-analogues.googlecode.com/svn/wiki/img/fig1-3.7.PNG

_**Figure 1** Effect of lag in the CCAFS dissimilarity measure: without lag (top), with lag (bottom)_

## 2. Perform analogue runs using lagged and non-lagged configurations for the Hallegatte measure ##

In the same way as done with the CCAFS measure, lag can be accounted when using the Hallegatte measure. We will use monthly total rainfall, thresholded at a fraction of 0.5 difference on the mean absolute relative monthly difference (`hal.mrd` parameter), for the same scenarios as in the previous exercise. First, with `across.year=F`, we configure and perform our run:

```
params <- createParameters(x=87.224,
                           y=26.62,
                           method="hal",
                           hal.rad=c(NA,NA),
                           hal.mad=c(NA,NA),
                           hal.mrd=c(NA,0.5),
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

nolag <- dissimilarity(params,training,weights)
writeRaster(nolag[[1]],
           "C:/analogues/results/3.7.hal.nolag.asc",
           format='ascii')
plot(nolag[[1]])
```

We now do the same with but this time we will take climate lag into account, we will switch the `across.year` parameter on in our `params` object, by typing:

```
params$across.year <- T
```

And we now run the function (we do not have to re-load the variables data or the weights):

```
lagged <- dissimilarity(params,training,weights)
plot(lagged[[1]])
writeRaster(lagged[[1]],
            "C:/analogues/results/3.7.hal.lagged.asc",
            format='ascii')
```

You can see the differences in the maps when plotting them (Figure 2). You will notice that now areas in the southern hemisphere have become reddish when lag is accounted. This is because they have similar climates but at different time of the year (i.e. different seasonal pattern).

http://ccafs-analogues.googlecode.com/svn/wiki/img/fig2-3.7.PNG

_**Figure 2** Effect of lag in the Hallegatte dissimilarity measure: without lag (top), with lag (bottom)_

You can now understand the importance of taking into account seasonal differences, particularly when looking at crop adaptation in the context of climate change.