# Exercise 3.8: #
## Understanding the effects of growing season ##

In this tutorial you will learn:
  1. how to limit your analyses to a desired growing season with the CCAFS measure,
  1. how to limit your analyses to a desired growing season with the Hallegatte measure,
  1. the importance of focusing in a particular growing season from an agriculture perspective,

What do you need to complete this tutorial?
  1. Either a Windows or Linux operating system,
  1. the latest version of R installed on your computer,
  1. to have completed tutorial 3.1 to 3.3 successfully,
  1. to understand the concept of analogues and the underlying principles and equations,


## 1. Perform analogue runs using a whole period round and a shorter (growing) period with the CCAFS measure ##

As the analogue tool is intended to be a method to be used by agricultural researchers for climate adaptation, its application is subjected to different aspects of agriculture. Up to now, we have performed all our analyses using a period of 12 months (a whole year round); however, crops grow in shorter periods and it is often very important to look at dissimilarities for a particular growing period.

We will test the differences of focusing in a particular growing period, against using a whole-year data. We will use the same site as in our last exercise, and will use mean temperature (weighted by diurnal temperature range), and rainfall (weighted by 1) for 12 months, first with a growing season of 1 to 12, and then with a growing season from May to October (5 to 10). We will not normalize our input data, and will run a `"backward"` analysis with current climate and 1 GCM.

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

res1ccafs <- dissimilarity(params,training,weights)
plot(res1ccafs[[1]],col=rainbow(20))
writeRaster(res1ccafs[[1]],
            "C:/analogues/results/3.8.ccafs.wholeGS.asc",
            format='ascii')
```

We now do the same with but this time we will take climate lag into account, we will change the `growing.season` parameter on in our `params` object, by typing:

```
params$growing.season <- c(5:10)
```

And now we run the function (we do not have to re-load the variables data or the weights):

```
res2ccafs <- dissimilarity(params,training,weights)
plot(res2ccafs[[1]],col=rainbow(20))
writeRaster(lagged[[1]],
            "C:/analogues/results/3.8.ccafs.shortGS.asc",
            format='ascii')
```

You can see the differences in the maps when plotting them (Figure 1). You will notice that now areas in the southern hemisphere have become reddish when lag is accounted. This is because they have similar climates but at different time of the year (i.e. different seasonal pattern).

http://ccafs-analogues.googlecode.com/svn/wiki/img/fig1-3.8.PNG

_**Figure 1** Effect of growing season length in the CCAFS dissimilarity measure: whole year (top), growing season between May and October (bottom)_

You can observe that dissimilarities in some areas decrease. In the same way you could test more growing periods, if you had, for instance, different crops.

## 2. Perform analogue runs using a whole period round and a shorter (growing) period with the Hallegatte measure ##

In the same way as done with the CCAFS measure, growing season can be changedwhen using the Hallegatte measure. We will use monthly total rainfall, thresholded at a fraction of 0.15 difference on the relative annual difference (`hal.rad` parameter), for the same scenarios as in the previous exercise. First, with growing.season equal to the whole year, we configure and perform our run:

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
           "C:/analogues/results/3.8.hal.wholeGS.asc",
           format='ascii')
plot(res1hal[[1]])
```

We now do the same with but this time we will modify the growing season, we will change the `growing.season` parameter on in our `params` object, by typing:

```
params$growing.season <- c(5:10)
```

And we now run the function (we do not have to re-load the variables data or the weights):

```
res2hal <- dissimilarity(params,training,weights)
plot(res2hal[[1]])
writeRaster(res2hal[[1]],
            "C:/analogues/results/3.8.hal.shortGS.asc",
            format='ascii')
```

You can see the differences in the maps when plotting them (Figure 2). You will notice that now areas in the southern hemisphere have become reddish when lag is accounted. This is because they have similar climates but at different time of the year (i.e. different seasonal pattern).

http://ccafs-analogues.googlecode.com/svn/wiki/img/fig2-3.8.PNG

_**Figure 2** Effect of growing season in the Hallegatte dissimilarity measure: whole year (top), growing season between May and October (bottom)_

You can now understand the importance of taking into account crop-specific growing seasons for each of the measures available in the analogues R-package.