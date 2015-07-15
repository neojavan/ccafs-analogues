# Exercise 3.9: #
## Understanding the effects of using different climate scenarios ##

In this tutorial you will learn:
  1. how to measure dissimilarity between different pairs of climate scenarios (i.e. different GCMs) with the CCAFS measure,
  1. how to measure dissimilarity between different pairs of climate scenarios (i.e. different GCMs) with the Hallegatte measure,
  1. the differences between different climate scenario-driven runs and the importance using different GCMs in analogue and climate change research,

What do you need to complete this tutorial?
  1. Either a Windows or Linux operating system,
  1. the latest version of R installed on your computer,
  1. to have completed tutorial 3.1 to 3.3 successfully,
  1. to understand the concept of analogues and the underlying principles and equations,


## 1. Perform analogue runs using two different climate scenarios with the CCAFS measure ##

Up to now we have only used one future climate scenario to look at dissimilarities: `"a1b_2020_2049_bccr_bcm2_0"` and the current climate always as reference climate. However, it is important to acknowledge the variability that can arise from climate change predictions. Climate change predictions are not intended to show how the future would look like in terms of climate but are rather designed to provide a range of possible futures. Thereby, it is of capital importance not to look only to one future climate scenario, but rather to more than one.

Therefore, here we will analyze dissimilarities using two different future climate scenarios. To that aim, we will run analogues using two different future climate scenarios, as predicted by the **UKMO-HadCM3** and the **CCCMA-CGCM3.1-T47** models. We will use monthly means of temperature (weighted by diurnal temperature range), and monthly rainfall, we will neither standardize nor take into account lags.

```
params <- createParameters(x=87.224,
                           y=26.62,
                           method="ccafs",
                           hal.rad=NA,
                           hal.mad=NA,
                           hal.mrd=NA,
                           z=2,
                           scenario=c("current","a1b_2020_2049_ukmo_hadcm3"),
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

ukmo <- dissimilarity(params,training,weights)
plot(ukmo[[1]],col=rainbow(20))
writeRaster(ukmo[[1]],
            "C:/analogues/results/3.9.ccafs.ukmo.asc",
            format='ascii')
```

We will now change the scenario parameter in our `params` object, by typing:

```
params$scenario <- c("current","a1b_2020_2049_cccma_cgcm3_1_t47")
```

And now we run the function (we **have** to re-load the variables data and the weights):

```
training <- loadData(params)
weights <- loadWeights(params)

cccma <- dissimilarity(params,training,weights)
plot(cccma[[1]],col=rainbow(20))
writeRaster(cccma[[1]],
            "C:/analogues/results/3.9.ccafs.cccma.asc",
            format='ascii')
```

You can see the differences in the maps when plotting them (Figure 1). Despite there are considerable similarities between the results, there are also some differences, and some of those occur in the high similarity areas (reddish areas).

http://ccafs-analogues.googlecode.com/svn/wiki/img/fig1-3.9.PNG

_**Figure 1** Effect of different climate scenarios in the CCAFS measure: UKMO-HadCM3 (top), CCCMA-CGCM3.1-T47 (bottom)_

## 2. Perform analogue runs using two different climate scenarios with the Hallegatte measure ##

In the same way as done with the CCAFS measure, we can analyze dissimilarities in different scenarios using the Hallegatte measure. We will load our `params` object with `hal.rad=0.15` for rainfall only switched on:

```
params <- createParameters(x=87.224,
                           y=26.62,
                           method="hal",
                           hal.rad=c(NA,0.15),
                           hal.mad=c(NA,NA),
                           hal.mrd=c(NA,NA),
                           z=NA,
                           scenario=c("current","a1b_2020_2049_ukmo_hadcm3"),
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

ukmo <- dissimilarity(params,training,weights)
writeRaster(ukmo[[1]],
           "C:/analogues/results/3.9.hal.ukmo.asc",
           format='ascii')
plot(ukmo[[1]])
```

We will now change the `scenario` parameter in our `params` object, by typing:

```
params$scenario <- c("current","a1b_2020_2049_cccma_cgcm3_1_t47")
```

And now we run the function (we **have** to re-load the variables data and the weights):

```
training <- loadData(params)
weights <- loadWeights(params)

cccma <- dissimilarity(params,training,weights)
plot(cccma[[1]])
writeRaster(cccma[[1]],
            "C:/analogues/results/3.9.hal.cccma.asc",
            format='ascii')
```

You can see the differences in the maps when plotting them (Figure 2). There are noticeable differences in China and also in Central Africa.

http://ccafs-analogues.googlecode.com/svn/wiki/img/fig2-3.9.PNG

_**Figure 2** Effect of different climate scenarios in the Hallegatte measure: UKMO-HadCM3 (top), CCCMA-CGCM3.1-T47 (bottom)_

You can now understand the importance of using different future climate scenarios for the different measures. The different measures and thresholds are sensitive at different extents to the use of different climate scenarios.