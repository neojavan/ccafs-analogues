# Exercise 3.10: #
## Selecting candidate analogue sites: thresholding ##

In this tutorial you will learn:
  1. how to select the most likely analogue sites from a particular CCAFS measure analogue run using a range of values,
  1. how to select the most likely analogue sites from a particular Hallegatte  measure analogue run using a probability threshold,
  1. understand the importance of a well-calibrated way of selecting "likely" analogue sites,

What do you need to complete this tutorial?
  1. Either a Windows or Linux operating system,
  1. the latest version of R installed on your computer,
  1. to have completed tutorial 3.1 to 3.3 successfully,
  1. to understand the concept of analogues and the underlying principles and equations,


## 1. Perform a run and select likely analogues using a range of values ##

One very important aspect when analyzing dissimilarities is related to the question **"how analogue is an analogue?"**, that is, **"how similar should a site be in order to be considered analogous to another site?"** Answering that question is not as easy as one would like and hence one has to rely on subjective judgments of the dissimilarity index in order to find the target analogue sites.

As the Hallegatte measure is already based on thresholds, the exercise of thresholding explained in this tutorial is only related to the CCAFS measure. Visually inspecting the dissimilarity results that we have seen in all the previous training materials (those that arise from the CCAFS dissimilarity) one might think that the reddish areas should be considered as candidate analogue sites; however, numerically inspecting those areas would show that there is a significant variability, and that it is therefore difficult to set a threshold at which to cut off the values and hence select **"true"** analogous areas.

In the analogues R-package we have implemented two different ways of thresholding. Here we are testing first the range-based thresholding, in which you provide a minimum and a maximum value and discard all values outside that range. These thresholding capabilities are implemented through the function `applyThreshold()`, whose arguments are:

| **Variable** | **Description** |
|:-------------|:----------------|
| results      |	Either a `RasterLayer` or a `RasterStack with` the rasters to be thresholded. |
| range        |	A vector of length 2 containing the minimum and maximum values that are to be kept. Any values outside that range will be set as `NA` in the `RasterLayers`.  |
| best         |	A numeric value, either as a percentage or as a fraction (<1) indicating the fraction of pixels (quantile in the probability distribution of dissimilarity values) to be retained. Pixels to be retained will be the closest pixels at the fraction specified. |

Now we set up our analogues run, using the CCAFS method, for mean monthly temperatures (weighted by diurnal temperature range), and total monthly rainfall, 12 months. We will not standardize and will not take into account the lag and will use the current climate and one future scenario in a backward-type analysis. We will use UKMO-HadCM3 as our GCM. You will type in the R console:

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

result <- dissimilarity(params,training,weights)
```

We are not interested in plotting or saving these results, so we will now proceed with the range-based thresholding using values between 0 and 500, by typing in the console:

```
ranged <- applyThreshold(result[[1]],range=c(0,500))
```

As a reference, we will load the global administrative divisions map from the `maptools` R package:

```
data(wrld_simpl)
```

Now you can plot and save this result,
```
plot(ranged,col=rainbow(20))
plot(wrld_simpl,add=T) #this would plot the global admin divisions map on top
writeRaster(ranged,
            "C:/analogues/results/3.10.ccafs.ranged.asc",
            format='ascii')
```

The result would then look like Figure 1

http://ccafs-analogues.googlecode.com/svn/wiki/img/fig1-3.10.PNG

_**Figure 1** Result of thresholding the CCAFS measure result using a range between 0 and 500_

## 2. Select likely analogues using a probability threshold ##

You can also threshold you raster to the 1% closest pixels, by typing in the R console:

```
closest1 <- applyThreshold(result[[1]],best=0.01)
```

Now, store and plot the result (Figure 2)

```
writeRaster(closest1,
            "C:/analogues/results/3.10.ccafs.closest1.asc",
            format='ascii')
plot(closest1,col=rainbow(20),horizontal=T)
plot(wrld_simpl,add=T) #this would plot the global admin divisions map on top
```

http://ccafs-analogues.googlecode.com/svn/wiki/img/fig2-3.10.PNG

_**Figure 2** Result of thresholding the CCAFS measure result using the best 1% sites_

You might vary either the range of values or the percent of sites that you want to retain when thresholding. We suggest you to do an exploration on your results before thresholding, and, whenever possible, use well-calibrated thresholding.