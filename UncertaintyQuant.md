# Exercise 3.11: #
## Quantifying uncertainty ##

In this tutorial you will learn:
  1. how to use basic uncertainty quantification measures (ensemble MEAN, Standard deviation and Coefficient of Variation) for the CCAFS measure results,
  1. how to quantify CCAFS measure uncertainty when thresholded,
  1. how to quantify Hallegatte measure GCM agreement and uncertainty,
  1. understand the importance of uncertainty quantification in the context of analogues and climate change,

What do you need to complete this tutorial?
  1. Either a Windows or Linux operating system,
  1. the latest version of R installed on your computer,
  1. to have completed tutorial 3.1 to 3.3, and tutorial 3.9 successfully,
  1. to understand R **`for`** loops,
  1. to understand the importance of uncertainty quantification in climate-change and agriculture (see exercise 3.9),
  1. to understand the concept of analogues and the underlying principles and equations,


## 1. Calculate CCAFS measure ensemble MEAN, standard deviation (SD) and coefficient of variation (CV) ##

As stated in tutorial 3.9, uncertainty when using the analogues tool can arise from the usage of different GCM formulations. As our purpose with this series of tutorials is not to provide a comprehensive understanding of uncertainty, we suggest you to look into the literature (climate change and agriculture) to get a much better idea of what are, where do they come from and how can uncertainties be quantified.

In this exercise we will use basic measures to quantify the uncertainty in our analogue results. First, we need to configure an analogues run. We will use mean monthly temperature (weighted by diurnal temperature range) and monthly rainfall. We will use the current climate and each of the 24 GCMs in a `"backward"`-type analysis, each combination at a time to avoid RAM memory overflow. We will neither standardize nor take lag into account, and we will use a growing season of 12 months. We first configure our `params` object,

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
```

At this point we will neither load any data nor perform the `dissimilarity()` function. As we need to perform this function many times (24 GCMs) it would be very lengthy to type in 24 times the same thing in the R console. Rather, we will use a **`for`** loop. To do so, we first need to create a vector with our GCMs. We will call this object `gcms`,

```
gcms <- c("bccr_bcm2_0",
          "cccma_cgcm3_1_t47",
          "cccma_cgcm3_1_t63",
          "cnrm_cm3",
          "csiro_mk3_0",
          "csiro_mk3_5",
          "gfdl_cm2_0",
          "gfdl_cm2_1",
          "giss_aom",
          "giss_model_eh",
          "giss_model_er",
          "iap_fgoals1_0_g",
          "ingv_echam4",
          "inm_cm3_0",
          "ipsl_cm4",
          "miroc3_2_hires",
          "miroc3_2_medres",
          "miub_echo_g",
          "mpi_echam5",
          "mri_cgcm2_3_2a",
          "ncar_ccsm3_0",
          "ncar_pcm1",
          "ukmo_hadcm3",
          "ukmo_hadgem1")
```

Now we will add the `"a1b_2020_2049_"` part to our models to make them complete, and we will call this new vector `futures`, to do this we type,

```
futures <- paste("a1b_2020_2049_",gcms,sep="")
```

Each of the future scenarios we want to use is now an element of the vector `futures`. Ensure that you have not done any spelling mistake. Now we will loop through this object each time changing the scenarios parameter in our `params` object. We will store our results in an object of type list, so we type,

```
results <- list()
```

To create an empty list, now we do the loop,

```
i <- 1
for (scen in futures) {
	params$scenario <- c("current",scen)
	training <- loadData(params)
	weights <- loadWeights(params)
	results[[i]] <- dissimilarity(params,training,weights)
	i <- i+1
}
```

Each time the loop is executing the `loadData()` and `loadWeights()` functions, you will notice it is using a different future scenario. Now, if you type,

```
length(results)
```

You will notice that your object has a length equal to 24. Each element will be a list with one element (a `RasterLayer`) resultant from the calculation of each of your scenarios and the current climate. You can plot any of these layers by typing,

```
plot(results[[10]][[1]],col=rainbow(20),main=futures[10])
```

This would plot the scenario number 10, in this case corresponding to the GCM "giss\_model\_eh". If you want to save these rasters in your file system, you just need to do another for loop,

```
i <- 1
for (scen in futures) {
	name <- paste("C:/analogues/results/3.11.ccafs.",scen,".asc",sep="")
	writeRaster(results[[i]][[1]],name,format="ascii")
    i <- i+1
}
```

As the processing might take a while, we have provided the results of this process (directory "./analogues/tutorials/", or http://code.google.com/p/ccafs-analogues/downloads/list). The file is named tutorial-3.11-data.zip. You would only need to copy/download them into your file system, and then load them. You already know how to download and decompress zipped files.

So we focus on the loading, assuming that you have decompressed these data in a folder named "tutorials" inside your analogues folder in "C:/analogues". Now you create the same loop but this time you only need to use the command `raster` from the raster package, this should put the data in your list without calculating dissimilarities.

```
results <- list()
i <- 1
for (scen in futures) {
	name<-paste("C:/analogues/tutorials/3.11-data/part-1/3.11.",scen,".asc",sep="")
	results[[i]] <- list()
	results[[i]][[1]] <- raster(name)
	i <- i+1
}
```

This should take a lot less time than processing it. Now you can calculate the MEAN, SD and CV of all these results, hence somehow quantifying uncertainties. First you create a `RasterStack` from this list,

```
rstk <- stack(unlist(results))
```

The raster package would provide the remaining things you need, so you don't really have to worry too much about the particularities of calculating our statistics out from the `rstk` object we just created. To calculate the desired metrics you just type (we will add na.rm=T to the function so that any missing data will not be taken into account),

```
rmean <- mean(rstk,na.rm=T)
rsd <- calc(rstk,sd,na.rm=T)
rcv <- rsd/rmean * 100
```

You can now store and plot these results by typing,

```
writeRaster(rmean,
            "C:/analogues/results/3.11.ccafs.mean.asc",
            format=�ascii�)
plot(rmean,col=rainbow(20))

writeRaster(rsd,
            "C:/analogues/results/3.11.ccafs.sd.asc",
            format=�ascii�)
plot(rsd,col=rev(heat.colors(10)))

writeRaster(rcv,
            "C:/analogues/results/3.11.ccafs.cv.asc",
            format=�ascii�)
plot(rcv,col=rev(heat.colors(10)))
```

All this would give rise to the results in Figure 1.

http://ccafs-analogues.googlecode.com/svn/wiki/img/fig1-3.11.PNG

_**Figure 1** Ensemble mean and uncertainty of 24 different GCM-specific analogue runs. MEAN (top), SD (middle), CV (bottom)_

There you can see the degree of variability in the analogue results for the different areas of the globe.

## 2. Calculate thresholded CCAFS measure agreement amongst GCMs ##

Other option to quantify uncertainty is to look at the degree at which the different GCMs agree in selecting a given site as an analogue. To do that, we will threshold our results and will them count how many times each pixel is counted as an analogue out from the 24 different GCMs. We will use the results from the previous section of this exercise and will use a new for loop, as follows,

```
i <- 1
for (scen in futures) {
  rs <- results[[i]][[1]]
  
  b1 <- quantile(rs[],probs=0.01,na.rm=T)
 
  th <- rs
  th[which(rs[] > b1)] <- 0
  th[which(rs[] <= b1)] <- 1
  
  if (i == 1) {
    out <- th
  } else {
    out <- out+th
  }

  i <- i+1
}
```

In the first part of this for loop and object named **`rs`** is created from each of the GCM-specific results as stored in the **`results`** list object generated in the previous section. Next, an object named **`b1`** is created with the corresponding value at 1% probability (`probs=0.01`) from the probability distribution of the RasterLayer **`rs`** values (**`rs[]`**).

Next, a new object (**`th`**) is created from the object **`rs`** (a clone of that object), and then is assigned 0 for those values of **`rs`** that are above **`b1`**, else assigned 1. Next, a new object is created that contains the sum of each **`th`** object, hence producing an object in which each pixel would total the number of times that pixel is below **`b1`**, that is, within the closest 1% sites. Plotting the results you will have Figure 2,

```
writeRaster(out,
            "C:/analogues/results/3.11.ccafs.agreement.asc",
            format='ascii')
plot(out,col=c("grey",rev(heat.colors(24))))
```



http://ccafs-analogues.googlecode.com/svn/wiki/img/fig2-3.11.PNG

_**Figure 2** Agreement among GCMs in selecting analogue areas at 99% probability within the spatial domain of the whole world_

You can now see that there are some interesting things to note. GCMs agree at a considerable extent in selecting certain areas (reddish areas in Figure 2), but the actual dissimilarity index values can have up to 60% variability in some cases (Figure 1, bottom).


## 3. Calculate Hallegatte measure agreement amongst GCMs ##

We will use mean monthly temperature and monthly rainfall and will use only the `hal.mad` parameter for rainfall, set at 0.15. We will use the current climate and each of the 24 GCMs in a backward-type analysis, each combination at a time within a **`for`** loop to avoid RAM memory overflow. We will neither standardize nor take lag into account, and we will use a growing season of 12 months. We first configure our `params` object,

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
```

Each of the future scenarios we want to use is already an element of the vector `futures` created in section 1. Now we will loop through this object each time changing the scenarios parameter in our `params` object. We will store our results in an object of type list, so we type,

```
results <- list()
```

To create an empty list, now we perform the loop,

```
i <- 1
for (scen in futures) {
	params$scenario <- c("current",scen)
	training <- loadData(params)
	weights <- loadWeights(params)
	results[[i]] <- dissimilarity(params,training,weights)
	i <- i+1
}
```

Each time the loop is executing the `loadData()` and `loadWeights()` functions, you will notice it is using a different future scenario. Now, if you type,

```
length(results)
```

You will notice that your object has a length equal to 24. Each element will be a list with 1 `RasterLayer` resultant from the calculation of each of your scenarios and the current climate. You can plot any of these layers by typing,

```
plot(results[[10]][[1]],col=rainbow(20),main=futures[10])
```

This would plot the scenario number 10, in this case corresponding to the GCM "giss\_model\_eh". If you want to save these rasters in your file system, you just need to do another for loop,

```
i <- 1
for (scen in futures) {
	name <- paste("C:/analogues/results/3.11.hal.",scen,".asc",sep="")
	writeRaster(results[[i]][[1]],name,format='ascii')
	i <- i+1
}
```

As the processing might take a while, we have provided the results of this process (directory "C:/analogues/tutorials/3.11-data/part-2"). We will assume that you already downloaded or copied these data and properly unzipped them (otherwise go back to part 1 of this same exercise). You would only need to load them, to do so, do the same loop but this time you only need to use the command `raster` from the raster package, this should put the data in your list without calculating dissimilarities.

```
results <- list()
i <- 1
for (scen in futures) {
	name<- paste("C:/analogues/tutorials/3.11-data/part-2/3.11.",scen,".asc",sep="")
	results[[i]] <- list()
	results[[i]][[1]] <- raster(name)
	i <- i+1
}
```

This should take a lot less time than processing it. Now you can calculate the agreement among these predictions similarly as we did in section 2 of this same tutorial,

```
i <- 1
for (scen in futures) {
  rs <- results[[i]][[1]]
  
  if (i == 1) {
    out <- rs
  } else {
    out <- out+rs
  }

  i <- i+1
}
```

In the first part of this for loop and object named **`rs`** is created from each of the GCM-specific results as stored in the **`results`** list object generated previously. Next, a new object is created that contains the sum of each **`rs`** object, hence producing an object in which each pixel would total the number of times that pixel is an analogue according to the Hallegatte method. Plotting the results you will have Figure 3,

```
writeRaster(out,
            "C:/analogues/results/3.11.hal.agreement.asc",
            format='ascii')
plot(out,col=c("grey",rev(heat.colors(24))))
```

This would produce Figure 3, where the most reddish areas indicate strong agreement. It can also be seen that those gray areas indicate also strong agreement in which those sites are NOT analogues according to the Hallegatte configuration we just used.

http://ccafs-analogues.googlecode.com/svn/wiki/img/fig3-3.11.PNG

_**Figure 3** Agreement among GCMs in selecting analogue areas using the Hallegatte method_

Now you understand and can quantify in few steps the uncertainty of the results from the analogue tool when used with different climate scenarios as predicted by different GCMs.