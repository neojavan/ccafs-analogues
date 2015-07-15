# Exercise 3.4: #
## Understanding the effects of direction ##

In this tutorial you will learn:
  1. how to perform analogue runs using different target and reference scenarios with the CCAFS measure,
  1. how to perform analogue runs using different target and reference scenarios with the Hallegatte measure,
  1. the differences between the different types of analyses,

What do you need to complete this tutorial?
  1. Either a Windows or Linux operating system,
  1. the latest version of R installed on your computer,
  1. to have completed tutorial 3.1 to 3.3 successfully,
  1. to understand the concept of analogues and the underlying principles and equations,


## 1. Perform `"backward"`, `"forward"` and `"no"` direction analyses using the CCAFS method ##

As you know, the analogues concept objective is to look at analogies in the geographic space between one site and other(s) in a series of climate scenarios. With analogues you can look at dissimilarities in three different directions: `"backward"`, `"forward"` and `"none"`:

  * you will choose `"backward"` when you want to calculate dissimilarities between your site's future climate and the other site(s) current climate,
  * you will choose `"forward"` when you want to calculate dissimilarities between your site's current climate and the other site(s) future climates, and
  * you will choose `"none"` when you want to calculate dissimilarities between your site's current climate and the other site(s) current climate, or when you want to calculate dissimilarities between your site's future climate and the other site(s) future climate.

The extent at which these are used to resolve science questions is up to the researcher. But each of them might be of interest in different contexts and in different disciplines.

We will now perform three types of analyses using the CCAFS measure to see what the difference in the results is between the different types of analyses. We will do `"backward"`, then `"forward"` and finally `"none"` direction, using the same site as in exercise 3.3, using mean temperatures and total rainfall at the monthly level, we will weight temperature with diurnal temperature range, and we will use the current climate and only one future scenario as our scenarios. First, we will configure our `params` object and load the data and weighs, by typing in the R console:

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
```

Now we will run the dissimilarity analysis, then plot it and store it, as follows:

```
res1ccafs <- dissimilarity(params,training,weights)
plot(res1ccafs[[1]],col=rainbow(20),horizontal=T,zlim=c(0,2200))
writeRaster(res1ccafs[[1]],
            "C:/analogues/results/3.4.ccafs.backward.asc",
            format='ascii')
```

Now, you need to modify the configuration of your `params` object. To do so you can re-run the function `createParameters()`, or you can simply modify the element that you want. As you already know how to do the former, for the latter, you will type:

```
params$direction <- "forward"
```

And now you will perform the dissimilarity function again, then you will plot and then save your result by:

```
res2ccafs <- dissimilarity(params,training,weights)
plot(res2ccafs[[1]],col=rainbow(20),horizontal=T,zlim=c(0,2200))
writeRaster(res2ccafs[[1]],
            "C:/analogues/results/3.4.ccafs.forward.asc",
            format='ascii')
```

You will note that there are some differences in the resultant map. Now you need to repeat your last four R commands to run the `"no"` direction analysis:

```
params$direction <- "none"
```

```
res3ccafs <- dissimilarity(params,training,weights)
plot(res3ccafs[[1]],col=rainbow(20),horizontal=T,zlim=c(0,2200))
writeRaster(res3ccafs[[1]],
            "C:/analogues/results/3.4.ccafs.none.asc",
            format='ascii')
```

You will, again, notice some differences in your maps (Figure 1)

http://ccafs-analogues.googlecode.com/svn/wiki/img/fig1-3.4.PNG

_**Figure 1** Effect of using different directions in the CCAFS measure. Top: backward, middle: forward, bottom: no direction_

You can numerically compare the outputs by further plotting scattergrams. We suggest you to do a further exploration on the data.

## 2. Perform `"backward"`, `"forward"` and `"no"` direction analyses using the Hallegatte method ##

In the same way as for the CCAFS method, you will now configure the `params` object to perform the Hallegatte method. To do so we will use only the relative annual difference equation and we will set it to 0.15 for precipitation and to `NA` for temperature. Variables in the Hallegatte method are not weighted so we put 1 as weights. Other configuration options would be the same as for the CCAFS method in the previous steps. So you will create the `params` object, load the data and weights. Type in the R console:

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
```

Now you will perform you analogues run, by typing:

```
res1hal <- dissimilarity(params,training,weights)
plot(res1hal[[1]],zlim=c(0,1))
writeRaster(res1hal[[1]],
            "C:/analogues/results/3.4.hal.backward.asc",
            format='ascii')
```

Now you change the direction parameter for the forward run, and will run the `dissimilarity()` function again:

```
params$direction <- "forward"
res2hal <- dissimilarity(params,training,weights)
plot(res2hal[[1]],zlim=c(0,1))
writeRaster(res2hal[[1]],
            "C:/analogues/results/3.4.hal.forward.asc",
            format='ascii')
```

And now for the no direction run:

```
params$direction <- "none"
res3hal <- dissimilarity(params,training,weights)
plot(res3hal[[1]],zlim=c(0,1))
writeRaster(res3hal[[1]],
            "C:/analogues/results/3.4.hal.none.asc",
            format='ascii')
```

You will notice few changes in the resultant map (Figure 2)

http://ccafs-analogues.googlecode.com/svn/wiki/img/fig2-3.4.PNG

_**Figure 2** Effect of using different directions in the Hallegatte measure. Top: backward, middle: forward, bottom: no direction_

You can now analyze your results in more detail, by doing some complementary analyses over specific regions, either with R or with any other GIS you might have at hand.



