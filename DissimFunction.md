# Exercise 3.3: #
## First analogue run ##

In this tutorial you will learn:
  1. how to perform an analogue run using the CCAFS method,
  1. how to perform an analogue run using the Hallegatte method,
  1. how to visualize both outputs, and will gather a basic understanding of the differences between them, and
  1. how to print a basic report with your results,

What do you need to complete this tutorial?
  1. Either a Windows or Linux operating system,
  1. the latest version of R installed on your computer,
  1. to have completed tutorial 3.1 and 3.2 successfully,
  1. to understand the concept of analogues and the underlying principles and equations,


## 1. What are the required parameters and data for an analogue run? ##

According to tutorial 3.2 you should already have 3 objects loaded in your R session: `params`, `training` and `weights`. This example will assume that you're using gridded data and thus performing grid-based analyses. You can now verify that you have your method properly configured by typing in the R console:

```
params$method
```

You can see any of the parameters listed in your object `params` in the same way. Now, to perform your run will use the function `dissimilarity()`, so you type in the R console:

```
result <- dissimilarity(params,training,weights)
```

There are three arguments to the function dissimilarity:

**`params`**: An object of type list created with the function createParameters().

**`training`**: An object of type list created with the function loadData() and based on an object previously created with createParameters(). This object contains all the variables data to perform an analogue run.

**`weights`**: An object of type list created with the function loadWeights() and based on an object previously created with createParameters(). This object contains all the weights data used to weight the variables when performing an analogue run.

This function will take about 30 seconds or 1 minute in an standard 2-core Intel 2GB RAM laptop with Windows XP (speed might increase/decrease depending on your computer's technical characteristics) and will return an object of type list with length equal to the number of comparisons done, that is, the number of different scenarios minus 1 (as we assume in the first scenario provided is always the base of the comparisons, namely, the current climate). Each object in this list would be a `RasterLayer` and can be accessed using R's indexing system as:

```
result[[index]]
```

Where `index` can be any number between 1 and the total length of your `result` list (in this case 1).

Now you could print your result using R built-in functions wrapped within the `raster` package, by simply typing:

```
plot(result[[1]],col=rainbow(20),horizontal=T)
```

And this will give you a nice map of the whole world, where the most orange-reddish areas are the most similar to your target point. To visualize your target point in the map just type in the R console:

```
points(params$x,params$y,cex=2,pch=20)
```

The point should appear as a big black dot over Nepal, giving rise to Figure 1.

http://ccafs-analogues.googlecode.com/svn/wiki/img/fig1-3.3.PNG

_**Figure 1** Result of running analogues using the CCAFS measure_

You can now store this result using functions of the raster package. First, create a folder named `results` within your folder "C:/analogues", and then type in your R console:

```
writeRaster(result[[1]],
            "C:/analogues/results/3.3.ccafs.asc",
            format='ascii')
```

This should save your result in your folder, as an ESRI ASCII file, that you can read in any time after you close R.

## 2. How to set up an analogue run? ##

Now you can perform exactly the dissimilarity analysis but using the Hallegatte measure this time, to do so, you would need to change some parameters in your `params` object. To do so you would need to generate the `params` object again using the `createParameters()` function, as in tutorial 3.2.

You will use the same point (x, y coordinates), but this time you will specify `method="hal"`, and you will use only the relative absolute difference threshold (`hal.rad`), and you will set it up only for rainfall at a value of 0.15.

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

And now you need to re-load your variables and weights data

```
training <- loadData(params)
weights <- loadWeights(params)
```

You can now call the dissimilarity function, as for the CCAFS method, this time we will name our output list as `result2` so that it does not replace our existing result in the computer's memory:

```
result2 <- dissimilarity(params,training,weights)
```

Which will produce a list with your result(s), that you can access using R's indexing system, in the same way you did for the CCAFS dissimilarity result. You can now plot this result, by typing:

```
plot(result2[[1]],col=c("grey","dark green"))
```

And you will have produced a plot as in Figure 2, where the green areas are areas marked as analogues, and the others are marked as non-analogues, according to the Hallegatte method.

http://ccafs-analogues.googlecode.com/svn/wiki/img/fig2-3.3.PNG

_**Figure 2** Result of running analogues using the Hallegatte measure_

You might want to save this result, to do so just type in the R console:

```
writeRaster(result2[[1]],
            "C:/analogues/results/3.3.hallegatte.asc",
            format='ascii')
```

This should save your result in your folder, as an ESRI ASCII file, that you can read in any time after you close R.

By visually comparing both maps you can see the differences in the methods and make some preliminary judgments on what method suits best for your analyses. Also, you can further run the Hallegatte method with other conditions and/or combinations of conditions, to see the differences.

## 3. How to print your results in a basic report? ##

Now that you have learned how to run analogues with a basic configuration, you can store your results in a PDF report so that you can further associate them and show them in your presentations. To do that, we will use our existing objects `params`, `training`, `result` and `result2`.

The report will only take the growing season and the variables out from our `params` object, so it means we can plot our CCAFS and Hallegatte result in the same report. To do that, we first need to create a list with our model details, so you just type in the R console:

```
mod <- list(params=params,
            training=training,
            results=list(result[[1]],result2[[1]]),
            r.lab=c("CCAFS method","Hallegatte method"),
            m.lab="2030")
```

This list MUST contain the following elements:

**`params`**: An object of type list created with the function `createParameters()`. This object contains all the relevant information to be plotted in the PDF report.

**`training`**: An object of type list created with the function `loadData()` and based on an object previously created with `createParameters()`. This object contains all the variables data to perform an analogue run and will be used by the report to extract data for plotting.

**`results`**: An object of type list containing all the results you want to plot in the form of RasterLayers.

**`r.lab`**: Vector of names of your `RasterLayers`. Makes reference to the list of RasterLayers that you want to plot and is of the same length of the element `results` of this same list.

**`m.lab`**: A general label to refer to your charts, often is the year to which you are calculating, i.e. 2030.

Once you configured this list, you just type in the R console:

```
report(models=list(mod),
       pdf.name="C:/analogues/results/report.3.1.pdf")
```

The report function needs to be specified with two arguments:

**`models`**: An object of type list with each element being a configuration. Each configuration is itself a list, as done in the previous step.

**`pdf.name`**: A path and name to the pdf file that will be generated.

This should generate a report in your folder `results`, that is within your folder 'analogues', in your file system. Open that file to see the results.








