# Exercise 3.12: #
## Agriculturally meaningful point-based analyses of dissimilarity ##

In this tutorial you will learn:
  1. how to quantify dissimilarity using specific points using the CCAFS measure,
  1. how to relate dissimilarity values with agricultural yields in the context of adaptation,
  1. understand the importance of contextualizing analogue analyses results with adaptation,

What do you need to complete this tutorial?
  1. Either a Windows or Linux operating system,
  1. the latest version of R installed on your computer,
  1. to have completed tutorial 3.1 to 3.3 successfully,
  1. to know how to extract data from `RasterLayers` and `RasterStacks` using the `raster` package, and
  1. to understand the concept of analogues and the underlying principles and equations,


## 1. Calculating dissimilarity over a number of points using the CCAFS measure ##

All previous tutorials have focused on performing dissimilarity analyses using gridded datasets; however, at some point agricultural research needs to go down to the field scale, for which most of the times gridded datasets are either not available, not necessary, inaccurate, or not enough. For such purposes, we have also developed functions within our analogues R-package that allow users to analyze dissimilarities using field-scale data.

The data can be, as for the gridded data, for any set of variable with any given time-step, as long as they are consistent. For instance, the time-step of all variables needs to be the same. Weighting variables can also be included, and all the same features that are available for grid-based analyses are also available for point-based analyses. As you might guess, there are some differences in the way the analyses are carried out. These differences are inevitably inherent to the format in which the data is read.

Thereby, we created three functions: `loadDataPoints()`, `loadWeightsPoints()` and `dissimilarityPoints()`, as you might already know. When performing point-based analyses with the analogue tool the input data must be present in your R session (i.e. in your computer's memory) rather than in different files in your computer. These data need to be matrices, with the following characteristics

  * Number of columns is equal to the number of time steps of your variable
  * Number of rows is equal to the number of points you are analyzing. Must be greater than 1.

There must be a **`matrix`** in your R session for each scenario-variable or scenario-weight (if the weight is another variable) combination. For example, if you are working with two scenarios: `"current"` and `"a1b_2020_2049_ukmo_hadcm3"`, two variables: `"tmean"` and `"prec"`, and two different weights: `"dtr"` and 1, for 12 months, and 30 sites. This means, you need to have 6 objects of type (**`matrix`**) loaded in your R session, whose names should be:

  * `tmean.current`
  * `prec.current`
  * `dtr.current`
  * `tmean.a1b_2020_2049_ukmo_hadcm3`
  * `prec.a1b_2020_2049_ukmo_hadcm3`
  * `dtr.a1b_2020_2049_ukmo_hadcm3`

Each of which **MUST** consist in 30 rows and 12 months and **MUST** have no missing data (i.e. `NA`). You can potentially have all these data stored in any format (i.e. comma-separated values, Fortran-formatted, tab-delimited, space-delimited), as long as you know how to load them properly in your R session.

You can extract these data from raster datasets using a function called **`extract()`** from the raster package. Hence, if you want to analyze dissimilarities for your points but the data for such points is in a gridded dataset, you can extract it and store it either in your computer's memory or in a file in your hard drive.

We have provided some example data (on the CD it should be in "./analogues/tutorials/", whereas on the web should be at http://code.google.com/p/ccafs-analogues/downloads/list. The file is named **tutorial-3.12-data.zip**. You need to decompress these data inside your "C:/analogues/tutorials" folder. If you want, you could open these files in excel or any text editor to see what their structure is.

You will find a number of .csv (comma-separated values) files, each referring to a different variable and climate scenario, plus a file named **"yields.csv"**. You will notice that there are data for mean temperatures (tmean), total rainfall (prec), and diurnal temperature range (dtr), for current climates (current) and the UKMO-HadCM3 climate model for SRES-A1B emissions scenario and the 30 year mean 2020-2049 period (a1b\_2020\_2049\_ukmo\_hadcm3). So you will now load the yields.csv file into an object called `siteData`. To do so we will type in the R console,

```
siteData <- read.csv("C:/analogues/tutorials/3.12-data/yields.csv")
```

You could type `str(siteData)` to see what the structure and variables contained in your new object (of type **`data.frame`**) are. The data you just have loaded consists in average yields of 221 different trials performed at different sites across the world, in the data you have an `id` column, as well as columns for longitude (`x`), latitude (`y`) and yield (`yield`).

As we are interested in observing the yields, we will plot a histogram of our yield data by typing in the R console,

```
hist(siteData$yield,breaks=20,main=NA,xlab="Yield (kg)")
```

This will give rise to Figure 1, in which you can observe the large variability of the yields across the 221 sites. This variability might arise either from the agronomic management, varieties, experimental design and environmental conditions at each of the sites (weather, soils).

http://ccafs-analogues.googlecode.com/svn/wiki/img/fig1-3.12.PNG

_**Figure 1** Histogram of yield of all trials in the dataset, note that despite referring to the same crop, there is large variability in yield that can be partly attributed to climate, and partly to agronomic management_

In this exercise we are interested in assessing the potential usage of analogues for climate adaptation. Therefore, we will assume that a low-yielding trial is a site that might require some strategies to fill that yield-gap. These strategies can be transferred from other sites (sites that are close analogues). We will first look for a very low yielding site within our trials, by typing in the R console,

```
which(siteData$yield == min(siteData$yield))
```

This should drop **`71`** as an answer. This means that the site with the poorest yield is the site in row 71, which data we can see if we type,

```
siteData[71,]
```

This should show us that it is in `row=79`, has an `id=84`, `country=NA` (missing), `site=Site.71`, `y= -6.73`, `x=23.93`, `alt= .` (missing), and `yield=28.62` (kg).

We will assume that under future conditions this yield might drop even more or see very little improvement; in whichever case the site needs new technologies. We are thus interested in looking at dissimilarities from site 71 (S71 hereafter) future conditions to all other sites current conditions, using current climates and those predicted by the **`"ukmo_hadcm3"`** model (**`"a1b_2020_2049"`**). We will use our available data, and will configure an analogue run with monthly mean temperature (weighted by diurnal temperature range) and total annual rainfall, we will use the 12 months as growing period (as we don't know what crop is this so we don't know what the growing season is).

First, we will load the data,

```
tmean.current <- as.matrix(read.csv("C:/analogues/tutorials/3.12-data/tmean.current.csv"))
dtr.current <- as.matrix(read.csv("C:/analogues/tutorials/3.12-data/dtr.current.csv"))
prec.current <- as.matrix(read.csv("C:/analogues/tutorials/3.12-data/prec.current.csv"))
```

For current climates, and

```
tmean.a1b_2020_2049_ukmo_hadcm3 <- as.matrix(read.csv("C:/analogues/tutorials/3.12-data/
                                                      tmean.a1b_2020_2049_ukmo_hadcm3.csv "))
dtr.a1b_2020_2049_ukmo_hadcm3 <- as.matrix(read.csv("C:/analogues/tutorials/3.12-data/
                                                    dtr.a1b_2020_2049_ukmo_hadcm3.csv "))
prec.a1b_2020_2049_ukmo_hadcm3 <- as.matrix(read.csv("C:/analogues/tutorials/3.12-data/
                                                     prec.a1b_2020_2049_ukmo_hadcm3.csv "))
```

For our future scenario. Now you need to configure the `params` object so that the later functions, namely, `loadDataPoints()` and `loadWeightsPoints()`, you will type,

```
params <- createParameters(x=siteData$x,
                           y=siteData$y,
                           method="ccafs",
                           hal.rad=NA,
                           hal.mad=NA,
                           hal.mrd=NA,
                           z=2,
                           scenario=c("current","a1b_2020_2049_ukmo_hadcm3"),
                           vars=c("tmean","prec"),
                           weights=c("dtr",1),
                           ndivisions=12,
                           env.data=NA,
                           ext=NA,
                           direction="backwd",
                           growing.season=c(1:12),
                           across.year=F,
                           normalise=F,
                           keep.lag=F)
```

Note that in this case **`env.data=NA`**, and **`ext=NA`**. This is because we are not loading data from the file system but rather taking it from the R session. To load the data into our `training` and `weights` object we type,

```
training <- loadDataPoints(params)
weights <- loadWeightsPoints(params)
```

This would create two objects that can be further used in your dissimilarity calculation, which is done as follows,

```
result <- dissimilarityPoints(params,training,weights)
```

This object is a list of 221 elements, each being a vector of 221 values. Each element in the list is the dissimilarity between that point's future climate and all the others current climates. Thus, if we want to know the dissimilarities between our S71's future climates to all other points� current climates we need to access the element number 71 of that list. To illustrate the last sentence, we will plot a histogram of the dissimilarities from S71 to all other points by typing,

```
hist(result[[71]],breaks=20,main=NA,xlab="Dissimilarity from point 71's future")
```

This will produce Figure 2. You will notice that there is a considerable variability in the outcome, with some sites showing very low dissimilarity (hence being highly analogous) and others showing high values in the dissimilarity index.

http://ccafs-analogues.googlecode.com/svn/wiki/img/fig2-3.12.PNG

_**Figure 2** Histogram of dissimilarity from point-71 (low yielding point) to all other points. Note that there are some points with low dissimilarity values, meaning these hold similar environmental conditions to point-71_

Dissimilarities of other points' future climates to the others' current climates can be accessed, and plotted in the same way. You can save all that data in files using the `write.csv` or `write.table` commands in R.

## 2. Relating the outcomes of the dissimilarity function with agricultural yields ##

In this part we will try to take some conclusions out from the analyses we just carried out. We want to find sites that have high yields and low dissimilarity values to S71. To achieve that, we will find the four quartiles of our yield and dissimilarity index,

```
yield.q <- quantile(siteData$yield,probs=c(0.25,0.5,0.75,1))
diss.q <- quantile(result[[71]],probs=c(0.25,0.5,0.75,1))
```

This will result in two vectors, each with four values, with all quartiles for yields (**`yield.q`**) and dissimilarity (**`diss.q`**). Now you will plot your points by typing in the R console,

```
plot(x=result[[71]],y=siteData$yield,
     pch=20,cex=0.8,
     xlab="Dissimilarity from point 71's future",
     ylab="Yield (kg)")
```

A new plot should have appeared. Don't look at it yet as we're not done. We will do some further plotting on this very same plot that just appeared. This is the magic of R. In this case the _x-axis_ will be dissimilarity and the _y-axis_ will be yield. We will now plot four horizontal lines using R's **`abline()`** command, one line for each yield quartile. This will give us an idea of where are the statistically-high yielding sites,

```
abline(h=yield.q[1],col="red")
abline(h=yield.q[2],col="blue")
abline(h=yield.q[3],col="dark green")
abline(h=yield.q [4],col="black")
```

The first quartile will be plotted in red, the second (the median) in blue, the third in green and the fourth in black. Now we do the same for dissimilarities, this time plotting vertical lines,

```
abline(v=diss.q[1],col="red",lty=2)
abline(v=diss.q[2],col="blue",lty=2)
abline(v=diss.q[3],col="dark green",lty=2)
abline(v=diss.q [4],col="black",lty=2)
```

This will plot each of the quartiles as vertical lines; this time the lines will be dashed (lty=2). We will now select and plot the points that are in our upper most quantile of yields and bottom most quantile of dissimilarity. First we select the data,

```
disTop <- result[[71]][which(result[[71]] < diss.q[1])]
yieTop <- siteData$yield[which(siteData$yield > yield.q[3])]
```

And now we plot them using red crosses,

```
points(disTop,yieTop,pch="+",cex=1,col="red")
```

Finally, we plot S71 as a big blue asterisk, by

```
points(result[[71]][71],siteData$yield[71],pch="*",cex=2.5,col="blue")
```

We will finally have Figure 3. There you can see the degree of variability in the analogue results for the different areas of the globe. The red crosses are sites in the lowest quartile of dissimilarity and in the top-most quartile of yield. Similar analyses can be done with higher probabilities (i.e. not splitting the data each 25% -quartiles, but doing so each 10%, or 5%).

Either management practices, varieties, and other technologies from those sites with high yield could be potentially (1) tested and (2) transferred to S71 to decrease the yield gap. These and other more complex analyses are the type of analyses that agricultural researchers should aim at when using the analogues tool.

http://ccafs-analogues.googlecode.com/svn/wiki/img/fig3-3.12.PNG

_**Figure 3** Dissimilarity from S71 (very low yielding point) future climates to all other points' current climates (_x-axis_) and yield of each of those points (_y-axis_). The blue asterisk is point-71, the red, blue, green and black lines are the first, second (median), third quartile, and 100% of the data for yield (continuous lines) and dissimilarities (dashed lines). Red crosses show points above the top 75% in each variable; these are trials of interest for point-71: high yield and low dissimilarity._
