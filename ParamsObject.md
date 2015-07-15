# Exercise 3.2: #
## Understanding the basics of the analogue R-library: the `createParameters()` function and functions for loading data and weights ##

In this tutorial you will learn:
  1. what are the different parameters and data required for an analogue run,
  1. how to set up a given run,
  1. how to load data once a run is configured,
  1. how to load weights once a run is configured

What do you need to complete this tutorial?
  1. Either a Windows or Linux operating system,
  1. the latest version of R installed on your computer,
  1. to have completed tutorial 3.1 successfully,
  1. to understand the concept of analogues and the underlying principles and equations,


## 1. What are the required parameters and data for an analogue run? ##

In our analogue library there are a number of parameters (i.e. variables) that need to be specified in order to be able to perform a given analogue run. There are a total of 19 different parameters that can be specified in our tool. So it is worth spending some time going through each of them.

But before we get into the parameters, we need to know how these parameters are stored in R. To store the parameters we have created a function named `createParameters()`, in which the 19 different parameters can be specified. Once the 19 (sometimes less) parameters are specified, these are stored (by the function) as an object of type `list` in the RAM memory of your computer.

The different parameters of the analogues tool to be specified when calling the function `createParameters()` are:

| **Parameter** | **Description** |
|:--------------|:----------------|
| **`x`**       | Longitude or coordinate X of the reference site when performing grid-based calculations, or a vector of length equal to the number of sites containing the longitudes or coordinates X of the sites being analyzed when performing point-based analyses. These must be in the same coordinate system of `y` and in the same coordinate system in which the raster data are. |
| **`y`**       | Longitude or coordinate Y of the reference site when performing grid-based calculations, or a vector of length equal to the number of sites containing the longitudes or coordinates Y of the sites being analyzed when performing point-based analyses. These must be in the same coordinate system of `x` and in the same coordinate system in which the raster data are. |
| **`method`**  | Method to be used for the dissimilarity calculation. Currently, there are two methods to calculate dissimilarity: `"ccafs"`, and `"hal"`, referring to CCAFS and Hallegate method, respectively. |
| **`hal.rad`** | Maximum tolerable relative annual difference. Only needs to be specified when `method="hal"`, can be one single value or a vector of various values (length equal to the number of variables included). If set to `NA` the condition will not be taken into account for the dissimilarity calculation. |
| **`hal.mad`** | Maximum tolerable mean absolute difference. Only needs to be specified when `method="hal"`, can be one single value or a vector of various values (length equal to the number of variables included). If set to NA the condition will not be taken into account for the dissimilarity calculation. |
| **`hal.mrd`** | Maximum tolerable mean absolute relative difference. Only needs to be specified when `method="hal"`, can be one single value or a vector of various values (length equal to the number of variables included). If set to NA the condition will not be taken into account for the dissimilarity calculation. |
| **`z`**       | Exponent for the CCAFS dissimilarity equation. Default value is 2. Only needs to be provided when `method="ccafs"` |
| **`scenario`** | A vector listing the various climate scenarios to be used. The first element of this vector is always assumed the current climate scenario, others are targets. You can specify as many scenarios as you have climate data for. The name of a scenario should be quoted ("scenario") and should be a combination of an SRES, a period and a GCM, or otherwise should be the current climate (however it is called in your file system or in your computer's memory). |
| **`vars`**    | A vector listing the different variables to be used in the analysis. These should exist either in your file system (in your climate data folder) or as objects in memory if you're performing a point-based analysis. All variables need to have the same time step (i.e. monthly, daily, yearly). |
| **`weights`** | A vector listing the different weights to be used for the variables, with the same length of the vector vars. The position of each weight MUST match the position of the variable. For instance, the variable in position 1 in vars will be weighted with the weight in position 1 in weights. Variable names (that must exist in your file system or in memory) or single numbers can be provided for weighting the variables. |
| **`ndivisions`** | Number of time steps in your variables. For example, if monthly, this would equal 12, if daily then 365, if quarterly then 4. |
| **`env.data`** | Directory in your file system in which your climate data is stored  (for grid-based analyses). For point-based analyses this can be set to `NA`. |
| **`ext`**     | Extension of your raster files. Only needs to be provided for grid-based analyses. |
| **`direction`** | Type of analysis to perform. This would tell to analogues what scenario is being taken as reference. If you use `"backward"` it will use the future climate as a reference and will look for analogues in the current climate; if you use `"forward"` it will use the current climate as a reference and will look for analogues in the future climate; whereas if you use `"none"` it will use the same scenario as reference and target for finding analogues. |
| **`growing.season`** | Period during which dissimilarities are being analyzed, specified as a vector of one or more time steps. |
| **`across.year`** | Switch for lag. If set to `TRUE` then the lag is accounted and the minimum dissimilarity of all the possible combinations of reference to target scenarios is used as the result, whereas if set to `FALSE`, only the corresponding growing season is compared. |
| **`normalise`** | Switch for normalization. If set to `TRUE` the variables and weights will be scaled to have a mean of 0 and an standard deviation of 1 for all climate scenarios. |
| **`keep.lag`** | Switch for keeping or not keeping lagged grids. Switch this to `FALSE` to avoid memory overload. |


## 2. How to set up an analogue run? ##

Here we will assume that you have a folder "C:/analogues", within which you have two more folders, one named 'packages' and the other named 'climate-data', in which all your climate data is stored. If not, please go back to tutorial 3.1.

To set up an analogue run you would need to open the R software:
> _Start > All Programs > R > R 2.13.1_

Now you need to type in the R console

```
library(analogues)
```

Which should load the analogues package. Now you will type the following:

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

This should not take more than a fraction of a second and should create an object called `params` of type list with all the information required to perform the analogue run.

## 3. How to load data once the run is configured? ##

Now that we have created the parameters object we need the package to load the data for them to be later used in a dissimilarity calculation. To do this, we will use the function `loadData()` (for grid-based analyses), and `loadDataPoints()` (for point-based analyses).

The only argument to any of these functions is the object created with the function `createParameters()`, that is, the `params` object you just created in the previous step. To load your data you then need to type:

```
training <- loadData(params)
```

For grid-based calculations, or alternatively (**don't try the following with the current `params` configuration**)

```
training <- loadDataPoints(params)
```

For point based calculations.

If the loading fails it will give you an error message on the specific problem. Sometimes it's because you are pointing wrongly to the `env.data` folder, or due to a misspelling in one or more of the parameters in the `params` object. In any case, if this function fails it means your `params` object is not properly set up, and hence you need to re-initialize the object by executing the command `createParameters()` again.

The variables data (object `training`) are loaded as a list of length equal to the number of variables times the number of scenarios. Thus the first item in the list is the first variable of the first scenario, the second is the second variable of the first scenario, and so on, until all variables and scenarios are present. For grid based calculations each element would be a `RasterLayer` or a `RasterStack` (conjunction of `RasterLayers`), whereas for point-based calculations the elements would be matrices.

## 4. How to load weights once the run is configured? ##

Now that we have created the parameters object and loaded the relevant input climate data, we need the package to load the weights data for them to be later used in a dissimilarity calculation. To do this, we will use the function `loadWeights()` (for grid-based analyses), and `loadWeightsPoints()` (for point-based analyses).

The only argument to any of these two functions is the object created with the function `createParameters()`, that is, the `params` object you just created two steps back. To load your data you then need to type:

```
weights <- loadWeights(params)
```

For grid-based calculations, or alternatively (again, **don't try the following with the current `params` configuration**)

```
weights <- loadWeightsPoints(params)
```

For point based calculations.

If the command fails it will give you an error message on the specific problem. Sometimes it's because you are pointing wrongly to the `env.data` folder, or due to a misspelling in one or more of the parameters in the `params` object. In any case, if this function fails it means your `params` object is not properly set up, and hence you need to re-initialize the object by executing the command `createParameters()` again.

The weights data are loaded as a list with number of elements equal to the variables data (`training`) as loaded in the previous step. Depending on the type of weighting the type of elements will vary. For gridded based calculations with variables weighted by numbers the elements would be matrices, but if the variables are weighted using other variables (such as when mean temperature is weighted by diurnal temperature range) the elements in the weights list would be a `RasterLayer` or a `RasterStack` (conjunction of `RasterLayers`).

## 5. Structure of weights and variable data? ##

If you want to see the structure of the data you just have loaded, then you only need to type in the R console:

```
str(training)
```

For the variables data, and

```
str(weights)
```

For the weights; this command will display on the screen the main characteristics of your data.











