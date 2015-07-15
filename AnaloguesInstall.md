# Exercise 3.1: #
## Installing the package, viewing help files, storing accompanying climate data, understanding accompanying climate data ##

In this tutorial you will learn:
  1. where to get the latest update of the analogues package,
  1. how to download it,
  1. how to install it on your computer,
  1. how to load the package,
  1. how to browse the package's help,
  1. how to get accompanying climate data, and
  1. what is the accompanying climate data,

What do you need to complete this tutorial?
  1. Either a Windows or Linux operating system,
  1. the latest version of R installed on your computer,
  1. a basic understanding of R,
  1. internet connection and an internet browser (i.e. Firefox, Google Chrome, Internet Explorer, or any other),
  1. a software to decompress zip files, and
  1. a basic understanding of geographic gridded data


## 1. Where to get the latest version of the analogues R-package? ##

The analogue package exists as an online Google code project, at
http://code.google.com/p/ccafs-analogues/

The latest version can be downloaded at
http://code.google.com/p/ccafs-analogues/downloads/list

There you will see a list of files available for download (Figure 1). We are interested in the latest version of the library (in this case 0.0.12) for whichever Operating System (i.e. Linux, Windows) you have (both versions are circled red in Fig. 1).

**Note:** to realise what the latest version of the package is, just go to the project's homepage, there you will see the latest version, the date released and some other details (http://code.google.com/p/ccafs-analogues/).

For older versions of the package, you need to go to the downloads section and browse the "Deprecated" downloads.

http://ccafs-analogues.googlecode.com/svn/wiki/img/fig1-3.1.PNG

_**Figure 1** Downloads section in the Google code repository_

## 2. How to download it? ##

Clicking on the corresponding file of Figure 1 will guide you to a new webpage (Figure 2), in which you will be able to see the name of the file and a brief description. It is important that you read the description, as this would inform you on the major modifications and/or improvements done to the package.

http://ccafs-analogues.googlecode.com/svn/wiki/img/fig2-3.1.PNG

_**Figure 2** Downloading the latest version of the package_

Clicking on the link to the file would lead prompt a message asking you where to save this file (though this depends on the configuration of your browser) (Figure 3). You will select on "Save File", and then click on "OK".

http://ccafs-analogues.googlecode.com/svn/wiki/img/fig3-3.1.PNG

_**Figure 3** Saving the package file_

A new window will prompt, asking you the specific location where to put this file. You will go to "C:/" and will create a folder named "analogues" (Figure 4A), inside which you will create another folder called "package" (Figure 4B). There you will store this file by clicking on "Save" (Figure 4C).

http://ccafs-analogues.googlecode.com/svn/wiki/img/fig4-3.1.PNG

_**Figure 4** Creating analogues folder and saving the package file_

In this particular case, we have provided you with the latest package update in a CD-ROM. You can copy and paste it into the analogues folder.

## 3. How to install it onto your computer? ##

To do this, you will need to open the R interface, either from the Desktop shortcut or by clicking Start > All Programs > R > R 2.13.1 (or whichever version you might have). This will take you to the R Graphical User Interface (Gui) and the respective R Console (Figure 5), you now need to click on the "Packages" menu in the menu toolbar and then click on "Install package(s) from local zip files..." (Highlighted in blue in Figure 5).

http://ccafs-analogues.googlecode.com/svn/wiki/img/fig5-3.1.PNG

_**Figure 5** The R Gui and console, and the Packages menu_

A new window will prompt and you will be able to search for files. Go to the location where you stored the analogues package as downloaded from internet or copied from the CD and click on "Open". This will install the package as a library in your current R installation folder. Now you can load the package.

**Note:** Alternatively, you could type in your R console:

```
install.packages("C:/analogues/package/analogues_0.0.11.zip",repos=NULL)
```

And this will also install the package (change the path to the library and the version to your needs). In **UNIX** systems you will type (in the bash, sh or ksh console):

```
R CMD INSTALL /path-to-package/analogues_0.0.11.tar.gz
```

In such a case you must have the **Unix** version of the library, also available in the downloads section of this project.

### Important Installing depdendencies!!! ###

The **`analogues`** package depends on a number of other packages, some of them are already part of the standard R installation, so you don't need to worry about them, but others are not, so you will need to install them. You will need an internet connection to do this. These packages are:

  * `rgdal`
  * `sp`
  * `raster`
  * `stringr`
  * `maptools`
  * `maps`
  * `spgrass6`
  * `akima`
  * `grid` (already in the standard R installation)
  * `rimage`

To install these packages type in your R console:

```
install.packages(c("sp","raster","rgdal","stringr","maptools","maps","spgrass","akima","rimage"))
```

You will be prompted to choose a repository: choose the closest to your location and click **`OK`**. This should take between few minutes to install all the packages, though that depends on your internet connection. If the command fails (i.e. the communication fails) try the command again. If it keeps failing and no packages are installed, check your proxy configuration, and your internet connection. If it still does not work, download the packages from **"http://www.r-project.org"** and install them manually from the R Gui.

## 4. How to load the package in R? ##

Once you have installed the package, you can load the library 'analogues' by typing in the R console:

```
library(analogues)
```

This will take less than a second to load, and you won't notice any difference. You will know the loading did not fail, as you will not get any error message.

## 5. How to browse the package's help? ##

If you want to browse the package's available help you can type in the R console:

```
help(package="analogues")
```

This will open a small window showing you the main characteristics of the package (Figure 6), including the maintainer(s), the authors, version, and date released, and also an index of all the functions and methods within the package.

http://ccafs-analogues.googlecode.com/svn/wiki/img/fig6-3.1.PNG

_**Figure 6** Help of the analogue package_

If you want to see more specific help (i.e. on a particular function) you need to type in the R console:

```
help(topic)
```

Where `topic` can be any of the functions or objects that are documented in the package help files. For instance, if you want to see the documentation of the function `createParameters()`, then you type:

```
help(createParamters)
```

This would open a new tab or window in your default browser with that function's documentation.

## 6. How to download the accompanying climate data? ##

To get the accompanying climate data of this R-package you need to go again to the downloads section of the Google code repository (Figure 1):
http://code.google.com/p/ccafs-analogues/downloads/list

In that list, you need to search for the "Sample climate data (updated)", the file is named "current\_a1b\_2030.zip". Following the same procedure as in 2 you will download this file and store it into your 'analogues' folder, inside a folder named 'climate-data'. After downloading the file, you will decompress it using any unzipping software. We suggest 7-zip (freely available at http://www.7-zip.org/), or just unzip them with the default Windows unzipping explorer-based tool.

Else, you can grab these data from the CD we provided, copy it to the required folder (i.e. C:/analogues/climate-data) and unzip it.

## 7. What are the accompanying sample climate data? ##

After you unzip these files you will see that many different files will appear on your climate-data folder. These are geographically referenced gridded datasets, in a format called ESRI-ASCII, in a Geographic Coordinate System with datum WGS-1984.

Each of the gridded files contains data for the whole globe land areas (from 180W to 180E in longitude and from 60S to 90N in latitude), every 50 kilometers (0.5 degree) and can be read into R using the 'raster' package. These can be simply referred to as 'grids' or 'rasters'.

There are two types of climate data in our set of files: present-day (referred to as "current" hereafter) and future climates (combination of 1 SRES emissions scenario, one period, and 24 different Global Climate Models). Current climate data are named as:

> __current_{variable name}**{time step}.asc**_

Where 'variable name' can be either tmean (for mean temperature), prec (for rainfall), and bio\_1 to bio\_19 for the 19 bioclimatic indices we calculated from monthly data (Table 1). For tmean and prec there are 12 time steps (corresponding to the 12 months of the year), and for each of the 19 bioclimatic indices, there is only one time step, as they represent seasonal trends or yearly averages of temperatures or rainfall.

_**Table 1** List of bioclimatic indices_

http://ccafs-analogues.googlecode.com/svn/wiki/img/tab1-3.1.PNG

Future climate data are named as:

> __{SRES}_{period}_{GCM}_{variable name}**{time step}.asc**_

In this case the SRES emissions scenario is A1B (a1b), the period is 2030s (average of the 30 year period between 2020 and 2049, that is, 2020\_2049), the 24 different GCMs are listed in Table 2, the variables are the same as for the current climate.

_**Table 2** List of Global Circulation Models (GCMs) for which the data is provided_

http://ccafs-analogues.googlecode.com/svn/wiki/img/tab2-3.1.PNG
