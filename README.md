
<!-- Edit the README.Rmd only!!! The README.md is generated automatically from README.Rmd. -->
deweather: an R package to remove meteorological variation from air quality data
================================================================================

<img src="inst/plume.png" alt="openair logo" width="35%" />

**deweather** is an R package developed for the purpose of 'removing' the influence of meteorology from air quality time series data. It is part of the [openair](http://davidcarslaw.github.io/openair/) suite of packages designed to support the analysis of air quality data and related data.

The **deweather** package uses a *boosted regression tree* approach for modelling air quality data. These and similar techniques provide powerful tools for building statistical models of air quality data. They are able to take account of the many complex interactions between variables as well as non-linear relationships between the variables.

The modelling can be computationally intensive and therefore **deweather** makes use of the parallel processing, which should work on Windows, Linux and Mac OSX.

Installation
------------

Installation of **deweather** from GitHub should be easy using the devtools package.

``` r
require(devtools)
install_github('davidcarslaw/deweather')
```

Description
-----------

Meteorology play a central role in affecting the concentrations of pollutants in the atmosphere. When considering trends in air pollutants it can be very difficult to know whether a change in concentration is due to emissions or meteorology.

The **deweather** package uses a powerful statistical technique based on *boosted regression trees* using the **gbm** package (Ridgeway, 2017). Statistical models are developed to explain concentrations using meteorological and other variables. These models can be tested on randomly withheld data with the aim of developing the most appropriate model.

Example data set
----------------

The **deweather** package comes with a comprehensive data set of air quality and meteorological data. The air quality data is from Marylebone Road in central London (obtained from the **openair** package) and the meteorological data from Heathrow Airport (obtained from the **worldmet** package).

The `road_data` data frame contains various pollutants such a NOx, NO2, ethane and isoprene as well as meteorological data including wind speed, wind direction, relative humidity, ambient temperature and cloud cover.

``` r
library(deweather)
head(road_data)
##                  date nox no2 ethane isoprene benzene  ws  wd air_temp
## 1 1998-01-01 00:00:00 546  74     NA       NA      NA 1.0 280     3.60
## 2 1998-01-01 01:00:00  NA  NA     NA       NA      NA 1.0 230     3.50
## 3 1998-01-01 02:00:00  NA  NA     NA       NA      NA 1.5 180     4.25
## 4 1998-01-01 03:00:00 944  99     NA       NA      NA  NA  NA       NA
## 5 1998-01-01 04:00:00 894 149     NA       NA      NA 1.5 180     3.80
## 6 1998-01-01 05:00:00 506  80     NA       NA      NA 1.0 190     3.50
##         RH cl
## 1 89.41776  2
## 2 90.67753  2
## 3 87.60679  2
## 4       NA NA
## 5 89.43347  1
## 6 89.40989 NA
```

For those interested in obtaining the data directly, the following code can be used.

``` r
library(openair)
library(worldmet)
library(dplyr)

# import AQ data
road_data <- importAURN(site = "my1", year = 1998:2016, hc = TRUE)

# import met data
met <- importNOAA(year = 1998:2016)

# join together but ignore met data in road_data because it is modelled
road_data <- left_join(select(road_data, -ws, -wd), met, by = "date")

road_data <- select(road_data, date, nox, no2, ethane, isoprene, 
                    benzene, ws, wd, air_temp, RH, cl)
```

Construct and test model(s)
---------------------------

Build a model
-------------

Examine the partial dependencies
--------------------------------

Apply meteorological averaging
------------------------------

References
----------

Carslaw, D.C. and P.J. Taylor (2009). Analysis of air pollution data at a mixed source location using boosted regression trees. Atmospheric Environment. Vol. 43, pp. 3563–3570.

Carslaw, D.C., Williams, M.L. and B. Barratt A short-term intervention study — impact of airport closure on near-field air quality due to the eruption of Eyjafjallajökull. (2012) Atmospheric Environment, Vol. 54, 328–336.

Greg Ridgeway with contributions from others (2017). gbm: Generalized Boosted Regression Models. Rpackage version 2.1.3. (<https://CRAN.R-project.org/package=gbm>)
