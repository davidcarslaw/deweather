
<div align="center">

<img src="man/figures/logo.png" height="200"/>

## **deweather**
### open source tools to remove meteorological variation from air quality data

<!-- badges: start -->
[![R-CMD-check](https://github.com/davidcarslaw/deweather/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/davidcarslaw/deweather/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/deweather)](https://CRAN.R-project.org/package=deweather)
<br>
[![github](https://img.shields.io/badge/CODE-github-black?logo=github)](https://github.com/davidcarslaw/deweather)
[![website](https://img.shields.io/badge/DOCS-website-black)](https://davidcarslaw.github.io/deweather)
[![book](https://img.shields.io/badge/DOCS-book-black)](https://bookdown.org/david_carslaw/openair/)
<!-- badges: end -->

</div>

**deweather** is an R package developed for the purpose of "removing" the influence of meteorology from air quality time series data. The package uses a *boosted regression tree* approach for modelling air quality data. These and similar techniques provide powerful tools for building statistical models of air quality data. They are able to take account of the many complex interactions between variables as well as non-linear relationships between the variables.

<div align="center">

*Part of the openair toolkit*

[![openair](https://img.shields.io/badge/openair_core-06D6A0?style=flat-square)](https://davidcarslaw.github.io/openair) | 
[![worldmet](https://img.shields.io/badge/worldmet-26547C?style=flat-square)](https://davidcarslaw.github.io/worldmet) | 
[![openairmaps](https://img.shields.io/badge/openairmaps-FFD166?style=flat-square)](https://davidcarslaw.github.io/openairmaps) | 
[![deweather](https://img.shields.io/badge/deweather-EF476F?style=flat-square)](https://davidcarslaw.github.io/deweather)

</div>

<hr>

## 💡 Core Features

**deweather** makes it straightforward to test, build, and evaluate models in R.

- **Test and build meteorological normalisation models** flexibly using `testMod()` and `buildMod()`.

- **Plot and examine models** in a myriad of ways, including visualising partial dependencies, using functions like `plotPD()` and `plot2Way()`.

- **Apply meteorological averaging** using `metSim()` to obtain a meteorologically normalised air quality timeseries.

Modelling can be computationally intensive and therefore **deweather** makes use of the parallel processing, which should work on Windows, Linux and Mac OSX.

<div align="center">
<img src="man/figures/feature-banner.png" width="800">
</div>

<hr>

## 📖 Documentation

All **deweather** functions are fully documented; access documentation using R in your IDE of choice.

```r
?deweather::buildMod
```

Documentation is also hosted online on the **package website**.

[![website](https://img.shields.io/badge/website-documentation-blue)](https://davidcarslaw.github.io/deweather)

A guide to the openair toolkit can be found in the **online book**, which contains lots of code snippets, demonstrations of functionality, and ideas for the application of **openair**'s various functions.

[![book](https://img.shields.io/badge/book-code_demos_and_ideas-blue)](https://bookdown.org/david_carslaw/openair/)

<hr>

## 🗃️ Installation

**deweather** is not yet on **CRAN**.

The development version of **deweather** can be installed from GitHub using `{pak}`:

``` r
# install.packages("pak")
pak::pak("davidcarslaw/deweather")
```

<hr>

🏛️ **deweather** is primarily maintained by [David Carslaw](https://github.com/davidcarslaw).

📃 **deweather** is licensed under the [GNU General Public License](https://davidcarslaw.github.io/deweather/LICENSE.html).

🧑‍💻 Contributions are welcome from the wider community. See the [contributing guide](https://davidcarslaw.github.io/deweather/CONTRIBUTING.html) and [code of conduct](https://davidcarslaw.github.io/deweather/CODE_OF_CONDUCT.html) for more information.
