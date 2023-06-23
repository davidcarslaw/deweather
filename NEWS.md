# deweather 0.7.2

## Breaking changes

* All functions which expect a `{deweather}` object now use the argument name `dw_model` for consistency.

* The default values for `vars` in any model-building function now uses "air_temp" over "temp". This brings it in line with `road_data` and `{worldmet}` outputs.

* `gbm.interactions()` has been renamed `gbmInteractions()` for consistency with other functions.

* `gbmInf()` has been renamed `plotInfluence()` to be more descriptive and consistent with other plotting functions.

* `plotAllPD()` has been combined with `plotPD()` and has therefore been removed. 

* `dat` is no longer exported. Users should use `road_data` to demo `{deweather}` functions.

## New features

* `plotPD()` now holds all the functionality for plotting partial dependency plots, deprecating `plotAllPD()`. (#12)

  * By default, all partial dependencies are plotted (similar to `plotAllPD()`). The `variable` argument allows users to specify specific variables (similar to the old `plotPD()`). Multiple variables can be provided.
  
  * The `col` argument controls the colour of the PD plots. If multiple colours are specified, they are repeated until all variables have been visualised.
  
  * The `polar.wd` argument will optionally show the wind direction PD on polar coordinates.
  
* `plotInfluence()` (previously `gmbInf()`) has gained two new arguments:

  * `sort` (defaults to `TRUE`) will sort the variables by their mean influence, ordering the values on the y-axis.
  
  * `col` controls the colours. Users can specify `var` (which makes each bar a different colour) or `mean` (which colours by the x-axis values).
  
* Many plotting functions have received the `plot` argument to suppress printing their plots, bringing `{deweather}` in line with recent versions of `{openair}`.

* `testMod()` invisibly returns its table of statisitcs along with its plot. (#9)

* `testMod()` now prints one plot and one table rather than one plot and two tables. This ensures the printed plot will be less distorted.

* The lists returned by `plotPD()` are now named, making them easier to index. (#10)

## Bug fixes

* `diurnalGbm()` now no longer plots extra 2-way graphs as it performs its internal calculations.

# deweather 0.7-1

* Development version of `{deweather}`.
