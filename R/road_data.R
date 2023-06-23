#' Example data for deweather
#'
#' The `road_data` dataset is provided as an example dataset as part of the
#' `deweather` package. The dataset contains hourly measurements of air
#' pollutant concentrations, wind speed and wind direction collected at the
#' Marylebone (London) air quality monitoring supersite between 1st January 1998
#' and 23rd June 2005.
#'
#' @docType data
#' @format Data frame with 65533 observations (rows) on the following 10
#'   variables: \describe{ \item{list("date")}{Observation date/time stamp in
#'   year-month-day hour:minute:second format (POSIXct). }
#'   \item{list("ws")}{Wind speed, in m/s, as numeric vector.}
#'   \item{list("wd")}{Wind direction, in degrees from North, as a numeric
#'   vector.} \item{list("nox")}{Oxides of nitrogen concentration, in ppb, as a
#'   numeric vector.} \item{list("no2")}{Nitrogen dioxide concentration, in ppb,
#'   as a numeric vector.} \item{list("o3")}{Ozone concentration, in ppb, as a
#'   numeric vector.} \item{list("pm10")}{Particulate PM10 fraction measurement,
#'   in ug/m3 (raw TEOM), as a numeric vector.} \item{list("so2")}{Sulfur
#'   dioxide concentration, in ppb, as a numeric vector.}
#'   \item{list("co")}{Carbon monoxide concentration, in ppm, as a numeric
#'   vector.} \item{list("pm25")}{Particulate PM2.5 fraction measurement, in
#'   ug/m3, as a numeric vector.} }
#' @keywords datasets
#' @family built-in datasets
#' @examples
#'
#' # basic structure
#' head(road_data)
"road_data"
