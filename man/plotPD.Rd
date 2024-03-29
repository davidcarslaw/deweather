% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotPD.R
\name{plotPD}
\alias{plotPD}
\title{Function to plot partial dependence plots with bootstrap uncertainties}
\usage{
plotPD(
  dw_model,
  variable = "all",
  intervals = 40,
  ylim = NULL,
  ylab = NULL,
  col = "tomato",
  nrow = NULL,
  polar.wd = FALSE,
  auto.text = TRUE,
  plot = TRUE
)
}
\arguments{
\item{dw_model}{Model object from running \code{\link[=buildMod]{buildMod()}}.}

\item{variable}{The variable(s) to plot. Defaults to \code{"all"}, which plots all
variables.}

\item{intervals}{Number of intervals to to calculate partial dependence over.}

\item{ylim}{user-specified \code{ylim}.}

\item{ylab}{y-axis label. By default this is the pollutant name.}

\item{col}{Colour(s) to use for the lines/points/uncertainty ribbons. If
multiple colours are provided (e.g., \code{cols = c("tomato", "royalblue")}),
they will be cycled through until all variables are plotted.}

\item{nrow}{Number of rows for the plots.}

\item{polar.wd}{Plot the any wind direction components, labelled "wd", on a
polar axis? Defaults to \code{FALSE}.}

\item{auto.text}{Either \code{TRUE} (default) or \code{FALSE}. If \code{TRUE} titles and
axis labels will automatically try and format pollutant names and units
properly e.g.  by subscripting the `2' in NO2.}

\item{plot}{Should a plot be produced? \code{FALSE} can be useful when analysing
data to extract plot components and plotting them in other ways.}
}
\value{
Invisibly returns a list containing the plot and the data used to
make the plot to allow post processing, e.g., modifying the \code{ggplot}.
}
\description{
Function to plot partial dependence plots with bootstrap uncertainties
}
\seealso{
Other deweather model plotting functions: 
\code{\link{plot2Way}()},
\code{\link{plotInfluence}()}
}
\author{
David Carslaw
}
\concept{deweather model plotting functions}
