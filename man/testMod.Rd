% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/testMod.R
\name{testMod}
\alias{testMod}
\title{Function to test different meteorological normalisation models.}
\usage{
testMod(dat, vars = c("trend", "ws", "wd", "hour", "weekday", "temp"),
  pollutant = "nox", train.frac = 0.8)
}
\arguments{
\item{dat}{Data frame to analyse.}

\item{vars}{Explanatory variables to use.}

\item{pollutant}{The name of the variable to apply meteorological
normalisation to.}

\item{train.frac}{Fraction of data to train a model on. The model
is tested against the withheld 0.2 proportion.}
}
\value{
Returns to be added.
}
\description{
Function to build and test a gbm model
}
\details{
Info here
}
\author{
David Carslaw
}
