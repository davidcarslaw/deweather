##' Function to prepare data frame for modelling
##'
##' .. content for \details{} ..
##' @title Prepare data for boosting algorithm
##' @param mydata A data frame to process.
##' @return 
##' @author David Carslaw
prep.data <- function(mydata) {
   ## add in other variables
    mydata <- transform(mydata, weekday = as.factor(format(date, "%A")),
                        hour = as.numeric(format(date, "%H")),
                        hour.local = as.numeric(format(as.POSIXct(format(date,
                                                             tz = "Europe/London")), "%H")),
                        week = as.numeric(format(date, "%W")),
                        trend = as.numeric(format(date, "%Y")) +
                        as.numeric(format(date, "%j")) / 366)

    mydata$trend <- round(mydata$trend, 3)

    ## NaN spells trouble for gbm for some reason
    mydata[] <- lapply(mydata, function(x){replace(x, which(is.nan(x)), NA)})
    mydata
}
