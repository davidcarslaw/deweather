##' Function to prepare data frame for modelling
##'
##' Info here
##' @title Prepare data for boosting algorithm
##' @param mydata A data frame to process.
##' @param add Names of explanatory variables to include.
##' @param local.tz Used if hour needs to be expressed in local
##' time. This can be useful for situations where the anthropogenic
##' emissions source is strong and follows local time rather than UTC.
##' @param lag Variables(s) to lag. Any variables included here will
##' add new columns to the data frame. For example \code{lag = "ws"}
##' with add a new columns \code{lag1ws}. Adding some varaibles here
##' can improve the explanatory power of the models.
##' @export
##' @return A data frame with new variables.
##' @author David Carslaw
prepData <- function(mydata, add = c("hour", "hour.local", "weekday", "trend", "week",
                                  "jday", "month"), local.tz = "Europe/London",
                      lag = NULL) {

    if ("hour" %in% add) mydata$hour <- as.numeric(format(mydata$date, "%H"))

    if ("hour.local" %in% add)
        mydata$hour.local <- as.numeric(format(as.POSIXct(format(mydata$date,
                                                                 tz = local.tz)), "%H"))

    if ("weekday" %in% add) mydata$weekday <- as.factor(format(mydata$date, "%A"))

    if ("trend" %in% add) mydata$trend <- as.numeric(format(mydata$date, "%Y")) +
      as.numeric(format(mydata$date, "%j")) / 366

    if ("week" %in% add) mydata$week <- as.numeric(format(mydata$date, "%W"))

    if ("jday" %in% add) mydata$jday <- as.numeric(format(mydata$date, "%j"))

    if ("month" %in% add) mydata$month <- as.factor(format(mydata$date, "%B"))

    
    ## add lagged variables
    if (!is.null(lag)) {

        for (i in seq_along(lag))
            mydata[[paste0("lag1", lag[i])]] <- mydata[[lag[i]]][c(NA, 1:(nrow(mydata) - 1))]
        
    }
    
   ## NaN spells trouble for gbm for some reason
    mydata[] <- lapply(mydata, function(x){replace(x, which(is.nan(x)), NA)})
    mydata
}
