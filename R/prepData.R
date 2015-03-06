##' Function to prepare data frame for modelling
##'
##' This function takes a data frame that contains a field \code{date}
##' and other variables and adds other common variables needed by the
##' modelling functions. This function is run automatically by
##' \code{buildMod} but can be used separately for further
##' analysis. These variables include:
##'
##' \itemize{
##'
##' \item hour The hour of the day from 0 to 23.
##'
##' \item hour.local The hour in the local time zone. Note that the
##' local time zone will need to be supplied (see
##' \code{local.tz}). The purpose of using local time rather than UTC
##' is that emissions can vary more strongly by local time rather than
##' UTC.
##'
##' \item weekday The day of the week.
##'
##' \item trend The trend is calculated as a decimal year. 
##'
##' \item week The week of the year. Useful for taking account of
##' long-term seasonal variations.
##'
##' \item jday The Julian Day number.
##'
##' \item month month of the year. Useful for taking account of
##' long-term seasonal variations.
##'
##' }
##'
##' 
##' 
##' @title Prepare data for boosting algorithm
##' @param mydata A data frame to process.
##' @param add Names of explanatory variables to include.
##' @param local.tz Used if hour needs to be expressed in local
##' time. This can be useful for situations where the anthropogenic
##' emissions source is strong and follows local time rather than UTC.
##' @param lag Variables(s) to lag. Any variables included here will
##' add new columns to the data frame. For example \code{lag = "ws"}
##' with add a new columns \code{lag1ws}. Adding some varaibles here
##' can improve the explanatory power of the models. Variables are
##' lagged by one unit of time.
##' @export
##' @return A data frame with new variables.
##' @author David Carslaw
prepData <- function(mydata, add = c("hour", "hour.local", "weekday", "trend", "week",
                                  "jday", "month"), local.tz = "Europe/London",
                     lag = NULL) {

    ## Some cheack to make sure data are OK.
    if (!"date" %in% names(mydata)) stop("No date field supplied.")

    if (class(mydata$date) != "Date" || class(mydata$date) != "POSIXct")
        stop("Date format is not correct - should be POSIXct or Date.")

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
