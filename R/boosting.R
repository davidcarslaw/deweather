




##########################################################################################
## Two ways of running this function:                                                   ##
## The first is to use bootstrap simulations to calculate the uncertainty in the trend  ##
## which requires a FIXED prediction data set. This requires producing lots of models   ##
## and is very time consuming.                                                          ##
##                                                                                      ##
## The second is to run the model once to get the best one and then randomly sample the ##
## met and other data many times to get an idea of meteorologically averaged trends.    ##
## This is also intensive but less so than the first option. The advantage is that      ##
## it produces more realistic concentrations i.e. typical, whereeas the first approach  ##
## depends very much on how the prediction set is defined.                              ##
##########################################################################################

runPredictions <- function(mydata = mydata, variables, pollutant = "nox", start = "1/1/2007",
                           end = "10/4/2010", pred.data, return.mod = FALSE,
                           met.sim = FALSE, test = FALSE) {

    ## check variables are in data frame
    ## add date to variables


    mydata <- openair:::checkPrep(mydata, c("date", variables, pollutant), type = "default")
    variables <- paste(variables, collapse = "+")
    eq <- formula(paste(pollutant, "~", variables))

    ## select period
    mydata <- selectByDate(mydata, start = start, end = end)
    gbm.mod <- run.gbm(i = 1, mydata, eq = eq, pollutant = pollutant, return.mod = TRUE,
                       test = test, bootstrap = FALSE)

    do.pred <- function(mydata) {

        date <- seq(min(mydata$date), max(mydata$date), by = "hour")
        n <- length(date) ## random samples
        id <- sample(1 : nrow(mydata), n, replace = TRUE)
        min.trend <- min(mydata$trend)
        max.trend <- max(mydata$trend)
        trend <- seq(min.trend, max.trend, length = n)

        temp <- subset(mydata, select = -trend)
        newdata <- data.frame(trend = seq(min.trend, max.trend, length = n), temp[id, ])
        prediction <- predict.gbm(gbm.mod[[3]], newdata, 1000)
        prediction <- data.frame(date = date, pollutant = prediction)
        names(prediction)[2] <- pollutant
        prediction
    }


    ## run model once but predict many times for randon meteorology
    if (met.sim) {

        library(doSNOW)

        cl <- makeCluster(8)
        registerDoSNOW(cl)


        prediction <- foreach (i = 1:100,  .inorder = FALSE,
                               .packages = "gbm") %dopar%
        do.pred(mydata = mydata)
      #  prediction <- ldply(1:1000, do.pred, mydata)

        stopCluster(cl)

    } else {

        prediction <- predict.gbm(gbm.mod[[3]], pred.data, 1000)
        prediction <- data.frame(date = pred.data$date, pollutant = prediction)
        names(prediction)[2] <- pollutant

    }

    prediction
}



########## models #################################
run.gbm <- function(i, mydata, eq, sub.size = 1, pollutant = "nox", test = FALSE,
                    bootstrap = FALSE, return.mod = FALSE){

    ## if test, leave out 25% for prediction
    if (test) sub.size = 0.75

    missing <- which(is.na(mydata[, pollutant])) ## missing data

    if (length(missing) > 0) mydata <- mydata[-missing, ]

    ## bootstraps required - sample data if yes
    if (bootstrap) {
        mydata <- mydata[sample(1:nrow(mydata), nrow(mydata), replace = TRUE), ]
    }


    N <- nrow(mydata)
    idx <- sample(1:N, round(N * sub.size)) # sample the data
    mydata.sub <- mydata[idx, ]

    test.dat <- mydata[-idx, ]



    trees <- 1000
    gbm.mod <- gbm(eq, data = mydata.sub, distribution = "gaussian", n.trees = trees,
                   shrinkage = 0.1, interaction.depth = 10, bag.fraction = 0.5,
                   train.fraction = 1,  n.minobsinnode = 10,
                   keep.data = TRUE,  verbose = FALSE)

    summary(gbm.mod)

    if (test) {## see how good a model fit is
        ## see how good the results are
        f.predict <- predict.gbm(gbm.mod, test.dat, trees)
        rms <- mean((f.predict - test.dat[,pollutant]) ^ 2)^0.5
        print(rms)
        plot(test.dat[, pollutant], f.predict, pch = ".")
        lm1 <- lm(f.predict ~ test.dat[, pollutant])
        print(summary(lm1))
    }

    ## pull out components needed (trend and weekday/diurnal)
    trend <- plot(gbm.mod, 1, cont = 1000, return.grid = TRUE)

    if (length(grep("weekday", eq)) == 1) {
        diurnal <- plot(gbm.mod, i.var= c("hour.local", "weekday"), 1000, return = T, cont = 24)
    } else {
        diurnal <- NULL
    }

    if (return.mod) {
        list(trend, diurnal, gbm.mod)
    } else {
        list(trend, diurnal)
    }
}


decimalDate <- function(x, date = "date") {
    thedata <- x
    x <- x[, date]
    x.year <- floor(x)
    ## fraction of the year
    x.frac <- x - x.year
    ## number of seconds in each year
    x.sec.yr <- unclass(ISOdate(x.year + 1, 1, 1, 0, 0, 0)) - unclass(ISOdate(x.year, 1, 1, 0, 0, 0))
    ## now get the actual time
    x.actual <- ISOdate(x.year, 1, 1, 0, 0, 0) + x.frac * x.sec.yr
    x.actual <- as.POSIXct(trunc(x.actual, "hours"), "GMT")
    thedata$date <- x.actual
    thedata
}


#### bootstrap uncertainties #########################################################


plot.trend <- function(trend, pollutant, date.breaks = 7, ...) {
    myform <- formula(paste(pollutant, "~ date"))

    blues3 <-  brewer.pal(3, "Blues")
    dates <- openair:::dateBreaks(trend$date, date.breaks)$major ## for date scale
    formats <- openair:::dateBreaks(trend$date, date.breaks)$format
    xlim <- range(trend$date)

    plt <- xyplot(myform, data = trend, type = "l",
                  ylab = quickText(paste(pollutant, "(ug/m3)")),
                  xlab = "date",
                  xlim = xlim,
                  ylim = c(0, NA),
                  scales = list(x = list(at = dates, format = formats)),

                  panel = function(x, y, subscripts, ...) {
                      lrect(xleft = as.numeric(as.POSIXct("2010-04-15 12:00")), ybottom = -10,
                            xright = as.numeric(as.POSIXct("2010-04-21 08:00")),
                            ytop = 1000, border = NA, col = "grey95")
                      panel.grid(-1, 0)
                      panel.abline(v = dates, col = "grey90")


                      lpolygon(c(trend$date, rev(trend$date)), c(trend[, "lower"], rev(trend[, "upper"])),
                               col = rgb(1, 0.5, 0.4, 0.3), border = NA)
                      panel.xyplot(trend$date[subscripts], trend[subscripts, "mean"],
                                   col =  rgb(1, 0.5, 0.4, 1), lwd = 2, ...)

                      panel.xyplot(x, y, col = blues3[3], lwd = 2, ...)
                  }
                  )
    print(plt)
    plt

}





## concentrations under business as usual





