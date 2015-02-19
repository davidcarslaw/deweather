

partialDep <- function(dat, eq) {
    sims <- 100


    cl <- makeCluster(4)
    registerDoParallel(cl)

    pred <- foreach (i = 1:sims, .inorder = FALSE, 
                     .packages = c("gbm", "plyr")) %dopar%
    runGbm(dat, eq)

    stopCluster(cl)



    ## partial dependence plots
    pd <- lapply(pred, "[[", 1)
    pd <- do.call(rbind, pd)



    resCI <- group_by(pd, var, x) %>%
      summarise(mean = mean(y),
                lower = quantile(y, probs = c(0.025)),
                upper = quantile(y, probs = c(0.975)))

}

plotPD <- function(dat, variable, ylim = NULL, ...) {

    ## select variable of interest
    dat <- dat[dat$var == variable, ]
    
    gap <- openair:::prettyGap(dat$x, 40)
    dat$x <- openair:::round_any(dat$x, gap)
    
    dat <- group_by(dat, var, x) %>%
      summarise_each(funs(mean(.)))

    myform <- formula("mean ~ x")
    
    blues3 <-  RColorBrewer::brewer.pal(3, "Blues")

    if (missing(ylim)) ylim <- rng(dat)


    if (!variable %in% c("trend", "weekday")) {

        plt <- xyplot(myform, data = dat, type = "l",
               xlab = quickText(variable),
                      ylab = quickText(pollutant),
                      ylim = ylim, ..., 
               
               panel = function(x, y, subscripts, ...) {
                   
                   panel.grid(-1, -1)
                   
                   lpolygon(c(dat$x, rev(dat$x)),
                            c(dat[["lower"]], rev(dat[["upper"]])),
                            col = blues3[2], border = NA)
                   
                   
                   panel.xyplot(x, y, col = blues3[3], lwd = 2, ...)
               }
               )
        
    } 

    if (variable == "trend") {
        myform <- formula("mean ~ date")
        dat <- decimalDate(dat, date = "x")
        
        plt <- xyplot(myform, data = dat, type = "l",
               xlab = quickText(variable),
                      ylab = quickText(pollutant), ylim = ylim, ..., 
               
                      panel = function(x, y, ...) {
                   
                   panel.grid(-1, -1)
                   
                   lpolygon(c(dat$date, rev(dat$date)),
                            c(dat[["lower"]], rev(dat[["upper"]])),
                            col = blues3[2], border = NA)
                   
                   
                   panel.xyplot(as.POSIXct(x), y, col = blues3[3], lwd = 2, ...)
               }
               )
        
    }

    if (variable == "weekday") {
        
        ## change to weekday names
        dat$x <- factor(dat$x)
        weekday.names <- format(ISOdate(2000, 1, 2:8), "%A")
        levels(dat$x) <- sort(weekday.names)

        dat$x <- ordered(dat$x, levels = weekday.names)
        
        myform <- formula("mean ~ x")
        
        plt <- xyplot(myform, data = dat, type = "l",
                      scales = list(x = list(at = 1:7, labels = weekday.names)),
                      xlab = quickText(variable),
                      ylab = quickText(pollutant),
                      ylim = ylim, ..., 
                      
                      panel = function(x, y, ...) {

                          panel.grid(-1, 0)
                          panel.abline(v = 1:7, col = "grey85")

                          lrect(as.numeric(dat$x) - 0.3, dat$lower, as.numeric(dat$x) + 0.3,
                                dat$upper, col = blues3[2], border = NA)
                          
                          panel.xyplot(1:7, dat$mean[as.numeric(factor(weekday.names))],
                                       col = blues3[3], lwd = 2, ...)
                      }
                      )
        
    }

    print(plt)
    
}


## Two-way intercation plots
plot2Way <- function(mod, variable = c("ws", "temp")) {

    res <- plot.gbm(mod, i.var = variable, continuous.resolution = 100,
                    return.grid = TRUE)

    scatterPlot(res, x = variable[1], y = variable[2], z = "y", method = "level")
    
    
}
