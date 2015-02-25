

partialDep <- function(dat, eq, vars, B = 100) {
    
    cl <- makeCluster(4)
    registerDoParallel(cl)

    pred <- foreach (i = 1:B, .inorder = FALSE, 
                     .packages = c("gbm", "plyr")) %dopar%
    runGbm(dat, eq, vars, simulate = TRUE)

    stopCluster(cl)

    ## partial dependence plots
    pd <- lapply(pred, "[[", 1)
    pd <- do.call(rbind, pd)



    resCI <- group_by(pd, var, x) %>%
      summarise(mean = mean(y),
                lower = quantile(y, probs = c(0.025)),
                upper = quantile(y, probs = c(0.975)))

    return(resCI)

}


##' Function to plot partial dependence plots with bootstrap uncertainties
##'
##' 
##' @title Partial dependence plots with uncertainties.
##' @param dat Model object from running \code{buildMod}.
##' @param variable The variable to plot.
##' @param ylim user-specified \code{ylim}.
##' @param plotit Should a plot be produced?
##' @param ... other arguments for plotting.
##' @export
##' @return A plot
##' @author David Carslaw
plotPD <- function(dat, variable, ylim = NULL, plotit = TRUE, ...) {

    if (class(dat) != "deweather") stop ("Need to supply a deweather object from buildMod.")

    ## extract from deweather object
    mod <- dat$model
    data <- dat$data
    dat <- dat$pd

    ## variable modelled
    ylab <- mod$response.name

    ## check if variable present
    if (!variable %in% dat$var) stop ("Variable not present in data.")
    
    ## select variable of interest
    dat <- dat[dat$var == variable, ]
    
    gap <- openair:::prettyGap(dat$x, 40)
    dat$x <- openair:::round_any(dat$x, gap)
    
    dat <- group_by(dat, var, x) %>%
      summarise_each(funs(mean(.)))

    myform <- formula("mean ~ x")
    
    blues3 <-  RColorBrewer::brewer.pal(3, "Blues")
    
    if (is.null(ylim)) ylim <- rng(dat)


    if (!variable %in% c("trend", "weekday")) {

        plt <- xyplot(myform, data = dat, type = "l",
               xlab = quickText(variable),
                      ylab = quickText(ylab),
                      ylim = ylim, ..., 
               
               panel = function(x, y, subscripts, ...) {
                   
                   panel.grid(-1, -1)
                   
                   lpolygon(c(dat$x, rev(dat$x)),
                            c(dat[["lower"]], rev(dat[["upper"]])),
                            col = blues3[2], border = NA)
                   
                   
                   panel.xyplot(x, y, col = blues3[3], lwd = 2, ...)
                   
                   panel.rug(x = quantile(data[[as.character(variable)]],
                                 probs = 0:10 / 10, na.rm = TRUE), col = "firebrick", lwd = 2)
               }
               )
        
    } 

    if (variable == "trend") {
        myform <- formula("mean ~ date")
        dat <- decimalDate(dat, date = "x")
        
        plt <- xyplot(myform, data = dat, type = "l",
               xlab = quickText(variable),
                      ylab = quickText(ylab), ylim = ylim, ..., 
               
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
        weekday.names <- format(ISOdate(2000, 1, 2:8), "%a")
        levels(dat$x) <- sort(weekday.names)

        dat$x <- ordered(dat$x, levels = weekday.names)
        
        myform <- formula("mean ~ x")
        
        plt <- xyplot(myform, data = dat, type = "l",
                      scales = list(x = list(at = 1:7, labels = weekday.names)),
                      xlab = quickText(variable),
                      ylab = quickText(ylab),
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

    if (plotit) print(plt)
    invisible(plt)
    
}

##' Function to plot all partial dependencies
##'
##' .
##' @title Plot all partial dependencies
##' @param dat Model object from running \code{buildMod}.
##' @param ylim y-axis label. Will default to pollutant name.
##' @param layout The plot layout e.g. \code{c(3, 2)} makes a 3
##' columns by 2 rows grid. If not supplied a square grid is used of
##' sufficient size to accommodate all teh plots.
##' @param ... extra plotting arguments.
##' @export
##' @return A plot
##' @author David Carslaw
plotAllPD <- function(dat, ylim = NULL, layout = NULL, ...) {
    
    if (class(dat) != "deweather") stop ("Need to supply a deweather object from buildMod.")

    ## names of explanatory variables
    var.names <- dat$model$var.names

    ## number of variables
    n <- length(var.names)

    ## dimension of plotting layout
    n.plot <- ceiling(n ^ 0.5)

    ## Layout of plots
    if (!is.null(layout)) {

        combs <- expand.grid(1:layout[1], 1:layout[2])
        
    } else {

        combs <- expand.grid(1:n.plot, 1:n.plot)
        layout <- c(n.plot, n.plot)
        
    }

    

    ## plot most influencial predictor first
    influ <- dat$influence

    ## plot everything
    for (i in 1:n) {

        ## the plot
        plt <- plotPD(dat, variable = influ$var[i], plotit = FALSE,
                      main = list(label = as.character(influ$var[i]), col = "darkorange",
                          fontface = "bold"),
                      sub = paste("Influence", round(influ$rel.inf[i], 1), "%"), ylim, 
                  ...)
                    

        if (i == n) more <- FALSE else more <- TRUE

        print(plt, split = c(combs[i, "Var1"], combs[i, "Var2"], layout[1], layout[2]), more = more)

    }

    
}


##' Two-way intercation plots
##'
##' To add
##' @title Plot two-way interactions from gbm model
##' @param dat Model object from running \code{buildMod}.
##' @param variable The variables to plot. Must be of length two
##' e.g. \code{variables = c("ws", "wd"}.
##' @param ... other arguments to send to \code{openair} \code{scatterPlot}.
##' @export
##' @return To add
##' @author David Carslaw
plot2Way <- function(dat, variable = c("ws", "temp"), ...) {

    if (class(dat) != "deweather") stop ("Need to supply a deweather object from buildMod.")

    ## extract from deweather object
    mod <- dat$model

    res <- plot.gbm(mod, i.var = variable, continuous.resolution = 100,
                    return.grid = TRUE)

    
    if (all(sapply(res, is.numeric))) {
        
        scatterPlot(res, x = variable[1], y = variable[2], z = "y", method = "level", ...)

    } else {

        ## need to rename variables that use openair dates
        if ("hour" %in% variable) {

            id <- which(variable == "hour")
            variable[id] <- "Hour"
            res <- rename(res, Hour = hour)
            res$Hour <- factor(round(res$Hour))

        }

        if ("weekday" %in% variable) {

            id <- which(variable == "weekday")
            variable[id] <- "Weekday"
            res <- rename(res, Weekday = weekday)

            weekday.names <- format(ISOdate(2000, 1, 2:8), "%a")
            levels(res$Weekday) <- sort(weekday.names)
            res$Weekday <- ordered(res$Weekday, levels = weekday.names)
          

        }

        trendLevel(res, x = variable[1], y = variable[2], pollutant = "y", ...)

    }
    
    
}
