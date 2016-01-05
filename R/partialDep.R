

partialDep <- function(dat, eq, vars, B = 100, n.core = 4) {

    ## silence R check
    x = y = rel.inf = NULL

    if (B == 1) return.mod <- TRUE else return.mod <- FALSE

    cl <- makeCluster(n.core)
    registerDoParallel(cl)

    pred <- foreach (i = 1:B, .inorder = FALSE,
                     .packages = c("gbm", "plyr")) %dopar%
    runGbm(dat, eq, vars, return.mod, simulate = TRUE)

    stopCluster(cl)

    ## partial dependence plots
    pd <- lapply(pred, "[[", 1)
    pd <- do.call(rbind, pd)

    ## relative influence
    ri <- lapply(pred, "[[", 2)
    ri <- do.call(rbind, ri)


    resCI <- group_by(pd, var, x) %>%
      summarise(mean = mean(y),
                lower = quantile(y, probs = c(0.025)),
                upper = quantile(y, probs = c(0.975)))

    resRI <- group_by(ri, var) %>%
      summarise(mean = mean(rel.inf),
                lower = quantile(rel.inf, probs = c(0.025)),
                upper = quantile(rel.inf, probs = c(0.975)))

    if (return.mod) {

        mod <- pred[[1]]$model


        return(list(resCI, resRI, mod))

    } else {

        return(list(resCI, resRI))

    }

}


##' Function to plot partial dependence plots with bootstrap uncertainties
##'
##'
##' @title Partial dependence plots with uncertainties.
##' @param dat Model object from running \code{buildMod}.
##' @param variable The variable to plot.
##' @param ylim user-specified \code{ylim}.
##' @param plotit Should a plot be produced?
##' @param auto.text Either \code{TRUE} (default) or \code{FALSE}. If
##' \code{TRUE} titles and axis labels will automatically try and
##' format pollutant names and units properly e.g.  by subscripting
##' the `2' in NO2.
##' @param ... other arguments for plotting.
##' @export
##' @return A plot
##' @author David Carslaw
plotPD <- function(dat, variable, ylim = NULL, plotit = TRUE,
                   auto.text = TRUE, ...) {

    ## silence R check
    x = . = lower = upper = NULL

    if (class(dat) != "deweather")
        stop ("Need to supply a deweather object from buildMod.")

    ## extract from deweather object
    influ <- dat$influence
    mod <- dat$model
    data <- dat$data
    dat <- dat$pd

    ## make sure variable is character
    variable <- as.character(variable)

    ## variables to be treated specifically
    special <- c("trend", "weekday")

    ## select influence of interest
    influ <- influ[influ$var == variable, ]

    ## title for plot
    title <- paste0(variable, " (influ. = ", round(influ$mean, 1), "%)")
    
    ## Args setup
    Args <- list(...)
    
    Args$ylab <- if ("ylab" %in% names(Args))
                           quickText(Args$ylab, auto.text) else mod$response.name
    
    ## check if variable present
    if (!variable %in% dat$var) stop ("Variable not present in data.")

    ## select variable of interest
    dat <- dat[dat$var == variable, ]

    gap <- openair:::prettyGap(dat$x, 40)
    dat$x <- openair:::round_any(dat$x, gap)

    dat <- group_by(dat, var, x) %>%
      summarise_each(funs(mean(.)))

    if (is.null(ylim)) ylim <- rng(dat)
    

    if (!variable %in% special && is.numeric(data[[variable]])) {

        quants <- data.frame(x = quantile(data[[as.character(variable)]],
                                          probs = 0:10 / 10, na.rm = TRUE))

        plt <- ggplot(dat, aes(x, mean, ymin = lower, ymax = upper)) +
            geom_line(size = 1, col = "tomato") +
            geom_ribbon(alpha = 0.3, fill = "tomato") +
            xlab(quickText(variable)) +
            ylab(quickText(Args$ylab)) +
            ylim(ylim) +
            ggtitle(title) +
            theme(plot.title = element_text(lineheight = 0.8, face = "bold")) +
            geom_rug(aes(x = x), data = quants, sides = "b",
                     inherit.aes = FALSE, size = 1)

       }

    if (variable == "trend") {
        
        dat <- decimalDate(dat, date = "x")

        plt <- ggplot(dat, aes(date, mean, ymin = lower, ymax = upper)) +
            geom_line(size = 1, col = "tomato") +
            geom_ribbon(alpha = 0.3, fill = "tomato") +
            xlab(quickText(variable)) +
            ylab(quickText(Args$ylab)) +
            ggtitle(title) +
            theme(plot.title = element_text(lineheight = 0.8, face = "bold")) +
            ylim(ylim) 
        
    }

    ## for factors/character variables
    
    if (!variable %in% special && !is.numeric(data[[variable]])) {
        
        dat$x <- factor(dat$x)
    
        plt <- ggplot(dat, aes(x, mean, ymin = lower, ymax = upper,
                               xmin = as.numeric(x) - 0.4,
                               xmax = as.numeric(x) + 0.4)) +
            geom_point(size = 2, col = "tomato") +
            geom_rect(alpha = 0.4, fill = "tomato") +
            xlab(quickText(variable)) +
            ylab(quickText(Args$ylab)) +
            ggtitle(title) +
            theme(plot.title = element_text(lineheight = 0.8, face = "bold")) +
            ylim(ylim) 

    }

    
    if (variable == "weekday") {

        ## change to weekday names
        dat$x <- factor(dat$x)
        weekday.names <- format(ISOdate(2000, 1, 2:8), "%a")
        levels(dat$x) <- sort(weekday.names)

        dat$x <- ordered(dat$x, levels = weekday.names)

        plt <- ggplot(dat, aes(x, mean, ymin = lower, ymax = upper,
                               xmin = as.numeric(x) - 0.4, xmax = as.numeric(
                                                               x) + 0.4)) +
            geom_point(size = 2, col = "tomato") +
            geom_rect(alpha = 0.4, fill = "tomato") +
            xlab(quickText(variable)) +
            ylab(quickText(Args$ylab)) +
            ggtitle(title) +
            theme(plot.title = element_text(lineheight = 0.8, face = "bold")) +
            ylim(ylim) 

    }

   
    if (plotit) return(plt)
    invisible(plt)

}

##' Function to plot all partial dependencies
##'
##' .
##' @title Plot all partial dependencies
##' @param dat Model object from running \code{buildMod}.
##' @param ylim y-axis label. Will default to pollutant name.
##' @param nrow Number of rows for th eplots.
##' @param ylab The y-axis label to over-ride default.
##' @param ... extra plotting arguments.
##' @export
##' @return A plot
##' @author David Carslaw
plotAllPD <- function(dat, ylim = NULL, nrow = NULL, ylab = NULL, ...) {

    if (class(dat) != "deweather")
        stop ("Need to supply a deweather object from buildMod.")

    
    ## plot most influencial predictor first
    influ <- dat$influence
    influ <- arrange(influ, desc(mean))

    ## plot everything
    plots <- lapply(influ$var, plotPD, dat = dat, ylab = ylab,
                    ylim = ylim)

    do.call(grid.arrange, c(plots, nrow = nrow))
   
}


##' Two-way interaction plots
##'
##' To add
##' @title Plot two-way interactions from gbm model
##' @param dat Model object from running \code{buildMod}.
##' @param variable The variables to plot. Must be of length two
##' e.g. \code{variables = c("ws", "wd"}.
##' @param res Resolution in x-y i.e. number of points in each dimension.
##' @param exlude Should surfaces exlude predictions too far from
##' original data? The default is \code{TRUE}.
##' @param cols Colours to be used for plotting. Options include
##' \dQuote{default}, \dQuote{increment}, \dQuote{heat}, \dQuote{jet}
##' and user defined. For user defined the user can supply a list of
##' colour names recognised by R (type \code{colours()} to see the
##' full list). An example would be \code{cols = c("yellow", "green",
##' "blue")}
##' @param ... Other arguments to be passed for plotting.
##' @export
##' @importFrom mgcv exclude.too.far
##' @return To add
##' @author David Carslaw
plot2Way <- function(dat, variable = c("ws", "temp"), res = 100,
                     exlude = TRUE, cols = "default", ...) {

    ## silence R check
    hour = weekday = NULL

    if (class(dat) != "deweather")
        stop ("Need to supply a deweather object from buildMod.")

    ## extract from deweather object
    data <- dat$data
    mod <- dat$model

    res <- plot.gbm(mod, i.var = variable, continuous.resolution = res,
                    return.grid = TRUE)
    
    ## exclude predictions too far from data (from mgcv)

    if (exlude && !"weekday" %in% variable) {
        sub <- na.omit(data[, variable]) ## pairs of variables
        x <- sub[[variable[1]]] ## x data
        y <- sub[[variable[2]]] ## y data
        
        mx <- unique(res[, 1])
        my <- unique(res[, 2])
        n <- length(mx)
        gx <- rep(mx, n)
        gy <-rep(my, rep(n, n))
        tf <- mgcv::exclude.too.far(gx, gy, x, y, 0.05)
        
        res$y[tf] <- NA
    }

    if (all(sapply(res, is.numeric))) {

        var1 <- variable[1]
        var2 <- variable[2]
        
        
        plt <- ggplot(res, aes_string(var1, var2, fill = "y")) +
            geom_raster() +
            scale_fill_gradientn(colours = openColours(cols, 100))

        if (any(is.na(res$y))) {
            
            plt <- plt + geom_tile(data = subset(res, is.na(y)),
                                   aes(colour = "missing"),
                                   linetype = 0, fill = "grey92",
                                   alpha = 1)
        }

        print(plt)
            
    } else {

        var1 <- variable[1]
        var2 <- variable[2]

        ## need to rename variables that use openair dates
        if ("hour" %in% variable) {

            id <- which(variable == "hour")
            variable[id] <- "Hour"
            var2 <- variable[which(variable != "Hour")]
            res <- rename(res, Hour = hour)
          #  res$Hour <- factor(round(res$Hour))
            var1 <- "Hour"

        }

        if ("weekday" %in% variable) {

            id <- which(variable == "weekday")
            variable[id] <- "Weekday"
            var2 <- variable[which(variable != "Weekday")]
            res <- rename(res, Weekday = weekday)

            weekday.names <- format(ISOdate(2000, 1, 2:8), "%a")
            levels(res$Weekday) <- sort(weekday.names)
            res$Weekday <- ordered(res$Weekday, levels = weekday.names)
            var1 <- "Weekday"

        }
        
         plt <- ggplot(res, aes_string(var1, var2, fill = "y")) +
            geom_tile() +
            scale_fill_gradientn(colours = openColours(cols, 100)) 

        print(plt)

     }

    invisible(res)


}
