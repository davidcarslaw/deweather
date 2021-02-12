

partialDep <- function(dat, eq, vars, B = 100, n.core = 4, n.trees) {
  
  ## silence R check
  x = y = rel.inf = NULL
  
  if (B == 1) return.mod <- TRUE else return.mod <- FALSE
  
  if (B == 1) {
    
    pred <- runGbm(dat, eq, vars, return.mod = TRUE, simulate = FALSE, n.trees = n.trees)
    
  } else {
    
    cl <- makeCluster(n.core)
    registerDoParallel(cl)
    
    pred <- foreach (i = 1:B, .inorder = FALSE,
                     .packages = "gbm") %dopar%
      runGbm(dat, eq, vars, return.mod = FALSE, simulate = TRUE, n.trees = n.trees)
    
    stopCluster(cl)
    
  }
  
  # partial dependence plots
  
  if (B == 1) {
    
    pd <- pred$pd
    ri <- pred$ri
    mod <- pred$model
    
  } else {
    
    pd <- lapply(pred, "[[", 1)
    pd <- do.call(rbind, pd)
    
    ## relative influence
    ri <- lapply(pred, "[[", 2)
    ri <- do.call(rbind, ri)
    
    mod <- pred[[1]]$model
    
  }
  
  
  resCI <- group_by(pd, var, var_type, x) %>%
    summarise(mean = mean(y),
              lower = quantile(y, probs = c(0.025)),
              upper = quantile(y, probs = c(0.975)))
  
  resRI <- group_by(ri, var) %>%
    summarise(mean = mean(rel.inf),
              lower = quantile(rel.inf, probs = c(0.025)),
              upper = quantile(rel.inf, probs = c(0.975)))
  
  if (return.mod) {
    
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
##'   \code{TRUE} titles and axis labels will automatically try and format
##'   pollutant names and units properly e.g.  by subscripting the `2' in NO2.
##' @param ... other arguments for plotting.
##' @export
##' @return Invisibly returns a list containing the plot and the data used to
##'   make the plot for \code{plotPD} and \code{plot2Way} to allow post
##'   processing e.g. modifying the \code{ggplot}.
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
  
  # if type is numeric
  if (dat$var_type[1] == "numeric") {
    dat$x <- as.numeric(dat$x)
    gap <- prettyGap(dat$x, 50)
    dat$x <- round_any(dat$x, gap)
  }
  
  dat <- group_by(dat, var, var_type, x) %>%
    summarise(mean = mean(mean), lower = min(lower), upper = max(upper))
  
  if (is.null(ylim)) ylim <- rng(dat)
  
  
  if (!variable %in% special && is.numeric(data[[variable]])) {
    
    quants <- data.frame(x = quantile(data[[as.character(variable)]],
                                      probs = 0:10 / 10, na.rm = TRUE))
    
    plt <- ggplot(dat, aes(x, mean, ymin = lower, ymax = upper)) +
      geom_line(size = 1, col = "tomato") +
      geom_ribbon(alpha = 0.3, fill = "tomato") +
      xlab(quickText(variable)) +
      ylab(quickText(Args$ylab)) +
      coord_cartesian(ylim = ylim) +
      ggtitle(title) +
      theme(plot.title = element_text(lineheight = 0.8, face = "bold")) +
      geom_rug(aes(x = x), data = quants, sides = "b",
               inherit.aes = FALSE, size = 1)
    
    ## better hour x-scaling
    if (variable %in% c("hour", "hour.local"))
      plt <- plt + scale_x_continuous(breaks = c(0, 6, 12, 18))
    
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
      coord_cartesian(ylim = ylim) 
    
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
  
  invisible(list(plot = plt, data = dat))
  
}

##' Function to plot all partial dependencies
##'
##' .
##' @title Plot all partial dependencies
##' @param dw_model Model object from running \code{buildMod}.
##' @param ylim y-axis label. Will default to pollutant name.
##' @param nrow Number of rows for th eplots.
##' @param ylab The y-axis label to over-ride default.
##' @param ... extra plotting arguments.
##' @export
##' @return A plot
##' @author David Carslaw
plotAllPD <- function(dw_model, ylim = NULL, nrow = NULL, ylab = NULL, ...) {
  
  if (class(dw_model) != "deweather")
    stop ("Need to supply a deweather object from buildMod.")
  
  
  ## plot most influencial predictor first
  influ <- dw_model$influence
  influ <- arrange(influ, desc(mean))
  
  ## plot everything
  plots <- lapply(influ$var, plotPD, dat = dw_model, ylab = ylab,
                  ylim = ylim)
  
  do.call(gridExtra::grid.arrange, c(plots, nrow = nrow))
  
}


##' Two-way interaction plots
##'
##' To add
##' @title Plot two-way interactions from gbm model
##' @param dw_model Model object from running \code{buildMod}.
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
##' @param dist When plotting surfaces, \code{dist} controls how far from the
##' original data the predictions should be made. See
##' \code{exclude.too.far} from the \code{mgcv} package. Data are
##' first transformed to a unit square. Values should be between 0 and
##' 1.
##' @param ... Other arguments to be passed for plotting.
##' @export
##' @importFrom mgcv exclude.too.far
##' @return To add
##' @author David Carslaw
plot2Way <- function(dw_model, variable = c("ws", "temp"), res = 100,
                     exlude = TRUE, cols = "default", dist = 0.05, ...) {
  
  ## silence R check
  hour = weekday = NULL
  
  if (class(dw_model) != "deweather")
    stop ("Need to supply a deweather object from buildMod.")
  
  ## extract from deweather object
  data <- dw_model$data
  mod <- dw_model$model
  
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
    tf <- mgcv::exclude.too.far(gx, gy, x, y, dist)
    
    res$y[tf] <- NA
  }
  
  if ("trend" %in% names(res)) {
    res <- decimalDate(res, "trend")
    res$trend <- res$date
  }
  
  
  
  if (all(sapply(res, is.numeric))) {
    
    var1 <- variable[1]
    var2 <- variable[2]
    
    
    plt <- ggplot(res, aes_string(var1, var2, fill = "y")) +
      geom_raster() +
      scale_fill_gradientn(colours = openColours(cols, 100))
    
    if (any(is.na(res$y))) {
      
      plt <- plt + 
        geom_tile(data = subset(res, is.na(y)),
                  aes(colour = "missing"),
                  linetype = 0, fill = "grey92",
                  alpha = 1, show.legend = FALSE) +
        labs(fill = mod$response.name)
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
      scale_fill_gradientn(colours = openColours(cols, 100)) +
      labs(fill = mod$response.name)
    
    print(plt)
    
  }
  
  invisible(list(plot = plt, data = res))
  
  
}
