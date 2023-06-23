#' Plot two-way interactions from gbm model
#'
#' @param dw_model Model object from running [buildMod()].
#' @param variable The variables to plot. Must be of length two
#' e.g. `variables = c("ws", "wd"`.
#' @param res Resolution in x-y i.e. number of points in each dimension.
#' @param exlude Should surfaces exlude predictions too far from
#' original data? The default is `TRUE`.
#' @param cols Colours to be used for plotting. Options include
#' \dQuote{default}, \dQuote{increment}, \dQuote{heat}, \dQuote{jet}
#' and user defined. For user defined the user can supply a list of
#' colour names recognised by R (type `colours()` to see the
#' full list). An example would be `cols = c("yellow", "green",
#' "blue")`
#' @param dist When plotting surfaces, `dist` controls how far from the
#' original data the predictions should be made. See
#' `exclude.too.far` from the `mgcv` package. Data are
#' first transformed to a unit square. Values should be between 0 and
#' 1.
#' @param ... Other arguments to be passed for plotting.
#' @export
#' @return To add
#' @family deweather model plotting functions
#' @author David Carslaw
plot2Way <- function(dw_model,
                     variable = c("ws", "air_temp"),
                     res = 100,
                     exlude = TRUE,
                     cols = "default",
                     dist = 0.05,
                     ...) {
  if (!inherits(dw_model, "deweather")) {
    stop("Need to supply a deweather object from buildMod.")
  }
  
  ## extract from deweather object
  data <- dw_model$data
  mod <- dw_model$model
  
  res <- gbm::plot.gbm(
    mod,
    i.var = variable,
    continuous.resolution = res,
    return.grid = TRUE
  )
  
  
  ## exclude predictions too far from data (from mgcv)
  
  if (exlude && all(sapply(res[variable], is.numeric))) {
    sub <- stats::na.omit(data[, variable]) ## pairs of variables
    x <- sub[[variable[1]]] ## x data
    y <- sub[[variable[2]]] ## y data
    
    mx <- unique(res[, 1])
    my <- unique(res[, 2])
    n <- length(mx)
    gx <- rep(mx, n)
    gy <- rep(my, rep(n, n))
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
    
    
    plt <-
      ggplot2::ggplot(res, ggplot2::aes(.data[[var1]], .data[[var2]], fill = .data[["y"]])) +
      ggplot2::geom_raster() +
      ggplot2::scale_fill_gradientn(colours = openair::openColours(cols, 100),
                                    na.value = "transparent")
    
    if (any(is.na(res$y))) {
      plt <- plt +
        ggplot2::labs(fill = openair::quickText(mod$response.name))
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
      res <- dplyr::rename(res, Hour = .data$hour)
      #  res$Hour <- factor(round(res$Hour))
      var1 <- "Hour"
    }
    
    if ("weekday" %in% variable) {
      id <- which(variable == "weekday")
      variable[id] <- "Weekday"
      var2 <- variable[which(variable != "Weekday")]
      res <- dplyr::rename(res, Weekday = .data$weekday)
      
      weekday.names <- format(ISOdate(2000, 1, 2:8), "%a")
      levels(res$Weekday) <- sort(weekday.names)
      res$Weekday <- ordered(res$Weekday, levels = weekday.names)
      var1 <- "Weekday"
    }
    
    plt <-
      ggplot2::ggplot(res, ggplot2::aes(.data[[var1]], .data[[var2]], fill = .data[["y"]])) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradientn(colours = openair::openColours(cols, 100),
                                    na.value = "transparent") +
      ggplot2::labs(fill = openair::quickText(mod$response.name))
    
    print(plt)
  }
  
  invisible(list(plot = plt, data = res))
}
