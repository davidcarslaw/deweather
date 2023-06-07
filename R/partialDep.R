
#' @noRd
#' @importFrom rlang .data
partialDep <- function(dat, eq, vars, B = 100, n.core = 4, n.trees, seed) {
  if (B == 1) return.mod <- TRUE else return.mod <- FALSE

  if (B == 1) {
    pred <- runGbm(dat, eq, vars,
      return.mod = TRUE, simulate = FALSE,
      n.trees = n.trees, seed
    )
  } else {
    cl <- parallel::makeCluster(n.core)
    doParallel::registerDoParallel(cl)

    pred <- foreach::foreach(
      i = 1:B, .inorder = FALSE,
      .packages = "gbm", .export = "runGbm"
    ) %dopar%
      runGbm(dat, eq, vars,
        return.mod = FALSE, simulate = TRUE,
        n.trees = n.trees
      )

    parallel::stopCluster(cl)
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


  resCI <-
    dplyr::group_by(pd, .data$var, .data$var_type, .data$x) %>%
    dplyr::summarise(
      mean = mean(.data$y),
      lower = stats::quantile(.data$y, probs = c(0.025)),
      upper = stats::quantile(.data$y, probs = c(0.975))
    )

  resRI <- dplyr::group_by(ri, .data$var) %>%
    dplyr::summarise(
      mean = mean(.data$rel.inf),
      lower = stats::quantile(.data$rel.inf, probs = c(0.025)),
      upper = stats::quantile(.data$rel.inf, probs = c(0.975))
    )

  if (return.mod) {
    return(list(resCI, resRI, mod))
  } else {
    return(list(resCI, resRI))
  }
}


#' Function to plot partial dependence plots with bootstrap uncertainties
#'
#' @param dat Model object from running [buildMod()].
#' @param variable The variable to plot.
#' @param ylim user-specified \code{ylim}.
#' @param plotit Should a plot be produced?
#' @param intervals Number of intervals to to calculate partial dependence over.
#' @param auto.text Either \code{TRUE} (default) or \code{FALSE}. If \code{TRUE}
#'   titles and axis labels will automatically try and format pollutant names
#'   and units properly e.g.  by subscripting the `2' in NO2.
#' @param ... other arguments for plotting.
#' @export
#' @importFrom rlang .data
#' @return Invisibly returns a list containing the plot and the data used to
#'   make the plot for [plotPD()] and [plot2Way()] to allow post
#'   processing, e.g., modifying the \code{ggplot}.
#' @author David Carslaw
plotPD <- function(dat, variable, ylim = NULL, plotit = TRUE,
                   intervals = 40,
                   auto.text = TRUE, ...) {
  if (!inherits(dat, "deweather")) {
    stop("Need to supply a deweather object from buildMod.")
  }

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

  Args$ylab <- if ("ylab" %in% names(Args)) {
    openair::quickText(Args$ylab, auto.text)
  } else {
    mod$response.name
  }

  ## check if variable present
  if (!variable %in% dat$var) stop("Variable not present in data.")

  ## select variable of interest
  dat <- dat[dat$var == variable, ]

  # if type is numeric
  if (dat$var_type[1] == "numeric") {
    dat$x <- as.numeric(dat$x)
    gap <- prettyGap(dat$x, intervals)
    dat$x <- round_any(dat$x, gap)
  }

  dat <- dplyr::group_by(dat, .data$var, .data$var_type, .data$x) %>%
    dplyr::summarise(
      mean = mean(.data$mean),
      lower = min(.data$lower),
      upper = max(.data$upper)
    )

  if (is.null(ylim)) ylim <- rng(dat)


  if (!variable %in% special && is.numeric(data[[variable]])) {
    quants <- data.frame(x = stats::quantile(data[[as.character(variable)]],
      probs = 0:10 / 10, na.rm = TRUE
    ))

    plt <- ggplot2::ggplot(dat, ggplot2::aes(.data$x, .data$mean,
      ymin = .data$lower, ymax = .data$upper
    )) +
      ggplot2::geom_line(size = 1, col = "tomato") +
      ggplot2::geom_ribbon(alpha = 0.3, fill = "tomato") +
      ggplot2::xlab(openair::quickText(variable)) +
      ggplot2::ylab(openair::quickText(Args$ylab)) +
      ggplot2::coord_cartesian(ylim = ylim) +
      ggplot2::ggtitle(title) +
      ggplot2::theme(plot.title = ggplot2::element_text(lineheight = 0.8, face = "bold")) +
      ggplot2::geom_rug(ggplot2::aes(x = .data$x),
        data = quants, sides = "b",
        inherit.aes = FALSE, size = 1
      )

    ## better hour x-scaling
    if (variable %in% c("hour", "hour.local")) {
      plt <- plt + ggplot2::scale_x_continuous(breaks = c(0, 6, 12, 18))
    }
  }

  if (variable == "trend") {
    dat <- decimalDate(dat, date = "x")

    plt <- ggplot2::ggplot(dat, ggplot2::aes(.data$date, .data$mean,
      ymin = .data$lower, ymax = .data$upper
    )) +
      ggplot2::geom_line(size = 1, col = "tomato") +
      ggplot2::geom_ribbon(alpha = 0.3, fill = "tomato") +
      ggplot2::xlab(openair::quickText(variable)) +
      ggplot2::ylab(openair::quickText(Args$ylab)) +
      ggplot2::ggtitle(title) +
      ggplot2::theme(plot.title = ggplot2::element_text(lineheight = 0.8, face = "bold")) +
      ggplot2::coord_cartesian(ylim = ylim)
  }

  ## for factors/character variables

  if (!variable %in% special && !is.numeric(data[[variable]])) {
    dat$x <- factor(dat$x)

    plt <- ggplot2::ggplot(dat, ggplot2::aes(.data$x, .data$mean,
      ymin = .data$lower, ymax = .data$upper,
      xmin = as.numeric(.data$x) - 0.4,
      xmax = as.numeric(.data$x) + 0.4
    )) +
      ggplot2::geom_point(size = 2, col = "tomato") +
      ggplot2::geom_rect(alpha = 0.4, fill = "tomato") +
      ggplot2::xlab(openair::quickText(variable)) +
      ggplot2::ylab(openair::quickText(Args$ylab)) +
      ggplot2::ggtitle(title) +
      ggplot2::theme(plot.title = ggplot2::element_text(lineheight = 0.8, face = "bold")) +
      ggplot2::ylim(ylim)
  }


  if (variable == "weekday") {
    ## change to weekday names
    dat$x <- factor(dat$x)
    weekday.names <- format(ISOdate(2000, 1, 2:8), "%a")
    levels(dat$x) <- sort(weekday.names)

    dat$x <- ordered(dat$x, levels = weekday.names)

    plt <- ggplot2::ggplot(dat, ggplot2::aes(.data$x, .data$mean,
      ymin = .data$lower,
      ymax = .data$upper,
      xmin = as.numeric(.data$x) - 0.4,
      xmax = as.numeric(.data$x) + 0.4
    )) +
      ggplot2::geom_point(size = 2, col = "tomato") +
      ggplot2::geom_rect(alpha = 0.4, fill = "tomato") +
      ggplot2::xlab(openair::quickText(variable)) +
      ggplot2::ylab(openair::quickText(Args$ylab)) +
      ggplot2::ggtitle(title) +
      ggplot2::theme(plot.title = ggplot2::element_text(lineheight = 0.8, face = "bold")) +
      ggplot2::ylim(ylim)
  }

  if (plotit) print(plt)

  invisible(list(plot = plt, data = dat))
}

#' Plot all partial dependencies
#'
#' @param dw_model Model object from running \code{buildMod}.
#' @param ylim y-axis label. Will default to pollutant name.
#' @param nrow Number of rows for the plots.
#' @param ... extra plotting arguments.
#' @export
#' @return A plot
#' @author David Carslaw
plotAllPD <- function(dw_model, ylim = NULL, nrow = NULL, ...) {
  if (!inherits(dw_model, "deweather")) {
    stop("Need to supply a deweather object from buildMod.")
  }

  ## plot most influencial predictor first
  influ <- dw_model$influence
  influ <- dplyr::arrange(influ, dplyr::desc(mean))

  ## plot everything
  plots <- lapply(influ$var, plotPD,
    dat = dw_model,
    ylim = ylim, plotit = FALSE, ...
  )

  # extract first element of list, which is the plot
  thedata <- sapply(plots, "[", 2)
  plots <- sapply(plots, "[", 1)
  
  # plot all outputs
  do.call(gridExtra::grid.arrange, c(plots, nrow = nrow))
  
  # name plots & data for easy indexing
  names(thedata) <- influ$var
  names(plots) <- influ$var
  
  # invisibly return list
  invisible(list(plot = plots, data = thedata))
}


#' Plot two-way interactions from gbm model
#'
#' @param dw_model Model object from running \code{buildMod}.
#' @param variable The variables to plot. Must be of length two
#' e.g. \code{variables = c("ws", "wd"}.
#' @param res Resolution in x-y i.e. number of points in each dimension.
#' @param exlude Should surfaces exlude predictions too far from
#' original data? The default is \code{TRUE}.
#' @param cols Colours to be used for plotting. Options include
#' \dQuote{default}, \dQuote{increment}, \dQuote{heat}, \dQuote{jet}
#' and user defined. For user defined the user can supply a list of
#' colour names recognised by R (type \code{colours()} to see the
#' full list). An example would be \code{cols = c("yellow", "green",
#' "blue")}
#' @param dist When plotting surfaces, \code{dist} controls how far from the
#' original data the predictions should be made. See
#' \code{exclude.too.far} from the \code{mgcv} package. Data are
#' first transformed to a unit square. Values should be between 0 and
#' 1.
#' @param ... Other arguments to be passed for plotting.
#' @export
#' @return To add
#' @author David Carslaw
plot2Way <- function(dw_model, variable = c("ws", "temp"), res = 100,
                     exlude = TRUE, cols = "default", dist = 0.05, ...) {
  if (!inherits(dw_model, "deweather")) {
    stop("Need to supply a deweather object from buildMod.")
  }

  ## extract from deweather object
  data <- dw_model$data
  mod <- dw_model$model

  res <- gbm::plot.gbm(mod,
    i.var = variable, continuous.resolution = res,
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


    plt <- ggplot2::ggplot(res, ggplot2::aes(.data[[var1]], .data[[var2]], fill = .data[["y"]])) +
      ggplot2::geom_raster() +
      ggplot2::scale_fill_gradientn(colours = openair::openColours(cols, 100), na.value = "transparent")

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

    plt <- ggplot2::ggplot(res, ggplot2::aes(.data[[var1]], .data[[var2]], fill = .data[["y"]])) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradientn(
        colours = openair::openColours(cols, 100),
        na.value = "transparent"
      ) +
      ggplot2::labs(fill = openair::quickText(mod$response.name))

    print(plt)
  }

  invisible(list(plot = plt, data = res))
}
