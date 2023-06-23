#' Function to plot partial dependence plots with bootstrap uncertainties
#'
#' @param dw_model Model object from running [buildMod()].
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
plotPD <- function(dw_model,
                   variable,
                   ylim = NULL,
                   plotit = TRUE,
                   intervals = 40,
                   auto.text = TRUE,
                   ...) {
  if (!inherits(dw_model, "deweather")) {
    stop("Need to supply a deweather object from buildMod.")
  }
  
  ## extract from deweather object
  influ <- dw_model$influence
  mod <- dw_model$model
  data <- dw_model$data
  dat <- dw_model$pd
  
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
plotAllPD <- function(dw_model,
                      ylim = NULL,
                      nrow = NULL,
                      ...) {
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
