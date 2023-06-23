#' Function to plot partial dependence plots with bootstrap uncertainties
#'
#' @param dw_model Model object from running [buildMod()].
#' @param variable The variable(s) to plot. Defaults to `"all"`, which plots all
#'   variables.
#' @param ylim user-specified `ylim`.
#' @param intervals Number of intervals to to calculate partial dependence over.
#' @param col Colour(s) to use for the lines/points/uncertainty ribbons. If
#'   multiple colours are provided (e.g., `cols = c("tomato", "royalblue")`),
#'   they will be cycled through until all variables are plotted.
#' @param nrow Number of rows for the plots.
#' @param polar.wd Plot the any wind direction components, labelled "wd", on a
#'   polar axis? Defaults to `FALSE`.
#' @param auto.text Either `TRUE` (default) or `FALSE`. If `TRUE` titles and
#'   axis labels will automatically try and format pollutant names and units
#'   properly e.g.  by subscripting the `2' in NO2.
#' @param plot Should a plot be produced? `FALSE` can be useful when analysing
#'   data to extract plot components and plotting them in other ways.
#' @export
#' @importFrom rlang .data
#' @family deweather model plotting functions
#' @return Invisibly returns a list containing the plot and the data used to
#'   make the plot to allow post processing, e.g., modifying the `ggplot`.
#' @author David Carslaw
plotPD <- function(dw_model,
                   variable = "all",
                   intervals = 40,
                   ylim = NULL,
                   col = "tomato",
                   nrow = NULL,
                   polar.wd = FALSE,
                   auto.text = TRUE,
                   plot = TRUE) {
  if (!inherits(dw_model, "deweather")) {
    stop("Need to supply a deweather object from buildMod.")
  }

  ## plot most influencial predictor first
  influ <- dw_model$influence
  influ <- dplyr::arrange(influ, dplyr::desc(mean))

  if (!"all" %in% variable) {
    influ <- dplyr::filter(influ, .data$var %in% variable)
  }

  ## plot everything
  plots <- purrr::map2(
    .x = influ$var,
    .y = rep(col, length(influ$var))[seq_along(influ$var)],
    .f = ~plot_pd_helper(
    dw_model = dw_model,
    ylim = ylim,
    variable = .x,
    col = .y,
    polar.wd = polar.wd,
    intervals = intervals,
    auto.text = auto.text
  ))

  # extract first element of list, which is the plot
  thedata <- sapply(plots, "[", 2)
  plots <- sapply(plots, "[", 1)

  # plot all outputs
  if (plot) {
    do.call(gridExtra::grid.arrange, c(plots, nrow = nrow))
  }

  # name plots & data for easy indexing
  names(thedata) <- influ$var
  names(plots) <- influ$var

  # invisibly return list
  invisible(list(plot = plots, data = thedata))
}

#' helper function to plot partial dependencies
#' @noRd
plot_pd_helper <- function(dw_model,
                           variable,
                           ylim,
                           col,
                           intervals,
                           polar.wd,
                           auto.text) {
  ## extract from deweather object
  influ <- dw_model$influence
  mod <- dw_model$model
  data <- dw_model$data
  dat <- dw_model$pd

  ## variables to be treated specifically
  special <- c("trend", "weekday", "wd")

  ## select influence of interest
  influ <- influ[influ$var == variable, ]

  ## title for plot
  title <- paste0(variable, " (influ. = ", round(influ$mean, 1), "%)")

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

  # function to theme a partial dep plot
  theme_pd_plot <- function(polar = FALSE) {
    theme <-
      list(
        ggplot2::xlab(openair::quickText(variable)),
        ggplot2::ylab(openair::quickText(mod$response.name, auto.text = auto.text)),
        ggplot2::ggtitle(title),
        ggplot2::theme(plot.title = ggplot2::element_text(lineheight = 0.8, face = "bold"))
      )

    if (!polar) {
      append(theme, ggplot2::coord_cartesian(ylim = ylim))
    } else {
      append(theme, ggplot2::coord_polar())
    }
  }

  if (!variable %in% special && is.numeric(data[[variable]])) {
    quants <- data.frame(x = stats::quantile(data[[as.character(variable)]],
      probs = 0:10 / 10, na.rm = TRUE
    ))

    plt <- ggplot2::ggplot(dat, ggplot2::aes(.data$x, .data$mean,
      ymin = .data$lower, ymax = .data$upper
    )) +
      ggplot2::geom_line(size = 1, col = col) +
      ggplot2::geom_ribbon(alpha = 0.3, fill = col) +
      ggplot2::geom_rug(ggplot2::aes(x = .data$x),
        data = quants, sides = "b",
        inherit.aes = FALSE, size = 1
      ) +
      theme_pd_plot()

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
      ggplot2::geom_line(size = 1, col = col) +
      ggplot2::geom_ribbon(alpha = 0.3, fill = col) +
      theme_pd_plot()
  }

  if (variable == "wd") {
    if (polar.wd) {
      plt <-
        ggplot2::ggplot(
          dat,
          ggplot2::aes(
            .data$x,
            .data$mean,
            ymin = .data$lower,
            ymax = .data$upper
          )
        ) +
        theme_pd_plot(polar.wd) +
        ggplot2::geom_line(size = 1, col = col) +
        ggplot2::geom_ribbon(alpha = 0.3, fill = col) +
        ggplot2::scale_x_continuous(
          limits = c(0, 360),
          labels = c("N", "E", "S", "W"),
          breaks = seq(0, 270, 90)
        ) +
        ggplot2::scale_y_continuous(limits = ylim)
    } else {
      quants <- data.frame(x = stats::quantile(data[[as.character(variable)]],
        probs = 0:10 / 10, na.rm = TRUE
      ))

      plt <- ggplot2::ggplot(dat, ggplot2::aes(.data$x, .data$mean,
        ymin = .data$lower, ymax = .data$upper
      )) +
        ggplot2::geom_line(size = 1, col = col) +
        ggplot2::geom_ribbon(alpha = 0.3, fill = col) +
        ggplot2::geom_rug(ggplot2::aes(x = .data$x),
          data = quants, sides = "b",
          inherit.aes = FALSE, size = 1
        ) +
        theme_pd_plot(FALSE)
    }
  }

  ## for factors/character variables

  if (!variable %in% special && !is.numeric(data[[variable]])) {
    dat$x <- factor(dat$x)

    plt <- ggplot2::ggplot(dat, ggplot2::aes(.data$x, .data$mean,
      ymin = .data$lower, ymax = .data$upper,
      xmin = as.numeric(.data$x) - 0.4,
      xmax = as.numeric(.data$x) + 0.4
    )) +
      ggplot2::geom_point(size = 2, col = col) +
      ggplot2::geom_rect(alpha = 0.4, fill = col) +
      theme_pd_plot()
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
      ggplot2::geom_point(size = 2, col = col) +
      ggplot2::geom_rect(alpha = 0.4, fill = col) +
      theme_pd_plot()
  }

  return(list(plot = plt, data = dat))
}