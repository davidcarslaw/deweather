#' Plot a GBM influence plot
#'
#' @param dw_model Model object from running [buildMod()].
#' @param sort Sort the variables by their relative variable influences?
#'   Defaults to `TRUE`. `FALSE` displays them alphabetically.
#' @param col Colour to use to use to colour the bars. Alternatively, users can
#'   provide "var" which will colour each bar differently, or "mean" which will
#'   colour each bar by its mean relative variable influence.
#' @export
#' @return Plot
#' @family deweather model plotting functions
#' @author David Carslaw
plotInfluence <- function(dw_model,
                          col = "grey30",
                          sort = TRUE) {
  check_dwmod(dw_model)

  ## extract influence data from object
  influ <- dw_model$influence

  # sort variables?
  if (!sort) {
    influ$var <- as.character(influ$var)
  }

  # deal with special colours
  if (!col %in% c("var", "mean")) {
    the_aes <- ggplot2::aes(y = .data$var, x = .data$mean)
    bar <- ggplot2::geom_bar(stat = "identity", fill = col)
    lab <- NULL
  } else {
    the_aes <- ggplot2::aes(y = .data$var, x = .data$mean, fill = .data[[col]])
    bar <- ggplot2::geom_bar(stat = "identity")
    lab <- switch(col,
      var = "variable",
      mean = "relative\ninfluence (%)"
    )
  }

  ggplot2::ggplot(influ, mapping = the_aes) +
    bar +
    ggplot2::labs(
      x = "relative variable influence (%)",
      y = "variable",
      fill = lab
    ) +
    ggplot2::scale_x_continuous(labels = scales::label_percent(scale = 1))
}
