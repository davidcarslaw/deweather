#' Plot a GBM influence plot
#'
#' @param dw_model Model object from running [buildMod()].
#' @export
#' @return Plot
#' @family deweather model plotting functions
#' @author David Carslaw
plotInfluence <- function(dw_model) {
  if (!inherits(dw_model, "deweather")) {
    stop("Need to supply a deweather object from buildMod.")
  }

  ## extract influence data from object
  influ <- dw_model$influence

  ggplot2::ggplot(influ, ggplot2::aes(y = .data$var, x = .data$mean)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::xlab("relative variable influence (%)") +
    ggplot2::ylab("variable")
}
