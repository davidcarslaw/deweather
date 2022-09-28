#' Plot a GBM influence plot
#'
#' @param dat Model object from running [buildMod()].
#' @export
#' @return Plot
#' @author David Carslaw

gbmInf <- function(dat) {
  if (!inherits(dat, "deweather")) {
    stop("Need to supply a deweather object from buildMod.")
  }

  ## extract influence data from object
  influ <- dat$influence

  ggplot2::ggplot(influ, ggplot2::aes(.data$var, .data$mean)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::coord_flip() +
    ggplot2::ylab("relative variable influence (%)") +
    ggplot2::xlab("variable")
}
