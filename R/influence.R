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

  ggplot2::ggplot(influ, ggplot2::aes(y = .data$var, x = .data$mean)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::xlab("relative variable influence (%)") +
    ggplot2::ylab("variable")
}
