
#' @method print deweather
#' @export
#' @author Jack Davison
print.deweather <- function(x, ...){
  cli::cli_par(id = "Intro")
  cli::cli_h1("{.pkg deweather} model")
  cli::cli_inform(c("i" = "{.field $model} information:"))
  print(x$model)
  cli::cli_end(id = "Intro")
  
  other_objects <- names(x)[names(x) != "model"]
  
  cli::cli_par(id = "other-stuff")
  cli::cli_h3("Also contains:")
  cli::cli_ul(paste0("{.field $", other_objects, "}"))
  cli::cli_end(id = "other-stuff")
}

#' @method plot deweather
#' @export
plot.deweather <- function(x, ...){
  plotInfluence(x, ...)
}

#' @method summary deweather
#' @export
summary.deweather <- function(object, ...){
  dw_map(object, summary, ...)
}

#' @method head deweather
#' @export
head.deweather <- function(x, ...){
  dw_map(x, utils::head, ...)
}

#' @method tail deweather
#' @export
tail.deweather <- function(x, ...){
  dw_map(x, utils::tail, ...)
}

#' mapping helper to perform functions on each dataframe element of a DW model
#' @noRd
#' @author Jack Davison
dw_map <- function(x, FUN, ...) {
  dat <- names(x)[names(x) != "model"]
  
  out <- list()
  for (i in dat) {
    args = list(x[[i]], ...)
    proc <- do.call(FUN, args = args)
    cli::cli_par(id = i)
    cli::cli_inform(paste0("{.field $", i, "}"))
    print(proc)
    cli::cli_end(id = i)
    out <- append(out, list(proc))
  }
  
  names(out) <- dat
  return(invisible(out))
}
