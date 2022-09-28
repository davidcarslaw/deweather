#' Function to run random meteorological simulations on a gbm model
#'
#' @param dw_model Model object from running [buildMod()].
#' @param newdata Data set to which to apply the model. If missing the data used
#'   to build the model in the first place will be used.
#' @param metVars The variables that should be randomly varied.
#' @param n.core Number of cores to use.
#' @param B Number of simulations
#' @export
#' @return To add
#' @author David Carslaw
metSim <- function(dw_model, newdata, metVars = c("ws", "wd", "temp"),
                   n.core = 4, B = 200) {
  if (!inherits(dw_model, "deweather")) {
    stop("Need to supply a deweather object from buildMod.")
  }

  ## extract the model
  mod <- dw_model$model

  if (!"trend" %in% mod$var.names) {
    stop("The model must have a trend component as one of the explanatory variables.")
  }

  if (missing(newdata)) {
    ## should already have variables
    newdata <- dw_model$data
  } else {
    ## add variables needed
    newdata <- prepData(newdata)
  }

  cl <- parallel::makeCluster(n.core)
  doParallel::registerDoParallel(cl)

  prediction <- foreach::foreach(
    i = 1:B, .inorder = FALSE, .combine = "rbind",
    .packages = "gbm", .export = "doPred"
  ) %dopar%
    doPred(newdata, mod, metVars)

  parallel::stopCluster(cl)

  ## Aggregate results
  prediction <- dplyr::group_by(prediction, .data$date) %>%
    dplyr::summarise(pred = mean(.data$pred))

  return(prediction)
}


## randomly sample from original data
doPred <- function(mydata, mod, metVars) {

  ## random samples
  n <- nrow(mydata)
  id <- sample(1:n, n, replace = FALSE)

  ## new data with random samples
  mydata[metVars] <- lapply(mydata[metVars], function(x) x[id])

  prediction <- gbm::predict.gbm(mod, mydata, mod$n.trees)

  prediction <- data.frame(date = mydata$date, pred = prediction)

  return(prediction)
}
