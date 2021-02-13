
##' Function to do meteorological normalisation
##'
##' This is the main function to apply a gbm model to a data set.
##' @title Function to apply meteorological normalisation to.
##'   
##' @param dat Data frame to analyse. Must contain a POSIXct field 
##'   called \code{date}.
##' @param vars Explanatory variables to use. These variables will be 
##'   used to build the gbm model. Note that the model must include a
##'   trend component. Several variables can be automatically 
##'   calculated (see \code{\link{prepData}} for details).
##' @param pollutant The name of the variable to apply meteorological 
##'   normalisation to.
##' @param sam.size The number of random samples to extract from the
##'   data for model building. While it is possible to use the full
##'   data set, for data sets spanning years the model building can
##'   take a very long time to run. Additionally, there will be
##'   diminishing returns in terms of model accuracy. If
##'   \code{sam.size} is greater than the number of number of rows of
##'   data, the number of rows of data is used instead.
##' @param n.trees Number of trees to fit.
##' @param B Number of bootstrap simulations for partial dependence 
##'   plots.
##' @param n.core Number of cores to use for parallel processing.
##' @param seed Random number seed for reproducibility in returned model.
##' @import doParallel openair gbm dplyr ggplot2 parallel foreach 
##' @importFrom lubridate decimal_date
##' @importFrom gridExtra grid.arrange tableGrob
##' @importFrom stats formula lm na.omit quantile reorder resid var
##' @importFrom tidyr pivot_longer
##' @export
##' @return Returns a list including the model, influence data frame 
##'   and partial dependence data frame.
##' @author David Carslaw
buildMod <- function(dat, vars = c("trend", "ws", "wd", "hour",
                                   "weekday", "temp"),
                     pollutant = "nox", sam.size = round(2 * nrow(dat) / 3),
                     n.trees = 200,
                     B = 100, n.core = 4, seed = 123) {
  
  ## add other variables, select only those required for modelling
  dat <- prepData(dat)
  dat <- select(dat, c("date", vars, pollutant))
  
  variables <- paste(vars, collapse = "+")
  eq <- formula(paste(pollutant, "~", variables))
  
  ## make sure no NA in response
  id <- which(is.na(dat[[pollutant]]))
  if (length(id) > 0 )
    dat <- dat[-id, ]
  
  # randomly sample data according to sam.size
  if (sam.size > nrow(dat)) 
    sam.size <- nrow(dat)
  
  id <- sample(nrow(dat), size = sam.size)
  dat <- dat[id, ]
  
  ## if more than one simulation only return model ONCE
  if (B != 1L) {
    mod <- runGbm(dat, eq, vars, return.mod = TRUE, simulate = FALSE, 
                  n.trees = n.trees, seed)
  }
  
  # if model needs to be run multiple times
  res <- partialDep(dat, eq, vars, B, n.core, n.trees, seed)
  
  if (B != 1) Mod <- mod$model else Mod <- res[[3]]
  
  # return a list of model, data, partial deps
  result <- list(model = Mod, influence = res[[2]], data = dat, pd = res[[1]])
  class(result) <- "deweather"
  
  return(result)
  
}

extractPD <- function(vars, mod) {
  
  n <- 100 ## resolution of output
  
  if ("trend" %in% vars) n <- 500
  
  if (vars %in% c("hour", "hour.local")) n <- 24
  
  ## extract partial dependence values
  res <- plot.gbm(mod, vars, continuous.resolution = n, return.grid = TRUE)
  res <- data.frame(y = res$y, var = vars, x = res[[vars]],
                    var_type = ifelse(is.numeric(res[[vars]]), "numeric", "character"))

  return(res)
}




runGbm <- function(dat, eq, vars, return.mod, simulate, n.trees = n.trees, 
                   seed = seed) {
  
  ## sub-sample the data for bootstrapping
  if (simulate) 
    dat <- dat[sample(nrow(dat), nrow(dat), replace = TRUE), ]
  
  
  # these models for AQ data are not very sensitive to tree sizes > 1000
  # make reproducible
  if (!simulate) set.seed(seed)
  mod <- gbm(eq, data = dat, distribution = "gaussian", n.trees = n.trees,
             shrinkage = 0.1, interaction.depth = 6, bag.fraction = 0.5,
             train.fraction = 1, n.minobsinnode = 10, #cv.folds=5,
             keep.data = TRUE, verbose = FALSE)
  
  ## extract partial dependnece componets

  pd <- lapply(vars, extractPD, mod)
  pd <- do.call(rbind, pd)
  
  ## relative influence
  ri <- summary(mod, plotit = FALSE)
  ri$var <- reorder(ri$var, ri$rel.inf)
  
  if (return.mod) {
    
    result <- list(pd = pd, ri = ri, model = mod)
    
    return(result)
    
    
  } else {
    
    return(list(pd, ri))
    
  }
  
}



