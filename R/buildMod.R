#' Function to apply meteorological normalisation
#'
#' This is the main function to apply a gbm model to a data set.
#'
#' @param input_data Data frame to analyse. Must contain a POSIXct field called
#'   `date`.
#' @param vars Explanatory variables to use. These variables will be used to
#'   build the gbm model. Note that the model must include a trend component.
#'   Several variables can be automatically calculated (see [prepData()] for
#'   details).
#' @param pollutant The name of the variable to apply meteorological
#'   normalisation to.
#' @param sam.size The number of random samples to extract from the data for
#'   model building. While it is possible to use the full data set, for data
#'   sets spanning years the model building can take a very long time to run.
#'   Additionally, there will be diminishing returns in terms of model accuracy.
#'   If `sam.size` is greater than the number of number of rows of data, the
#'   number of rows of data is used instead.
#' @param n.trees Number of trees to fit.
#' @param simulate Should the original time series be randomly sampled with
#'   replacement? The default is `FALSE`. Setting `simulate = TRUE` can be
#'   useful for estimating model uncertainties. In which case models should be
#'   run multiple times with `B = 1` and a different value of `seed` e.g. `seed
#'   = runif(1)`.
#' @param B Number of bootstrap simulations for partial dependence plots.
#' @param n.core Number of cores to use for parallel processing.
#' @param seed Random number seed for reproducibility in returned model.
#' @export
#' @seealso [testMod()] for testing models before they are built.
#' @seealso [metSim()] for using a built model with meteorological simulations.
#' @seealso [plot2Way()], [plotInfluence()] and [plotPD()] for visualising built
#'   models.
#' @return Returns a list including the model, influence data frame and partial
#'   dependence data frame.
#' @author David Carslaw
buildMod <- function(input_data,
                     vars = c(
                       "trend", "ws", "wd", "hour",
                       "weekday", "air_temp"
                     ),
                     pollutant = "nox",
                     sam.size = nrow(input_data),
                     n.trees = 200,
                     simulate = FALSE,
                     B = 100,
                     n.core = 4,
                     seed = 123) {
  ## add other variables, select only those required for modelling
  input_data <- prepData(input_data)
  input_data <-
    dplyr::select(input_data, c("date", vars, pollutant))
  input_data <-
    stats::na.omit(input_data) # only build model where all data are available - can always predict in gaps

  variables <- paste(vars, collapse = "+")
  eq <- stats::formula(paste(pollutant, "~", variables))

  # randomly sample data according to sam.size
  if (sam.size > nrow(input_data)) {
    sam.size <- nrow(input_data)
  }

  if (simulate) {
    id <- sample(nrow(input_data), size = sam.size, replace = TRUE)
    input_data <- input_data[id, ]
  } else {
    id <- sample(nrow(input_data), size = sam.size)
    input_data <- input_data[id, ]
  }

  ## if more than one simulation only return model ONCE
  if (B != 1L) {
    mod <- runGbm(
      input_data,
      eq,
      vars,
      return.mod = TRUE,
      simulate = simulate,
      n.trees = n.trees,
      seed
    )
  }

  # if model needs to be run multiple times
  res <- partialDep(input_data, eq, vars, B, n.core, n.trees, seed)

  if (B != 1) {
    Mod <- mod$model
  } else {
    Mod <- res[[3]]
  }

  # return a list of model, data, partial deps
  result <-
    list(
      model = Mod,
      influence = res[[2]],
      data = input_data,
      pd = res[[1]]
    )
  class(result) <- "deweather"

  return(result)
}

#' Extract PD
#' @noRd
extractPD <- function(vars, mod) {
  n <- 100 ## resolution of output

  if ("trend" %in% vars) {
    n <- 500
  }

  if (vars %in% c("hour", "hour.local")) {
    n <- 24
  }

  ## extract partial dependence values
  res <-
    gbm::plot.gbm(mod,
      vars,
      continuous.resolution = n,
      return.grid = TRUE
    )
  res <- data.frame(
    y = res$y,
    var = vars,
    x = res[[vars]],
    var_type = ifelse(is.numeric(res[[vars]]), "numeric", "character")
  )

  return(res)
}

#' Run Gbm
#' @noRd
runGbm <-
  function(dat,
           eq,
           vars,
           return.mod,
           simulate,
           n.trees = n.trees,
           seed = seed) {
    ## sub-sample the data for bootstrapping
    if (simulate) {
      dat <- dat[sample(nrow(dat), nrow(dat), replace = TRUE), ]
    }

    # these models for AQ data are not very sensitive to tree sizes > 1000
    # make reproducible
    if (!simulate) {
      set.seed(seed)
    } else {
      set.seed(stats::runif(1))
    }

    mod <- gbm::gbm(
      eq,
      data = dat,
      distribution = "gaussian",
      n.trees = n.trees,
      shrinkage = 0.1,
      interaction.depth = 6,
      bag.fraction = 0.5,
      train.fraction = 1,
      n.minobsinnode = 10,
      # cv.folds=5,
      keep.data = TRUE,
      verbose = FALSE
    )

    ## extract partial dependnece componets

    pd <- lapply(vars, extractPD, mod)
    pd <- do.call(rbind, pd)

    ## relative influence
    ri <- summary(mod, plotit = FALSE)
    ri$var <- stats::reorder(ri$var, ri$rel.inf)

    if (return.mod) {
      result <- list(pd = pd, ri = ri, model = mod)

      return(result)
    } else {
      return(list(pd, ri))
    }
  }

#' @noRd
#' @importFrom rlang .data
partialDep <-
  function(dat,
           eq,
           vars,
           B = 100,
           n.core = 4,
           n.trees,
           seed) {
    if (B == 1) {
      return.mod <- TRUE
    } else {
      return.mod <- FALSE
    }

    if (B == 1) {
      pred <- runGbm(
        dat,
        eq,
        vars,
        return.mod = TRUE,
        simulate = FALSE,
        n.trees = n.trees,
        seed
      )
    } else {
      cl <- parallel::makeCluster(n.core)
      doParallel::registerDoParallel(cl)

      pred <- foreach::foreach(
        i = 1:B,
        .inorder = FALSE,
        .packages = "gbm",
        .export = "runGbm"
      ) %dopar%
        runGbm(
          dat,
          eq,
          vars,
          return.mod = FALSE,
          simulate = TRUE,
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
