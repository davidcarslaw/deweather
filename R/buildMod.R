#' Function to apply meteorological normalisation
#'
#' This is the main function to apply a [gbm::gbm()] model to a data set.
#'
#' @param input_data Data frame to analyse. Must contain a POSIXct field called
#'   `date`.
#' @param vars Explanatory variables to use. These variables will be used to
#'   build the [gbm::gbm()] model. Note that the model must include a trend
#'   component. Several variables can be automatically calculated (see
#'   [prepData()] for details).
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
#' @param shrinkage A shrinkage parameter applied to each tree in the expansion.
#'   Also known as the learning rate or step-size reduction; `0.001` to `0.1`
#'   usually work, but a smaller learning rate typically requires more trees.
#'   Default is `0.1`.
#' @param interaction.depth Integer specifying the maximum depth of each tree
#'   (i.e., the highest level of variable interactions allowed). A value of `1`
#'   implies an additive model, a value of `2` implies a model with up to 2-way
#'   interactions, etc. Default is `5`.
#' @param bag.fraction The fraction of the training set observations randomly
#'   selected to propose the next tree in the expansion. This introduces
#'   randomness into the model fit. If `bag.fraction < 1` then running the same
#'   model twice will result in similar but different.
#' @param n.minobsinnode Integer specifying the minimum number of observations
#'   in the terminal nodes of the trees. Note that this is the actual number of
#'   observations, not the total weight.
#' @param cv.folds Number of cross-validation folds to perform. If `cv.folds >
#'   1` then [gbm::gbm()], in addition to the usual fit, will perform a
#'   cross-validation, calculate an estimate of generalization error returned in
#'   `cv.error`.
#' @param seed Random number seed for reproducibility in returned model.
#'
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
                     shrinkage = 0.1,
                     interaction.depth = 5,
                     bag.fraction = 0.5,
                     n.minobsinnode = 10,
                     cv.folds = 0,
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
      shrinkage = shrinkage,
      interaction.depth = interaction.depth,
      bag.fraction = bag.fraction,
      n.minobsinnode = n.minobsinnode,
      cv.folds = cv.folds,
      seed
    )
  }

  # if model needs to be run multiple times
  res <- partialDep(input_data, eq, vars, B, n.core,
    n.trees = n.trees,
    shrinkage = shrinkage,
    interaction.depth = interaction.depth,
    bag.fraction = bag.fraction,
    n.minobsinnode = n.minobsinnode,
    cv.folds = cv.folds, seed = seed
  )

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
    n <- 100
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
           shrinkage = shrinkage,
           interaction.depth = interaction.depth,
           bag.fraction = bag.fraction,
           n.minobsinnode = n.minobsinnode,
           cv.folds = cv.folds,
           seed = seed,
           n.core = 4) {
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
      shrinkage = shrinkage,
      interaction.depth = interaction.depth,
      bag.fraction = bag.fraction,
      train.fraction = 1,
      n.minobsinnode = n.minobsinnode,
      cv.folds = cv.folds,
      keep.data = TRUE,
      verbose = FALSE,
      n.cores = n.core
    )

    ## extract partial dependence components
    pd <- purrr::map(vars, extractPD, mod = mod) %>%
      purrr::map(~ dplyr::nest_by(.x, var, var_type) %>%
        tidyr::pivot_wider(
          names_from = "var_type",
          values_from = "data"
        )) %>%
      dplyr::bind_rows()

    ## relative influence
    ri <- summary(mod, plotit = FALSE)
    ri$var <- stats::reorder(ri$var, ri$rel.inf)

    if (return.mod) {
      result <- list("pd" = pd, "ri" = ri, "model" = mod)

      return(result)
    } else {
      return(list("pd" = pd, "ri" = ri))
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
           n.trees = n.trees,
           shrinkage = shrinkage,
           interaction.depth = interaction.depth,
           bag.fraction = bag.fraction,
           n.minobsinnode = n.minobsinnode,
           cv.folds = cv.folds,
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
        shrinkage = shrinkage,
        interaction.depth = interaction.depth,
        bag.fraction = bag.fraction,
        n.minobsinnode = n.minobsinnode,
        cv.folds = cv.folds,
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
          n.trees = n.trees,
          shrinkage = shrinkage,
          interaction.depth = interaction.depth,
          bag.fraction = bag.fraction,
          n.minobsinnode = n.minobsinnode,
          cv.folds = cv.folds
        )

      parallel::stopCluster(cl)
    }

    # partial dependence plots

    if (B == 1) {
      pd <- pred$pd
      ri <- pred$ri
      mod <- pred$model
    } else {
      pd <- purrr::map(pred, "pd") %>%
        dplyr::bind_rows()

      ## relative influence
      ri <- purrr::map(pred, "ri") %>%
        dplyr::bind_rows()

      mod <- pred[[1]]$model
    }
    
    # if either character/numeric not in the output df, add dummy col
    if (!"character" %in% names(pd)) {
      pd$character <- rep(list(data.frame()), nrow(pd))
    }
    
    if (!"numeric" %in% names(pd)) {
      pd$numeric <- rep(list(data.frame()), nrow(pd))
    }
    
    # Calculate 95% CI for different vars
    resCI <-
      dplyr::group_by(pd, .data$var) %>%
      dplyr::reframe(
        numeric = list(dplyr::bind_rows(numeric)),
        character = list(dplyr::bind_rows(character))
      ) %>%
      dplyr::mutate(
        numeric = purrr::map(
          numeric,
          purrr::possibly(
            ~ dplyr::mutate(.x, x_bin = cut(.data$x, 100, include.lowest = TRUE)) %>%
              dplyr::group_by(x_bin) %>%
              dplyr::summarise(
                x = mean(x),
                mean = mean(y),
                lower = quantile(y, probs = 0.025),
                upper = quantile(y, probs = 0.975)
              ) %>%
              dplyr::ungroup()
          )
        ),
        character = purrr::map(
          character,
          purrr::possibly(
            ~ dplyr::group_by(.x, x) %>%
              dplyr::summarise(
                mean = mean(y),
                lower = quantile(y, probs = 0.025),
                upper = quantile(y, probs = 0.975)
              ) %>%
              dplyr::ungroup()
          )
        )
      )

    resRI <- dplyr::group_by(ri, .data$var) %>%
      dplyr::summarise(
        mean = mean(.data$rel.inf),
        lower = stats::quantile(.data$rel.inf, probs = c(0.025)),
        upper = stats::quantile(.data$rel.inf, probs = c(0.975))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(var = stats::reorder(.data$var, mean)) %>%
      dplyr::arrange(dplyr::desc(.data$var))

    if (return.mod) {
      return(list(resCI, resRI, mod))
    } else {
      return(list(resCI, resRI))
    }
  }
