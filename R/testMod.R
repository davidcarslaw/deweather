#' Function to test different meteorological normalisation models.
#'
#' @param input_data Data frame to analyse.
#' @param vars Explanatory variables to use.
#' @param pollutant The name of the variable to apply meteorological
#'   normalisation to.
#' @param train.frac Fraction of data to train a model on. The model is tested
#'   against the withheld 0.2 proportion.
#' @param n.trees Number of trees to use. If \code{n.trees = NA} then the
#'   function will conduct cross-validation to calculate the optimum number.
#' @param shrinkage a shrinkage parameter applied to each tree in the expansion.
#'   Also known as the learning rate or step-size reduction; 0.001 to 0.1
#'   usually work, but a smaller learning rate typically requires more trees.
#'   Default is 0.1.
#' @param interaction.depth Integer specifying the maximum depth of each tree
#'   (i.e., the highest level of variable interactions allowed). A value of 1
#'   implies an additive model, a value of 2 implies a model with up to 2-way
#'   interactions, etc. Default is 5.
#' @param bag.fraction he fraction of the training set observations randomly
#'   selected to propose the next tree in the expansion. This introduces
#'   randomnesses into the model fit. If bag.fraction < 1 then running the same
#'   model twice will result in similar but different.
#' @param n.minobsinnode Integer specifying the minimum number of observations
#'   in the terminal nodes of the trees. Note that this is the actual number of
#'   observations, not the total weight.
#' @param cv.folds Number of cross-validation folds to perform if \code{n.trees
#'   = NA}. If \code{cv.folds > 1} then gbm, in addition to the usual fit, will
#'   perform a cross-validation, calculate an estimate of generalization error
#'   returned in \code{cv.error}.
#' @param seed Random number seed for reproducibility in returned model.
#' @param plot The default, `TRUE`, automatically prints a plot and two tables
#'   of statistics to review the model output. `FALSE` disables this behaviour.
#' @export
#' @seealso [buildMod()] for fitting a final model
#' @return Returns to be added.
#' @author David Carslaw
testMod <- function(input_data,
                    vars = c(
                      "trend", "ws", "wd", "hour",
                      "weekday", "air_temp"
                    ),
                    pollutant = "nox",
                    train.frac = 0.8,
                    n.trees = NA,
                    shrinkage = 0.1,
                    interaction.depth = 5,
                    bag.fraction = 0.5,
                    n.minobsinnode = 10,
                    cv.folds = 5,
                    seed = 123,
                    plot = TRUE) {
  ## silence R check
  statistic <- value <- NULL

  ## add other variables, select only those required for modelling
  input_data <- prepData(input_data)
  input_data <- input_data[(c("date", vars, pollutant))]
  
  variables <- paste(vars, collapse = "+")
  eq <- stats::formula(paste(pollutant, "~", variables))
  
  ## make sure no NA in response
  id <- which(is.na(input_data[[pollutant]]))
  if (length(id) > 0) {
    input_data <- input_data[-id, ]
  }
  
  # make reproducible
  set.seed(seed)
  id <-
    sample(1:nrow(input_data), size = train.frac * nrow(input_data))
  train.dat <- input_data[id, ]
  pred.dat <- input_data[-id, ]
  
  
  if (is.na(n.trees)) {
    
    # if n.trees = NA, calculate optimum number using CV; use all data for this
    # because it will be randomly split select maximum of 10000 rows
    if (nrow(train.dat) > 10000) {
      data_for_CV <- train.dat %>%
        dplyr::slice_sample(n = 10000)
    } else {
      data_for_CV <- train.dat
    }
    
    CV_mod <- 
      gbm::gbm(
        eq,
        distribution = "gaussian",
        data = data_for_CV,
        n.trees = 5000,
        shrinkage = shrinkage,
        interaction.depth = interaction.depth,
        bag.fraction = bag.fraction,
        n.minobsinnode = n.minobsinnode,
        cv.folds = cv.folds,
        verbose = FALSE
      )  
    
    # find index for n trees with minimum CV error
    min_MSE <- which.min(CV_mod$cv.error)
    
  }
  
  if (is.na(n.trees)) {
    n.trees <- min_MSE
    cli::cli_inform(c("i" = "Optimum number of trees is {.strong {n.trees}}"))
    cli::cli_inform(c("i" = "RMSE from cross-validation is {.strong {round(sqrt(CV_mod$cv.error[min_MSE]), 2)}}"))
  }

  # predictions based on training data
  pred_train <-
    gbm::predict.gbm(
      CV_mod,
      newdata = data_for_CV,
      n.trees = n.trees,
      shrinkage = shrinkage,
      interaction.depth = interaction.depth,
      bag.fraction = bag.fraction,
      n.minobsinnode = n.minobsinnode,
      seed = seed
    )
  
  pred_train <- tibble(data_for_CV, pred = pred_train)
  
  ## calculate key model statistics
  stats_train <-
    openair::modStats(pred_train, obs = pollutant, mod = "pred")
  
  stats_train <- stats_train %>% 
    tidyr::pivot_longer(cols= -1) %>% 
    dplyr::rename(statistic = name) %>% 
    dplyr::select(-1) %>% 
    dplyr::mutate(value = round(value, 2)) %>% 
    dplyr::filter(!statistic %in% c("P", "COE", "IOA"))
  
  # predictions based on test data

  pred <-
    gbm::predict.gbm(
      CV_mod,
      newdata = pred.dat,
      n.trees = n.trees,
      shrinkage = shrinkage,
      interaction.depth = interaction.depth,
      bag.fraction = bag.fraction,
      n.minobsinnode = n.minobsinnode,
      seed = seed
    )
  
  pred <- data.frame(pred.dat, pred = pred)
  
  plt <-
    ggplot2::ggplot(pred, ggplot2::aes(.data[["pred"]], .data[[pollutant]])) +
    ggplot2::geom_point(
      fill = "grey30",
      color = "white",
      pch = 21,
      size = 2.5
    ) +
    ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      col = "deeppink",
      lwd = 1.5
    ) +
    ggplot2::geom_abline(
      slope = 0.5,
      intercept = 0,
      col = "turquoise4",
      lty = 5
    ) +
    ggplot2::geom_abline(
      slope = 2,
      intercept = 0,
      col = "turquoise4",
      lty = 5
    ) +
    ggplot2::xlab("predicted") +
    ggplot2::ylab("measured")
  
  ## calculate key model statistics
  stats <- openair::modStats(pred, obs = pollutant, mod = "pred")
  
  stats <- stats %>% 
    tidyr::pivot_longer(cols= -1) %>% 
    dplyr::rename(statistic = name) %>% 
    dplyr::select(-1) %>% 
    dplyr::mutate(value = round(value, 2)) %>% 
    dplyr::filter(!statistic %in% c("P", "COE", "IOA"))

  stats_both <-
    dplyr::left_join(
      dplyr::rename(stats_train, "train" = .data$value),
      dplyr::rename(stats, "test" = .data$value),
      by = "statistic"
    ) %>%
    dplyr::tibble()
  
  # plotting side effect - disabled w/ `plot` arg
  if (plot) {
    # print % difference in RMSE
    diff_rmse <- (stats$value[stats$statistic == "RMSE"] - stats_train$value[stats_train$statistic == "RMSE"]) / stats$value[stats$statistic == "RMSE"]
    diff_rmse <- scales::label_percent(accuracy = 0.1)(diff_rmse)
    cli::cli_inform(c("i" = "Percent increase in RMSE using test data is {.strong {diff_rmse}}"))
    
    # get table of stats
    tbl <-
      gridExtra::tableGrob(dplyr::rename(
        stats_both,
        "training data" = "train",
        "testing data" = "test"
      ),
      rows = NULL)
    
    # print plot
    gridExtra::grid.arrange(plt, tbl, nrow = 1, as.table = TRUE)
  }
  
  invisible(list(
    pred = dplyr::tibble(pred),
    stats = stats_both,
    plot = plt
  ))
}
