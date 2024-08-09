#' Function to test different meteorological normalisation models.
#' 
#' @inheritParams buildMod
#' @param train.frac Fraction of data to train a model on. The model is tested
#'   against the withheld 0.2 proportion.
#' @param n.trees Number of trees to use. If `n.trees = NA` then the
#'   function will conduct cross-validation to calculate the optimum number.
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
      train.dat <- train.dat %>%
        dplyr::slice_sample(n = 10000)
    } else {
      train.dat <- train.dat
    }
    
    mod <-
      gbm::gbm(
        eq,
        distribution = "gaussian",
        data = train.dat,
        n.trees = 5000,
        shrinkage = shrinkage,
        interaction.depth = interaction.depth,
        bag.fraction = bag.fraction,
        n.minobsinnode = n.minobsinnode,
        cv.folds = cv.folds,
        verbose = FALSE
      )
    
    # find index for n trees with minimum CV error
    n.trees <- which.min(mod$cv.error)
    
    # plot
    if (plot) {
      cli::cli_inform(
        c("i" = "Optimum number of trees is {.strong {n.trees}}. RMSE from cross-validation is {.strong {round(sqrt(mod$cv.error[n.trees]), 2)}}.")
      )
    }
  } else {
    mod <- 
      gbm::gbm(
        eq,
        distribution = "gaussian",
        data = train.dat,
        n.trees = n.trees,
        shrinkage = shrinkage,
        interaction.depth = interaction.depth,
        bag.fraction = bag.fraction,
        n.minobsinnode = n.minobsinnode,
        cv.folds = cv.folds,
        verbose = FALSE
      ) 
  }
  
  # predictions based on training data
  pred_train <-
    gbm::predict.gbm(
      mod,
      newdata = train.dat,
      n.trees = n.trees,
      shrinkage = shrinkage,
      interaction.depth = interaction.depth,
      bag.fraction = bag.fraction,
      n.minobsinnode = n.minobsinnode,
      seed = seed
    )
  
  pred_train <- dplyr::tibble(train.dat, pred = pred_train)
  
  ## calculate key model statistics
  stats_train <-
    openair::modStats(pred_train, obs = pollutant, mod = "pred")
  
  stats_train <- stats_train %>% 
    tidyr::pivot_longer(cols = -1) %>% 
    dplyr::rename("statistic" = "name") %>% 
    dplyr::select(-1) %>% 
    dplyr::mutate(value = round(.data$value, 2)) %>% 
    dplyr::filter(!.data$statistic %in% c("P", "COE", "IOA"))
  
  # predictions based on test data
  
  pred <-
    gbm::predict.gbm(
      mod,
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
    tidyr::pivot_longer(cols = -1) %>% 
    dplyr::rename("statistic" = "name") %>% 
    dplyr::select(-1) %>% 
    dplyr::mutate(value = round(.data$value, 2)) %>% 
    dplyr::filter(!.data$statistic %in% c("P", "COE", "IOA"))
  
  stats_both <-
    dplyr::left_join(
      dplyr::rename(stats_train, "train" = "value"),
      dplyr::rename(stats, "test" = "value"),
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
    pw <- patchwork::wrap_plots(plt, tbl, nrow = 1)
    print(pw)
  }
  
  invisible(list(
    pred = dplyr::tibble(pred),
    stats = stats_both,
    plot = plt,
    optimum_trees = n.trees
  ))
}
