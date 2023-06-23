#' Function to test different meteorological normalisation models.
#'
#' @param input_data Data frame to analyse.
#' @param vars Explanatory variables to use.
#' @param pollutant The name of the variable to apply meteorological
#'   normalisation to.
#' @param train.frac Fraction of data to train a model on. The model is tested
#'   against the withheld 0.2 proportion.
#' @param n.trees Number of trees to use.
#' @param seed Random number seed for reproducibility in returned model.
#' @param plot The default, `TRUE`, automatically prints a plot and two
#'   tables of statistics to review the model output. `FALSE` disables this
#'   behaviour.
#' @export
#' @return Returns to be added.
#' @author David Carslaw
testMod <- function(input_data,
                    vars = c(
                      "trend", "ws", "wd", "hour",
                      "weekday", "air_temp"
                    ),
                    pollutant = "nox",
                    train.frac = 0.8,
                    n.trees = 200,
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

  mod <- runGbm(
    train.dat,
    eq,
    vars,
    return.mod = TRUE,
    simulate = FALSE,
    n.trees = n.trees,
    seed
  )

  # predictions based on training data
  pred_train <-
    gbm::predict.gbm(mod$model, newdata = train.dat, n.trees = n.trees)

  pred_train <- data.frame(train.dat, pred = pred_train)

  ## calculate key model statistics
  stats_train <-
    openair::modStats(pred_train, obs = pollutant, mod = "pred")
  stats_train <- as.data.frame(t(stats_train))
  names(stats_train) <- "value"
  stats_train$statistic <- rownames(stats_train)
  stats_train <- stats_train[-1, ]
  stats_train <-
    dplyr::select(stats_train, .data$statistic, .data$value)
  stats_train$value <- as.numeric(as.character(stats_train$value))
  stats_train$value <- round(stats_train$value, 2)


  # predictions based on test data

  pred <-
    gbm::predict.gbm(mod$model, newdata = pred.dat, n.trees = n.trees)

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
  stats <- as.data.frame(t(stats))
  names(stats) <- "value"
  stats$statistic <- rownames(stats)
  stats <- stats[-1, ]
  stats <- dplyr::select(stats, .data$statistic, .data$value)
  stats$value <- as.numeric(as.character(stats$value))
  stats$value <- round(stats$value, 2)

  head_train <- data.frame(
    statistic = "Training",
    value = NA,
    stringsAsFactors = FALSE
  )

  head_test <- data.frame(
    statistic = "Test data",
    value = NA,
    stringsAsFactors = FALSE
  )

  stats <- dplyr::bind_rows(head_test, stats)
  stats_train <- dplyr::bind_rows(head_train, stats_train)

  # print % difference in RMSE
  if (plot) {
    diff_rmse <-
      round(100 * (stats$value[8] - stats_train$value[8]) / stats$value[8], 1)
    print(paste0("Percent increase in RMSE using test data is ", diff_rmse, "%"))
  }

  tbl <- gridExtra::tableGrob(stats, rows = NULL)
  tbl_train <- gridExtra::tableGrob(stats_train, rows = NULL)

  # plotting side effect (disabled with `plot` arg)
  if (plot) {
    gridExtra::grid.arrange(plt, tbl, tbl_train, nrow = 1, as.table = TRUE)
  }

  # prep stats output
  out_stats <-
    dplyr::left_join(
      stats,
      stats_train,
      by = c("statistic"),
      suffix = c("_test", "_train")
    )
  out_stats <- dplyr::tibble(out_stats[-1, ])

  invisible(list(
    pred = dplyr::tibble(pred),
    stats = out_stats,
    plot = plt
  ))
}
