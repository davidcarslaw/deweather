##' Function to build and test a gbm model
##'
##' Info here
##' @title Function to test different meteorological normalisation models.
##' @param dat Data frame to analyse.
##' @param vars Explanatory variables to use.
##' @param pollutant The name of the variable to apply meteorological
##'     normalisation to.
##' @param train.frac Fraction of data to train a model on. The model
##'     is tested against the withheld 0.2 proportion.
##' @export
##' @return Returns to be added.
##' @author David Carslaw
testMod <- function(dat, vars = c("trend", "ws", "wd", "hour",
                                  "weekday", "temp"),
                    pollutant = "nox", train.frac = 0.8) {

    ## silence R check
    statistic = value = NULL

    ## add other variables, select only those required for modelling
    dat <- prepData(dat)
    dat <- dat[(c("date", vars, pollutant))]

    variables <- paste(vars, collapse = "+")
    eq <- formula(paste(pollutant, "~", variables))

    ## make sure no NA in response
    id <- which(is.na(dat[[pollutant]]))
    if (length(id) > 0 )
        dat <- dat[-id, ]

    id <- sample(1:nrow(dat), size = train.frac * nrow(dat))
    train.dat <- dat[id, ]
    pred.dat <- dat[-id, ]
    
    mod <- runGbm(train.dat, eq, vars, return.mod = TRUE, simulate = FALSE)
    
    # predictions based on training data
    pred_train <- predict.gbm(mod$model, newdata = train.dat, n.trees = 250)
    
    pred_train <- data.frame(train.dat, pred = pred_train)
    
        ## calculate key model statistics
    stats_train <- modStats(pred_train, obs = pollutant, mod = "pred")
    stats_train <- as.data.frame(t(stats_train))
    names(stats_train) <- "value"
    stats_train$statistic <- rownames(stats_train)
    stats_train <- stats_train[-1, ]
    stats_train <- select(stats_train, statistic, value)
    stats_train$value <- as.numeric(as.character(stats_train$value))
    stats_train$value <- round(stats_train$value, 2)
    
    
    # predictions based on test data

    pred <- predict.gbm(mod$model, newdata = pred.dat, n.trees = 250)

    pred <- data.frame(pred.dat, pred = pred)

    plt <- ggplot(pred, aes_string("pred", pollutant)) +
      geom_point(fill = "grey30", color = "white", pch = 21, size = 3) +
      xlab("predicted") + 
      ylab("measured")
      

    ## calculate key model statistics
    stats <- modStats(pred, obs = pollutant, mod = "pred")
    stats <- as.data.frame(t(stats))
    names(stats) <- "value"
    stats$statistic <- rownames(stats)
    stats <- stats[-1, ]
    stats <- select(stats, statistic, value)
    stats$value <- as.numeric(as.character(stats$value))
    stats$value <- round(stats$value, 2)

    head_train <- data.frame(statistic = "Training", value = NA, 
                             stringsAsFactors = FALSE)
    
    head_test <- data.frame(statistic = "Test data", value = NA,
                            stringsAsFactors = FALSE)
    
    stats <- bind_rows(head_test, stats)
    stats_train <- bind_rows(head_train, stats_train)

    tbl <- gridExtra::tableGrob(stats, rows = NULL)
    tbl_train <- gridExtra::tableGrob(stats_train, rows = NULL)

    gridExtra::grid.arrange(plt, tbl, tbl_train, nrow = 1, as.table = TRUE)

    
    invisible(pred)

    }
