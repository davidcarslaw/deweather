##' Function to build and test a gbm model
##'
##' Info here
##' @title Function to apply meteorological normalisation to.
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

    pred <- predict.gbm(mod$model, newdata = pred.dat, n.trees = 1000)

    pred <- data.frame(pred.dat, pred = pred)

    plt <- ggplot(pred, aes_string("pred", pollutant)) +
        geom_point()

    stats <- modStats(pred, obs = pollutant, mod = "pred")
    stats <- as.data.frame(t(stats))
    names(stats) <- "value"
    stats$statistic <- rownames(stats)
    stats <- stats[-1, ]
    stats <- subset(stats, select = c(statistic, value))
    stats$value <- as.numeric(as.character(stats$value))
    stats$value <- round(stats$value, 2)

    tbl <- tableGrob(stats, rows = NULL)

    grid.arrange(plt, tbl, nrow = 1, as.table=TRUE)

    
    invisible(pred)

    }
