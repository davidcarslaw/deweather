
##' Function to do meteorological normalisation
##'
##' Info here
##' @title Function to apply meteorological normalisation to.
##' @param dat Data frame to analyse.
##' @param vars Explanatory variables to use
##' @param pollutant The name of the variable to apply meteorological
##' normalisation to.
##' @param partial.dep Should the partial dependencies be calculated?
##' @param B Number of bootstrap simulations for partial dependence plots.
##' @import doParallel openair gbm dplyr lattice parallel foreach
##' @importFrom plyr ddply ldply dlply llply numcolwise
##' @export
##' @return Returns a list including the model, influence data frame
##' and partial dependence data frame.
##' @author David Carslaw
buildMod <- function(dat, vars = c("ws", "wd"), pollutant = "nox", B = 100) {

    ## add other variables, select only those required for modelling
    dat <- prepData(dat)
    dat <- dat[(c(vars, pollutant))]

    variables <- paste(vars, collapse = "+")
    eq <- formula(paste(pollutant, "~", variables))

    ## make sure no NA in response
    id <- which(is.na(dat[[pollutant]]))
    if (length(id) > 0 )
        dat <- dat[-id, ]

    ## if more than one simulation only return model ONCE
    if (B != 1)
        mod <- runGbm(dat, eq, vars, return.mod = TRUE, simulate = FALSE)


    res <- partialDep(dat, eq, vars, B)

    if (B != 1) Mod <- mod$model else Mod <- res[[3]]

    result <- list(model = Mod, influence = res[[2]], data = dat, pd = res[[1]])
    class(result) <- "deweather"

    return(result)

    }

extractPD <- function(vars, mod) {

    n <- 100 ## resolution of output
    if (vars %in% c("hour", "hour.local")) n <- 24

    ## extract partial dependence values
    res <- plot(mod, vars, cont = n, return.grid = TRUE)
    res <- data.frame(y = res$y, var = vars, x = res[[vars]])
    return(res)
}




runGbm <- function(dat, eq, vars, return.mod = FALSE, simulate = FALSE) {

    ## sub-sample the data for bootstrapping
    if (simulate)
        dat <- dat[sample(1:nrow(dat), nrow(dat), replace = TRUE), ]

    trees <- 1000
    mod <- gbm(eq, data = dat, distribution = "gaussian", n.trees = trees,
               shrinkage = 0.1, interaction.depth = 10, bag.fraction = 0.7,
               train.fraction = 1,  n.minobsinnode = 10,
               keep.data = TRUE, verbose = FALSE)

    ## extract partial dependnece componets
    pd <- plyr::ldply(vars, extractPD, mod)

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



