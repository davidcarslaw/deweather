
##' Function to do meteorological normalisation
##'
##' Info here
##' @title Function to apply meteorological normalisation to.
##' @param dat Data frame to analyse.
##' @param vars Explanatory variables to use
##' @param pollutant The name of the variable to apply meteorological
##' normalisation to.
##' @import doParallel openair gbm dplyr lattice
##' @importFrom plyr ddply ldply dlply llply numcolwise
##' @export 
##' @return A list of stuff
##' @author David Carslaw
deMet <- function(dat, vars = c("ws", "wd"), pollutant = "nox") {

    dat <- prepData(dat)

    pollutant <- "nox"

    vars = c("ws", "wd", "temp", "trend", "hour", "weekday", "week", "pressure", "cl")
    variables <- paste(vars, collapse = "+")
    eq <- formula(paste(pollutant, "~", variables))

    id <- which(is.na(dat[[pollutant]]))
    if (length(id) > 0 )
        dat <- dat[-id, ]

}

extractPD <- function(vars, mod) {
    
    ## extract partial dependence values
    res <- plot(mod, vars, cont = 100, return.grid = TRUE)
    res <- data.frame(y = res$y, var = vars, x = res[[vars]])
    return(res)
}




runGbm <- function(dat, eq, return.mod = FALSE) {
    ## sub-sample the data for bootstrapping
    dat <- dat[sample(1:nrow(dat), nrow(dat), replace = TRUE), ]

    trees <- 1000
    mod <- gbm(eq, data = dat, distribution = "gaussian", n.trees = trees,
               shrinkage = 0.1, interaction.depth = 10, bag.fraction = 0.7,
               train.fraction = 1,  n.minobsinnode = 10,
               keep.data = FALSE, verbose = FALSE)

    ## extract partial dependnece componets
    pd <- plyr::ldply(vars, extractPD, mod)

    ## relative influence
    ri <- summary(mod, plotit = FALSE)

    if (return.mod) {
        
        return(list(pd, mod))
        
        
    } else {
        
        return(list(pd))
        
    }

}



