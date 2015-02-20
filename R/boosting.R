
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
buildMod <- function(dat, vars = c("ws", "wd"), pollutant = "nox", partial.dep = TRUE) {

    ## add other variables
    dat <- prepData(dat)

    variables <- paste(vars, collapse = "+")
    eq <- formula(paste(pollutant, "~", variables))

    ## make sure no NA in response
    id <- which(is.na(dat[[pollutant]]))
    if (length(id) > 0 )
        dat <- dat[-id, ]

    ## Influence plot
   
    
    mod <- runGbm(dat, eq, vars, return.mod = TRUE, simulate = FALSE)
    mod <- mod[[2]] ## second list item
    influ <- summary(mod, plotit = FALSE)
    
    influ$var <- reorder(influ$var, influ$rel.inf)

  
    ## partial dependence calculations
    if (partial.dep) {

        pd <- partialDep(dat, eq, vars)

        return(list(model = mod, influence = influ, pd = pd))
        
    } else {

        return(list(model = mod, influence = influ))
    }

    

}

extractPD <- function(vars, mod) {
    
    ## extract partial dependence values
    res <- plot(mod, vars, cont = 100, return.grid = TRUE)
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



