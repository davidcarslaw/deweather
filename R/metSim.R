

##' Function to run random meteorological simulations on a gbm model
##'
##' 
##' @title Run random meteorology on a gbm model
##' @param dat 
##' @param newdata Model object from running \code{buildMod}.
##' @param metVars Teh variables that should be randomly varied.
##' @param n.core Number of cores to use
##' @param B Number of simulations
##' @export
##' @return To add
##' @author David Carslaw
metSim <- function(dat, newdata, metVars = c("ws", "wd", "temp"), n.core = 4, B = 200) {

    ## extract the model
    mod <- dat$model
    
    cl <- makeCluster(n.core)
    registerDoParallel(cl)

    prediction <- foreach (i = 1:B, .inorder = FALSE, .combine = "rbind", 
                           .packages = "gbm", .export = "doPred") %dopar%
    doPred(newdata, mod, metVars)
    
    stopCluster(cl)

    ## Aggregate results
    prediction <- group_by(prediction, date) %>%
      summarise(pred = mean(pred))

    return(prediction)
}


## randomly sample from original data
doPred <- function(mydata, mod, metVars) {

    ## random samples 
    n <- nrow(mydata) 
    id <- sample(1 : n, n, replace = FALSE)
        
    ## new data with random samples
    mydata[metVars] <- lapply(mydata[metVars], function (x) x[id])
    
    prediction <- predict.gbm(mod, mydata, 1000)
    prediction <- data.frame(date = mydata$date, pred = prediction)
    
    return(prediction)
}
