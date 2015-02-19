
## function to run random simulations usining random sampling from met data
metSim <- function(mod, newdata, metVars = c("ws", "wd", "temp"), n.core = 4, B = 200) {

 
    cl <- makeCluster(n.core)
    registerDoParallel(cl)

    prediction <- foreach (i = 1:B, .inorder = FALSE, .combine = "rbind", 
                           .packages = "gbm", .export = "doPred") %dopar%
    doPred(newdata, mod, metVars)
    
    stopCluster(cl)

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
