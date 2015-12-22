
##' Plot a gbm influence plot
##'
##' 
##' @title Influence plot
##' @param dat Model object from running \code{buildMod}
##' @export
##' @return Plot
##' @author David Carslaw
gbmInf <- function(dat) {

    if (class(dat) != "deweather")
        stop ("Need to supply a deweather object from buildMod.")

    ## extract influence data from object
    influ <- dat$influence

    ggplot(influ, aes(var, mean)c) +
        geom_bar(stat = "identity") +
        coord_flip() +
        ylab("relative variable influence (%)") +
        xlab("variable")

    }
