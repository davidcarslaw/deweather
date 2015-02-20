
##' Plot a gbm influence plot
##'
##' 
##' @title Influence plot
##' @param modOb Model object from running \code{buildMod}
##' @export
##' @return Plot
##' @author David Carslaw
gbmInf <- function(modOb) {

    ## extract influence data from object
    influ <- modOb$influence

    dotplot(var ~ rel.inf, data = influ, type = c("p", "h"),
            xlab = "relative variable influence (%)",
            xlim = c(0, NA), 
            panel = function(x, y){
                panel.grid(v = -1, h = -1, col = "grey85")
            panel.barchart(x, y, col = "grey30", border = NA)
          })

}
