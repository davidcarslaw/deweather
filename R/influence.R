gbmInf <- function(dat, eq) {
    mod <- runGbm(dat, eq, return.mod = TRUE)
    mod <- mod[[2]] ## second list item
    influ <- summary(mod, plotit = FALSE)
    
    influ$var <- reorder(influ$var, influ$rel.inf)

    dotplot(var ~ rel.inf, data = influ, type = c("p", "h"),
            xlab = "relative variable influence (%)",
            xlim = c(0, NA), 
            panel = function(x, y){
                panel.grid(v = -1, h = -1, col = "grey85")
                                        #  panel.dotplot(x, y, cex = 1.5)
                panel.barchart(x, y, col = "grey30", border = NA)
                                        # panel.xyplot(x, y, type = "h", horiz = TRUE)
            })

}
