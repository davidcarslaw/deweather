## diurnal changes
##' 
##'
##' To add
##' @title Plot diurnal changes, removing the effect of meteorology
##' @param dat A data frame to analyse.
##' @param vars The explanatory variables used in the model.
##' @param pollutant Name of teh pollutant to apply meteorological
##' normalisation to.
##' @param date A vector of dates, length three. These dates are used
##' to parition the data into two categories (before/after). The date
##' format is UK e.g. \code{date = c("19/2/2005", "19/2/2007",
##' "19/2/2010")}.
##' @param single Not used.
##' @import reshape2
##' @export
##' @return Some data
##' @author David Carslaw
diurnalGbm <- function(dat, vars = c("ws", "wd", "hour", "weekday"),  pollutant = "nox", 
                       date = c("01/01/2012", "31/12/2012", "31/12/2013"), single = FALSE){


    ## set graphics
    current.strip <- trellis.par.get("strip.background")
    
    ## reset graphic parameters
    on.exit(trellis.par.set(strip.background = current.strip))
    
    suppressWarnings(trellis.par.set(list(strip.background = list(col = "white"))))

    mod1 <- buildMod(selectByDate(dat, start = date[1], end = date[2]), vars = vars,
                     pollutant = pollutant, B = 1)
    
    res1 <- plot2Way(mod1, variable = c("weekday", "hour"))

    name1 <- paste(date[1], "-", date[2], sep = "")
    res1[[name1]] <- name1
    results <- res1

    if (!single) {

        if (length(date) == 4) {
            start1 <- date[3]
            end1 <- date[4]
        } else {
            start1 <- date[2]
            end1 <- date[3]
        }
        
        mod2 <- buildMod(selectByDate(dat, start = start1, end = end1), vars = vars,
                         pollutant = pollutant, B = 1)

        res2 <- plot2Way(mod2, variable = c("weekday", "hour"))

        name2 <- paste(start1, "-", end1, sep = "")
        res2[[name2]] <- name2
        
        results <- merge(res1, res2, by = c("Hour", "Weekday"))
        results <- arrange(results, Hour) ## order Hours/weekdays
        
        ## only need weekday/sat/sun
        ids <- which(results$Weekday %in% c("Sat", "Sun"))
        results$Weekday <- as.character(results$Weekday)
        results$Weekday[-ids] <- "Weekday"
        results <- plyr::ddply(results, plyr::.(Weekday = Weekday, Hour = Hour), plyr::numcolwise(mean))
        results$Weekday <- ordered(results$Weekday, levels = c("Weekday", "Sat", "Sun"),
                                   labels = c("Weekday", "Saturday", "Sunday"))

        ## difference
        results$difference <- results[, 4] - results[, 3]

        results <- melt(results, id.var = c("Weekday", "Hour", "difference"))
        ylim <- range(c(results$difference, results$value)) * 1.03
        
        plt <- xyplot(value ~ Hour | Weekday, data = results, type = "l",
                      ylim = ylim,
                      group = variable,
                      as.table = TRUE, lwd = 2,
                      layout = c(3, 1),
                      scales = list(x = list(at = c(0, 6, 12, 18, 23))),
                      key = simpleKey(c(name1, name2), space = "top",  columns = 2,
                          lines = TRUE, points = FALSE),
                      ylab = quickText(paste(pollutant, "(ug/m3)")),
                      panel = panel.superpose,

                      panel.groups = function(x, y, subscripts, groups, group.number,...){
                          if (group.number == 1) {
                              panel.grid(-1, 0)
                              panel.abline(v = c(0, 6, 12, 18, 23), col = "grey85")
                          }

                          panel.abline(h = 0, lty =5)
                          panel.xyplot(x, y, ...)

                          lpolygon(c(0:23, rev(0:23)), c(results$difference[subscripts],
                                                         rep(0, 24)), col = "forestgreen",
                                   alpha = 0.2, border = NA)

                      })

    } else {


        results <- arrange(results, Hour) ## order Hours/Weekdays
        
        ## only need Weekday/sat/sun
        ids <- which(results$Weekday %in% c("Sat", "Sun"))
        results$Weekday <- as.character(results$Weekday)
        results$Weekday[-ids] <- "Weekday"
        results <- plyr::ddply(results, plyr::.(Weekday = Weekday, Hour = Hour),
                               plyr::numcolwise(mean))
        results$Weekday <- ordered(results$Weekday, levels =c("Weekday", "Sat", "Sun"),
                                   labels = c("Weekday", "Saturday", "Sunday"))

        plt <- xyplot(y ~ Hour | Weekday, data = results, type = "l",
                      as.table = TRUE, lwd = 2,
                      scales = list(x = list(at = c(0, 6, 12, 18, 23))),
                      layout = c(3, 1),
                      key = simpleKey(c(name1), space = "top", lines = TRUE, points = FALSE),
                      ylab = quickText(pollutant),
                      panel = function(x, y,...){
                          panel.grid(-1, 0)
                          panel.abline(v = c(0, 6, 12, 18, 23), col = "grey85")
                          
                          panel.xyplot(x, y, ...)
                      })

    }
    print(plt)
    invisible(list(results, plt))

}

