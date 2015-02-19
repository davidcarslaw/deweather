## diurnal changes
##' 
##'
##' To add
##' @title Plot diurnal changes, removing the effect of meteorology
##' @param mydata A data frame to analyse.
##' @param variables The explanatory variables used in the model.
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
diurnal.gbm <- function(mydata, variables = variables,  pollutant = "nox", 
                        date = c("19/2/2005", "19/2/2007", "19/2/2010"), single = FALSE){

    variables <- paste(variables, collapse = "+")
    eq <- formula(paste(pollutant, "~", variables))

    mod1 <- runGbm(1, selectByDate(mydata, start = date[1], end = date[2]), eq, sub.size = 1,
                    pollutant = pollutant)

    res1 <- mod1[[2]]

    name1 <- paste(date[1], "-", date[2], sep = "")
    names(res1) <- c("hour", "weekday", name1)
    results <- res1

    if (!single) {

        mod2 <- runGbm(1, selectByDate(mydata, start = date[2], end = date[3]), eq, sub.size = 1,
                        pollutant = pollutant)

        res2 <- mod2[[2]]

        name2 <- paste(date[2], "-", date[3], sep = "")
        names(res2) <- c("hour", "weekday", name2)

        results <- merge(res1, res2, by = c("hour", "weekday"))
        results <- arrange(results, hour) ## order hours/weekdays

        ## only need weekday/sat/sun
        ids <- which(results$weekday %in% c("Saturday", "Sunday"))
        results$weekday <- as.character(results$weekday)
        results$weekday[-ids] <- "weekday"
        results <- ddply(results, .(weekday = weekday, hour = hour), numcolwise(mean))
        results$weekday <- ordered(results$weekday, levels =c("weekday", "Saturday", "Sunday"))

        ## difference
        results$difference <- results[, 4] - results[, 3]

        results <- melt(results, id.var = c("weekday", "hour", "difference"))
        ylim <- range(c(results$difference, results$value)) * 1.03

        plt <- xyplot(value ~ hour | weekday, data = results, type = "l",
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
                                                         rep(0, 24)), col = "forestgreen", alpha = 0.2,
                                   border = NA)

                      })

    } else {


        results <- arrange(results, hour) ## order hours/weekdays

        ## only need weekday/sat/sun
        ids <- which(results$weekday %in% c("Saturday", "Sunday"))
        results$weekday <- as.character(results$weekday)
        results$weekday[-ids] <- "weekday"
        results <- ddply(results, .(weekday = weekday, hour = hour), numcolwise(mean))
        results$weekday <- ordered(results$weekday, levels =c("weekday", "Saturday", "Sunday"))


        plt <- xyplot(get(name1 ) ~ hour | weekday, data = results, type = "l",
                      as.table = TRUE, lwd = 2,
                      layout = c(3, 1),
                      key = simpleKey(c(name1), space = "top", lines = TRUE, points = FALSE),
                      ylab = quickText(pollutant),
                      panel = function(x, y,...){
                          panel.grid(-1, -1)
                          panel.abline(h = 0, lty =5)
                          panel.xyplot(x, y, ...)
                      })

    }
    print(plt)
    list(results, plt)

}

processDiurnal <- function(site = "BT4", pollutant = "nox", variables = variables,
                           date = c("19/2/2005", "19/2/2007", "19/2/2010")) {

    results <- diurnal.gbm(mydata, variables = variables, 1, pollutant = pollutant, test = F,
                           date = date)

    pollutantName <- toupper(pollutant)
    if (pollutantName == "NOX") pollutantName <- "NOx"
    pdf(paste(site, "Diurnal", pollutantName, ".pdf", sep = ""), width = 6, height = 5)
    print(results[[2]])
    dev.off()
    results <- results[[1]]
    results$site <- site
    results$pollutant <- pollutant
    results

}

