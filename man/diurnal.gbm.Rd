\name{diurnal.gbm}
\alias{diurnal.gbm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
diurnal.gbm(mydata, variables = variables, sub.size = 1, pollutant = "nox", test = FALSE, date = c("19/2/2005", "19/2/2007", "19/2/2010"), single = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydata}{
%%     ~~Describe \code{mydata} here~~
}
  \item{variables}{
%%     ~~Describe \code{variables} here~~
}
  \item{sub.size}{
%%     ~~Describe \code{sub.size} here~~
}
  \item{pollutant}{
%%     ~~Describe \code{pollutant} here~~
}
  \item{test}{
%%     ~~Describe \code{test} here~~
}
  \item{date}{
%%     ~~Describe \code{date} here~~
}
  \item{single}{
%%     ~~Describe \code{single} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (mydata, variables = variables, sub.size = 1, pollutant = "nox", 
    test = FALSE, date = c("19/2/2005", "19/2/2007", "19/2/2010"), 
    single = FALSE) 
{
    variables <- paste(variables, collapse = "+")
    eq <- formula(paste(pollutant, "~", variables))
    mod1 <- run.gbm(1, selectByDate(mydata, start = date[1], 
        end = date[2]), eq, sub.size = 1, pollutant = pollutant)
    res1 <- mod1[[2]]
    name1 <- paste(date[1], "-", date[2], sep = "")
    names(res1) <- c("hour", "weekday", name1)
    results <- res1
    if (!single) {
        mod2 <- run.gbm(1, selectByDate(mydata, start = date[2], 
            end = date[3]), eq, sub.size = 1, pollutant = pollutant)
        res2 <- mod2[[2]]
        name2 <- paste(date[2], "-", date[3], sep = "")
        names(res2) <- c("hour", "weekday", name2)
        results <- merge(res1, res2, by = c("hour", "weekday"))
        results <- arrange(results, hour)
        ids <- which(results$weekday \%in\% c("Saturday", "Sunday"))
        results$weekday <- as.character(results$weekday)
        results$weekday[-ids] <- "weekday"
        results <- ddply(results, .(weekday = weekday, hour = hour), 
            numcolwise(mean))
        results$weekday <- ordered(results$weekday, levels = c("weekday", 
            "Saturday", "Sunday"))
        results$difference <- results[, 4] - results[, 3]
        results <- melt(results, id.var = c("weekday", "hour", 
            "difference"))
        ylim <- range(c(results$difference, results$value)) * 
            1.03
        plt <- xyplot(value ~ hour | weekday, data = results, 
            type = "l", ylim = ylim, group = variable, as.table = TRUE, 
            lwd = 2, layout = c(3, 1), scales = list(x = list(at = c(0, 
                6, 12, 18, 23))), key = simpleKey(c(name1, name2), 
                space = "top", columns = 2, lines = TRUE, points = FALSE), 
            ylab = quickText(paste(pollutant, "(ug/m3)")), panel = panel.superpose, 
            panel.groups = function(x, y, subscripts, groups, 
                group.number, ...) {
                if (group.number == 1) {
                  panel.grid(-1, 0)
                  panel.abline(v = c(0, 6, 12, 18, 23), col = "grey85")
                }
                panel.abline(h = 0, lty = 5)
                panel.xyplot(x, y, ...)
                lpolygon(c(0:23, rev(0:23)), c(results$difference[subscripts], 
                  rep(0, 24)), col = "forestgreen", alpha = 0.2, 
                  border = NA)
            })
    }
    else {
        results <- arrange(results, hour)
        ids <- which(results$weekday \%in\% c("Saturday", "Sunday"))
        results$weekday <- as.character(results$weekday)
        results$weekday[-ids] <- "weekday"
        results <- ddply(results, .(weekday = weekday, hour = hour), 
            numcolwise(mean))
        results$weekday <- ordered(results$weekday, levels = c("weekday", 
            "Saturday", "Sunday"))
        plt <- xyplot(get(name1) ~ hour | weekday, data = results, 
            type = "l", as.table = TRUE, lwd = 2, layout = c(3, 
                1), key = simpleKey(c(name1), space = "top", 
                lines = TRUE, points = FALSE), ylab = quickText(pollutant), 
            panel = function(x, y, ...) {
                panel.grid(-1, -1)
                panel.abline(h = 0, lty = 5)
                panel.xyplot(x, y, ...)
            })
    }
    print(plt)
    list(results, plt)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
