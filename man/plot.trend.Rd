\name{plot.trend}
\alias{plot.trend}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.trend(trend, pollutant, date.breaks = 7, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{trend}{
%%     ~~Describe \code{trend} here~~
}
  \item{pollutant}{
%%     ~~Describe \code{pollutant} here~~
}
  \item{date.breaks}{
%%     ~~Describe \code{date.breaks} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (trend, pollutant, date.breaks = 7, ...) 
{
    myform <- formula(paste(pollutant, "~ date"))
    blues3 <- brewer.pal(3, "Blues")
    dates <- openair:::dateBreaks(trend$date, date.breaks)$major
    formats <- openair:::dateBreaks(trend$date, date.breaks)$format
    xlim <- range(trend$date)
    plt <- xyplot(myform, data = trend, type = "l", ylab = quickText(paste(pollutant, 
        "(ug/m3)")), xlab = "date", xlim = xlim, ylim = c(0, 
        NA), scales = list(x = list(at = dates, format = formats)), 
        panel = function(x, y, subscripts, ...) {
            lrect(xleft = as.numeric(as.POSIXct("2010-04-15 12:00")), 
                ybottom = -10, xright = as.numeric(as.POSIXct("2010-04-21 08:00")), 
                ytop = 1000, border = NA, col = "grey95")
            panel.grid(-1, 0)
            panel.abline(v = dates, col = "grey90")
            lpolygon(c(trend$date, rev(trend$date)), c(trend[, 
                "lower"], rev(trend[, "upper"])), col = rgb(1, 
                0.5, 0.4, 0.3), border = NA)
            panel.xyplot(trend$date[subscripts], trend[subscripts, 
                "mean"], col = rgb(1, 0.5, 0.4, 1), lwd = 2, 
                ...)
            panel.xyplot(x, y, col = blues3[3], lwd = 2, ...)
        })
    print(plt)
    plt
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
