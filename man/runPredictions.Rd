\name{runPredictions}
\alias{runPredictions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
runPredictions(mydata = mydata, variables, pollutant = "nox", start = "1/1/2007", end = "10/4/2010", pred.data, return.mod = FALSE, met.sim = FALSE, test = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydata}{
%%     ~~Describe \code{mydata} here~~
}
  \item{variables}{
%%     ~~Describe \code{variables} here~~
}
  \item{pollutant}{
%%     ~~Describe \code{pollutant} here~~
}
  \item{start}{
%%     ~~Describe \code{start} here~~
}
  \item{end}{
%%     ~~Describe \code{end} here~~
}
  \item{pred.data}{
%%     ~~Describe \code{pred.data} here~~
}
  \item{return.mod}{
%%     ~~Describe \code{return.mod} here~~
}
  \item{met.sim}{
%%     ~~Describe \code{met.sim} here~~
}
  \item{test}{
%%     ~~Describe \code{test} here~~
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
function (mydata = mydata, variables, pollutant = "nox", start = "1/1/2007", 
    end = "10/4/2010", pred.data, return.mod = FALSE, met.sim = FALSE, 
    test = FALSE) 
{
    mydata <- openair:::checkPrep(mydata, c("date", variables, 
        pollutant), type = "default")
    variables <- paste(variables, collapse = "+")
    eq <- formula(paste(pollutant, "~", variables))
    mydata <- selectByDate(mydata, start = start, end = end)
    gbm.mod <- run.gbm(i = 1, mydata, eq = eq, pollutant = pollutant, 
        return.mod = TRUE, test = test, bootstrap = FALSE)
    do.pred <- function(mydata) {
        date <- seq(min(mydata$date), max(mydata$date), by = "hour")
        n <- length(date)
        id <- sample(1:nrow(mydata), n, replace = TRUE)
        min.trend <- min(mydata$trend)
        max.trend <- max(mydata$trend)
        trend <- seq(min.trend, max.trend, length = n)
        temp <- subset(mydata, select = -trend)
        newdata <- data.frame(trend = seq(min.trend, max.trend, 
            length = n), temp[id, ])
        prediction <- predict.gbm(gbm.mod[[3]], newdata, 1000)
        prediction <- data.frame(date = date, pollutant = prediction)
        names(prediction)[2] <- pollutant
        prediction
    }
    if (met.sim) {
        library(doSNOW)
        cl <- makeCluster(8)
        registerDoSNOW(cl)
        prediction <- foreach(i = 1:100, .inorder = FALSE, .packages = "gbm") \%dopar\% 
            do.pred(mydata = mydata)
        stopCluster(cl)
    }
    else {
        prediction <- predict.gbm(gbm.mod[[3]], pred.data, 1000)
        prediction <- data.frame(date = pred.data$date, pollutant = prediction)
        names(prediction)[2] <- pollutant
    }
    prediction
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
