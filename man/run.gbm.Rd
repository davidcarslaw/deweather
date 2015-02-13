\name{run.gbm}
\alias{run.gbm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
run.gbm(i, mydata, eq, sub.size = 1, pollutant = "nox", test = FALSE, bootstrap = FALSE, return.mod = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{i}{
%%     ~~Describe \code{i} here~~
}
  \item{mydata}{
%%     ~~Describe \code{mydata} here~~
}
  \item{eq}{
%%     ~~Describe \code{eq} here~~
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
  \item{bootstrap}{
%%     ~~Describe \code{bootstrap} here~~
}
  \item{return.mod}{
%%     ~~Describe \code{return.mod} here~~
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
function (i, mydata, eq, sub.size = 1, pollutant = "nox", test = FALSE, 
    bootstrap = FALSE, return.mod = FALSE) 
{
    if (test) 
        sub.size = 0.75
    missing <- which(is.na(mydata[, pollutant]))
    if (length(missing) > 0) 
        mydata <- mydata[-missing, ]
    if (bootstrap) {
        mydata <- mydata[sample(1:nrow(mydata), nrow(mydata), 
            replace = TRUE), ]
    }
    N <- nrow(mydata)
    idx <- sample(1:N, round(N * sub.size))
    mydata.sub <- mydata[idx, ]
    test.dat <- mydata[-idx, ]
    trees <- 1000
    gbm.mod <- gbm(eq, data = mydata.sub, distribution = "gaussian", 
        n.trees = trees, shrinkage = 0.1, interaction.depth = 10, 
        bag.fraction = 0.5, train.fraction = 1, n.minobsinnode = 10, 
        keep.data = TRUE, verbose = FALSE)
    summary(gbm.mod)
    if (test) {
        f.predict <- predict.gbm(gbm.mod, test.dat, trees)
        rms <- mean((f.predict - test.dat[, pollutant])^2)^0.5
        print(rms)
        plot(test.dat[, pollutant], f.predict, pch = ".")
        lm1 <- lm(f.predict ~ test.dat[, pollutant])
        print(summary(lm1))
    }
    trend <- plot(gbm.mod, 1, cont = 1000, return.grid = TRUE)
    if (length(grep("weekday", eq)) == 1) {
        diurnal <- plot(gbm.mod, i.var = c("hour.local", "weekday"), 
            1000, return = T, cont = 24)
    }
    else {
        diurnal <- NULL
    }
    if (return.mod) {
        list(trend, diurnal, gbm.mod)
    }
    else {
        list(trend, diurnal)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
