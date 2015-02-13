\name{processDiurnal}
\alias{processDiurnal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
processDiurnal(site = "BT4", pollutant = "nox", variables = variables, date = c("19/2/2005", "19/2/2007", "19/2/2010"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{site}{
%%     ~~Describe \code{site} here~~
}
  \item{pollutant}{
%%     ~~Describe \code{pollutant} here~~
}
  \item{variables}{
%%     ~~Describe \code{variables} here~~
}
  \item{date}{
%%     ~~Describe \code{date} here~~
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
function (site = "BT4", pollutant = "nox", variables = variables, 
    date = c("19/2/2005", "19/2/2007", "19/2/2010")) 
{
    results <- diurnal.gbm(mydata, variables = variables, 1, 
        pollutant = pollutant, test = F, date = date)
    pollutantName <- toupper(pollutant)
    if (pollutantName == "NOX") 
        pollutantName <- "NOx"
    pdf(paste(site, "Diurnal", pollutantName, ".pdf", sep = ""), 
        width = 6, height = 5)
    print(results[[2]])
    dev.off()
    results <- results[[1]]
    results$site <- site
    results$pollutant <- pollutant
    results
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
