\name{rangeaxis}
\alias{rangeaxis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rangeaxis(side, summ, at = NULL, mingap = 0.5, digits = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{side}{
%%     ~~Describe \code{side} here~~
}
  \item{summ}{
%%     ~~Describe \code{summ} here~~
}
  \item{at}{
%%     ~~Describe \code{at} here~~
}
  \item{mingap}{
%%     ~~Describe \code{mingap} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
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
function (side, summ, at = NULL, mingap = 0.5, digits = 2) 
{
    amin <- summ[1]
    aq1 <- summ[2]
    amed <- summ[3]
    amean <- summ[4]
    aq3 <- summ[5]
    amax <- summ[6]
    parside <- if (side == 1) {
        flip <- 1
        xaxis <- TRUE
        islog <- par("xlog")
        otherlog <- par("ylog")
        3
    }
    else if (side == 2) {
        flip <- 1
        xaxis <- FALSE
        islog <- par("ylog")
        otherlog <- par("xlog")
        1
    }
    else if (side == 3) {
        flip <- -1
        xaxis <- TRUE
        islog <- par("xlog")
        otherlog <- par("ylog")
        4
    }
    else if (side == 4) {
        flip <- -1
        xaxis <- FALSE
        islog <- par("ylog")
        otherlog <- par("xlog")
        2
    }
    if (is.null(at)) 
        ticks <- axTicks(side)
    else ticks <- at
    ticks <- ticks[(ticks >= amin) & (ticks <= amax)]
    numticks <- length(ticks)
    if (islog) 
        axgap <- (log10(ticks[numticks]) - log10(ticks[numticks - 
            1])) * mingap
    else axgap <- (ticks[numticks] - ticks[numticks - 1]) * mingap
    numticks <- length(ticks)
    firsttick <- ticks[1]
    lasttick <- ticks[numticks]
    if (islog && (log10(amax) - log10(lasttick) < axgap)) {
        ticks[numticks] <- amax
    }
    else if (amax - lasttick < axgap) {
        ticks[numticks] <- amax
    }
    else {
        ticks <- c(ticks, amax)
    }
    if (islog && (abs(log10(amin) - log10(firsttick)) < axgap)) {
        ticks[1] <- amin
    }
    else if (firsttick - amin < axgap) {
        ticks[1] <- amin
    }
    else {
        ticks <- c(amin, ticks)
    }
    numticks <- length(ticks)
    lmin <- format(round(ticks[1], digits), nsmall = digits, 
        trim = TRUE)
    lmax <- format(round(ticks[numticks], digits), nsmall = digits, 
        trim = TRUE)
    middle <- format(ticks[2:(numticks - 1)], trim = TRUE)
    labels <- c(lmin, middle, lmax)
    oldlend <- par(lend = "butt")
    on.exit(par(oldlend))
    bg <- par("bg")
    if (bg == "transparent") 
        bg <- "white"
    lwd = 0.7
    axis(side, ticks, labels = FALSE, col = "gray50", lwd = lwd)
    overlwd = 1
    axis(side, ticks, labels = FALSE, col = "black", tcl = 0, 
        lwd = overlwd)
    axis(side, ticks, labels = labels, tick = FALSE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
