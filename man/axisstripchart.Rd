\name{axisstripchart}
\alias{axisstripchart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
axisstripchart(x, side, sshift = 0.3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{side}{
%%     ~~Describe \code{side} here~~
}
  \item{sshift}{
%%     ~~Describe \code{sshift} here~~
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
function (x, side, sshift = 0.3) 
{
    parside <- if (side == 1) {
        flip <- 1
        yaxis <- FALSE
        3
    }
    else if (side == 2) {
        flip <- 1
        yaxis <- TRUE
        1
    }
    else if (side == 3) {
        flip <- -1
        yaxis <- FALSE
        4
    }
    else if (side == 4) {
        flip <- -1
        yaxis <- TRUE
        2
    }
    base <- par("usr")[parside]
    plotwidth <- diff(par("usr")[1:2])
    plotheight <- diff(par("usr")[3:4])
    shift <- par("pin")[1] * 0.003 * flip
    gap <- par("pin")[1] * 0.003
    meanshift <- par("cin")[1] * 0.5 * flip
    stripshift <- par("cin")[1] * sshift * flip
    if (yaxis) {
        shift <- shift/par("pin")[1] * plotwidth
        meanshift <- meanshift/par("pin")[1] * plotwidth
        stripshift <- stripshift/par("pin")[1] * plotwidth
        gap <- gap/par("pin")[2] * plotheight
    }
    else {
        shift <- shift/par("pin")[2] * plotheight
        meanshift <- meanshift/par("pin")[2] * plotheight
        stripshift <- stripshift/par("pin")[2] * plotheight
        gap <- gap/par("pin")[1] * plotwidth
    }
    if (yaxis) 
        offset = flip * par("cin")[2]/par("cin")[1]
    else offset = flip
    oldxpd <- par(xpd = TRUE)
    on.exit(par(oldxpd))
    stripchart(x, method = "stack", vertical = yaxis, offset = offset, 
        pch = 15, cex = 0.2, add = TRUE, at = base + shift + 
            stripshift, col = "red")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
