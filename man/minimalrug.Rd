\name{minimalrug}
\alias{minimalrug}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
minimalrug(x, lwd = 0.7, tcl = 0.3, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{lwd}{
%%     ~~Describe \code{lwd} here~~
}
  \item{tcl}{
%%     ~~Describe \code{tcl} here~~
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
function (x, lwd = 0.7, tcl = 0.3, ...) 
{
    oldlend <- par(lend = "butt")
    on.exit(par(oldlend))
    bg <- par("bg")
    if (bg == "transparent") 
        bg <- "white"
    rug(x, ticksize = NA, lwd = lwd, ...)
    overlwd = 1
    axis(..., at = x, col = bg, tcl = 0, label = FALSE, lwd = overlwd)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
