\name{minimalrug}
\alias{minimalrug}
\title{
Draw a rug plot, omitting the baseline.
}
\description{
This function shows a rug plot without the baseline, as normally shown by 
the R rug() function, to eliminate the redundant baseline.
}
\usage{
minimalrug(x, lwd = 0.7, tcl = 0.3, ...)
}
\arguments{
  \item{x}{
numeric vector of data to show in rugplot
}
  \item{lwd}{
line width to use for rug marks (default = 0.7)
}
  \item{tcl}{
length of tick marks as a fraction of the height of a line of text (default = 0.3)
}
  \item{\dots}{
other arguments to rug()
}
}

\value{
None
}

\references{
"The Visual Display of Quantitative Information", 2nd Ed., E. Tufte, 2001. 
}

\author{
Steven J. Murdoch <http://www.cl.cam.ac.uk/users/sjm217/>
}

\note{
This function could also be used by itself to produce the dot-dash plot also suggested by Tufte.
}

\seealso{
\code{\link{rug}}
}

\examples{
	data(faithful)
  xdata <- faithful$waiting
  ydata <- faithful$eruptions*60
  len=length(xdata)
    
  # Plot the data
  plot(xdata,ydata,
       # Omit axes
       axes=FALSE,
       pch=20,
       main=sprintf("Old Faithful Eruptions (%d samples)", len),
       xlab="Time till next eruption (min)",
       ylab="Duration (sec)",
       # Leave some space for the rug plot
       xlim=c(41,max(xdata)),
       ylim=c(70,max(ydata)),
       cex=0.5)

  # Add the axes, passing in the summary to provide quartile and mean
  fancyaxis(1,summary(xdata))
  fancyaxis(2,summary(ydata))

  # This data is heavily rounded and there are lots of ties, so use
  #  jitter to show distribution. It is not ideal but will do for
  #  and example
  jx <- clippedjitter(xdata, amount=0.4)
  jy <- clippedjitter(ydata, amount=0.1)

  # Draw the rug for X
  minimalrug(jx, side=1, line=-0.8)
  # Draw the rug for Y
  minimalrug(jy, side=2, line=-0.8)

}

\keyword{ aplot }
