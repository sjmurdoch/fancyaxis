\name{rangeaxis}
\alias{rangeaxis}

\title{
Plot an axis showing the minimum and maximum values
}
\description{
Use a data summary to plot an axis labelled with minimum and maximum values, 
rather than default "pretty" values.
}
\usage{
rangeaxis(side, summ, at = NULL, mingap = 0.5, digits = 2)
}

\arguments{
  \item{side}{
integer specifying the side to place the axis on: 1 - bottom, 2 - left, 3 - top, 4 -right
}
  \item{summ}{
summary object, as those returned by summary()
}
  \item{at}{
positions of tickmarks if not NULL (default = NULL)
}
  \item{mingap}{
the smallest gap permitted between two tickmarks, expressed as a 
fraction of the default tickmark gap (default = 0.5)
}
  \item{digits}{
the number of digits to round minimum and maximum to (default = 2)
}
}

\value{
None
}

\references{
"The Visual Display of Quantitative Information", 2nd Ed., E. Tufte, 2001. 
}

\note{
Code adapted from fancyaxis by Steven J. Murdoch.
}

\author{
Peter N. Steinmetz <http://www.steinmetz.org/peter>
}

\seealso{
\code{\link{summary}}, \code{\link{fancyaxis}}
}

\examples{
	data(faithful)
  xdata <- faithful$waiting
  ydata <- faithful$eruptions*60
  len=length(xdata)
  
  xdata=xdata[2:len]
  ydata=ydata[2:len]
  len=length(xdata)
    
  plot(xdata,ydata,
       # Omit axes
       axes=FALSE,
       pch=20,
       main=sprintf("Old Faithful Eruptions (\%d samples)", len),
       xlab="Time till next eruption (min)",
       ylab="Duration (sec)",
       # Leave some space for the rug plot
       xlim=c(41,max(xdata)),
       ylim=c(90,max(ydata)),
       cex=0.5)

  # Add the axes, passing in the summary to provide quartile and mean
  rangeaxis(1,summary(xdata))
  axis(2)

}

\keyword{ aplot }
