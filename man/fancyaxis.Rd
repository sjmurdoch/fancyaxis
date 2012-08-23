\name{fancyaxis}
\alias{fancyaxis}
\title{
Draws an axis showing information about the marginal distribution of a variable
}
\description{
Draws an axis showing the mean, median, 1st and 3rd quartiles, as well as the range
of a variable.
}
\usage{
fancyaxis(side, summ, at = NULL, mingap = 0.5, digits = 2, shiftfac = 0.003, gapfac = 0.003, mdotcol = "red")
}
\arguments{
  \item{side}{
side of plot to draw axis on: 1 - bottom, 2 - left, 3 - top, 4 - right
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
  \item{shiftfac}{
proportion of plot width used to offset the broken axis (default = 0.003)
}
  \item{gapfac}{
proportion of plot width used to leave for median gap (default = 0.003)
}
  \item{mdotcol}{
color of dot at mean position (default="red")
}
}

\value{
None.
}

\details{
This is very much a work in progress and still of alpha quality. It currently does not fully deal with logarithmic scales and needs manual tweaking of several values to suit different data and output device resolution. Drawing tickmarks and labels is performed by axis(), which always draws a baseline. This is then erased with the background colour so it does not work properly with a transparent background. Also, in some rendering engines of PDF and Postscript output the erasure is not complete and some parts of the baseline are still visible. A reimplementation of axis() in R code was intolerably slow so the full solution would be to modify the C implementing axis(), adding an option to disable the drawing of the baseline.
}

\references{
"The Visual Display of Quantitative Information", 2nd Ed., E. Tufte, 2001. 
}

\author{
Steven J. Murdoch \url{http://www.cl.cam.ac.uk/users/sjm217/}
}

\seealso{
\code{\link{rangeaxis}}, \code{\link{summary}}
}

\examples{
	data(faithful)
  xdata <- faithful$waiting
  ydata <- faithful$eruptions*60
  len=length(xdata)
  
  # Label event with its previous duration
  lag=ydata[1:len-1]
  lma=max(lag)
  lmi=min(lag)
  lag=lag-lmi
  lag=lag/(lma-lmi)
  colours <- gray(lag)

  xdata=xdata[2:len]
  ydata=ydata[2:len]
  len=length(xdata)
    
  plot(xdata,ydata,
       # Omit axes
       axes=FALSE,
       pch=20,
       main=paste("Old Faithful Eruptions (",len," samples)", sep=''),
       xlab="Time till next eruption (min)",
       ylab="Duration (sec)",
       # Leave some space for the rug plot
       xlim=c(41,max(xdata)),
       ylim=c(90,max(ydata)),
       cex=0.5,
       col=colours)

  # Add the axes, passing in the summary to provide quartile and mean
  fancyaxis(1,summary(xdata))
  fancyaxis(2,summary(ydata))
	
}

\keyword{ aplot }
