\name{axisstripchart}
\alias{axisstripchart}
\title{
Draw a bar plot on an axis.
}
\description{
Draws a bar plot on the axis, showing the frequency distribution of the respective variable.
}
\usage{
axisstripchart(x, side, sshift = 0.3)
}
\arguments{
  \item{x}{
numeric vector of data to plot along axis
}
  \item{side}{
side of plot to draw axis on: 1 - bottom, 2 - left, 3 - top, 4 - right
}
  \item{sshift}{
amount to shift the stripchart away from the axis (default = 0.3)
}
}

\details{
While clippedjitter() can be used to display density information of a variable with many ties, this is not always ideal, since it hides information about the rounding applied to the data. As an alternative, axisstripchart() draws a bar plot on the axis, showing the frequency distribution of the respective variable.}

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
Does not handle log axes well.
}

\seealso{
\code{\link{clippedjitter}}, \code{\link{stripchart}}
}

\examples{
  data(faithful)
  xdata <- faithful$waiting
  ydata <- faithful$eruptions*60
  len=length(xdata)
  
  # Label event with its previous duration

  split=180

  lag=ydata[1:len-1]
  colours <- lag
  colours[lag>=split] <- "red"
  colours[!(lag>=split)] <- "blue"

  xdata=xdata[2:len]
  ydata=ydata[2:len]
  len=length(xdata)
  
  # Plot the data
  plot(xdata,ydata,
       # Omit axes
       axes=FALSE,
       pch=20,
       main=paste("Old Faithful Eruptions (",len," samples)",sep=''),
       xlab="Time till next eruption (min)",
       ylab="Duration (sec)",
       # Leave some space for the rug plot
       xlim=c(41,max(xdata)),
       ylim=c(70,max(ydata)),
       cex=0.5,
       col=colours)

  axp=par("xaxp")
  axp[3] <- axp[3]*2
  par("xaxp"=axp)
  
  # Add the axes, passing in the summary to provide quartile and mean
  fancyaxis(1,summary(xdata), digits=0)
  fancyaxis(2,summary(ydata), digits=0)

  # Add the stripcharts
  axisstripchart(xdata, 1)
  axisstripchart(ydata, 2)

  lines(c(min(xdata),max(xdata)),c(split,split),lty=2, col="gray50", xpd=FALSE)
  h=par("cxy")[2]/2
  points(rep(max(xdata),2),c(split+h,split-h),col=c("red","blue"), pch=20)
  text(95,split+h, "Previous duration", adj=c(1,0.5))

}

\keyword{ aplot }