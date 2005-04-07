##     fancyaxis: Draw axis which shows minimum, maximum, quartiles
##                 and mean
##
##     Copyright (C) 2005 Steven J. Murdoch <http://www.cl.cam.ac.uk/users/sjm217/>
##
##     This program is free software; you can redistribute it and/or modify
##     it under the terms of the GNU General Public License as published by
##     the Free Software Foundation; either version 2 of the License, or
##     (at your option) any later version.
##
##     This program is distributed in the hope that it will be useful,
##     but WITHOUT ANY WARRANTY; without even the implied warranty of
##     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##     GNU General Public License for more details.
##
##     You should have received a copy of the GNU General Public License
##     along with this program; if not, write to the Free Software
##     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

##     This is very much a work in progress and still of alpha
##     quality. See the example at the end of file for usage. It
##     currently does not deal with logarithmic scales properly and
##     needs manual tweaking of several values to suit different data
##     and output device resolution. Comments and suggestions are
##     appreciated. My contact details can be found here:
##        http://www.cl.cam.ac.uk/users/sjm217/     
##
##     The design of the graph is based on a scatterplot presented in
##     "The Visual Display of Quantitative Information", Edward Tufte.
##
##     $Id$

source("fancyaxis.R")

opendevice <- function() {
  # Background colour
  par("bg"="#fffff0")
  #par("bg"="transparent")

  # Output device. If outputing to a file, remember to use dev.off() at
  #  the end
  #png(file="/tmp/faithful.png",width=480,height=320,bg=par("bg"))
  #postscript(file="/tmp/faithful.ps",paper="A4",bg=par("bg"))

  # A4 paper
  #pdf(file="/tmp/faithful.pdf", width=297/25.4, height=210/25.4, bg=par("bg"))

  # Wider paper, used for the small multiple graph
  pdf(file="/tmp/faithful.pdf", width=297/25.4*1.5, height=210/25.4/2, bg=par("bg"))

  #X11(bg=par("bg"))

  # Make axis labels horizontal
  par(las=1)
}

closedevice <- function() {
  # Use this if outputing to a file
  dev.off()
}

stripchartexample <- function() {
  #opendevice()

  #xdata=iris$Petal.Width
  #ydata=iris$Petal.Length

  #xdata=cars$speed
  #ydata=cars$dist

  # Sample dataset from R
  xdata <- faithful$waiting
  ydata <- faithful$eruptions*60

  # Label event age by a colour in the range (0,0.75)
  #colours <- gray((1:length(xdata))/length(xdata)*0.75)

  # Plot the data
  plot(xdata,ydata,
       # Omit axes
       axes=FALSE,
       pch=20,
       main="Old Faithful Eruptions",
       xlab="Time till next eruption (min)",
       ylab="Duration (sec)",
       # Leave some space for the rug plot
       xlim=c(41,max(xdata)),
       ylim=c(70,max(ydata)),
       cex=0.5,
       col="black")

  # Add the axes, passing in the summary to provide quartile and mean
  fancyaxis(1,summary(xdata))
  fancyaxis(2,summary(ydata))

  # Add the stripcharts
  axisstripchart(xdata, 1)
  axisstripchart(ydata, 2)

  #closedevice()
}

rugexample <- function() {
  #opendevice()

  #xdata=iris$Petal.Width
  #ydata=iris$Petal.Length

  #xdata=cars$speed
  #ydata=cars$dist

  # Sample dataset from R
  xdata <- faithful$waiting
  ydata <- faithful$eruptions*60
  len=length(xdata)
  
  # Label event age by a colour in the range (0,0.75)
  #colours <- gray((1:length(xdata))/length(xdata)*0.75)

  # Label event with its previous duration
  lag=ydata[2:len]
  #lma=max(lag)
  #lmi=min(lag)
  #lag=lag-lmi
  #lag=lag/(lma-lmi)
  #colours <- gray(c(0,lag))
  colours <- lag
  colours[lag>180] <- "red"
  colours[!(lag>180)] <- "blue"

  xdata=xdata[1:len-1]
  ydata=ydata[1:len-1]
  
  # Plot the data
  plot(xdata,ydata,
       # Omit axes
       axes=FALSE,
       pch=20,
       main="Old Faithful Eruptions",
       xlab="Time till next eruption (min)",
       ylab="Duration (sec)",
       # Leave some space for the rug plot
       xlim=c(41,max(xdata)),
       ylim=c(70,max(ydata)),
       cex=0.5,
       col=colours)

  # Add the axes, passing in the summary to provide quartile and mean
  fancyaxis(1,summary(xdata))
  fancyaxis(2,summary(ydata))

  # This data is heavily rounded and there are lots of ties, so use
  #  jitter to show distribution. It is not ideal but will do for
  #  and example
  jx <- clippedjitter(xdata, amount=0.4)
  jy <- clippedjitter(ydata, amount=0.1)

  # Draw the rug for X
  minimalrug(jx, side=1, line=-0.7, tcl=0.3)
  # Draw the rug for Y
  minimalrug(jy, side=2, line=-0.7, tcl=0.3)

  #closedevice()
}

pairexample <- function(){
  opendevice()

  # Split the screen in two columns
  split.screen(c(1,2))
  
  # Left graph
  screen(1)
  stripchartexample()

  # Right graph
  screen(2)
  rugexample()

  # Tidy up
  close.screen(all = TRUE)
  closedevice()
}

multipleexample <- function() {
  opendevice()

  xdata <- faithful$waiting
  ydata <- faithful$eruptions*60

  # Split the data in 5 sections
  len=length(xdata)
  s=floor(len/5)

  # Split the screen in 5 columns
  par(las=1)
  split.screen(c(1,5))

  # Image 1
  screen(1)
  x=xdata[1:s]
  y=ydata[1:s]
  par(mar=c(5.1, 4.1, 4.1, 2.1))
  plot(x,y,
       # Omit axes
       axes=FALSE,
       pch=20,
       xlab="Time till next eruption (min)",
       # Normal Y label
       ylab="Duration (sec)",
       # Leave some space for the rug plot
       xlim=c(41,max(xdata)),
       ylim=c(70,max(ydata)),
       cex=0.5,
       col="black")
  fancyaxis(1,summary(x),digits=0,shiftfac=0.01, gapfac=0.01)
  fancyaxis(2,summary(y),digits=0,shiftfac=0.01, gapfac=0.01)
  axisstripchart(x, 1, sshift=0.4)
  axisstripchart(y, 2, sshift=0.4)

  # Image 2
  screen(2)
  x=xdata[(s+1):(2*s)]
  y=ydata[(s+1):(2*s)]
  par(mar=c(5.1, 4.1, 4.1, 2.1))
  plot(x,y,
       # Omit axes
       axes=FALSE,
       pch=20,
       xlab="Time till next eruption (min)",
       # Ommit Y label
       ylab="",
        # Leave some space for the rug plot
       xlim=c(41,max(xdata)),
       ylim=c(70,max(ydata)),
       cex=0.5,
       col="black")
  fancyaxis(1,summary(x),digits=0,shiftfac=0.01, gapfac=0.01)
  fancyaxis(2,summary(y),digits=0,shiftfac=0.01, gapfac=0.01)
  axisstripchart(x, 1, sshift=0.4)
  axisstripchart(y, 2, sshift=0.4)

  # Image 3
  screen(3)
  x=xdata[(2*s+1):(3*s)]
  y=ydata[(2*s+1):(3*s)]
  par(mar=c(5.1, 4.1, 4.1, 2.1))
  plot(x,y,
       # Omit axes
       axes=FALSE,
       pch=20,
       # Middle plot, so show title
       main="Old Faithful Eruptions (272 observations)",
       xlab="Time till next eruption (min)",
       # Ommit Y label
       ylab="",
        # Leave some space for the rug plot
       xlim=c(41,max(xdata)),
       ylim=c(70,max(ydata)),
       cex=0.5,
       col="black")
  fancyaxis(1,summary(x),digits=0,shiftfac=0.01, gapfac=0.01)
  fancyaxis(2,summary(y),digits=0,shiftfac=0.01, gapfac=0.01)
  axisstripchart(x, 1, sshift=0.4)
  axisstripchart(y, 2, sshift=0.4)

  # Image 4
  screen(4)
  x=xdata[(3*s+1):(len)]
  y=ydata[(3*s+1):(len)]
  par(mar=c(5.1, 4.1, 4.1, 2.1))
  plot(x,y,
       # Omit axes
       axes=FALSE,
       pch=20,
       xlab="Time till next eruption (min)",
       # Ommit Y label
       ylab="",
       # Leave some space for the rug plot
       xlim=c(41,max(xdata)),
       ylim=c(70,max(ydata)),
       cex=0.5,
       col="black")
  fancyaxis(1,summary(x),digits=0,shiftfac=0.01, gapfac=0.01)
  fancyaxis(2,summary(y),digits=0,shiftfac=0.01, gapfac=0.01)
  axisstripchart(x, 1, sshift=0.4)
  axisstripchart(y, 2, sshift=0.4)

  # Image 5
  screen(5)
  x=xdata[(4*s+1):(len)]
  y=ydata[(4*s+1):(len)]
  par(mar=c(5.1, 4.1, 4.1, 2.1))
  plot(x,y,
       # Omit axes
       axes=FALSE,
       pch=20,
       xlab="Time till next eruption (min)",
       # Ommit Y label
       ylab="",
       # Leave some space for the rug plot
       xlim=c(41,max(xdata)),
       ylim=c(70,max(ydata)),
       cex=0.5,
       col="black")
  fancyaxis(1,summary(x),digits=0,shiftfac=0.01, gapfac=0.01)
  fancyaxis(2,summary(y),digits=0,shiftfac=0.01, gapfac=0.01)
  axisstripchart(x, 1, sshift=0.4)
  axisstripchart(y, 2, sshift=0.4)

  # Cleanup
  close.screen(all = TRUE)
  closedevice()
}  
