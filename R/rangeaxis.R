##     Copyright (C) 2005 Steven J. Murdoch <http://www.cl.cam.ac.uk/users/sjm217/>
##     Permissions at bottom.
##
##     This is very much a work in progress and still of alpha
##     quality. See the example file for usage. It currently does not
##     deal with logarithmic scales properly and needs manual tweaking
##     of several values to suit different data and output device
##     resolution. Comments and suggestions are appreciated. My
##     contact details can be found here:
##       http://www.cl.cam.ac.uk/users/sjm217/#contact
##
##     The design of the graph is based on a scatterplot presented in
##     "The Visual Display of Quantitative Information", Edward Tufte.
##     Thanks to Paul Murrell for assistance with handling the log axes
##
##     From $Id: fancyaxis.R 6927 2009-03-08 12:49:17Z sjm217 $

# 
# draw a range axis which indicates minimum and maximum
#
rangeaxis <- function(side, summ, at=NULL, mingap=0.5, digits=2) {
  # side: like axis()
  # summ: a summary object, for example returned by summary()
  # mingap: the smallest gap permitted between two tickmarks,
  #         expressed as a fraction of the default tickmark gap
  # digits: the number of digits to round minimum and maximum to
  
  # TODO:
  # Deal with case where length(axTicks)<2
  # Deal with logarithmic axis case properly, as axTicks difference
  #  is not uniform.
  
  # Get summary information
  amin <- summ[1]
  aq1 <- summ[2]
  amed <- summ[3]
  amean <- summ[4]
  aq3 <- summ[5]
  amax <- summ[6]

  # Find out the properties of the side we are doing
  parside <-
    if (side==1){
      # Does the outside of the plot have larger or smaller vales
      flip <- 1
      # Are we on the xaxis
      xaxis <- TRUE
      # Is this axis logarithmic
      islog <- par("xlog")
      # Is the other axis logarithmic
      otherlog <- par("ylog")
      # Relevant index of par("usr")
      3
    }
    else if (side==2) {
      flip <- 1
      xaxis <- FALSE
      islog <- par("ylog")
      otherlog <- par("xlog")
      1
    }
    else if (side==3) {
      flip <- -1
      xaxis <- TRUE
      islog <- par("xlog")
      otherlog <- par("ylog")
      4
    }
    else if (side==4) {
      flip <- -1
      xaxis <- FALSE
      islog <- par("ylog")
      otherlog <- par("xlog")
      2
    }

  # Calculate default positions of ticks
  if (is.null(at))
    ticks <- axTicks(side)
  else
    ticks <- at

  # Remove any ticks outside the range
  ticks <- ticks[(ticks>=amin) & (ticks<=amax)]
  
  # Calculate the minimum desired gap between ticks
  numticks <- length(ticks)
  if (islog)
    axgap <- (log10(ticks[numticks])-log10(ticks[numticks-1]))*mingap
  else
    axgap <- (ticks[numticks]-ticks[numticks-1])*mingap

  # Get new range of tickmarks
  numticks <- length(ticks)
  firsttick <- ticks[1]
  lasttick <- ticks[numticks]
  
  # If max tick will be too close to the last tick, replace it,
  #  otherwise append it
  if (islog && (log10(amax) - log10(lasttick) < axgap)) {
    ticks[numticks]<-amax
  } else if (amax - lasttick < axgap) {
    ticks[numticks]<-amax	
  } else {	
    ticks<-c(ticks,amax)
  }
  
  # Similarly for first tick
  if (islog && (abs(log10(amin)-log10(firsttick)) < axgap)) {
    ticks[1]<-amin	
  } else if (firsttick - amin < axgap) {
    ticks[1]<-amin	
  } else {	
    ticks<-c(amin, ticks)
  }

  # Format the labels. min and max should have as many
  #  trailing zeros they were rounded to, the others
  #  should have the minimum needed to represent the tick marks
  numticks <- length(ticks)

  # Min and max
  lmin <- format(round(ticks[1], digits), nsmall=digits, trim=TRUE)
  lmax <- format(round(ticks[numticks], digits), nsmall=digits, trim=TRUE)

  # The others
  middle <- format(ticks[2:(numticks-1)], trim=TRUE)

  # Combine them
  labels <- c(lmin,middle,lmax)

  # Draw the axis
  oldlend <- par(lend = "butt")
  on.exit(par(oldlend))

  # Used for overwriting the axis line to leave tickmarks
  bg <- par("bg")
  if (bg == "transparent")
    bg <- "white"

  lwd=0.7
  # Draw the axis and tickmarks in gray
  axis(side, ticks, labels=FALSE, col="gray50", lwd=lwd)
  # Draw the axis line in black
  overlwd=1
  axis(side, ticks, labels=FALSE, col='black', tcl = 0, lwd=overlwd)
  
  # Draw the labels
  axis(side, ticks, labels=labels, tick=FALSE)
  
}

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