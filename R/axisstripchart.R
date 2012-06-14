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

# Draw a stripchart on an axis, showing marginal frequency
# TODO: Does not handle log axes well
axisstripchart <- function(x, side, sshift=0.3) {
  # x:    the data from which the plots are to be produced.
  # side: as in axis()
  
  # Find out the properties of the side we are doing
  parside <-
    if (side==1){
      # Does the outside of the plot have larger or smaller vales
      flip <- 1
      # Are we on the yaxis
      yaxis <- FALSE
      # Relevant index of par("usr")
      3
    }
    else if (side==2) {
      flip <- 1
      yaxis <- TRUE
      1
    }
    else if (side==3) {
      flip <- -1
      yaxis <- FALSE
      4
    }
    else if (side==4) {
      flip <- -1
      yaxis <- TRUE
      2
    }

  # Axis position
  base<-par("usr")[parside]
  
  # Width and height in user units
  plotwidth <- diff(par("usr")[1:2])
  plotheight <- diff(par("usr")[3:4])
  
  # Shift for the q2 and q3 axis from the base (in inches)
  shift <- par("pin")[1]*0.003*flip
  # Gap for the median
  gap <- par("pin")[1]*0.003
  # Shift for the mean pointer away from the axis
  meanshift <- par("cin")[1]*0.5*flip
  # Shift away from the q2 and q3 axis for the stripchart
  stripshift <- par("cin")[1]*sshift*flip
  
  # Scale lengths so both axes are equal on output device
  if (yaxis) {
    shift <- shift/par("pin")[1]*plotwidth
    meanshift <- meanshift/par("pin")[1]*plotwidth
    stripshift <- stripshift/par("pin")[1]*plotwidth
    gap <- gap/par("pin")[2]*plotheight
  } else {
    shift <- shift/par("pin")[2]*plotheight
    meanshift <- meanshift/par("pin")[2]*plotheight
    stripshift <- stripshift/par("pin")[2]*plotheight
    gap <- gap/par("pin")[1]*plotwidth
  }

  # If vertical, stripchart assumes offset is a factor of character
  # width, if horizontal, character height (bug?). So correct for this
  if (yaxis)
    offset=flip*par("cin")[2]/par("cin")[1]
  else
    offset=flip

  # Don't clip the chart
  oldxpd <- par(xpd = TRUE)
  on.exit(par(oldxpd))
  
  stripchart(x, method="stack", vertical=yaxis, offset=offset, pch=15,
             cex=0.2, add=TRUE, at=base+shift+stripshift, col="red")
}