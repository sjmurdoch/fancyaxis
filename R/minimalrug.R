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

# Draw a rug plot, but ommit the baseline (actually, draw over it
minimalrug <- function(x, lwd=0.7, tcl=0.3, ...) {
  # x:   a numeric vector
  # ...: parameters passed to rug()
  
  # Rounded ends don't work well with erasing one end
  oldlend <- par(lend = "butt")
  on.exit(par(oldlend))

  # Used for overwriting the axis line to leave tickmarks
  bg <- par("bg")
  if (bg == "transparent")
    bg <- "white"

  # Draw the rug
  rug(x, ticksize=NA, lwd=lwd, ...)
  # Acrobat shows "shadows" around a line erased with a line
  #  of similar width, so use a thicker line
  overlwd=1
  # Remove the baseline (... is put first to allow other the other
  #  parameters to override it)
  axis(..., at=x, col=bg, tcl=0, label=FALSE, lwd=overlwd)
}

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
