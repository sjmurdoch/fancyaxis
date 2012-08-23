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

# Add a small amount of noise to a numeric vector, preserving minimum
# and minimum
clippedjitter <- function(x, ...) {
  # x:   numeric to which jitter should be added.
  # ...: parameters passed to jitter()
  
  mi <- min(x)
  ma <- max(x)

  len=length(x)

  # Find a position for the min and max
  mipos <- ((1:len)[x==mi])[1]
  mapos <- ((1:len)[x==ma])[1]
  
  # The standard jittered data
  xj <- jitter(x, ...)

  # Find the elements which are outside the limits
  under <- xj<mi
  over <- xj>ma

  # Find the distance away from the limit
  dunder <- mi-xj[under]
  dover <- xj[over]-ma

  # Reflect over the limit
  repunder <- dunder+mi
  repover <- ma-dover

  # Replace out of limit values with the reflected ones
  xj[under] <- repunder
  xj[over] <- repover
  
  # Replace a jittered min/max with the original min/max
  xj[mipos] <- mi
  xj[mapos] <- ma
  
  # Return updated array
  xj
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
