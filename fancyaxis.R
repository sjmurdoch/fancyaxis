fancyaxis <- function(side, summ, mingap=0.5, digits=2) {

  # Get summary information
  amin<-summ[1]
  aq1<-summ[2]
  amed<-summ[3]
  amean<-summ[4]
  aq3<-summ[5]
  amax<-summ[6]

  # Calculate default positions of ticks
  ticks<-axTicks(side)

  # Calculate the minimum desired gap between ticks
  numticks<-length(ticks)
  axgap<-(ticks[numticks]-ticks[numticks-1])*mingap

  # Trim of any ticks that are outside the range
  tmax<-round(amax,digits)
  tmin<-round(amin,2)
  ticks<-ticks[ticks<=tmax]
  ticks<-ticks[ticks>=tmin]

  # Get new range of tickmarks
  numticks<-length(ticks)
  firsttick<-ticks[1]
  lasttick<-ticks[numticks]
                                        # Check len=0 and len=1 cases
                                        # Deal with logarithmic case
  
  # If max tick will be too close to the last tick, replace it,
  #  otherwise append it
  if (tmax - lasttick < axgap) {
    ticks[numticks]<-tmax	
  } else {	
    ticks<-c(ticks,tmax)
  }
  
  # Similarly for first tick
  if (abs(tmin-firsttick) < axgap) {
    ticks[1]<-tmin	
  } else {	
    ticks<-c(tmin, ticks)
  }

  # Format the labels. min and max should have as many
  #  trailing zeros they were rounded to, the others
  #  should have the minimum needed to represent the tick marks
  numticks<-length(ticks)

  # Min and max
  lmin=format(ticks[1], nsmall=digits, trim=TRUE)
  lmax=format(ticks[numticks], nsmall=digits, trim=TRUE)

  # The others
  middle=format(ticks[2:(numticks-1)], trim=TRUE)

  # Combine them
  labels<-c(lmin,middle,lmax)

  # Draw the axis
  par("lend"="square")

  bg <- par("bg")
  if (bg == "transparent")
    bg <- "white"
  
  axis(side, ticks, labels=FALSE, col="gray50", lwd=0.05)
  axis(side, ticks, labels=FALSE, col=bg, tcl = 0, lwd=0.2)
  axis(side, ticks, labels=labels, tick=FALSE, tcl = 0, lwd=0.2)

  # Find out the properties of the side we are doing
  parside <-
    if (side==1){
      # Does the outside of the plot have larger or smaller vales
      flip <- 1
      # Are we on the xaxis
      xaxis <- TRUE
      # Relevant index of par("usr")
      3
    }
    else if (side==2) {
      flip <- 1
      xaxis <- FALSE
      1
    }
    else if (side==3) {
      flip <- -1
      xaxis <- TRUE
      4
    }
    else if (side==4) {
      flip <- -1
      xaxis <- FALSE
      2
    }

  # Axis position
  base<-par("usr")[parside]

  # Width and height in user units
  plotwidth=diff(par("usr")[1:2])
  plotheight=diff(par("usr")[3:4])

  # Shift for the q2 and q3 axis from the base (in inches)
  shift <- par("pin")[1]*0.005*flip
  # Gap for the median
  gap <- par("pin")[1]*0.005
  # Shift for the mean pointer away from the axis
  meanshift <- par("cin")[1]*0.5*flip

  # Scale lengths so both axes are equal on output device
  if (xaxis==0) {
    shift <- shift/par("pin")[1]*plotwidth
    meanshift <- meanshift/par("pin")[1]*plotwidth
    gap <- gap/par("pin")[2]*plotheight
  } else {
    shift <- shift/par("pin")[2]*plotheight
    meanshift <- meanshift/par("pin")[2]*plotheight
    gap <- gap/par("pin")[1]*plotwidth
  }

  # Position of q2 and q3 axis segments
  offset <- base+shift

  oldlend <- par(lend = "butt")
  on.exit(par(oldlend))
  
  # Draw q1 and q4 axis segments
  if (xaxis==0) {
    #     xs,         ys,          Don't clip, Line width, Don't overlap 
    lines(rep(base,2),c(amin,aq1), xpd=TRUE, lwd=0.2)
    lines(rep(base,2),c(aq3,amax), xpd=TRUE, lwd=0.2)
  } else {
    lines(c(amin,aq1),rep(base,2), xpd=TRUE, lwd=0.2)
    lines(c(aq3,amax),rep(base,2), xpd=TRUE, lwd=0.2)
    
  }

  # Draw q2 and q3 axis segments
  if (xaxis==0) {
    lines(rep(offset,2),c(aq1,amed-gap), xpd=TRUE, lwd=0.2)
    lines(rep(offset,2),c(amed+gap,aq3), xpd=TRUE, lwd=0.2)
  } else {
    lines(c(aq1,amed-gap),rep(offset,2), xpd=TRUE, lwd=0.2)
    lines(c(amed+gap,aq3),rep(offset,2), xpd=TRUE, lwd=0.2)
  }

  # Which segment is the mean in?
  if((amean>aq3) || (amean<aq1)) {
    # Mean is in q1/q4, so move relative to base
    meanbase=base-meanshift
  } else {
    # Mean is in q2/q3, so move relative to shifted base
    meanbase=offset-meanshift
  }

  # Draw the mean
  if (xaxis==0) {
    points(meanbase, amean, pch=18, cex=0.3, col="red", xpd=TRUE)
  } else {
    points(amean, meanbase, pch=18, cex=0.3, col="red", xpd=TRUE)
  }
}

par("bg"="#fffff0")
#par("bg"="transparent")
#png(file="/tmp/faithful.png",width="480",height="320")
#postscript(file="/tmp/foo.pdf",paper="A4",bg=par("bg"))
#xdata=iris$Petal.Width
#ydata=iris$Petal.Length
#xdata=cars$speed
#ydata=cars$dist
xdata=faithful$waiting
ydata=faithful$eruptions*60
colours=(1:length(xdata))/length(xdata)*0.75
par(las=1)
plot(xdata,ydata,axes=FALSE,pch=20,main="Old Faithful Eruptions",
     xlab="Time till next eruption (min)",
     ylab="Eruption duration (sec)",
     xlim=c(40,max(xdata)),
     ylim=c(94,max(ydata)),
     cex=0.5,
     col=gray(colours))
fancyaxis(1,summary(xdata))
fancyaxis(2,summary(ydata))
jx=jitter(xdata,amount=0.4)
jy=jitter(ydata,amount=0.1)

rug(jx,side=1,line=-0.7,tcl=0.3)
axis(side=1,at=jx, col=par("bg"), tcl=0, line=-0.7, label=FALSE)
rug(jy,side=2,line=-0.7,tcl=0.3)
axis(side=2,at=jy, col=par("bg"), tcl=0, line=-0.7, label=FALSE)
#dev.off()
