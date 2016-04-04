#addCompass-----------------------------2016-04-01
#  Add a compass rose to a map.
#-----------------------------------------------RH
addCompass <- function(X, Y, rot="magN", cex=1, 
   col.compass=c("gainsboro","blue","yellow","black"), ...)
{
	## Geomagnetic North Pole
	Npole = as.EventData(data.frame(
		EID = 2015:2020,
		X   = -c(72.6, 72.7, 72.8, 73.0, 73.1, 73.2),
		Y   = c(80.4, 80.4, 80.5, 80.5, 80.6, 80.6)),
		projection="LL")
	if (rot=="magN") {
		year = substring(Sys.Date(),1,4)
		Nmag = Npole[Npole$EID%in%year,]
		rot = -calcGCdist(X,Y,Nmag$X,Nmag$Y)$theta
	}
	oldcex  = par(cex=cex, no.readonly=TRUE); on.exit(par(oldcex))
	mheight = strheight("M")
	xylim   = par("usr")
	plotdim = par("pin")
	xmult   = (xylim[2]-xylim[1])/(xylim[4]-xylim[3])*plotdim[2]/plotdim[1]

	## Add circles
	clrs = rep(col.compass,4)[1:4]
	col.cir=clrs[1]; col.rot=clrs[2]; col.but=clrs[3]; col.pch=clrs[4]
	circles = list(); rads=c(3.5,3)
	for (r in rads) {
		rr = as.character(r)
		circle.angles = seq(0,2*pi,len=1440)
		cspans  = rep(mheight*r,length(circle.angles))
		xcircle = cos(circle.angles) * cspans * xmult + X
		ycircle = sin(circle.angles) * cspans + Y
		circles[[rr]] = data.frame(X=xcircle,Y=ycircle)
		lines(xcircle,ycircle,col=col.cir)
	}
	subcircs = lapply(circles,function(x,z){x[seq(1,nrow(x),z),]},z=40)
	bearings = cbind(subcircs[[1]],subcircs[[2]])
	apply(bearings,1,function(x){lines(x[c(1,3)],x[c(2,4)],col=col.cir)})

	## Add cross
	cross.angles = pi*c(0,.5,1,1.5)
	xspans  = rep(mheight*rads[1],length(cross.angles))
	xcross  = cos(cross.angles) * xspans * xmult + X
	ycross  = sin(cross.angles) * xspans + Y
	lines(xcross[c(1,3,5,2,4)],ycross[c(1,3,5,2,4)],col=col.cir)

	## Add user's rotation
	rotate.angles = cross.angles + pi*rot/180
	rspans  = rep(mheight*rads[1],length(rotate.angles))
	xrotate = cos(rotate.angles) * rspans * xmult + X
	yrotate = sin(rotate.angles) * rspans + Y
	lines(xrotate[c(1,3,5,2,4)],yrotate[c(1,3,5,2,4)],col=col.rot,lwd=2)
	points(mean(xcross),mean(ycross),pch=21,col=col.pch,bg=col.but)

	## Add NEWS labels
	txtxpoints = cos(rotate.angles) * 1.25 * rspans[1] * xmult + X
	txtypoints = sin(rotate.angles) * 1.25 * rspans[1] + Y
	text(txtxpoints,txtypoints,c("E","N","W","S"),...)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~addCompass


#calcGCdist-----------------------------2016-04-01
# Calculate great-circle distance.
# source: http://www.movable-type.co.uk/scripts/latlong.html
#
# Haversine formula:
# ------------------
#  a = sin^2((phi2-phi1)/2) + cos(phi1)cos(phi2)sin^2((lam2-lam1)/2)
#  c = 2*atan2(sqrt(a), sqrt(1-a))
#  d = R * c 
#
# where
#  phi = latitude (in radians)
#  lam = longitude (in radians)
#  R   = radius of the Earth
#  a   = square of half the chord length between the points
#  c   = angular distance in radians
#  d   = great-circle distance between two points
#
# Spherical Law of Cosines
# ------------------------
#  d = acos( sin(phi1)sin(phi2) + cos(phi1)cos(phi2)cos(lam2-lam1) ) * R
#
# Initial bearing (aka forward azimuth)
# ---------------
#  theta = atan2( sin(lam2-lam1)cos(ph12), cos(ph1)sin(ph2) - sin(phi1)cos(ph2)cos(lam2-lam1) )

#-----------------------------------------------RH

calcGCdist = function(lon1, lat1, lon2, lat2, R=6371.2)
{
	#if (length(lon1)>1 || length(lat1)>1)
	#	stop("Start coordinate must be a single point (multiple end points allowed)")

	## Transform degree coordinates to radians
	lam1 = lon1 * pi/180
	lam2 = lon2 * pi/180
	phi1 = lat1 * pi/180
	phi2 = lat2 * pi/180
	dellam = (lon2 - lon1) * pi/180
	delphi = (lat2 - lat1) * pi/180

	## Haversine formula
	a = sin(delphi/2) * sin(delphi/2) +
			cos(phi1) * cos(phi2) * sin(dellam/2) * sin(dellam/2)
	c = 2 * atan2(sqrt(a), sqrt(1-a))
	d = R * c
	
	## Spherical Law of Cosines
	d2 = acos(sin(phi1)*sin(phi2) + cos(phi1)*cos(phi2)*cos(dellam) ) * R
	
	## Initial bearing (aka forward azimuth)
	theta = atan2( sin(dellam)*cos(phi2), cos(phi1)*sin(phi2) - sin(phi1)*cos(phi2)*cos(dellam) )

	return(list(a=a,c=c,d=d,d2=d2,theta=theta*180/pi))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~calcGCdist
#.addAxis2------------------------------2013-03-13
# Modified from the function `.addAxis` to 
# add axes to any side of the plot. 
# Note: temporary until incorporation and testing.
#--------------------------------------------NB/RH
.addAxis2 <-
function (side=1:2, xlim, ylim, tckLab, tck, tckMinor, ...) 
{
	tckLab <- rep(tckLab, length.out = 4)
	tck <- rep(tck, length.out = 4)
	tckMinor <- rep(tckMinor, length.out = 4)
	mai <- par()$mai
	# Nick: The following calls to par() are very strange to me as 
	# repeated calls to `.addAxis` sequently reduced the size of cex
	#par(cex = par()$cex * 0.8)
	#par(mai = mai)
	#par(mex = par()$cex)
	lim <- list(xlim,ylim,xlim,ylim)
	rotate <- list(0,90,0,90)
	for (i in side) {
		if (((i %in% c(1,3)) && (par()$xaxt != "n")) || ((i %in% c(2,4)) && (par()$yaxt != "n"))) {
			ticks <- pretty(lim[[i]])
			ticksMinor <- pretty(c(0, diff(ticks)[1]))
			ticksMinor <- (sort(rep(ticks, length(ticksMinor))) + 
				rep(ticksMinor, length(ticks)))
			ticks <- ticks[ticks > lim[[i]][1] & ticks < lim[[i]][2]]
			ticksMinor <- ticksMinor[ticksMinor > lim[[i]][1] & 
				ticksMinor < lim[[i]][2]]
			if (!tckLab[i]) {
				tickLabels <- FALSE
			}
			else {
				tickLabels <- as.character(ticks)
			}
			axis(side = i, at = ticks, labels = tickLabels, tck = tck[i], srt = rotate[[i]], ...)
			axis(side = i, at = ticksMinor, labels = FALSE, tck = tckMinor[i], ...)
		}
	}
	invisible(NULL)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.addAxis2