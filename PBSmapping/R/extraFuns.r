##================================================
## Copyright (C) 2003-2018  Fisheries & Oceans Canada
## Nanaimo, British Columbia
## This file is part of PBS Mapping.
##================================================

##------------------------------------------------
## Extra functionality added by RH & DC after 
## NB & JTS moved on to bigger and better things.
## Functions:
## =========
##  addBubbles...........Draw bubbles of radius proportional to the z variable
##  addCompass...........Add a compass rose to a map
##  calcGCdist...........Calculate great-circle distance
##  makeTopography.......Make topography data suitable for contourLines()
##  placeHoles...........Place holes under correct solids
##  rotateEvents.........Rotate EventData clockwise around a central point
##  rotatePolys..........Rotate PolySet clockwise around a central point
##------------------------------------------------


## addBubbles---------------------------2010-01-07
##  Takes EventData and optional arguments to draw
##  bubbles of radius proportional to the z variable.
##
##  Modified (and for the legend, strongly inspired) from:
##    S. Tanimura, C. Kuroiwa, and T. Mizota. Proportional symbol
##    mapping in R. Journal of Statistical Software, 15(5):1-7,
##    Jan. 2006. [http://www.jstatsoft.org]
##
##  Modifications by Denis Chabot allow it to work with PBSmapping,
##  add one type of bubble (z proportional to volume of bubble)
##  and make it possible to draw several maps with bubbles that all
##  have the same radii (instead of each bubble plot having a radii
##  that depends on the max z value for that plot). Can add a
##  legend in one of 4 corners or at a specific x-y positiion.
## ------------------------------------------DC/RH
addBubbles <- function(events, type = c("perceptual", "surface", "volume"),
   z.max = NULL, min.size = 0, max.size = 0.8, symbol.zero = "+",
   symbol.fg = rgb(0,0,0,0.60), symbol.bg = rgb(0,0,0,0.30),
   legend.pos = "bottomleft", legend.breaks = NULL,
   show.actual = FALSE,
   legend.type = c("nested", "horiz", "vert"),
   legend.title = "Abundance", legend.cex = .8, ...)
{
  ## validate events
  events <- .validateEventData(events)
  if (is.character(events))
    stop(paste("Invalid EventData 'events'.\n", events, sep=""));
  if (!is.element("Z", names(events)))
    stop ("EventData is missing required column 'Z'.\n");

  ## check arguments before we get too far
  type <- match.arg(type)
  if (!is.null(legend.pos))
    legend.type <- match.arg(legend.type)

  ## set z.max if necessary
  if (is.null(z.max) || is.na(z.max))
    z.max <- max(events$Z, na.rm=TRUE)

  ## adjust legend breaks if necessary
  if (is.null(legend.breaks) || is.na(legend.breaks))
    legend.breaks <- pretty(range(events$Z), 3)[-1]
  else if (is.vector(legend.breaks) && length(legend.breaks) == 1)
    legend.breaks <- pretty(range(events$Z), legend.breaks)[-1]

  if (show.actual)
    legend.breaks <- signif(legend.breaks / max(legend.breaks)
                            * max(events$Z, na.rm=TRUE), 3)

  ## determine x/y range of plotting region
  usr.xdiff <- par("usr")[2] - par("usr")[1]
  usr.ydiff <- par("usr")[4] - par("usr")[3]

  ## for sizing in inches, it's important to use the X rather than Y axis
  ##
  ## max.size is diameter (inches); /2 for radius; /par()$pin[1] (inches)
  ## for fraction of width (inches); *usr.xdiff to convert to width to
  ## user coordinates
  ##
  ## min.size is diameter (inches)
  stand.rad <- (max.size / 2) / par("pin")[1] * usr.xdiff
  stand.rad.min <- (min.size / 2) / par("pin")[1] * usr.xdiff
  
  ## sorting from large to small ensures that small bubbles will not be hidden
  ## behind large bubbles
  events <- events[order(events$Z, decreasing=TRUE), ]
 
  ## determine the size of each circle/legend circle based on the selected type
  type <- match.arg(type)
  switch(type,
         volume = {
           radii     <- stand.rad.min + ((events$Z      / z.max)^(1/3)) * (stand.rad - stand.rad.min)
           radii.leg <- stand.rad.min + ((legend.breaks / z.max)^(1/3)) * (stand.rad - stand.rad.min)
         },
         surface = {
           radii     <- stand.rad.min + sqrt(events$Z      / z.max) * (stand.rad - stand.rad.min)
           radii.leg <- stand.rad.min + sqrt(legend.breaks / z.max) * (stand.rad - stand.rad.min)
         },
         perceptual = {
           # default (if type unspecified)
           radii     <- stand.rad.min + ((events$Z      / z.max)^0.57) * (stand.rad - stand.rad.min)
           radii.leg <- stand.rad.min + ((legend.breaks / z.max)^0.57) * (stand.rad - stand.rad.min)
         }
         )

  ## handle multiple colours
  if (is.vector (symbol.bg) && length(symbol.bg) > 1)
    getColour <- colorRamp (symbol.bg)
  else
    ## ensure that the function returns X colours when called with X values
    getColour <- function(x) { t(col2rgb(symbol.bg, alpha=TRUE))[rep(1,length(x)), ] }

  ## obtain colours for the background (as a matrix)
  bgs <- getColour((events$Z - min(legend.breaks)) / (max(legend.breaks) - min(legend.breaks)))
  if (ncol(bgs) == 3)
    bgs <- cbind(bgs, 255) # add the alpha channel if necessary
  ## ... now deal with Z values outside of the range
  if (is.vector (symbol.bg) && length(symbol.bg) > 1)
    {
      outside <- events$Z < min(legend.breaks) | events$Z > max(legend.breaks)
      if (sum(outside) > 0)
        {
          bgs[outside,] <- matrix(c(255,255,255,0), ncol=4)[rep(1,sum(outside)), ]
          warning(sum(outside),
                  " events were outside the legend range and were plotted with",
                  " transparent interiors.  Consider using the addBubbles",
                  " arguments 'legend.breaks', 'min.size', and 'symbol.zero'",
                  " to improve the output.")
        }
    }
  
  ## obtain colours for the legend (as a matrix)
  bgs.leg <- getColour((legend.breaks - min(legend.breaks)) / (max(legend.breaks) - min(legend.breaks)))
  if (ncol(bgs.leg) == 3)
    bgs.leg <- cbind(bgs.leg, 255) # add the alpha channel if necessary
  
  ## convert the matrices to hex values (#RRGGBBAA)
  bgs <- rgb(bgs[,1], bgs[,2], bgs[,3], bgs[,4], maxColorValue=255)
  bgs.leg <- rgb(bgs.leg[,1], bgs.leg[,2], bgs.leg[,3], bgs.leg[,4], maxColorValue=255)

  ## compare events$Z to 0; cannot simply use "== 0" given floating-point type
  isZero <- unlist(lapply(events$Z, all.equal, current = 0)) == "TRUE"

  ## plot the circles (data with non-zero radii)
  symbols(events$X[!isZero], events$Y[!isZero], circles = radii[!isZero],
          inches = FALSE, bg = bgs[!isZero], fg = symbol.fg, add = TRUE)

  ## plot the zero symbol for points (where necessary)
  if (any(isZero) && (!is.logical(symbol.zero) || symbol.zero)) {
    if (is.logical(symbol.zero))
      symbol.zero <- "+" # set to default

    dots <- list(...);
    if (!is.null(dots$pch))
      stop("Specify 'pch' through 'symbol.zero'")

    points(events$X[isZero], events$Y[isZero], pch = symbol.zero, ...)
  }

  ## plot the legend if there's a position specified for it
  if (!is.null(legend.pos)) {
    # only plot zero symbol if used
    if (!any(isZero))
      symbol.zero <- FALSE;
    .addBubblesLegend (radii.leg, usr.xdiff, usr.ydiff, symbol.zero, symbol.fg,
                       bgs.leg, legend.pos, legend.breaks, legend.type,
                       legend.title, legend.cex, ...)
  }
  invisible()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~addBubbles


## addCompass---------------------------2016-04-01
##  Add a compass rose to a map.
## ---------------------------------------------RH
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


## calcGCdist---------------------------2016-04-01
##  Calculate great-circle distance.
##  source: http://www.movable-type.co.uk/scripts/latlong.html
##
## Haversine formula:
## ------------------
##  a = sin^2((phi2-phi1)/2) + cos(phi1)cos(phi2)sin^2((lam2-lam1)/2)
##  c = 2*atan2(sqrt(a), sqrt(1-a))
##  d = R * c 
##
##  where
##   phi = latitude (in radians)
##   lam = longitude (in radians)
##   R   = radius of the Earth
##   a   = square of half the chord length between the points
##   c   = angular distance in radians
##   d   = great-circle distance between two points
##
## Spherical Law of Cosines
## ------------------------
##  d = acos( sin(phi1)sin(phi2) + cos(phi1)cos(phi2)cos(lam2-lam1) ) * R
##
## Initial bearing (aka forward azimuth)
## ---------------
##  theta = atan2( sin(lam2-lam1)cos(ph12), cos(ph1)sin(ph2) - sin(phi1)cos(ph2)cos(lam2-lam1) )
##
## ---------------------------------------------RH
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
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~calcGCdist


## makeTopography-----------------------2009-07-30
## Make topography data suitable for contourLines().
## ---------------------------------------------RH
makeTopography <- function (dat, digits=2, func=NULL) {
	if (!is.data.frame(dat) || length(dat) < 3) {
		stop("'dat' must be a data frame with at least three columns.\n") }
	N   = nrow(dat)
	X   = round(dat[[1]],digits); Y  = round(dat[[2]],digits)
	ID  = complex(length.out=N,real=X,imaginary=Y)
	tmp = split(dat[[3]], ID)
	if (is.null(func)) func = function(x){mean(x,na.rm=TRUE)} # mean value
	z1  = unlist(sapply(tmp,func,simplify=FALSE))
	id  = as.complex(names(z1))
	x1  = Re(id); y1 = Im(id)
	x   = sort(unique(x1)); y = sort(unique(y1))
	nr  = length(x); nc = length(y); nz=nr*nc
	z   = matrix(NA,nrow=nr,ncol=nc,dimnames=list(x,y))
	for (i in x) { # populate matrix row by row
		xx=as.character(i); zx=is.element(x1,i)
		yy=as.character(y1[zx])
		zz=z1[zx]; z[xx,yy]=zz  }
	result = list()
	result$x = x; result$y = y; result$z = z
	return(result)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~makeTopography


## placeHoles---------------------------2018-09-10
##  Place holes under correct solids.
##  Store orphaned holes as an attribute.
## ---------------------------------------------RH
placeHoles = function(polyset, minVerts=3, 
   orient=FALSE, show.progress=FALSE, unique.pid=FALSE)
{
	## initialization
	newpoly = oddpoly = NULL

	## Re-create PBStools' function (2011-09-30) -- flush the cat down the console
	.flush.cat = function(...) { cat(...); flush.console(); invisible() }

	## Remove small polygons before starting
	ipoly = polyset
	ipoly$idxo = .createIDs(ipoly, cols=c("PID", "SID"))
	nvert = table(ipoly$idxo)
	keep  = names(nvert)[nvert >= minVerts]
	ipoly = ipoly[is.element(ipoly$idxo,keep),]
	ipoly$idx = .createIDs(ipoly, cols=c("PID", "SID"));
	
	## May need to calculate orientation if object comes from shapefile
	if (orient) {
		orien = .calcOrientation (ipoly)
		orien$idx = .createIDs(orien, cols=c("PID", "SID"));
		holes = is.element(orien$orientation,-1)
		names(holes) = orien$idx
	} else {
		## Use code in '.validateData'
		idxFirst = which(!duplicated(ipoly$idx));
		idxLast  = c((idxFirst-1)[-1], length(ipoly$idx));
		holes    = (ipoly$POS[idxFirst] > ipoly$POS[idxLast])
		names(holes) = unique(ipoly$idx)
		idxOuter = rep(!holes, times=((idxLast-idxFirst)+1))
		idxInner = !idxOuter;
		sidx = unique(ipoly$idx[idxOuter])
		hidx = unique(ipoly$idx[idxInner])
	}
	hidx = as.numeric(names(holes)[holes])
	sidx = as.numeric(names(holes)[!holes])
	## Basically re-make the PIDs to avoid duplication later
	if(unique.pid) {
		uPID = 1:length(sidx); names(uPID) = sidx
	}

	if (show.progress){
		.flush.cat("There are ", sum(!holes), " outer contours with ", sum(holes), " holes after removing polygons with <", minVerts, " vertices.\n",
			"   Function 'placeHoles' will be slow if the number of holes|contours is high...\n\n", sep="")
		if (sum(holes)>0)
			.flush.cat("Number of holes left to place:\n")
		ndump = 0
	}

	for (s in sidx) { ## solids (outer contours)
		ss        = as.character(s)
		if (unique.pid) {
			sPID   = uPID[ss]
			spoly$PID = sPID  ## assign new PID
		}
		orphans   = adopted = NULL
		spoly     = ipoly[is.element(ipoly$idx,s),]
		newpoly   = rbind(newpoly, spoly)
		if (length(hidx)==0) next
		sxr = range(spoly$X)
		syr = range(spoly$Y)

		for (h in hidx) { ## holes (inner contours)
			hpoly = ipoly[is.element(ipoly$idx,h),]
			hxr = range(hpoly$X)
			hyr = range(hpoly$Y)
			## Save time by skipping holes that fall ouside the solid:
			if (all(hxr<sxr) | all(hxr>sxr) | all(hyr<syr) | all(hyr>syr) ){
				orphans = c(orphans,h)
			} else{
				io = .is.in(hpoly,spoly)
#if(h=="3.043") {browser();return()}
				if (io$all.in && !io$all.bdry) adopted = c(adopted, h)
				else orphans = c(orphans,h)
			}
		} ## end h (hidx) loop
		
		if (show.progress){
			ndump = ndump + 1
			.flush.cat(length(orphans),ifelse(ndump%%10==0 || length(orphans)==0 || s==rev(sidx)[1],"\n",", "),sep="")
		}
		if (!is.null(adopted)) {
			apoly     = ipoly[is.element(ipoly$idx,adopted),]
			if (unique.pid) aPID = sPID
			else            aPID = unique(spoly$PID)
			apoly$PID = aPID ## re-assign holes to new solids if necessary
			maxSID    = max(newpoly$SID[is.element(newpoly$PID,aPID)])
			nSID      = rle(apoly$SID)$lengths
			apoly$SID = rep((1:length(nSID)) + maxSID, nSID)  ## re-number sequentially from largest SID in PID
			newpoly   = rbind(newpoly, apoly)
#browser();return()
		}
		if (!is.null(orphans) && s==rev(sidx)[1])
			oddpoly = rbind(oddpoly, ipoly[is.element(ipoly$idx,orphans),])
		hidx = orphans
	} ## end s (sidx) loop

	Natts = names(attributes(polyset))
	natts = names(attributes(newpoly))
	Datts = setdiff(Natts,natts)
	if (length(Datts)>0){
		Aatts = attributes(polyset)[Datts]
		attributes(newpoly) = c(attributes(newpoly),Aatts)
	}
	if (!is.null(oddpoly)) {
		attr(newpoly,"orphans") = oddpoly
		if (show.progress){
			.flush.cat("Orphaned hole polygons PID.SID:\n--> ", paste0(sort(unique(oddpoly$idx)),collapse=", "), "\n\n", sep="")
		}
	}
	return(newpoly)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~placeHoles


## rotateEvents--------------------------2018-11-15
## Rotate EventData clockwise around a central point.
## ---------------------------------------------RH
rotateEvents = function(data, angle=40, centroid=c(500,5700),
   proj.out, zone, plot=FALSE, keep.extra=FALSE, ...)
{
	data = .validateEventData(data)
	if (is.null(angle)||is.na(angle)||!is.numeric(angle))
		stop("Supply a numeric angle for rotation (clockwise)")
	proj = attributes(data)$projection
	if (missing(proj.out))
		proj.out = proj
	if (!missing(zone) && is.numeric(zone))
		attr(data, "zone") = zone

	keep = data[,c("X","Y")]
	names(keep)[names(keep) %in% c("X","Y")] = c("X0","Y0")
	if (proj=="LL") {
		data = convUL(data)
		keep = cbind(keep, data[,c("X","Y")])
		names(keep)[names(keep) %in% c("X","Y")] = c("uX0","uY0")
	}
	zone = attributes(data)$zone
	if (is.null(zone)||is.na(zone))
		zone = 9
	if (is.null(centroid)||is.na(centroid))
		centroid = c(mean(range(data$X,na.rm=TRUE)), mean(range(data$Y,na.rm=TRUE)))

	radian =  pi * (-angle)/180   ## rotate clockwise
	R      = matrix(c(cos(radian),-sin(radian),sin(radian),cos(radian)), nrow=2, byrow=TRUE)

	data$X = data$X - centroid[1]
	data$Y = data$Y - centroid[2]
	keep   = cbind(keep, data[,c("X","Y")])
	names(keep)[names(keep) %in% c("X","Y")] = c("aX","aY") ## adjusted

	XYrot  = t(R %*% t(data[,c("X","Y")]))
	colnames(XYrot) = c("X","Y")
	keep   = cbind(keep, XYrot)
	names(keep)[names(keep) %in% c("X","Y")] = c("tX","tY") ## transformed

	data$X = XYrot[,"X"] + centroid[1]
	data$Y = XYrot[,"Y"] + centroid[2]
	keep   = cbind(keep, data[,c("X","Y")])
	names(keep)[names(keep) %in% c("X","Y")] = c("rX","rY") ## rotated

	if (proj=="LL" & proj.out=="LL"){
		data = convUL(data)
	}
	projection = attributes(data)$projection ## collecting proj and zone for rotation redundant but just in case

	if (keep.extra) {
		atts = attributes(data)
		atts.keep = atts[setdiff(names(atts),c("names","row.names"))]
		data = cbind(data,keep)
		atts.poly = attributes(data)
		attributes(data) = c(atts.poly[setdiff(names(atts.poly),names(atts.keep))], atts.keep)
	}
	if (plot){
		xlim = extendrange(data$X,f=0.5); ylim = extendrange(data$Y,f=0.1)
		plotPoints(data, pch=20, col="red", projection=projection, xlim=xlim, ylim=ylim, ...) ## just for testing
	}
	rotation = list()
	collect  = c("angle","radian","centroid","R","projection","zone")
	for (i in collect)
		rotation[[i]] = get(i)
	attr(data, "rotation") = rotation
	invisible(return(data))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~rotateEvents


## rotatePolys--------------------------2018-11-15
## Rotate PolySet clockwise around a central point.
## ---------------------------------------------RH
rotatePolys = function(polys, angle=40, centroid=c(500,5700),
   proj.out, zone, xlim=c(-135,-121.5), ylim=c(47,56), plot=FALSE, keep.extra=FALSE, ...)
{
	polys  = .validatePolySet(polys)
	if (is.null(angle)||is.na(angle)||!is.numeric(angle))
		stop("Supply a numeric angle for rotation (clockwise)")
	proj   = attributes(polys)$projection
	if (missing(proj.out))
		proj.out = proj
	if (!missing(zone) && is.numeric(zone))
		attr(polys, "zone") = zone

	xylim = list()
	if (is.null(xlim)||is.na(xlim))
		xlim = quantile(seq(min(polys$X),max(polys$X),len=1000),c(0.25,0.75))
	if (is.null(ylim)||is.na(ylim))
		ylim = quantile(seq(min(polys$Y),max(polys$Y),len=1000),c(0.25,0.75))
	xybox   = as.PolySet(data.frame(
		PID  = rep(1,4), POS=1:4, X = xlim[c(1,1,2,2)], Y = ylim[c(1,2,2,1)] ), projection=proj)
	xylim[[proj]] = list(xlim=xlim, ylim=ylim, xybox=xybox)

	keep    = polys[,c("X","Y")]
	names(keep)[names(keep) %in% c("X","Y")] = c("X0","Y0")
	if (proj=="LL") {
		polys = convUL(polys)
		keep  = cbind(keep, polys[,c("X","Y")])
		names(keep)[names(keep) %in% c("X","Y")] = c("uX0","uY0")
		attr(xybox, "zone") = attributes(polys)$zone
		xybox = convUL(xybox)
		xylim[[attributes(xybox)$projection]] = list(xlim=range(xybox$X), ylim=range(xybox$Y), xybox=xybox)
	}
	zone = attributes(polys)$zone
	if (is.null(zone)||is.na(zone))
		zone = 9
	if (is.null(centroid)||is.na(centroid))
		centroid = c(mean(range(polys$X,na.rm=TRUE)), mean(range(polys$Y,na.rm=TRUE)))

	radian  =  pi * (-angle)/180   ## rotate clockwise
	R       = matrix(c(cos(radian),-sin(radian),sin(radian),cos(radian)), nrow=2, byrow=TRUE)

	polys$X = polys$X - centroid[1]
	polys$Y = polys$Y - centroid[2]
	keep    = cbind(keep, polys[,c("X","Y")])
	names(keep)[names(keep) %in% c("X","Y")] = c("aX","aY") ## adjusted

	XYrot   = t(R %*% t(polys[,c("X","Y")]))
	colnames(XYrot) = c("X","Y")
	keep    = cbind(keep, XYrot)
	names(keep)[names(keep) %in% c("X","Y")] = c("tX","tY") ## transformed

	polys$X = XYrot[,"X"] + centroid[1]
	polys$Y = XYrot[,"Y"] + centroid[2]
	keep    = cbind(keep, polys[,c("X","Y")])
	names(keep)[names(keep) %in% c("X","Y")] = c("rX","rY") ## rotated

	xybox$X = xybox$X - centroid[1]
	xybox$Y = xybox$Y - centroid[2]
	boxrot  = t(R %*% t(xybox[,c("X","Y")]))
	colnames(boxrot) = c("X","Y")
	xybox$X = boxrot[,"X"] + centroid[1]
	xybox$Y = boxrot[,"Y"] + centroid[2]
	xylim[["rot"]] = list(xlim=range(xybox$X), ylim=range(xybox$Y), xybox=xybox)
	xylim[["out"]] = xylim[["rot"]]

	if (proj=="LL" & proj.out=="LL"){
		polys = convUL(polys)
		attr(xybox, "zone") = attributes(polys)$zone
		xybox = convUL(xybox)
		xylim[["out"]] = list(xlim=range(xybox$X), ylim=range(xybox$Y), xybox=xybox)
	}
	if (keep.extra) {
		atts  = attributes(polys)
		atts.keep = atts[setdiff(names(atts),c("names","row.names"))]
		polys = cbind(polys,keep)
		atts.poly = attributes(polys)
		attributes(polys) = c(atts.poly[setdiff(names(atts.poly),names(atts.keep))], atts.keep)
	}
	if (plot){
		plotMap(polys, col="green", xlim=xylim$out$xlim, ylim=xylim$out$ylim, ...) ## just for testing
		addPolys(xybox, border="red", lwd=2)
	}
	projection = attributes(polys)$projection ## collecting proj and zone for rotation redundant but just in case

	if (!keep.extra)
		xylim = xylim[c(1, length(xylim))]
	rotation = list()
	collect  = c("angle","radian","centroid","R","xylim","projection","zone")
	for (i in collect)
		rotation[[i]] = get(i)
	attr(polys, "rotation") = rotation
	invisible(return(polys))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~rotatePolys

