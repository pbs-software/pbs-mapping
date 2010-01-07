#addBubbles-----------------------------2010-01-07
#  addBubbles takes x, y and z arguments (could be modified to
#    accept an EventData) and optional arguments to draw
#    bubbles of radius proportional to the z variable.
#    Modified (and for the legend, strongly inspired) from:
#Journal of Statistical Software
#January 2006, Volume 15, Issue 5. http://www.jstatsoft.org/
#Proportional Symbol Mapping in R
#Susumu Tanimura
#Chusi Kuroiwa
#Tsutomu Mizota

#  Modifications by Denis Chabot allow it to work with PBSmapping,
#  add one type of bubble (z proportional to volume of bubble)
#  and make it possible to draw several maps with bubbles that all
#  have the same scale (instead of each bubble plot having a scale
#  that depends on the max z value for that plot). Can add a 
#  legend in one of 4 corners or at a specific x-y positiion.
#--------------------------------------------DC/RH
addBubbles = function(x, y, z, type=c("perceptual","surface","volume"),
       z.max=max(z,na.rm=TRUE), max.size=0.05, symbol.zero="+",
       symbol.fg=rgb(0,0,0,0.60), symbol.bg=rgb(0,0,0,0.30),
       legend.pos="bottomleft", legend.breaks=pretty(range(z),3)[-1],
       show.actual=FALSE, legend.type=c("nested", "horiz", "vert"),
       legend.title="Abundance", legend.cex=.8, ...) {
	if(missing(z)) stop("variable to plot is missing")
	if (is.null(z.max) || is.na(z.max)) z.max <- max(z,na.rm=TRUE)
	real.breaks=as.character(signif(legend.breaks/max(legend.breaks)*max(z,na.rm=TRUE),3))
	spread.x <- par("usr")[2] - par("usr")[1]
	spread.y <- par("usr")[4] - par("usr")[3]
	ratio.lat.long <- spread.y / spread.x
	map.size <- par("pin")
	# les rayons des bulles sont en unités X.
	# the radius of bubbles in units X
	ratio.y.x = (spread.y/map.size[2]) / (spread.x/map.size[1])
	# To align in a "nested" fashion, must specify radius in units of Y
	# Attempts to size bubbles reasonably taking into acount the map coordinates.
	# Pour les alligner dans "nested" il faut rayons en Y.
	# ceci pour essayer d'avoir des tailles de bulles raisonables considérant les coordonnées de la carte
	stand.rad <- max.size * spread.x
	# Sorting from large to small insures that small bubbles will not be hidden behind large bubbles.
	tri = order(z, decreasing=TRUE)
	x = x[tri]
	y = y[tri]
	z = z[tri]
	type <- match.arg(type)
	switch(type,
		volume = {
			scale = ((z/z.max)^(1/3))*stand.rad;
			scale.leg = ((legend.breaks/z.max)^(1/3))*stand.rad },
		surface = { 
			scale = sqrt(z/z.max)*stand.rad;
			scale.leg = sqrt(legend.breaks/z.max)*stand.rad },
		perceptual = {
			scale <- ((z/z.max)^0.57)*stand.rad;  
			scale.leg <- ((legend.breaks/z.max)^0.57)*stand.rad }
	)
	z0=z==0; z1=!z0
	symbols(x[z1], y[z1], circles=scale[z1], inches=FALSE, bg=symbol.bg, fg=symbol.fg, add=TRUE)
	if (!is.logical(symbol.zero) || symbol.zero) {
		if (is.logical(symbol.zero)) symbol.zero="+" # set to default
		dots=list(...); if(!is.null(dots$pch)) stop("Specify 'pch' through 'symbol.zero'")
		if (any(z0)) {
			#legend.title=paste(legend.title," (",symbol.zero,"=0)",sep="")
			points(x[z0],y[z0],pch=symbol.zero,...) } }

	# If legend.pos is NULL, do not draw the legend.
	# Calculate the height and width of the legend, which is essential to calculate its position
	# si legend.pos est NULL, on ne dessine pas la légende
	# calculer la hauteur et la largeur de la légende, essentielle pour calculer sa position
	if (!is.null(legend.pos)) {
		gap.x <- par("cxy")[1]*legend.cex/2
		gap.y <- par("cxy")[2] *legend.cex/2
		scale.leg.y <- scale.leg * ratio.y.x
		leg.tex.w <- strwidth(legend.breaks, units="user")*legend.cex
		title.w = strwidth(legend.title)
		max.tex.w <- max(leg.tex.w)
		legend.type <- match.arg(legend.type)
		switch(legend.type,
			nested = {
				legend.height = 2* max(scale.leg) * ratio.y.x +3*gap.y
				# Height of the legend is the biggest bubble + title
				# hauteur de la légende est celle de la plus grosse bulle + titre
				legend.width = 2*max(scale.leg) + gap.x + max.tex.w },
			horiz = {
				legend.height = 2* max(scale.leg) * ratio.y.x +3*gap.y 
				# Height of the legend is the biggest bubble + title + tag
				# hauteur de la légende est celle de la plus grosse bulle + titre + étiquette
				legend.width = 2*sum(scale.leg) + (length(legend.breaks)-1)*gap.x },
			vert = {
				legend.height = 2*sum(scale.leg.y) +  (length(legend.breaks)-1)*gap.y + 3*gap.y
				legend.width = 2*max(scale.leg) + gap.x + max.tex.w }
		)
		# Reflect an adjustment in X if the title is broader than the legend
		# prévoir un ajustement en X si le titre est plus large que la légende
		if(title.w > legend.width) {w.adj = (title.w - legend.width)/2}  
		else {w.adj = 0}

		if (class(legend.pos) == "numeric") {
			legend.loc = legend.pos }
		else {
			# if we already have positions, they are kept.
			# If no positions in X and Y, rather one of the 4 corners, we calculate.
			# dond si on a déjà des positions, on les garde
			# si pas de positions en X et Y, plutôt un des 4 coins, faut les calculer
			corners = c("bottomleft", "bottomright", "topleft", "topright")
			if (legend.pos %in% corners) {
				switch(legend.pos,
					bottomleft = 
						legend.loc<-c(par("usr")[1] + 0.025*spread.x + 
						w.adj, par("usr")[3]+0.025*spread.y+legend.height),
					bottomright = 
						legend.loc<-c(par("usr")[2] - (0.025*spread.x +  
						legend.width + w.adj), par("usr")[3]+0.025*spread.y+legend.height),
					topleft = 
						legend.loc<-c(par("usr")[1] + 0.025*spread.x + w.adj,  
						par("usr")[4]-0.025*spread.y),
					topright = 
						legend.loc<-c(par("usr")[2] - (0.025*spread.x +  
						legend.width + w.adj), par("usr")[4]-0.025*spread.y)
				) # end switch
			} # fin corners / emd corners
		} # fin de legend.loc pas numérique / end of legend.loc not numeric

		# il faut ajuster peu ces coordonnées de position de légende selon le type, je me fais une copie
		leg = legend.loc
		legend.type <- match.arg(legend.type)
		if (show.actual) legbrk=real.breaks else legbrk=legend.breaks
		switch(legend.type,
			nested = {
				# To keep the same space to the left of the legend, should move on the biggest bubble.
				# pour garder le même espace à gauche de la légende, faut déplacer selon la plus grosse bulle
				leg[1] = legend.loc[1]+max(scale.leg)
				# It emerges from the bottom up. We must subtract the height of the legend of the position Y.
				# celle-ci se dessine de bas en haut. Il faut soustraire la  hauteur de la légende de la position Y
				leg[2] = legend.loc[2] - legend.height
				r <- rev(scale.leg)
				b <- rev(legend.breaks); bb=rev(legbrk)
				x.text.leg <- leg[1] + r[1] + gap.x + max.tex.w
				# Position of the right edge of the text labels legend.
				# position de la bordure droite du texte des étiquettes de légende
				x.title.leg <- leg[1] + legend.width/2 - max(scale.leg)
				for (i in 1:length(r)) {
					symbols(leg[1], leg[2] + r[i]*ratio.y.x,circle=r[i],inches=FALSE,add=TRUE,bg=symbol.bg,fg=symbol.fg)
					lines(c(leg[1], leg[1]+ r[1] + gap.x),rep(leg[2]+2*r[i]*ratio.y.x, 2))
					text(x.text.leg,leg[2]+2*r[i]*ratio.y.x, ifelse(show.actual,bb[i],b[i]),
						adj=c(ifelse(show.actual,.5,1),.5),cex=legend.cex) }  # end for
				text(x.title.leg, leg[2]+legend.height, legend.title, adj=c(0.5,0.5), cex=legend.cex+0.2, col="black") 
				zlab=c(x.title.leg, leg[2]+legend.height/4) },
			horiz = {
				leg[2] = legend.loc[2] - legend.height + max(scale.leg.y)
				s <- vector();
				for (i in 1:length(scale.leg))
					s[i] <- 2*sum(scale.leg[1:i]) - scale.leg[i] + (i-1)*gap.x
				#	inches=FALSE,bg=symbol.bg,fg=symbol.fg,add=TRUE),...)
				symbols(leg[1]+s, rep(leg[2],length(scale.leg)), circles=scale.leg,inches=FALSE, bg=symbol.bg, fg=symbol.fg, add=TRUE)
				text(leg[1]+s, leg[2]+scale.leg.y+gap.y, legbrk, adj=c(.5,0.5), cex=legend.cex)
				x.title.leg <- leg[1] + legend.width/2
				text(x.title.leg, leg[2]+legend.height-max(scale.leg.y), 
					legend.title, adj=c(0.5, 0.5), cex=legend.cex+0.2, col="black") 
				zlab=c(leg[1], leg[2]-legend.height/8) },
			vert = {
				# Part of the calculations have been made above to calculate the height of the legend.
				# une partie des calculs ont été fait ci-haut pour calculer la hauteur de la légende
				if (any(legend.pos==c("bottomleft","topleft"))) leg[1]=leg[1]+0.05*spread.x
				r <- scale.leg
				s <- vector()
				for (i in 1:length(legend.breaks))
					s[i] <- gap.y + 2*sum(scale.leg.y[1:i]) - scale.leg.y[i] + (i)*gap.y
				y.leg <- leg[2] - s
				x.text.leg <- leg[1] + max(scale.leg) + gap.x + max.tex.w
				x.title.leg <- leg[1] + legend.width/2 - max(scale.leg)
				symbols(rep(leg[1], length(legend.breaks)), y.leg, circles=scale.leg,bg=symbol.bg, fg=symbol.fg, inches=FALSE, add=TRUE)
				text(rep(x.text.leg, length(legend.breaks)), y.leg, legbrk, cex=legend.cex,
					adj=c(1,0.5), col="black")
				text(x.title.leg, leg[2], legend.title, adj=c(0.5, 0.5), cex=legend.cex+0.2, col="black") 
				zlab=c(x.title.leg, leg[2]) }
		) # end switch
		} # fin de legend.pos pas NULL / end of legend.pos not NULL
	if (!is.logical(symbol.zero) && any(z0)) 
		legend(zlab[1],zlab[2],legend="zero",pch=symbol.zero,xjust=0,yjust=1,bty="n",cex=.8,x.intersp=.5)
#browser();return()
	box();invisible()
	} # fin de la fonction / end of the function
#---------------------------------------addBubbles

