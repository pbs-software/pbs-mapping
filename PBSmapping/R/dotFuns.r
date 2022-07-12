##================================================
## Copyright (C) 2003-2018  Fisheries & Oceans Canada
## Nanaimo, British Columbia
## This file is part of PBS Mapping.
##================================================

##------------------------------------------------
## Dot (hidden) functions kept in one file for
## convenience (RH 2018-08-24)
##------------------------------------------------


## .addAxis-----------------------------2011-12-20
## 'xlim'/'ylim': vectors of length 2
## 'tckLab'/'tck'/'tckMinor': vectors of length 1 or 2
## Returns: NULL (invisible)
## ---------------------------------------------NB
.addAxis <- function(xlim, ylim, tckLab, tck, tckMinor, ...)
{
  # force 'tck*' to length 2
  tckLab   <- rep(tckLab,   length.out = 2);
  tck      <- rep(tck,      length.out = 2);
  tckMinor <- rep(tckMinor, length.out = 2);

  # reduce 'cex' and 'mex' to appropriate sizes for axis labels; these settings
  # should remain after the function termimates; .addLabels() will use the same
  # units for measuring lines

  # changing 'cex' causes plot(...) to change 'mai', so let's reset 'mai'
  # after setting 'cex'
  mai <- par()$mai;
  par(cex = par()$cex * 0.8);   # decrease font size
  par(mai = mai);
  par(mex = par()$cex);         # decrease spacing to match font size

  # 1 is the horizontal axis, 2 is the vertical axis
  lim <- list(xlim, ylim);
  rotate <- list(0, 90)
  for (i in 1:2) {
    if (((i == 1) && (par()$xaxt != "n")) ||
        ((i == 2) && (par()$yaxt != "n"))) {
      # create both major and minor ticks
      ticks <- pretty(lim[[i]]);
      ticksMinor <- pretty(c(0, diff(ticks)[1]));
      ticksMinor <- (sort(rep(ticks, length(ticksMinor))) +
                     rep(ticksMinor, length(ticks)));

      # filter them for anything on the extents
      ticks <- ticks[ticks > lim[[i]][1] & ticks < lim[[i]][2]];
      ticksMinor <-
        ticksMinor[ticksMinor > lim[[i]][1] & ticksMinor < lim[[i]][2]];

      if (!tckLab[i]) {
        tickLabels <- FALSE;
      } else {
        tickLabels <- as.character(ticks);
      }

      # plot the axis
      # major ticks
      axis(side = i, at = ticks, labels = tickLabels, tck = tck[i],
           srt = rotate[[i]]);
      # minor ticks
      axis(side = i, at = ticksMinor, labels = FALSE, tck = tckMinor[i]);
    }
  }

  invisible(NULL);
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.addAxis


## .addAxis2----------------------------2013-03-13
## Modified from the function `.addAxis` to 
## add axes to any side of the plot. 
## Note: temporary until incorporation and testing.
## ------------------------------------------NB/RH
.addAxis2 <- function (side=1:2, xlim, ylim, tckLab, tck, tckMinor, ...) 
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
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.addAxis2


#==============================================================================
.addCorners <- function(polys, ptSummary)
{
  xlim <- range(polys$X)
  ylim <- range(polys$Y)

  corners <- list(tl=c(xlim[1], ylim[2]), tr=c(xlim[2], ylim[2]),
                  bl=c(xlim[1], ylim[1]), br=c(xlim[2], ylim[1]));
  # two "tests" for each corner:
  # each list element is the function to apply to X and then Y
  tests <- list(tl=c(min, max), tr=c(max, max),
                bl=c(min, min), br=c(max, min));

  for (c in names(corners)) {
    # do we need to add a corner point? check whether a point is already in the
    # corner
    if (nrow(polys[polys$X == (corners[[c]])[1] &
                   polys$Y == (corners[[c]])[2], ]) > 0)
      next

    # find candidate PIDs for the corner;
    # candidates touch a boundary on either side of the corner point
    polysA <- polys[polys$X == (corners[[c]])[1], ]
    if (nrow(polysA) > 0)
      # for those polys with an X on the boundary, which ones have a Y
      # that is the min/max? get their PIDs
      PIDsA <- polysA[polysA$Y == ((tests[[c]])[2])[[1]](polysA$Y), "PID"]

    polysB <- polys[polys$Y == (corners[[c]])[2], ]
    if (nrow(polysB) > 0)
      PIDsB <- polysB[polysB$X == ((tests[[c]])[1])[[1]](polysB$X), "PID"]

    if (nrow(polysA) == 0 || nrow(polysB) == 0) {
      warning(paste("Unable to close a corner (", c, ").", sep=""));
      next
    }

    # determine candidates
    cand <- intersect(PIDsA, PIDsB)

    # no candidates (this shouldn't happen...)
    if (length(cand) == 0)
      stop(paste("Unable to close a corner (", c, ") since no candidates exist.",
                 sep=""));

    # more than one candidate and we need to find the appropriate one
    if (length(cand) > 1) {
      # compute the distance from each candidate point to the corner
      pts <- data.frame (x=ptSummary[cand, "x"], y=ptSummary[cand, "y"])
      pts$cornerX <- (corners[[c]])[1];
      pts$cornerY <- (corners[[c]])[2];
      pts$dist <- sqrt((pts$x - pts$cornerX)^2 + (pts$y - pts$cornerY)^2)
      shortest <- which(pts$dist == min(pts$dist))
      if (length(shortest) != 1)
        stop(paste(
"Unable to determine the appropriate polygon to close corner ", c, ".", sep=""))
      cand <- cand[shortest]
    }

    # add corner point
    newPoly <- polys[polys$PID == cand, ]
    polys <- polys[polys$PID != cand, ]

    xydata <- data.frame(X=c(newPoly$X, corners[[c]][1]),
                         Y=c(newPoly$Y, corners[[c]][2]));

    newPoly <- calcConvexHull(xydata)
    newPoly$PID <- cand

    polys <- rbind(polys, newPoly);
  }

  polys <- polys[order(polys$PID), ]
  return (polys)
}

#==============================================================================

#.addBubblesLegend----------------------2022-07-05
#  Construct legend for the function addBubble.
#--------------------------------------------DC|NB
.addBubblesLegend <- function(radii.leg, usr.xdiff, usr.ydiff,
   symbol.zero, symbol.fg, symbol.bg, legend.pos, legend.breaks,
   legend.type, legend.title, legend.cex, ...)
{
	## ratio of y to x: units-per-inch (Y) / units-per-inch (X)
	ratio.y.x = (usr.ydiff / par("pin")[2]) / (usr.xdiff / par("pin")[1])

	## calculate the height and width of the legend, which is essential to calculating its position
	gap.x <- par("cxy")[1] * legend.cex / 2
	gap.y <- par("cxy")[2] * legend.cex / 2
	radii.leg.y <- radii.leg * ratio.y.x
	leg.tex.w <- strwidth(legend.breaks, units = "user") * legend.cex
	title.w = strwidth(legend.title)
	max.tex.w <- max(leg.tex.w)

	switch(legend.type,
		nested = {
			## height of the legend is the biggest bubble + title
			legend.height <- 2 * max(radii.leg.y) + 3 * gap.y
			legend.width <- 2 * max(radii.leg) + gap.x + max.tex.w
		},
		horiz = {
			## height of the legend is the biggest bubble + title + tag
			legend.height <- 2 * max(radii.leg.y) + 3 * gap.y
			legend.width <- 2 * sum(radii.leg) + (length(legend.breaks) - 1) * gap.x
		},
		vert = {
			legend.height <- 2 * sum(radii.leg.y) + (length(legend.breaks) - 1) * gap.y + 3 * gap.y
			legend.width <- 2 * max(radii.leg) + gap.x + max.tex.w
		}
	)

	## reflect an adjustment in X if the title is broader than the legend
	if (title.w > legend.width) {
		w.adj <- (title.w - legend.width) / 2
	} else {
		w.adj <- 0
	}

	## if we already have positions, keep them; otherwise, calculate
	## positions given the described corner
	#if (class(legend.pos) == "numeric") {
	if (inherits(legend.pos, "numeric")) {
		legend.loc <- legend.pos
	} else {
		corners <- c("bottomleft", "bottomright", "topleft", "topright")
		if (legend.pos %in% corners) {
			legend.loc <- switch(legend.pos,
				bottomleft =
					c(par("usr")[1] + 0.025 * usr.xdiff + w.adj, par("usr")[3] + 0.025 * usr.ydiff + legend.height),
				bottomright =
					c(par("usr")[2] - (0.025 * usr.xdiff + legend.width + w.adj), par("usr")[3] + 0.025 * usr.ydiff + legend.height),
				topleft =
					c(par("usr")[1] + 0.025 * usr.xdiff + w.adj, par("usr")[4] - 0.025 * usr.ydiff),
				topright =
					c(par("usr")[2] - (0.025 * usr.xdiff + legend.width + w.adj), par("usr")[4] - 0.025 * usr.ydiff)
			);
		}
	}
	## the calling function should already have validated legend.type
	switch(legend.type,
		nested = {
			## legend.loc[1] specifies X for *center* of circle; we need
			## to shift it right by the largest radius
			legend.loc[1] <- legend.loc[1] + max(radii.leg)
			## the legend will be drawn from the bottom up; shift
			## legend.loc[2] so that it refers to the bottom
			legend.loc[2] <- legend.loc[2] - legend.height
			r <- rev(radii.leg)
			bb <- rev(legend.breaks)

			## position of the right edge of the text labels legend
			x.text.leg <- legend.loc[1] + max(r) + gap.x + max.tex.w

			## draw the circles, lines, and labels
			for (i in 1:length(r)) {
				symbols(legend.loc[1], legend.loc[2] + r[i] * ratio.y.x,
					circles=r[i], inches=FALSE, add=TRUE, bg=symbol.bg[length(r)-i+1], fg=symbol.fg)
				lines(c(legend.loc[1], legend.loc[1] + r[1] + gap.x), rep(legend.loc[2] + 2 * r[i] * ratio.y.x, 2))
				text(x.text.leg, legend.loc[2] + 2 * r[i] * ratio.y.x, bb[i], adj=c(1, .5), cex=legend.cex)
			}
			## add the title
			x.title.leg <- legend.loc[1] - max(radii.leg) + (legend.width / 2)
			text(x.title.leg, legend.loc[2]+legend.height, legend.title, adj=c(0.5,0.5), cex=legend.cex+0.2, col="black")

			## set positions for plotting of zero (later)
			zlab <- c(x.title.leg, legend.loc[2]+legend.height/4) 
		},
		horiz = {
			## legend.loc[2] currently identifies the top of the legend
			legend.loc[2] <- legend.loc[2] + max(radii.leg.y) - legend.height 
			## compute offsets for horizontal spacing
			offset <- vector()
			for (i in 1:length(radii.leg))
				offset[i] <- 2 * sum(radii.leg[1:i]) - radii.leg[i] + (i - 1) * gap.x
			## draw circles, labels
			symbols(legend.loc[1] + offset, rep(legend.loc[2],length(radii.leg)),
				circles = radii.leg, inches = FALSE, bg = symbol.bg, fg = symbol.fg, add = TRUE)
			text(legend.loc[1] + offset, legend.loc[2] + radii.leg.y + gap.y, 
				legend.breaks, adj = c(0.5, 0.5), cex = legend.cex)
			## add the title
			text(legend.loc[1] + legend.width / 2, legend.loc[2] + legend.height - max(radii.leg.y),
				legend.title, adj = c(0.5, 0.5), cex = legend.cex + 0.2, col = "black")
			## set positions for plotting of zero (later)
			zlab <- c(legend.loc[1], legend.loc[2] - legend.height / 8) 
		},
		vert = {
			## part of the calculations have been made above to calculate the height of the legend
			if (any(legend.pos == c("bottomleft","topleft")))
				legend.loc[1] <- legend.loc[1] + 0.05 * usr.xdiff
			## compute offsets for vertical spacing
			offset <- vector()
			for (i in 1:length(legend.breaks))
				offset[i] <- gap.y + 2 * sum(radii.leg.y[1:i]) - radii.leg.y[i] + i * gap.y
			## draw circles, labels
			symbols(rep(legend.loc[1], length(legend.breaks)), legend.loc[2] - offset,
				circles = radii.leg, bg = symbol.bg, fg = symbol.fg, inches = FALSE, add = TRUE)
			x.text.leg <- legend.loc[1] + max(radii.leg) + gap.x + max.tex.w
			text(rep(x.text.leg, length(legend.breaks)), legend.loc[2] - offset, legend.breaks,
				cex = legend.cex, adj = c(1, 0.5), col="black")
			## add the title
			text(legend.loc[1] + legend.width / 2 - max(radii.leg), legend.loc[2],
				legend.title, adj = c(0.5, 0.5), cex = legend.cex + 0.2, col = "black")
			## set positions for plotting of zero (later)
			zlab <- c(legend.loc[1] + legend.width / 8, legend.loc[2])
		}
	)
	## plot the zero if need be
	if (!is.logical(symbol.zero))
		legend(zlab[1], zlab[2], legend = "zero", pch = symbol.zero, xjust = 0,
			yjust = 1, bty = "n", cex = 0.8, x.intersp = 0.5)
	invisible(legend.loc)
}

#==============================================================================
.addFeature <- function(feature, data, polyProps, isEventData,
                        cex = NULL, col = NULL, font = NULL, pch = NULL, ...)
  # 'feature == "points"': add 'data' with points()
  # 'feature == "labels"': add 'data' with text()
  # 'data': if is PolyData, relaxed unique PID requirement (for addStipples())
  # 'isEventData': if TRUE, look for an EID column in 'data'
  # '...' contains arguments for either the 'text()' or 'points()' function
  #
  # Returns: PolyProps
{
  data <- .mat2df(data);
  if (isEventData) {
    type <- "e";
  } else {
    type <- "p";
  }

  # given 'feature', appropriate columns in polyProps
  if (feature == "points") {
    relevantProps <- c("cex", "col", "pch");
  } else {
    relevantProps <- c("cex", "col", "font");
  }

  # validate the polyProps argument
  polyProps <- .validatePolyProps(polyProps, parCols = relevantProps);
  if (is.character(polyProps))
    stop(paste("Invalid PolyData 'polyProps'.\n", polyProps, sep=""));

  # at this point, 'data' has:
  # (!isEventData): PID, (optional SID), X, Y, and label columns
  # (isEventData):  EID, X, Y, and label columns

  # defaults for 'polyProps'
  parValues <- list(cex = par("cex"), col = par("col"),
                    font = par("font"), pch = par("pch"));
  # don't replace existing values with defaults
  parValues <- parValues[setdiff(names(parValues), names(polyProps))];
  # keep only applicable values
  parValues <- parValues[intersect(names(parValues), relevantProps)];
  # override defaults with values passed as arguments
  if (!is.null(cex))  parValues[["cex"]]  <- cex;
  if (!is.null(col))  parValues[["col"]]  <- col;
  if (!is.null(font)) parValues[["font"]] <- font;
  if (!is.null(pch))  parValues[["pch"]]  <- pch;

  # make basic 'polyProps' if necessary
  if (is.null(polyProps)) {
    if (isEventData) {
      polyProps <- data.frame(EID = unique(data$EID));
    } else {
      polyProps <- data.frame(PID = unique(data$PID));
    }
  }
  # merge SIDs into PolyProps if necessary
  if (!isEventData
      && is.element("SID", names(data))
      && !is.element("SID", names(polyProps))) {
    # grab SIDs from 'polys' for all PIDs in 'polyProps'
    p <- data[is.element(data$PID, unique(polyProps$PID)), c("PID", "SID")];
    # by filtering things first (above), we speed up the paste() (below)
    # add those SIDs to 'polyProps'
    polyProps <- merge(polyProps,
                       p[!duplicated(paste(p$PID, p$SID)), c("PID", "SID")],
                       by="PID");
  }

  # flesh out 'polyProps'
  if (length(parValues) > 1)
    polyProps <- .addProps(type = type, polyProps = polyProps, parValues);
  polyPropsReturn <- polyProps;

  # reduce data to IDs found in 'polyProps'
  if (isEventData) {
    data <- data.frame(data[is.element(data$EID, unique(polyProps$EID)), ]);
  } else {
    if (is.element("SID", names(polyProps))) {
      # 'polyProps' may have an SID field and 'polys' may not
      if (!is.element("SID", names(data)))
        stop("Since 'polyProps' contains an SID column, 'data' must as well.\n");
      data <- data[is.element(paste(data$PID, data$SID),
                              unique(paste(polyProps$PID, polyProps$SID))), ];
    } else {
      data <- data[is.element(data$PID, unique(polyProps$PID)), ];
    }
  }

  # flatten relevant columns
  propColumns <- intersect(names(polyProps), relevantProps);
  # paste the columns together
  exprStr <- paste("paste(",
                   paste(paste('polyProps[, "', propColumns, '"]', sep=""),
                         collapse=", "),
                   ");", sep="");
  polyProps$props <- eval(parse(text=exprStr))

  # merge that 'props' column into data
  if (isEventData) {
    data <- merge(data, polyProps[, c("EID", "props")], by="EID");
  } else {
    # if SID exists in 'polyProps' it exists in 'data' (and vice-versa)
    if (is.element("SID", names(polyProps))) {
      data <- merge(data,
                    polyProps[, c("PID", "SID", "props")], by=c("PID", "SID"));
    } else {
      data <- merge(data, polyProps[, c("PID", "props")], by="PID");
    }
  }

  # split data on the 'props' column
  data <- split(data, data$props);
  # keep polyProps for getting at individual properties
  polyProps <- polyProps[!duplicated(polyProps$props), ];

  # plot each set of properties on its own
  for (c in names(data)) {
    d <- (data[[c]]);
    p <- (as.list(polyProps[polyProps$props == c, propColumns]));

    if (feature == "labels") {
      text(x = d$X, y = d$Y, labels = as.character(d$label),
           cex = p$cex, col = p$col, font = p$font, ...);
    } else {
      points (x = d$X, y = d$Y,
              cex = p$cex, col = p$col, pch = p$pch, ...);
    }
  }
  return (polyPropsReturn);
}


## .addLabels---------------------------2008-08-25
## 'projection' should equal "LL" (for longitude/latitude),
## "UTM" (for Universal Transverse Mercator), or anything else (for X/Y).
## 'main', 'sub', 'xlab', 'ylab' may all be part of '...'
## ------------------------------------------NB/RH
.addLabels <- function(projection = NULL, ...)
{
  dots <- list(...);
  main=dots$main;  sub=dots$sub;  xlab=dots$xlab;  ylab=dots$ylab;

  # set label defaults (if necessary)
  if (!is.null(projection) && !is.na(projection) && projection == "UTM") {
    if (is.null(xlab)) xlab <- "UTM Easting (km)";
    if (is.null(ylab)) ylab <- "UTM Northing (km)";
  } else if (!is.null(projection) && !is.na(projection) && projection == "LL") {
    if (is.null(xlab)) xlab <- "Longitude (\u00B0)"
    if (is.null(ylab)) ylab <- "Latitude (\u00B0)"
  } else {
    if (is.null(xlab)) xlab <- "X";
    if (is.null(ylab)) ylab <- "Y";
  }
  if (is.null(main)) main <- "";
  if (is.null(sub))  sub  <- "";
  # 'xlab'/'ylab' cannot equal NULL at this point
  title (main=main, sub=sub, xlab=xlab, ylab=ylab) # cannot add dots - conflicts with formal arguments
  invisible(NULL);
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.addLabels


## .addProps-----------------------------2004-06-24
## cycles properties in '...' by PID
##  'type = "e"': EventData
##  'type = "p"': PolyData
##  'polyProps': a data frame
##  '...': parameters in the form (col = 1, cex = 1, ...),
##    each of which can be a list object
## If '...' contains parameters that already exist in 'polyProps', these
## parameters will overwrite the existing columns in 'polyProps' -- avoid
## this behaviour by not including properties in '...' that already exist
## in 'polyProps'.
## Returns: polyProps
##----------------------------------------------NB
.addProps <- function(type, polyProps, ...)
{
  # clean up param -- in case it contains one or more lists
  param <- list(...);
  newParam <- list();
  for (i in 1:length(param)) {
    if (is.list(param[[i]])) {
      newParam <- c(newParam, param[[i]]);
    } else {
      newParam <- c(newParam, param[i]);
    }
  }
  param <- newParam;

  if (type == "e") {
    polyProps$IDX <- polyProps$EID;
  } else if (type == "p") {
    # IDX is PID rather than paste(PID, SID) because we cycle by PID
    polyProps$IDX <- polyProps$PID;
  } else {
    stop (
"Unknown 'type'.  Must be either \"e\" or \"p\".");
  }

  uIDX <- unique(polyProps$IDX);

  for (c in names(param)) {
    # add it if it isn't NULL
    if (!is.null(param[[c]])) {
      # remove column if already exists
      if (is.element(c, names(polyProps))) {
        # use data.frame() to ensure it isn't reduced to a vector
        polyProps <- data.frame(polyProps[, !is.element(names(polyProps), c)]);
        # if reduced to 1 column, it loses the name; reset it
        if (ncol(polyProps) == 1) {
          if (type == "p") names(polyProps) <- "PID";
          if (type == "e") names(polyProps) <- "EID";
        }
      }

      # build new structure
      newColumn <- data.frame(IDX=uIDX);
      newColumn[, c] <- rep(param[[c]], length.out = length(uIDX));

      # merge new structure
      polyProps <- merge(polyProps, newColumn, by = "IDX");
    }
  }

  # remove IDX column; use data.frame() to ensure it isn't reduced to a vector
  polyProps <- data.frame(polyProps[, !is.element(names(polyProps), "IDX")]);
  # if reduced to 1 column, it loses the name; reset it
  if (ncol(polyProps) == 1) {
    if (type == "p") names(polyProps) <- "PID";
    if (type == "e") names(polyProps) <- "EID";
  }

  return (polyProps);
}

#==============================================================================
#==============================================================================
.calcDist <- function(polys)
  # Assumes 'polys' contains valid PolySet with 'projection' attribute
  # containing LL/UTM/1.
  #
  # Returns: distance vector (distances between each point)
{
  # calculate distance for UTM/1:1
  if (!is.null(attr(polys, "projection"))
      && !is.na(attr(polys, "projection"))
      && ((attr(polys, "projection") == "UTM")
          || (attr(polys, "projection") == 1))) {
    len <- nrow(polys)

    D <- c(sqrt((polys$X[1:(len-1)] - polys$X[2:len])^2
                 + (polys$Y[1:(len-1)] - polys$Y[2:len])^2),
            0);

  }
  # calculate distance for LL
  else if (!is.null(attr(polys, "projection"))
           && !is.na(attr(polys, "projection"))
           && (attr(polys, "projection") == "LL")) {
    # Equatorial radius 6,378.14 km
    # Polar radius 6,356.78 km
    # Mean radius 6,371.3 km
    # Sources:
    #   http://en.wikipedia.org/wiki/Earth
    #   http://en.wikipedia.org/wiki/Earth_radius
    R <- 6371.3;

    # degrees to radians
    polys[, c("X", "Y")] <- polys[, c("X", "Y")] * pi / 180.0
    len <- nrow(polys)

    # Source:
    #   http://www.census.gov/cgi-bin/geo/gisfaq?Q5.1
    # Algorithm originally in pseudocode.
    s0 <- 1:(len - 1);  # 's' for 's'hift
    s1 <- 2:len;        # (s0 + 1;)

    dlon <- polys$X[s1] - polys$X[s0];   # dlon = lon2 - lon1
    dlat <- polys$Y[s1] - polys$Y[s0];   # dlat = lat2 - lat1
    cosPolysY <- cos(polys$Y);
    # a = (sin(dlat/2))^2 + cos(lat1) * cos(lat2) * (sin(dlon/2))^2
    a <- (sin(dlat / 2))^2 + cosPolysY[s0] * cosPolysY[s1] *
      (sin(dlon / 2))^2;
    # c = 2 * arcsin(min(1, sqrt(a)))
    a <- sqrt(a);
    a[a > 1] <- 1;
    cc <- 2 * asin(a);
    # d = R * c
    D <- c(R * cc, 0);
  }
  # unknown projection
  else {
    stop(paste(
"Invalid projection attribute.  Supported projections include \"LL\",",
"\"UTM\", and 1.\n"));
  }

  return (D);
}

#==============================================================================
.calcOrientation <- function(polys)
  # Assumes 'polys' contains a valid PolySet.
  #
  # Returns:
  #   data frame (invisible) with 'orientation' column (-1 when
  #     counter-clockwise; 0 when N/A; +1 when clockwise)
  #   OR: NULL (invisible) (if no rows in output)
{
  inRows <- nrow(polys);
  # Memory requirement for output are lower than nrow(polys); in fact, they
  # are length(unique(paste(polys$PID, polys$SID))).
  # The extra time to compute the real memory requirements makes doing so
  # not worth it.
  outCapacity <- nrow(polys);

  # create the data structure that the C functions expect
  # Using $<col> notation seems faster than [, "col"] notation.
  if (!is.element("SID", names(polys))) {
    inID <- c(polys$PID, integer(length = inRows), polys$POS);
  } else {
    inID <- c(polys$PID, polys$SID, polys$POS);
  }
  inXY <- c(polys$X, polys$Y);

  # call the C function
  results <- .C("calcOrientation",
                inID = as.integer(inID),
                inXY = as.double(inXY),
                inVerts = as.integer(inRows),
                outID = integer(2 * outCapacity),
                outOrientation = double(outCapacity),
                outRows = as.integer(outCapacity),
                outStatus = integer(1),
                PACKAGE = "PBSmapping");
  # note: outRows is set to how much space is allocated -- the C function
  #       should consider this

  if (results$outStatus == 1) {
    stop(
"Insufficient physical memory for processing.\n");
  }
  if (results$outStatus == 2) {
    stop(paste(
"Insufficient memory allocated for output.  Please upgrade to the latest",
"version of the software, and if that does not fix this problem, please",
"file a bug report.\n",
               sep = "\n"));
  }

  # determine the number of rows in the result
  outRows <- as.vector(results$outRows);

  # extract the data from the C function results
  if (outRows > 0) {
    d <- data.frame(PID = results$outID[1:outRows],
                    SID = results$outID[(outCapacity+1):(outCapacity+outRows)],
                    orientation = results$outOrientation[1:outRows]);

    if (!is.element("SID", names(polys)))
      d$SID <- NULL;

    invisible(d);
  } else {
    invisible(NULL);
  }
}

#==============================================================================
.checkClipLimits <- function(limits)
{
  # Makes sure that the X & Y limits are within the bounds of the GSHHS databases
  if (limits[1] > limits[2])
    stop("xlim[1] is larger than xlim[2]")
  if (limits[3] > limits[4])
    stop("ylim[1] is larger than ylim[2]")
  if (limits[1] > 360 || limits[2] < -20)
    stop("xlim are outside of the range of c(-20,360)")
  if (limits[3] > 90 || limits[4] < -90)
    stop("ylim are outside of the range of c(-90,90)")
}

#==============================================================================
.checkProjection <- function(projectionPlot, projectionPoly)
{
  if (is.null(projectionPlot)) {
    projMapStr <- "NULL";
  } else {
    projMapStr <- as.character(projectionPlot);
  }
  if (is.null(projectionPoly)) {
    projPolyStr <- "NULL";
  } else {
    projPolyStr <- as.character(projectionPoly);
  }
  msg <- paste(
"The data's 'projection' attribute (", projPolyStr, ") differs from the\n",
"projection of the plot region (", projMapStr, ").\n", sep="");

  if (xor(is.null(projectionPlot), is.null(projectionPoly))) {
    warning(msg);
  } else if ((!is.null(projectionPlot) && !is.null(projectionPoly)) &&
             (xor(is.na(projectionPlot), is.na(projectionPoly)))) {
    warning(msg);
  } else if (!is.null(projectionPlot) && !is.null(projectionPoly) &&
             !is.na(projectionPlot) && !is.na(projectionPoly) &&
             (projectionPlot != projectionPoly)) {
    warning(msg);
  }
}

#==============================================================================
.checkRDeps <- function(caller = "unspecified", requires = NULL)
{
  if (is.null(version$language) || (version$language != "R")) {
    stop (paste ("
The function '", caller, "' requires several dependencies available only in R.\n",
"Please try again from within R.\n", sep=""));
  }

  err <- NULL;
  for (pkg in requires) {
    if (!require(pkg, character.only = TRUE)) {
      err <- append (err, pkg);
    }
  }
  if (!is.null (err)) {
    err <- paste (err, collapse="', '");
    stop (paste ("
The function '", caller, "' requires the package(s) '", err, "'.\n",
"Please install the package(s) and try again.\n", sep=""));
  }
}

#==============================================================================
.clip <- function(polys, xlim, ylim, isPolygons, keepExtra)
  # Does not validate 'polys'; called directly from addPolys since addPolys()
  # already validated data.
  #
  # Maintains extra attributes.
  #
  # Returns:
  #   data frame with 'oldPOS' column
  #   OR: NULL (if no rows in output)
{
  # if 'keepExtra', create PolyData from PolySet; after processing, merge these
  # data back into 'polys'
  if (keepExtra)
    pdata <- extractPolyData(polys);

  # save the attributes of the data frame
  attrNames <- setdiff(names(attributes(polys)),
                       c("names", "row.names", "class"));
  attrValues <- attributes(polys)[attrNames];

  inRows <- nrow(polys);
  outCapacity <- as.integer(2 * inRows);

  # create the data structures that the C function expects
  if (!is.element("SID", names(polys))) {
    inID <- c(polys$PID, integer(length = inRows), polys$POS);
  } else {
    inID <- c(polys$PID, polys$SID, polys$POS);
  }
  inXY <- c(polys$X, polys$Y);
  limits <- c(xlim, ylim);

  # call the C function
  results <- .C("clip",
                inID = as.integer(inID),
                inXY = as.double(inXY),
                inVerts = as.integer(inRows),
                polygons = as.integer(isPolygons),
                limits = as.double(limits),
                outID = integer(4 * outCapacity),
                outXY = double(2 * outCapacity),
                outRows = as.integer(outCapacity),
                outStatus = integer(1),
                PACKAGE = "PBSmapping");
  # note: outRows is set to how much space is allocated -- the C function
  #       should take this into consideration

#browser();return() ##***** need to be alert for orphaned holes
  if (results$outStatus == 1) {
    stop(
"Insufficient physical memory for processing.\n");
  }
  if (results$outStatus == 2) {
    stop(paste(
"Insufficient memory allocated for output.  Please upgrade to the latest",
"version of the software, and if that does not fix this problem, please",
"file a bug report.\n",
               sep = "\n"));
  }

  # determine the number of rows in the result
  outRows <- as.vector(results$outRows);

  # extract the data from the C function results
  if (outRows > 0) {
    # after creating a data frame several different ways, the following seemed
    # to be quickest
    d <- data.frame(PID = results$outID[1:outRows],
                    SID = results$outID[(outCapacity+1):(outCapacity+outRows)],
                    POS = results$outID[(2*outCapacity+1):(2*outCapacity+outRows)],
                    oldPOS = results$outID[(3*outCapacity+1):(3*outCapacity+outRows)],
                    X = results$outXY[1:outRows],
                    Y = results$outXY[(outCapacity+1):(outCapacity+outRows)]);

    # remove "SID" column if not in input
    if (!is.element("SID", names(polys)))
      d$SID <- NULL;

    # change oldPOSs of -1 to NA to meet specs.
    d$oldPOS[d$oldPOS == -1] <- NA;

    # if keepExtra, merge the PolyData back in...
    # 'merge' loses attributes!  Merge before restoring attributes
    if (keepExtra)
      d <- merge(x = d, y = pdata, all.x = TRUE,
                 by = intersect(c("PID", "SID"), names(d)));

    # restore the attributes
    attributes(d) <- c(attributes(d), attrValues);

    # a final test, to ensure the PolySet contains more than just
    # degenerate edges
    if (all(d$X == xlim[1]) || all(d$X == xlim[2]) ||
        all(d$Y == ylim[1]) || all(d$Y == ylim[2]))
      return(NULL);

    return(d);
  } else {
    return(NULL);
  }
}

#==============================================================================
.closestPoint <- function(pts, pt)
  # "pts": data frame with columns "X" and "Y"
  # "pt":  data frame with columns "X" and "Y", and ONE row
  #
  # Returns a vector of length "pts" where T indicates that the point is
  # closest to "pt".  Returns several Ts when several points equidistant.
{
  pts$Xorig <- pt$X
  pts$Yorig <- pt$Y

  pts$dist <- (pts$Xorig - pts$X) ^ 2 + (pts$Yorig - pts$Y) ^ 2

  return (pts$dist == min(pts$dist))
}

#==============================================================================
.createFastIDdig <- function(polysA, polysB = NULL, cols)
  # Determines the maximum number of digits in the second column of a data
  # frame.  If given two data frames ('polysA' and 'polysB', determines that
  # maximum between the two data frames.
  #
  # Assumptions:
  #   both 'cols' contain integers
  #
  # Arguments:
  #   'polysA': first data frame
  #   'polysB': second data frame, which may be missing one or both 'cols';
  #     if missing one or more 'cols', it's assumed they'll be derived from
  #     those in 'polysA'
  #   'cols': vector of length 2, listing columns to use
  #
  # Note:
  #   If it returns 0, then the first and second columns will not fit into
  #   a double, and they must be 'paste'd toegether.
  #
  # Returns:
  #   number of digits in SECOND column (max. of 'polysA' and 'polysB')
  #   OR NULL if the columns don't exist in the data frame(s)
{
  if ((length(cols) == 2)
      && all(is.element(cols, names(polysA)))) {
    digitsL <- floor(log10(max(polysA[[cols[1]]])) + 1);
    digitsR <- floor(log10(max(polysA[[cols[2]]])) + 1);

    # check 'polysB' as well
    if (!is.null(polysB)) {
      if (is.element(cols[1], names(polysB)))
        digitsL <- max(digitsL, floor(log10(max(polysB[[cols[1]]])) + 1));
      if (is.element(cols[2], names(polysB)))
        digitsR <- max(digitsR, floor(log10(max(polysB[[cols[2]]])) + 1));
    }

    # 'double' has 15 digits of precision (decimal), according to my
    # 'C Pocket Reference' by Prinz, P. and U. Kirch-Prinz
    if ((digitsL + digitsR) <= 15) {
      return (digitsR);
    } else {
      return (0);
    }
  } else {
    return (NULL);
  }
}

#==============================================================================
.createGridIDs <- function(d, addSID, byrow)
  # Create IDs for a grid according to the addSID and byrow arguments.
  #
  # Arguments:
  #  'd': PolySet (grid) created using addSID = T and byrow = T
  #  'addSID': if TRUE, include an SID column
  #  'byrow': if TRUE, increment PID along X
{
  if (addSID && !byrow) {
    # swap
    tmp <- d$PID;
    d$PID <- d$SID;
    d$SID <- tmp;
  } else if (!addSID && byrow) {
    d$PID <- (d$SID - 1) * (length(unique(d$X)) - 1) + d$PID;
    d$SID <- NULL;
  } else if (!addSID && !byrow) {
    d$PID <- (d$PID - 1) * (length(unique(d$Y)) - 1) + d$SID;
    d$SID <- NULL;
  }

  return (d);
}

#==============================================================================
.createIDs <- function(x, cols, fastIDdig = NULL)
  # Creates an IDs (or IDX) column from its input.
  #
  # Arguments:
  #   'x': data frame with one or more columns
  #   'cols': columns to use when creating the index; OK if individual
  #     columns are missing
  #   'fastIDdig': (optional) maximum number of digits in the second column, if
  #     only two integer columns, and the maximum number of digits between them
  #     is less than 15; often the output from '.createFastIDdig'; a user would
  #     want to pass in 'fastIDdig' when creating (some) matching indices for two
  #     different data frames
  #
  # Note:
  #   If 'fastIDdig' equals NULL, executes '.createFastIDdig' to create it (but
  #   only if two columns).
  #
  # Returns:
  #   index column if everything OK
  #   NULL if error
{
  # use 'is.element' instead of 'intersect' because 'intersect' gives no
  # guarantee of order (as far as I can see)
  presentCols <- cols[is.element(cols, names(x))]

  if (length(presentCols) == 1) {
    return (x[[presentCols]]);
  } else if (length(presentCols) == 2) {
    # if a fastIDdig wasn't passed in, try to create one
    if (is.null(fastIDdig)) {
      fastIDdig <- .createFastIDdig(polysA=x, polysB=NULL, cols=presentCols);
    }

    # if called the function above, 'fastIDdig' won't equal NULL (but might
    # equal 0)
    # if the user passed it in, it should only equal NULL if there is
    # only one ID column, in which case we shouldn't be in this branch
    # of the 'if'
    if (fastIDdig > 0) {
      return (as.double(x[[presentCols[1]]]
                        + (x[[presentCols[2]]] / 10^fastIDdig)));
    } else {
      return (paste(x[[presentCols[1]]], x[[presentCols[2]]], sep = "-"));
    }
  } else if (length(presentCols) > 2) {
    exprStr <- paste("paste(",
                     paste(paste("x$", presentCols, sep=""),
                           collapse=", "),
                     ");", sep="");
    return (eval(parse(text=exprStr)));
  }

  return (NULL);
}

#==============================================================================
.expandEdges <- function(polys, pts, xlim, ylim)
  # expects all arguments to be specified
{
  polyRange <- c(range(polys$X), range(polys$Y))
  ptsRange <- c(range(pts$X), range(pts$Y))

  # set "toFix" based on points outside the PolySet
  toFixPts <- c(ptsRange[1] < polyRange[1],
                ptsRange[2] > polyRange[2],
                ptsRange[3] < polyRange[3],
                ptsRange[4] > polyRange[4])
  # update "toFix" to extend PolySet to limits outside of its range
  toFixLim <- c(signif(xlim[1], 5) < signif(polyRange[1], 5),
                signif(xlim[2], 5) > signif(polyRange[2], 5),
                signif(ylim[1], 5) < signif(polyRange[3], 5),
                signif(ylim[2], 5) > signif(polyRange[4], 5))
  # the use of "signif" reduces the number of unnecessary edges in
  # "toFixLim" due to rouding error; digits = 6 seemed insufficient
  # to compensate for rounding error

  toFix <- toFixPts | toFixLim

  # if nothing to fix, return
  if (!any(toFix))
    return (polys)

  # side: 1 = left, 2 = right, 3 = bottom, 4 = top
  for (side in which(toFix)) {
    # a point that causes expansion
    if (side == 1) {
      PID <- which(pts$X < polyRange[side])
    } else if (side == 2) {
      PID <- which(pts$X > polyRange[side])
    } else if (side == 3) {
      PID <- which(pts$Y < polyRange[side])
    } else if (side == 4) {
      PID <- which(pts$Y > polyRange[side])
    } else {
      stop ("Internal error: unrecognized value of \"size\" in point check.")
    }

    # no points caused the expansion, so it must be a limit that causes it
    if (length(PID) == 0) {
      # must determine PID using another method
      if (side == 1 || side == 2) {
        PID <- which(.closestPoint(pts, data.frame(X=xlim[side], Y=mean(ylim))))
      } else if (side == 3 || side == 4) {
        PID <- which(.closestPoint(pts, data.frame(X=mean(xlim), Y=ylim[side - 2])))
      } else {
        stop ("Internal error: unrecognized value of \"size\" in limit check.")
      }
    }

    # sanity check
    if (length(PID) != 1)
      stop ("Internal error: unable to determine appropriate PID for expansion.")

    newPoly <- polys[polys$PID == PID, ]
    polys <- polys[polys$PID != PID, ]

    if (side == 1 || side == 2) {
      newXY <- data.frame(X=c(newPoly$X, rep(xlim[side], 2)),
                          Y=c(newPoly$Y, range(newPoly$Y)))
    } else if (side == 3 || side == 4) {
      newXY <- data.frame(X=c(newPoly$X, range(newPoly$X)),
                          Y=c(newPoly$Y, rep(ylim[side - 2], 2)))
    } else {
      stop ("Internal error: unrecognized value of \"size\" in data frame setup.")
    }

    newPoly <- calcConvexHull(newXY)
    newPoly$PID <- PID

    polys <- rbind(polys, newPoly)
  }

  polys <- polys[order(polys$PID), ]

  return (polys);
}

#==============================================================================
.fixGSHHSWorld <- function (world) {
  # store desired limits
  xlim <- range(world$X)
  ylim <- range(world$Y)
  ylim[1] <- -90
  
  # determine PID of Antarctica: we'll use it to extract the current Antarctica,
  # which we'll grow west/east, then clip, then merge with the other polygons
  event <- data.frame(EID = 1, X = 85, Y = -72)
  event <- findPolys(event, world)
  pid <- event$PID[1]
  
  # extract current Antarctica
  curAnt <- world[is.element(world$PID,pid), ]

  # ensure the points are ordered from left to right
  if (curAnt$X[1] > curAnt$X[nrow(curAnt)])
    curAnt <- curAnt[nrow(curAnt):1, ]

  # create a copy to the left and a copy to the right; add corners
  left <- curAnt[c(1,1:nrow(curAnt)), ]
  left$X <- left$X - 360
  left$Y[1] <- -90
  right <- curAnt[c(1:nrow(curAnt), nrow(curAnt)), ]
  right$X <- right$X + 360
  right$Y[nrow(right)] <- -90

  # merge to create a new, very wide Antarctica
  curAnt <- rbind(left, curAnt, right)
  curAnt$POS <- 1:nrow(curAnt)
  
  # clip
  curAnt <- clipPolys(curAnt, xlim, ylim)
  curAnt$oldPOS <- NULL

  # merge into the existing world
  world <- rbind(world[world$PID < pid, ], curAnt, world[world$PID > pid, ])
  row.names(world) <- 1:length(row.names(world))

  invisible(world)
}

#=============================================================================
.getBasename <- function (fn, ext)
  # If appropriate, remove the extension from 'fn' to obtain the shapefile
  # name without the extension.  When testing for file existance, use the
  # extension 'ext'.
{
  # if appending .shp does not give a valid file...
  if (!file.exists (paste(fn, ".", ext, sep=""))) {
    # attempt to remove extension and try again
    fn <- sub ("\\..{3}$", "", fn);

    if (!file.exists (paste(fn, ".", ext, sep=""))) {
      stop (paste ("Cannot find the file \"", fn, ".", ext, "\".", sep=""));
    }
  }

  return (fn);
}

#=============================================================================
.getGridPars <- function (polys, fullValidation = TRUE)
{
  res <- list();
  res$x <- sort(unique(polys$X));
  res$y <- sort(unique(polys$Y));

  lenx <- length(res$x);
  leny <- length(res$y);
  if ((lenx < 2) || (leny < 2)) {
    return (NULL);
  }

  # determine "addSID"
  if (is.element("SID", names(polys))) {
    res$addSID <- TRUE;
  } else {
    res$addSID <- FALSE;
  }

  # determine "byrow"
  if (lenx == 2 && leny == 2) {
    # special case: only one polygon; byrow does not matter
    res$byrow <- TRUE;
  } else if (lenx == 2) {
    # special case: the second polygon in polys is _above_ the
    # first
    if (polys[1, "PID"] + 1 == polys[5, "PID"]) {
      res$byrow <- FALSE;
    } else {
      res$byrow <- TRUE;
    }
  } else {
    # the second polygon in polys is to the _right_ of the
    # first
    if (polys[1, "PID"] + 1 == polys[5, "PID"]) {
      res$byrow <- TRUE;
    } else {
      res$byrow <- FALSE;
    }
  }

  res$projection <- attr(polys, "projection");
  res$zone <- attr(polys, "zone");

  if (fullValidation) {
    t <- makeGrid(x = res$x, y = res$y, byrow = res$byrow,
                  addSID = res$addSID, projection = res$projection,
                  zone = res$zone);
    if (is.character (all.equal (polys, t))) {
      return (NULL);
    }
  }

  return (res);
}

#==============================================================================
.initPlotRegion <- function(projection, xlim, ylim, plt)
  # Initialize the plot region, accounting for the aspect ratio.
  #
  # 'projection': projection
  #   "UTM"   => UTM        <-- as per specs.
  #   "LL"    => Geographic <-- as per specs.
  #   NA      => none       <-- for plotLines, plotPolys
  #   numeric => specifies aspect ratio
  # 'xlim': x-limits of the plot
  # 'ylim': y-limits of the plot
  # 'plt': requested plot size
  #
  # Returns: NULL (invisible)
  #
  # Notes:
  # In a new plot, mar/mai are 'in-sync' with pin/plt, meaning that changing
  # mar/mai causes pin/plt to change.  Manually setting pin/plt causes mar/mai
  # to fall 'out-of-sync', meaning that changing mar/mai will have no effect
  # on pin/plt.  In this function, do not cause pin/plt to fall out-of-sync
  # (i.e., never directly set pin/plt). However, if they are already
  # out-of-sync, honour the pin setting (which is consistent with what R/S-PLUS
  # does).
  #
  # Summary:
  # Read plt; whether in-sync or out-of-sync, this value reflects the
  # maximum plot region.  Calculate desired plot region, assume in-sync, and
  # set new region indirectly via mar/mai. If new region doesn't match the
  # expected region, it is out-of-sync, so set new region directly via pin/plt.
  #
  # If the user allows pin/plt to fall out-of-sync and sets plt to NULL, the
  # function will still produce the correct aspect ratio, but may not honour
  # the mai/mar par() parameters.
{
  # validate 'xlim' and 'ylim'
  if ((missing(xlim) || is.null(xlim)) || !is.vector(xlim) ||
      (length(xlim) != 2) ||
      (missing(ylim) || is.null(ylim)) || !is.vector(ylim) ||
      (length(ylim) != 2)) {
    stop(
"xlim and/or ylim is missing or invalid.\n");
  }
  if (diff(ylim) == 0 || diff(xlim) == 0) {
    stop(
"xlim/ylim must specify a region with area greater than 0.\n");
  }

  # since all high-level plot functions call this function, we can
  # advance to the next frame here, unless par(new=T) is specified.
  if(!par()$new)
    frame();

  # if 'plt' is NULL, the user doesn't want to use the default, so read its
  # current value from 'par' parameters
  if (is.null(plt))
    plt <- par("plt");

  # 'projection' cannot equal NULL
  if (is.null(projection))
    stop(
"'projection' argument must not equal NULL.\n");

  # if projection isn't NA, adjust for the aspect ratio; in this section
  # we must indirectly set plt/pin via mai/mar if possible; if that doesn't
  # work, we must set them directly
  if (!is.na(projection))
    {
      # use an additional scaling factor if LL
      xyRatio <- ifelse(projection == "LL", cos((mean(ylim) * pi) / 180), 1);

      # determine plot region aspect ratio; calculate indirectly using
      # fin and plt
      aspPlotRegion <-
        (par()$fin[1]*diff(plt[1:2])) / (par()$fin[2]*diff(plt[3:4]));
      if (is.infinite(aspPlotRegion))
        stop("Plot region must have an area greater than 0.\n");

      # determine desired aspect ratio; xyRatio != 1 only when proj == "LL"
      aspPolySet <- diff(xlim) / diff(ylim) * xyRatio;
      if (is.numeric(projection))
        aspPolySet <- aspPolySet / projection;
      if (is.infinite(aspPolySet) || (aspPolySet == 0))
        stop(paste(
"Either 'projection' is 0 or 'xlim'/'ylim' specify a region with an area",
"of 0.\n", sep="\n"));

      # adjust 'plt' to honour the aspect ratio
      if (aspPlotRegion < aspPolySet) {
        # shrink the Y direction
        pinX <- par()$fin[1]*diff(plt[1:2]);
        pinY <- pinX * aspPolySet^-1;
        toMove <- (diff(plt[3:4]) - pinY/par()$fin[2]) / 2;
        plt[3:4] <- plt[3:4] + c(toMove, -toMove);
      } else if (aspPlotRegion > aspPolySet) {
        # shrink the X direction
        pinY <- par()$fin[2]*diff(plt[3:4]);
        pinX <- pinY * aspPolySet;
        toMove <- (diff(plt[1:2]) - pinX/par()$fin[1]) / 2;
        plt[1:2] <- plt[1:2] + c(toMove, -toMove);
      }

      # set the plot region indirectly via mai/mar
      # mai: (bottom, left, top, right)
      # fin: (width, height)
      # plt: (left, right, bottom, top)
      # If out-of-sync, may issue a warning re: plot specified in inches
      # too large.
      par(mai=c(par()$fin[2]*plt[3], par()$fin[1]*plt[1],
            par()$fin[2]*(1-plt[4]), par()$fin[1]*(1-plt[2])));

      # verify that it set OK; if not, set it directly via plt
      parPlt <- signif(as.double(par()$plt), digits=5);
      locPlt <- signif(as.double(plt), digits=5);
      if (any(parPlt != locPlt))
        par(plt = plt);
    }
  # if projection is NA, continue at this point with no changes to
  # the plot region

  # must set 'usr' again because changing the other par() parameters changes it
  par(usr = c(xlim, ylim));

  invisible(NULL);
}

#==============================================================================
.insertNAs <- function(polys, idx) {
  # extract the polys of interest
  sel.polys <- polys[is.element(names(polys), idx)]
  # NOTE: the above code is _much_ faster in S-PLUS than an alternative...
  #  sel.polys <- polys[as.character(idx)]
  # ... which I tried first
  if (is.null(unlist(sel.polys)))
    return (NA)

  # lapply's are slower in S-PLUS than R; the next line is a bottle-neck
  # in S-PLUS (while having minimal impact on R runtime)
  lenPolys <- lapply(sel.polys, "length")
  nPolys <- length(lenPolys)

  # create a vector T, F, ..., T
  TFT <- rep(c(TRUE, FALSE), length.out=(nPolys * 2) - 1)

  # create a vector with the number of vertices/polygon, with each separated
  # by a 1
  reps <- rep(1, len=(nPolys * 2) - 1)
  reps[TFT] <- unlist(lenPolys)

  # create a vector that can select all of the NAs that seperate polygons
  # in the "new.polys" vector
  NAs <- rep(!TFT, times=reps)

  new.polys <- vector(length=(length(unlist(sel.polys)) + nPolys - 1))
  new.polys[NAs] <- NA
  new.polys[!NAs] <- unlist(sel.polys)

  return (new.polys)
}


## .is.in-------------------------------2018-09-06
## Check to see if a set of vertices occurs in a
## polygon (based on sp::point.in.polygon C code)
## ------------------------------------------NB|RH
.is.in =function(events, polys, use.names=TRUE)
{
	if (!is.PolySet(polys))
		stop("Supply a PolySet to argument 'polys'")
	if (!all(c(is.element(c("X","Y"),colnames(events)),is.element(c("X","Y"),colnames(polys)))))
		stop("Objects 'events' and 'polys' must have column names 'X' and 'Y'")

	## Code lifted from NB's code in `findPolys'
	## Create the data structure that the C function expects
	if (!is.EventData(events))
		events  = as.EventData(data.frame(EID=1:nrow(events), events), projection=attributes(polys)$projection)
	inEvents   = nrow(events)
	inEventsID = events$EID
	inEventsXY = c(events$X, events$Y)

	inPolys = nrow(polys)
	if (!is.element("SID", names(polys))) {
		inPolysID = c(polys$PID, integer(length=inPolys), polys$POS)
	} else {
		inPolysID = c(polys$PID, polys$SID, polys$POS)
	}
	inPolysXY = c(polys$X, polys$Y)

	## The maximum number of rows in the results occurs when each event
	## lies in all of the polygons; we can easily calculate this case
	## but it often results in too much allocated memory

	## Instead, simply use an argument that the user can tune (this makes
	## the behavior more consistent with joinPolys, too)
	outCapacity <- nrow(events)

	## Call the C function
	results <- .C("findPolys",
		inEventsID = as.integer(inEventsID),
		inEventsXY = as.double(inEventsXY),
		inEvents = as.integer(inEvents),
		inPolysID = as.integer(inPolysID),
		inPolysXY = as.double(inPolysXY),
		inPolys = as.integer(inPolys),
		outID = integer(4 * outCapacity),
		outRows = as.integer(outCapacity),
		outStatus = integer(1),
		PACKAGE = "PBSmapping")
	## Note: outRows is set to how much space is allocated -- the C function should consider this

	## Determine the number of rows in the result
	outRows <- as.vector(results$outRows)
	if (outRows == 0) {
		e.out  = events
		e.in   = events[0,]
		e.bdry = 0
	} else {
		## Extract the data from the C function results
		d <- data.frame(EID = results$outID[1:outRows],
			PID = results$outID[(outCapacity+1):(outCapacity+outRows)],
			SID = results$outID[(2*outCapacity+1):(2*outCapacity+outRows)],
			Bdry = results$outID[(3*outCapacity+1):(3*outCapacity+outRows)])
		e.in  = events[is.element(events$EID,d$EID),]
		e.out = events[!is.element(events$EID,d$EID),]
		e.bdry = d$Bdry
	}
	out= list()
	out[["e.in"]]     = e.in
	out[["e.out"]]    = e.out
	out[["all.in"]]   = nrow(e.in)==nrow(events)
	out[["all.out"]]  = nrow(e.out)==nrow(events)
	out[["all.bdry"]] = all(e.bdry==1)
	return(out)
#browser();return()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.is.in


## .is.in.defunct-------------------------------2018-08-24
## Check to see if a set of vertices occurs in a
## polygon (based on sp::point.in.polygon C code)
.is.in.defunct =function(events, polys, use.names=TRUE) {
	if (!all(c(is.element(c("X","Y"),colnames(events)),is.element(c("X","Y"),colnames(polys)))))
		stop("Objects 'events' and 'polys' must have column names 'X' and 'Y'")
	.checkRDeps(".is.in", c("sp"))
	eval(parse(text="in.out = .Call(\"R_point_in_polygon_sp\", as.numeric(events[,\"X\"]), as.numeric(events[,\"Y\"]), as.numeric(polys[,\"X\"]), as.numeric(polys[,\"Y\"]), PACKAGE = \"sp\")"))
	if (use.names) {
		if(is.PolySet(events) || is.PolyData(events))
			names(in.out) = .createIDs(events, intersect(c("PID","SID"), colnames(events)))
		if(is.EventData(events))
			names(in.out) = events[,"EID"]
	}
	out = list()
	out[["in.out"]] = in.out
	out[["all.in"]] = all(as.logical(in.out))
	return(out)
#browser();return()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.is.in.defunct

#==============================================================================
.mat2df <- function(data) {
  # Convert matrices to data.frames, preserving certain attributes.
  # Attributes to ignore: dim, dimnames, class
  pbsClass <- intersect(attributes(data)$class, c("EventData", "LocationSet",
              "PolyData", "PolySet"));
  attrNames <- names(attributes(data));
  if(all(is.element(attrNames, c("dim", "dimnames", "class")) == TRUE)) {
    addValues <- NULL;
  } else {
    addNames <- setdiff(attrNames, c("dim", "dimnames", "class"));
    addValues <- attributes(data)[addNames];
  }
  data <- data.frame(unclass(data));
  if(!is.null(addValues)) {
    attributes(data) <- c(attributes(data), addValues);
  }
  if(length(pbsClass) > 0) {
    attr(data, "class") <- c(pbsClass, "data.frame");
  }
  return(data);
}

#==============================================================================
.rollupPolys <- function(polys, rollupMode, exteriorCCW, closedPolys,
                         addRetrace)
  # rollupMode: method for rolling up the PolySet; essentially controls when
  #   to introduce a new PID (or PID/SID)
  #    1 = roll-up to the PID level (only PIDs in the result)
  #    2 = roll-up to the outer contour level (only outer contours in the
  #        result)
  #    3 = do not roll-up
  #
  # exteriorCCW: modify vertices orientation (CW/CCW)?
  #   -1 = don't modify
  #    0 = exterior should be CW
  #   +1 = exterior should be CCW
  #
  # closedPolys: whether the last and first vertices should be the same
  #   -1 = don't modify
  #    0 = ensure polygons do not close
  #   +1 = close the polygons
  #
  # addRetrace: determines whether it adds retrace lines to the first vertex
  # of the parent after outputting a child
  #    0 = don't add
  #    1 = add
  #
  # Note: does not validate the PolySet
  #
  # Returns: PolySet (invisible) or NULL (invisible)
{
  # save the attributes for the data frame (.validatePolySet returns a data
  # frame)
  attrNames <- setdiff(names(attributes(polys)),
                       c("names", "row.names", "class"));
  attrValues <- attributes(polys)[attrNames];

  inRows <- nrow(polys);
  outCapacity <- 2 * inRows;

  # create the data structure that the C function expects
  if (!is.element("SID", names(polys))) {
    inID <- c(polys$PID, integer(length = inRows));
  } else {
    inID <- c(polys$PID, polys$SID);
  }
  inXY <- c(polys$X, polys$Y);

  results <- .C("rollupPolys",
                inID = as.integer(inID),
                inPOS = as.double(polys$POS),
                inXY = as.double(inXY),
                inVerts = as.integer(inRows),
                outID = integer(3 * outCapacity),
                outXY = double(2 * outCapacity),
                outRows = as.integer(outCapacity),
                rollupMode = as.integer(rollupMode),
                exteriorCCW = as.integer(exteriorCCW),
                closedPolys = as.integer(closedPolys),
                addRetrace = as.integer(addRetrace),
                outStatus = integer(1),
                PACKAGE = "PBSmapping");
  # note: outRows is set to how much space is allocated -- the C function
  #       should take this into consideration

  if (results$outStatus == 1) {
    stop(
"Insufficient physical memory for processing.\n");
  }
  if (results$outStatus == 2) {
    stop(paste(
"Insufficient memory allocated for output.  Please upgrade to the latest",
"version of the software, and if that does not fix this problem, please",
"file a bug report.\n",
               sep = "\n"));
  }
  if (results$outStatus == 3) {
    stop(paste(
"Unable to rollup the polygons, as one or more children did not have a",
"parent.\n"));
  }

  # determine the number of rows in the result
  outRows <- as.vector(results$outRows);

  # extract the data from the C function results
  if (outRows > 0) {
    d <- data.frame(PID = results$outID[1:outRows],
                    SID = results$outID[(outCapacity+1):(outCapacity+outRows)],
                    POS = results$outID[(2*outCapacity+1):(2*outCapacity+outRows)],
                    X = results$outXY[1:outRows],
                    Y = results$outXY[(outCapacity+1):(outCapacity+outRows)]);

    if (!is.element("SID", names(polys)) || rollupMode == 1)
      d$SID <- NULL;

    # restore the attributes
    attributes(d) <- c(attributes(d), attrValues);

    invisible(d);
  } else {
    invisible(NULL);
  }
}

#=====================================================================2018-06-05
# Default for 'projection' must be logical type.
# For 'plotPoints()', 'cex'and 'pch' are part of '...' -- and they may equal
# NULL; when they equal NULL, don't try adding them to par()!
.plotMaps <- function(polys, xlim, ylim, projection, plt, polyProps,
                      border, lty, col, colHoles, density, angle, bg,
                      axes, tckLab, tck, tckMinor, isType, ...)
{
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # PART 1: deal with the '...' business; backup original 'par' values, and set
  #   values specified in '...' in 'par'
  # DO NOT backup 'new' (and some others)
  legalNames <- c("adj", "ann", "ask", "bg", "bty", "cex", "cex.axis",
                  "cex.lab", "cex.main", "cex.sub", "col", "col.axis",
                  "col.lab", "col.main", "col.sub", "crt", "csi", "err",
                  "exp", "fg", "font", "font.axis", "font.lab",
                  "font.main", "font.sub", "lab", "las", "lty",
                  "lwd", "mgp", "mkh", "pch", "smo", "srt", "tck",
                  "tcl", "tmag", "type", "xaxp", "xaxs", "xaxt", "xpd",
                  "yaxp", "yaxs", "yaxt");

  if (!is.null(version$language) && (version$language == "R")) {
    legalNames <- setdiff(legalNames, "csi");   # read-only in R
  }
  legalNames <- intersect(legalNames, names(par()));

  backupPar <- par(legalNames);
  on.exit(par(backupPar));

  dots <- list(...);

  extraArgs <- setdiff(names(dots), legalNames);
  extraArgs <- setdiff(extraArgs, c("main", "sub", "type", "xlab", "ylab"));
  if (length(extraArgs) > 0) {
    warning(paste(
"Ignored unrecognized argument '", paste(extraArgs, collapse="', '"), "'.\n",
                  sep = ""));
  }

  # special case: since they aren't explicit parameters, they end up being
  # part of '...'; but they may equal NULL (invalid values); remove them, since
  # they are explicitly passed into addPoints(), where they become part
  # of 'polyProps'
  if (isType == "points")
    legalNames <- setdiff(legalNames, c("cex", "pch"));

  par(dots[intersect(names(dots), legalNames)]);

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # PART 2: set 'projection' and 'labelprojection' to the appropriate values
  #  Since we validate here, it's unnecessary to validate when this function
  #    calls 'addLines'/'addPolys'

  # although 'projection' cannot equal NA from caller, we may set it
  # to NA within this function (to mean unprojected)
  if (is.null(projection) || is.na(projection))
    projection <- FALSE;

  if (is.logical(projection)) {
    if (projection) {
      if (!is.null(attr(polys, "projection"))) {
        projection <- attr(polys, "projection");
      } else {
        projection <- 1;
        warning(
"'projection' set to 1:1 since unspecified 'projection' argument/attribute.\n");
      }
    } else {
      projection <- NA;
    }
  } else {
    # user specified 'projection': should be numeric, "LL", "UTM", and should
    # not conflict with 'polys'
    if (is.numeric(projection) || is.element(projection, c("LL", "UTM"))) {
      if (!is.null(attr(polys, "projection")) &&
          !is.element(projection, attr(polys, "projection"))) {
        projection <- attr(polys, "projection");
        warning(
"'projection' argument overwritten with PolySet's 'projection' attribute.\n");
      }
    } else {
      stop(paste(
"Either omit 'projection' argument or set it to a numeric value, \"LL\", or",
"\"UTM\".\n",
                 sep="\n"));
    }
  }
  # prevent duplicate warnings on call to 'addLines'/'addPolys'
  if (!is.null(polys))
    attr(polys, "projection") <- projection;
  labelProjection <- projection;

  # 'projection' type no longer 'logical'; now
  #   NA: no projection
  #   numeric: specified projection
  #   "LL": longitude-latitude
  #   "UTM": UTM

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # PART 3: get on with the routine

  # special case of NULL PolySet
  if (is.null(polys)) {
    if (is.null(xlim) || is.null(ylim) || is.null(projection)) {
      stop(
"To plot a NULL PolySet, pass 'xlim', 'ylim', and 'projection' arguments.\n");
    }
  }
  else {
    # as far as this function is concerned, 'polys' must have an X/Y, and
    # that's it; do a basic check
    polys <- .validateXYData(polys);
    if (is.character(polys)) stop(paste("Invalid PolySet.\n", polys, sep=""));
  }

  # detect limits if necessary
  if (is.null(xlim)) xlim <- range(polys$X);
  if (is.null(ylim)) ylim <- range(polys$Y);

  # check 'dots' for 'type'
  if (is.element("type", names(dots))) {
    if (dots$type == "n") {
      # set 'polys' to NULL since we've decided that type == "n" is equivalent
      # to passing a NULL PolySet
      polys <- NULL;
    } else {
      stop(
"Either omit 'type' argument or set it to \"n\".\n");
    }
  }

  # set 'col' parameter appropriately for adding labels/etc.
  if (!is.null(polys)) {
    par(col = 1);
  } else {
    if (length(col) > 1) {
      stop(paste(
"Either omit 'col' argument or set it to a single-element vector when 'polys'",
"equals NULL or 'type = \"n\"'.\n",
                 sep = "\n"));
    } else if (!is.null(col)) {
      par(col = col);
    }
  }

  # save settings in 'options'
  options(map.xlim = xlim);
  options(map.ylim = ylim);
  options(map.projection = projection);

  # create plot region
  .initPlotRegion(projection=projection, xlim=xlim, ylim=ylim, plt=plt);

  ## plot background colour
  ## note (180605): RH had to specify xpd=TRUE to get `polygon' to honour the pin regions on Windows devga device.
  ## xpd: a logical value or NA. 
  ##      if FALSE, all plotting is clipped to the plot region (appears to be buggy), 
  ##      if TRUE,  all plotting is clipped to the figure region (adopted herein), and 
  ##      if NA, all plotting is clipped to the device region (can also use this but overkill).
  if (!is.null(bg))
    polygon(x=xlim[c(1,2,2,1)], y=ylim[c(1,1,2,2)], col=bg, border=0, xpd=TRUE);

  # plot PolySet 'polys'
  if (!is.null(polys)) {
    if (isType == "polygons") {
      # add polygons
      ret <- addPolys(polys, xlim = xlim, ylim = ylim, polyProps = polyProps,
                      border = border, lty = lty, col = col, colHoles = colHoles,
                      density = density, angle = angle);
    } else if (isType == "lines")  {
      # add lines
      ret <- addLines(polys, xlim = xlim, ylim = ylim, polyProps = polyProps,
                      lty = lty, col = col);
    } else if (isType == "points") {
      cex <- list(...)$cex;     # these were passed in '...'
      pch <- list(...)$pch;
      ret <- addPoints(polys, xlim = xlim, ylim = ylim, polyProps = polyProps,
                       cex = cex, col = col, pch = pch);
    } else {
      stop(
"Unrecognized 'isType'.\n");
    }
  } else {
    ret <- NULL;
  }

  if (axes) {
    .addAxis(xlim = xlim, ylim = ylim, tckLab = tckLab, tck = tck,
             tckMinor = tckMinor, ...);
  }
  else {
    options(map.xline = 1);
    options(map.yline = 1);
  }

  # labels must go after axis
  .addLabels(projection = labelProjection, ...);

  if (axes) {
    # since R won't plot outside the figure region, we don't want the
    # box to have a thicker line width
    box();
  }

  invisible(ret);
}

#==============================================================================
# .preparePolyProps:
#   Performs at least the following tasks:
#   1) creates 'polyProps' if it equals NULL
#   2) adds an SID column to 'polyProps' if one exists in 'polys'
#   3) removes from 'polyProps' any PIDs that do not exist in 'polys'
.preparePolyProps <- function(polysPID, polysSID, polyProps)
{
  # make 'polyProps' if necessary
  if (is.null(polyProps)) {
    polyProps <- data.frame(PID = unique(polysPID));
  } else {
    polyProps <- .validatePolyData(polyProps);
    if (is.character(polyProps))
      stop(paste("Invalid PolyData 'polyProps'.\n", polyProps, sep=""));
  }

  # if SIDs in 'polys' but not 'polyProps', add SIDs from 'polys' into
  # 'polyProps'
  if (!is.null(polysSID) && !is.element("SID", names(polyProps))) {
    # identify unique PIDs/SIDs within "polys"
    p <- data.frame(PID=polysPID, SID=polysSID);
    p <- p[!duplicated(.createIDs(p, cols = c("PID", "SID"))), ]

    # only keep PIDs that appear in 'polyProps'
    p <- p[is.element(p$PID, unique(polyProps$PID)), ];

    # add the SIDs to 'polyProps'
    polyProps <- merge(polyProps, p, by="PID");
  }
  return (polyProps);
}


## .validateData------------------------2018-09-10
## An element of noNACols and keyCols will only be 
## used if it exists in the data. To ensure it 
## exists in the data, make it a requiredCol.
## ------------------------------------------NB/RH
.validateData <- function(data, className,
	requiredCols=NULL, requiredAttr=NULL, noFactorCols=NULL,
	noNACols=NULL, keyCols=NULL, numericCols=NULL)
{
	fnam = as.character(substitute(data))
	## convert matrix to data frame
	if (is.matrix(data)) {
		data <- .mat2df(data);
	}

	if (is.data.frame(data) && (nrow(data) > 0)) {
		## validate optional class name
		if (!is.null(className) && (class(data)[1] != "data.frame")) {
			if (class(data)[1] != className) {
				return(paste("Unexpected class (", class(data)[1], ").\n", sep=""));
			}
		}

		## ensure all the required columns exist in the PolySet
		if (!is.null(requiredCols) && !all(is.element(requiredCols, names(data)))) {
			return(paste("One or more of the required columns is missing.\n",
			"Required columns: ", paste(requiredCols, collapse = ", "), ".\n", sep=""));
		}

		## ensure all the required attributes exists in the PolySet
		if (!is.null(requiredAttr) && !all(is.element(requiredAttr, names(attributes(data))))) {
			return(paste("One or more of the required attributes is missing.\n",
			"Required attributes: ", paste(requiredAttr, collapse = ", "), ".\n", sep=""));
		}

		## check for NAs
		presentCols <- intersect(noNACols, names(data));
		if (length(presentCols) > 0) {
			# build an expression
			exprStr <- paste(paste("any(is.na(data$", presentCols, "))", sep=""), collapse=" || ");
			if (eval(parse(text=exprStr))) {
				return(paste("One or more columns (where NAs are not allowed) contains NAs.\n",
				"Columns that cannot contain NAs: ", paste(presentCols, collapse = ", "),
				".\n", sep=""));
			}
		}

		## check for factors
		presentCols <- intersect(noFactorCols, names(data));
		if (length(presentCols) > 0) {
			# build an expression
			exprStr <- paste(paste("is.factor(data$", presentCols, ")", sep=""), collapse=" || ");
			if (eval(parse(text=exprStr))) {
				return(paste("One or more columns contains factors where they are not allowed.\n",
				"Columns that cannot contain factors: ", paste(presentCols, collapse = ", "),
				".\n", sep=""));
			}
		}

		## check for uniqueness of the keys
		presentCols <- intersect(keyCols, names(data));
		if (length(presentCols) > 0) {
			if (length(presentCols) == 1) {
				keys <- data[[presentCols]];
			} else if ((length(presentCols) == 2) && ((length(intersect(presentCols, c("PID","SID","POS","EID"))) == 2)
				 || (all(is.integer(data[[presentCols[1]]])) && all(is.integer(data[[presentCols[2]]]))))) {
				## additional tests above to "ensure" the two columns contain integers
				keys <- .createIDs(data, cols=presentCols);
			} else {
				## paste the columns together
				#exprStr <- paste0("paste(", paste0(paste0("data$", presentCols), collapse=", "),");");## no longer seems to work
				#keys <- eval(parse(text=exprStr));
				keys = apply(data[,presentCols],1,paste0,collapse=".")
			}
			## at this point, 'keys' is a vector
			if (any(duplicated(keys))) {
#browser();return()
				return(paste("The 'key' for each record is not unique.\n",
				"Columns in key: ", paste(presentCols, collapse = ", "), ".\n", sep=""));
			}
		}

		## check for numeric columns
		presentCols <- intersect(numericCols, names(data));
		if (length(presentCols) > 0) {
			exprStr <- paste(paste("any(!is.numeric(data$", presentCols, "))", sep=""), collapse=" || ");
			if (eval(parse(text=exprStr))) {
				return(paste("One or more columns requires numeric values, but contains non-numerics.\n",
				"Columns that must contain numerics: ", paste(presentCols, collapse = ", "),
				".\n", sep=""));
			}
		}

		## Check for increasing/descreasing POS
		if (!is.null(className) && className == "PolySet") {
			idx <- .createIDs(data, cols=c("PID", "SID"));
			idxFirst <- which(!duplicated(idx));
			idxLast <- c((idxFirst-1)[-1], length(idx));
			## identify the holes
			holes <- (data$POS[idxFirst] > data$POS[idxLast])
			## outer/inner contour indices
			idxOuter <- rep(!holes, times=((idxLast-idxFirst)+1))
			idxInner <- !idxOuter;

			# POS[i] < POS[i+1]?
			lt <- c(data$POS[1:(nrow(data)-1)] < data$POS[2:(nrow(data))], FALSE);

			## Check outer contours; change last vertex of each polygon to
			## what we expect for valid outer contours
			lt[idxLast] <- TRUE;
			## Check for any that aren't in order
			j <- any(!lt[idxOuter])
			if (j) {
				j <- !lt;
				j[idxInner] <- FALSE;
				# add 1 because it's actually the next row that break it
				j <- which(j) + 1;
				return(paste("POS column must contain increasing values for outer contours.\n",
				"Offending rows: ", paste(j, collapse=", "), ".\n",	sep=""));
			}

			## Check inner contours; change last vertex of each polygon to what
			## we expect for valid inner contours
			lt[idxLast] <- FALSE;
			# check for any that aren't in order
			j <- any(lt[idxInner])
			if (j) {
				j <- lt;
				j[idxOuter] <- FALSE;
				# do not add 1
				j <- which(j);

				return(paste("POS column must contain decreasing values for inner contours.\n",
				"Offending rows: ", paste(j, collapse=", "), ".\n",	sep=""));
			} ## end if j

			## Check for orphaned holes or hole vertices outside their solids
			## Note: takes a long time if there are thousand of holes (e.g. 'rca')
			## Originally reported all holes but now only reports bad hole vertices
			## (any hole vertex outside the enclosing solid)
			if (any(class(data) == "PolySet")) {
				polys = data
				polys$idx <- .createIDs(polys, cols=c("PID", "SID"));
				names(holes) = unique(idx) ## assume parallelism from above
				if (any(holes)) {
					solids = !holes
					snams = names(solids)[solids]
					snums = (1:length(solids))[solids]
					plist = list()
					for (i in 1:length(snums)) {
						ii  = snums[i]
						iii = snams[i]
						if (iii==rev(names(solids))[1] || solids[ii+1]) next
						endhole = ifelse (is.na(snums[i+1]) && rev(holes)[1], length(holes), (snums[i+1]-1))
						iholes = holes[(ii+1):endhole]
						if (!all(iholes)) stop ("Check '.validateData' code")
						jjj = names(iholes)
						jdx = idx[is.element(idx,jjj)]
						isolid = polys[is.element(polys$idx,iii),]
	
						ihole  = polys[is.element(polys$idx, jjj),]
						e.in.p = .is.in(ihole, isolid)
						if (e.in.p$all.in) next
						e.bad  = e.in.p$e.out
	
						plist[[iii]] = list()
						plist[[iii]][["solid"]] = isolid
						plist[[iii]][["hole.vout"]] = list()
						j.bad = split(e.bad, e.bad$idx)
						plist[[iii]][["hole.vout"]] = j.bad
	#if (length(j.bad)>1) {browser();return()}
					}
	#browser();return()
					if (length(plist)>0) {
						assign ("solids_holes", plist,  envir=.PBSmapEnv)
						swo  = sapply(plist, function(x){length(x$hole.vout)})  ## solids with # of orphan holes
						## solids with # of holes that have vertices outside enclosing solids (originally 'solids with orphans')
						swo  = sapply(plist, function(x){nh=length(x$hole.vout); nv=sapply(x$hole.vout,nrow); paste0(nh, " hole",ifelse(nh>1,"s",""),": ", paste0("[",names(nv),"]=", nv,"v",collapse="; "))})
						swo.txt = paste0("   Solid ",names(swo)," --> ", swo, collapse="\n")
						cat(paste0("\n******* WARNING *******\n", fnam, ": Hole vertices (v) exist outside solids [PID.SID]:\n", swo.txt, "\nSee object '.PBSmapEnv$solids_holes' for details.\n\n"))
					}
				} ## end if any holes
			} ## end if class==PolySet
#browser();return()
		} ## end if className==PolySet
	} else {
		return(paste("The object must be either a matrix or a data frame.\n"));
	}
	return(data);
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.validateData

#==============================================================================
.validateEventData <- function(EventData)
  # Perform some simple tests on the object to see if it can possibly be
  # an EventData object.
  # If the object is invalid, returns the error message.
{
  return(.validateData(EventData,
                       className = "EventData",
                       requiredCols = c("EID", "X", "Y"),
                       requiredAttr = NULL,
                       noFactorCols = c("EID", "X", "Y"),
                       noNACols = c("EID", "X", "Y"),
                       keyCols = c("EID"),
                       numericCols = c("EID", "X", "Y")));
}

#==============================================================================
.validateLocationSet <- function(LocationSet)
  # Perform some simple tests on the object to see if it can possibly be
  # an EventData object.
  # If the object is invalid, returns the error message.
{
  return(.validateData(LocationSet,
                       className = "LocationSet",
                       requiredCols = c("EID", "PID", "Bdry"),
                       requiredAttr = NULL,
                       noFactorCols = c("EID", "PID", "SID", "Bdry"),
                       noNACols = c("EID", "PID", "SID", "Bdry"),
                       keyCols = c("EID", "PID", "SID"),
                       numericCols = c("EID", "PID", "SID")));
}

#==============================================================================
.validatePolyData <- function(PolyData)
  # Perform some simple tests on the object to see if it can possibly be
  # a PolyData object.
  # If the object is invalid, returns the error message.
{
  return(.validateData(PolyData,
                       className = "PolyData",
                       requiredCols = c("PID"),
                       requiredAttr = NULL,
                       noFactorCols = c("PID", "SID"),
                       noNACols = c("PID", "SID"),
                       keyCols = c("PID", "SID"),
                       numericCols = c("PID", "SID")));
}

#==============================================================================
# 'polyProps' can be event properties, rather than polygon properties
.validatePolyProps <- function(polyProps, parCols = NULL)
{
  if (is.null(polyProps))
    return (NULL);

  return (.validateData(polyProps,
                        className = "PolyData",
                        requiredCols = NULL,            # PID/SID or EID
                        requiredAttr = NULL,
                        noFactorCols = parCols,
                        noNACols = NULL,
                        keyCols = NULL,
                        numericCols = NULL));
}

#==============================================================================
.validatePolySet <- function(polys)
  # Perform some simple tests on the object to see if it can possibly be
  # a PolySet object.
  # If the object is invalid, returns the error message.
{
  return(.validateData(polys,
                       className = "PolySet",
                       requiredCols = c("PID", "POS", "X", "Y"),
                       requiredAttr = NULL,
                       noFactorCols = c("PID", "SID", "POS", "X", "Y"),
                       noNACols = c("PID", "SID", "POS", "X", "Y"),
                       keyCols = c("PID", "SID", "POS"),
                       numericCols = c("PID", "SID", "POS", "X", "Y")));
}

#==============================================================================
.validateXYData <- function(xyData)
  # Perform some simple tests on the object to see if it can possibly be
  # an xyData object.
  # If the object is invalid, returns the error message.
{
  return(.validateData(xyData,
                       className = NULL,
                       requiredCols = c("X", "Y"),
                       requiredAttr = NULL,
                       noFactorCols = c("X", "Y"),
                       noNACols = c("X", "Y"),
                       keyCols = NULL,
                       numericCols = c("X", "Y")));
}

