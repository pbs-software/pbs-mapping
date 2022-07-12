/*=============================================================================
  Copyright (C) 2003-2022 Fisheries and Oceans Canada

  This file is part of PBS Mapping.

  PBS Mapping is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  PBS Mapping is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with PBS Mapping; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
=============================================================================*/

#ifndef __PBSMAPPING_H_
#define __PBSMAPPING_H_

#include <R.h>
#include <Rinternals.h>

#include "globals.h"

/*-----------------------------------------------------------------------------
  calcArea:
    This function calculates the areas of a set of polygons.
    It handles holes, but has no concept of projection.

  Author:  Nicholas Boers (July 11, 2003)

  Capabilities:
    [Y] Holes
    [>] Projection
        [N] LL
        [Y] UTM
        [Y] 1:1
  Robustness:
    [?] Floating-point
  Legend:
    [Y] yes   [N] no   [-] N/A   [?] good question   [>] details

  Notes:
    - maximum space required for out*: length(unique(paste(polys$PID,
                                                           polys$SID)))
  ---------------------------------------------------------------------------*/
void calcArea(PBSINT *inID, double *inXY, PBSINT *inVerts, PBSINT *outID,
     double *outArea, PBSINT *outVerts, PBSINT *status);

/*-----------------------------------------------------------------------------
  calcCentroid:
    This function calculates the centroids of a set of polygons.
    Since it uses signed area calculations, it can handle holes if they are
    properly described (in a CW/CCW GIS sense) in the PolySet, and they are
    "integrated" (holes have the same PID/SID as their parent). It has no
    concept of projection.

  Author:  Nicholas Boers (June 10, 2004)

  Capabilities:
    [>] Holes
        Yes in certain circumstances (see note above).
    [-] Projection
  Robustness:
    [?] Floating-point
  Legend:
    [Y] yes   [N] no   [-] N/A   [?] good question   [>] details

  Notes:
    - maximum space required for out*: length(unique(paste(polys$PID,
                                                           polys$SID)))
  ---------------------------------------------------------------------------*/
void calcCentroid(PBSINT *inID, double *inXY, PBSINT *inVerts, PBSINT *outID,
     double *outXY, PBSINT *outVerts, PBSINT *status);

/*-----------------------------------------------------------------------------
  calcOrientation:
    This function calculates the orientation of polygons (i.e., clockwise or
    counter-clockwise).  Holes are irrelevant and it needs no concept of
    projection.

  Author:  Nicholas Boers (June 9, 2004)

  Capabilities:
    [-] Holes
    [-] Projection
  Robustness:
    [?] Floating-point
  Legend:
    [Y] yes   [N] no   [-] N/A   [?] good question   [>] details

  Notes:
    - space required for out*: length(unique(paste(polys$PID, polys$SID)))

  Returns:
    -1 when counter-clockwise
     0 when N/A
    +1 when clockwise
  ---------------------------------------------------------------------------*/
void calcOrientation(PBSINT *inID, double *inXY, PBSINT *inVerts,
     PBSINT *outID, double *outOrientation, PBSINT *outVerts, PBSINT *status);

/*-----------------------------------------------------------------------------
  clip:
    This function clips polygons to a rectangular viewing window.

  Author:  Nicholas Boers (June 11, 2003)

  Implementation Notes:
    For each pair of points that are tested, the _first_ point of the
    pair is added to the final output.  If necessary, an intersection
    with the border is added as well.

  Notes:
    Recommended allocated space for out*: 2 x *inVerts
  ---------------------------------------------------------------------------*/
void clip(PBSINT *inID, double *inXY, PBSINT *inVerts, PBSINT *polygons,
     double *limits, PBSINT *outID, double *outXY, PBSINT *outVerts, PBSINT *status);

/*-----------------------------------------------------------------------------
  closePolys:
    "Fix" the closure of open polygons.

  Author:  Nicholas Boers

  Notes:
    - recommended allocated space for out*:

    - cornerToAdd was developed as follows:
                      EDGE_N (3)
           C_NW (0)                 C_NE (1)
                    ---------------
                    |             |
        EDGE_W (0)  |             |  EDGE_E (1)
                    |             |
                    ---------------
           C_SW (3)                 C_SE (2)
                       EDGE_S (2)

     - from the above picture, create a table:
       START EDGE | END EDGE   | CLOSEST CORNER || 1ST TO ADD | 2ND TO ADD
       -----------|------------|----------------||------------|-----------
       EDGE_W     | EDGE_W     | C_NW           || C_NONE     | C_NONE
       EDGE_W     | EDGE_W     | C_NE           || C_NONE     | C_NONE
       EDGE_W     | EDGE_W     | C_SE           || C_NONE     | C_NONE
       EDGE_W     | EDGE_W     | C_SW           || C_NONE     | C_NONE
       EDGE_W     | EDGE_E     | C_NW           || C_NE       | C_NW
       EDGE_W     | EDGE_E     | C_NE           || C_NE       | C_NW
       EDGE_W     | EDGE_E     | C_SE           || C_SE       | C_SW
       EDGE_W     | EDGE_E     | C_SW           || C_SE       | C_SW
       ...        | ...        | ...            || ...        | ...

     - the table should be completed for all combinations of START EDGE,
       END EDGE, and CLOSEST CORNER

     - the cornerToAdd array must follow the order in the table above, where
       CLOSEST corner is cycled through first for each (START EDGE, END EDGE),
       and then EDGE EDGE is cycled through for each START EDGE
   ---------------------------------------------------------------------------*/
void closePolys(PBSINT *inID, double *inXY, PBSINT *inVerts, double *limits,
     PBSINT *outID, double *outXY, PBSINT *outVerts, PBSINT *status);

/*-----------------------------------------------------------------------------
  convUL:
    Convert Lon/Lat <--> UTME/UTMN.

  Author:  Nicholas Boers

  Notes:
    Maximum space required for out*:
  ---------------------------------------------------------------------------*/
void convUL(double *inXY, PBSINT *inVerts, PBSINT *toUTM, PBSINT *zone,
     PBSINT *southern, double *outXY, PBSINT *outVerts, PBSINT *status);

/*-----------------------------------------------------------------------------
  findCells:
    Identify integers within a vector of break points.

  Author:  Nicholas Boers (Mar. 29, 2006)

  Notes:
    Some ideas from "findInterval()" in R source code.
  ---------------------------------------------------------------------------*/
void findCells(double *inPt, PBSINT *inPts, double *inBrk, PBSINT *inBrks,
     PBSINT *outCell, PBSINT *outBdry, PBSINT *status);

/*-----------------------------------------------------------------------------
  findPolys:
    Locate events within a polyset.

  Author:  Nicholas Boers

  Notes:
    - recommended allocated space for out*:
  ---------------------------------------------------------------------------*/
void findPolys(PBSINT *inEventsID, double *inEventsXY, PBSINT *inEvents,
     PBSINT *inPolysID, double *inPolysXY, PBSINT *inPolys, PBSINT *outID,
     PBSINT *outIDs, PBSINT *status);

/*-----------------------------------------------------------------------------
  isConvex:
    Determines whether a PolySet contains convex polygons.

  Author:  Nicholas Boers (June 30, 2004)

  Status values:
    PBS_SUCCESS:    everything OK
    PBS_ERR_MEM:    insufficient memory
    PBS_ERR_OUT:    output array full

  Notes:
    - maximum allocated space required for out*: number of polygons
  ---------------------------------------------------------------------------*/
void isConvex(PBSINT *inID, double *inXY, PBSINT *inVerts, PBSINT *outID,
     PBSINT *outResult, PBSINT *outVerts, PBSINT *status);

/*-----------------------------------------------------------------------------
  isIntersecting:
    Determines whether the polygons in a PolySet self-intersect.

  Non-standard parameters:
    numericResult: if TRUE, returns a numeric result (a count); otherwise,
      returns a Boolean for whether or not the polygon self-intersects

  Author:  Nicholas Boers (June 28, 2004)

  Status values:
    PBS_SUCCESS:    everything OK
    PBS_ERR_MEM:    insufficient memory
    PBS_ERR_OUT:    output array full

  Notes:
    - maximum allocated space required for out*: number of polygons
    - counts certain types of intersections (i.e., those involving vertices
      and those where an edge retraces over an edge) more than once
  ---------------------------------------------------------------------------*/
void isIntersecting(PBSINT *inID, double *inXY, PBSINT *inVerts,
     PBSINT *numericResult, PBSINT *outID, PBSINT *outResult,
     PBSINT *outVerts, PBSINT *status);

/*-----------------------------------------------------------------------------
  rollupPolys:
    Performs several operations on a PolySet.  See the arguments below for
    further details.

  Non-standard parameters:
    rollupMode: method for rolling up the PolySet
       1 = roll-up to the PID level (only PIDs in the result)
       2 = roll-up to the outer contour level (only outer contours in the
           result)
       3 = do not roll-up

    exteriorCCW: modify vertices orientation (CW/CCW)?
      -1 = don't modify
       0 = exterior should be CW
      +1 = exterior should be CCW

    closedPolys: whether the last and first vertices should be the same
      -1 = don't modify
       0 = ensure polygons do not close
      +1 = close the polygons

    addRetrace: determines whether it adds retrace lines to the first vertex
    of the parent after outputting a child
       0 = don't add
       1 = add

  Author:  Nicholas Boers (June 17, 2004)

  Status values:
    PBS_SUCCESS: everything OK
    PBS_ERR_MEM: insufficient memory
    PBS_ERR_OUT: output array full
    PBS_ERR_OT1: encountered child where not allowed

  Notes:
    - maximum allocated space required for out*:
      *inVerts + 1 (for each it needs to close) + 1 (for each retrace line)
    - recalculates the "POS" column
  ---------------------------------------------------------------------------*/
void rollupPolys(PBSINT *inID, double *inPOS, double *inXY, PBSINT *inVerts,
     PBSINT *outID, double *outXY, PBSINT *outVerts, PBSINT *rollupMode,
     PBSINT *exteriorCCW, PBSINT *closedPolys, PBSINT *addRetrace, PBSINT *status);

/*-----------------------------------------------------------------------------
  thickenPolys:
    This function thickens polygons.

  Author:  Nicholas Boers (June 8, 2004)

  Notes:
    - if units == 0, "LL": tolerance in kilometers and inXY in decimal-degrees
    - if units == 1, other: tolerance and inXY in same units
  ---------------------------------------------------------------------------*/
void thickenPolys(PBSINT *inID, double *inXY, PBSINT *inVerts, double *tolerance,
     PBSINT *filter, PBSINT *units, PBSINT *keepOrig, PBSINT *close,
     PBSINT *outID, double *outXY, PBSINT *outVerts, PBSINT *status);

/*-----------------------------------------------------------------------------
  thinPolys:
    This function thins polygons.

  Author:  Nicholas Boers (May 4, 2004)

  Notes:
    - X and Y are `PBSINT,' rather than the usual double
    - recommended allocated space for out*:
      *inVerts
    - does not renumber POS
    - if units == 0, "LL", and inXY should be in micro-degrees
    - if units == 1, "UTM", and inXY should be in meters
  ---------------------------------------------------------------------------*/
void thinPolys(PBSINT *inID, PBSINT *inXY, PBSINT *inVerts, double *tolerance,
     PBSINT *filter, PBSINT *units, PBSINT *outID, PBSINT *outXY,
     PBSINT *outVerts, PBSINT *status);

#endif /* __PBSMAPPING_H_ */
