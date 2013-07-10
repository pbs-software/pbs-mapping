/*=============================================================================
  Copyright (C) 2003-2013 Fisheries and Oceans Canada

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
/*-----------------------------------------------------------------------------
  File: clipperWrapper.cpp

  Interface between R and the Clipper library.

  Author:  Nicholas Boers
  ---------------------------------------------------------------------------*/
#define DEBUG	0	/* when 1, enable some debug messages */

/* the clipper headers must come first; if the R headers
   (R.h/Rdefines.h) appear before the clipper headers, the library
   will fail to build for R */
#include "clipper.h"
#include "clipperWrapper.h"

#ifdef STANDALONE

/* compiling for standalone execution (NO R) */
#include <stdio.h>
#define Rprintf printf
typedef int Sint;
typedef double Sfloat;

#else /* not defined STANDALONE */

/* compiling for inclusion in an R library */
#include <R.h>
#include <Rdefines.h>

#endif /* not defined STANDALONE */

using namespace std;
using namespace ClipperLib;

struct PolySet {
    vector<Sint> PID;
    vector<Sint> SID;
    vector<Sint> POS;
    vector<Sfloat> X;
    vector<Sfloat> Y;
};

struct PolySetState {
    Sint *PID;
    Sint *SID;
    Sfloat *X;
    Sfloat *Y;
    int n;
    int nextStart;
};

/*-----------------------------------------------------------------------------
  join (Polygons result):

  Author:  Nicholas Boers (Mar. 2013)

  Notes:
  Given subject (subj) and clip (clip) polygons, uses the Clipper
  library to join those polygons using the specified operation.

  This function's result type (Polygons) is suitable for further calls
  to Clipper.
  ---------------------------------------------------------------------------*/
static void
join (const Polygons &subj, enum ClipType op, const Polygons &clip,
      Polygons &result)
{
    Clipper c;

    c.AddPolygons(subj, ptSubject);
    c.AddPolygons(clip, ptClip);
    c.Execute(op, result);
}

/*-----------------------------------------------------------------------------
  join (PolyTree result):

  Author:  Nicholas Boers (Mar. 2013)

  Notes:
  Given subject (subj) and clip (clip) polygons, uses the Clipper
  library to join those polygons using the specified operation.

  This function's result type (PolyTree) is suitable for converting to
  a PolySet.
  ---------------------------------------------------------------------------*/
static void
join (const Polygons &subj, enum ClipType op, const Polygons &clip,
      PolyTree &result)
{
    Clipper c;

    c.AddPolygons(subj, ptSubject);
    c.AddPolygons(clip, ptClip);
    c.Execute(op, result);
}

/*-----------------------------------------------------------------------------
  getNextPolygon:

  Author:  Nicholas Boers (Mar. 2013)

  Notes:
  Given a structure that records the states of processing a PolySet, attempts
  to extract the next polygon from the PolySet.  When extracting a polygon,
  converts floating-point coordinates to integer coordinates.  The PID of
  the extracted polygon is available through the argument pid.

  Returns a polygon with > 0 contours on success (and 0 contours on failure).
  ---------------------------------------------------------------------------*/
static Polygons
getNextPolygon (struct PolySetState &pset, ulong64 scaleFactor, Sint &pid)
{
    Polygons p;
    int sidCount, polyNum;
    Sint lastPID, lastSID;

    /* if we're done... */
    if (pset.nextStart > pset.n) 
	return (Polygons(0));
    
    /* count components */
    sidCount = 1;
    lastPID = pset.PID[pset.nextStart];
    lastSID = pset.SID[pset.nextStart];
    for (int i = pset.nextStart; i < pset.n; i++) {
	if (pset.PID[i] != lastPID)
	    break;
	if (pset.SID[i] != lastSID) {
	    lastSID = pset.SID[i];
	    sidCount++;
	}
    }

    /* allocate memory */
    p = Polygons(sidCount);

    /* load */
    polyNum = 0;
    lastPID = pset.PID[pset.nextStart];
    lastSID = pset.SID[pset.nextStart];
    pid = lastPID;
    do {
	/* advance to next contour */
	if (pset.SID[pset.nextStart] != lastSID) {
	    lastSID = pset.SID[pset.nextStart];
	    polyNum++;
	}

	/* push integer points onto the vector */
	p[polyNum].push_back(
		     IntPoint((long64)(pset.X[pset.nextStart] * scaleFactor),
			      (long64)(pset.Y[pset.nextStart] * scaleFactor)));

	pset.nextStart++;
    } while (pset.nextStart < pset.n && pset.PID[pset.nextStart] == lastPID);

    /* removes self-intersections from p that are created by our approach to
       clipping, which maintains the original PID and does not introduce new
       SIDs */
    SimplifyPolygons(p, p);

    return (p);
}

/*-----------------------------------------------------------------------------
  thereAreMorePolygons:

  Author:  Nicholas Boers (Mar. 2013)

  Notes:
  Returns true if there are more polygons according to our state structure.
  ---------------------------------------------------------------------------*/
static int
thereAreMorePolygons (const struct PolySetState &st)
{
    return (st.nextStart < st.n);
}

/*-----------------------------------------------------------------------------
  appendToResult:

  Author:  Nicholas Boers (Mar. 2013)

  Notes:
  Appends a new PolyTree result to an existing PolySet.  In the process,
  converts integer coordinates to floating-point.  Sets the PolyTree's PID
  according to the argument pid.
  ---------------------------------------------------------------------------*/
static void
appendToResult (PolySet &pset, PolyTree &p, ulong64 scaleFactor, Sint pid)
{
    PolyNode *pn;
    Sint sidCount, posCount;

    /* return if nothing to add */
    if (p.Total() == 0)
	return;

    /* add the vertices */
    sidCount = 0;
    /* posCount will be initialized after we know whether it's a hole */
    pn = p.GetFirst();
    while (pn) {
	/* start a new contour */
	Polygon::iterator it = pn->Contour.begin();
	int isHole = pn->IsHole();

	/* set polygon's initial SID/POS values */
	if (isHole) {
	    sidCount++;
	    posCount = pn->Contour.size();
	} else {
	    sidCount++;
	    posCount = 1;
	}

	/* add the vertices */
	while (it != pn->Contour.end()) {
	    pset.PID.push_back(pid);
	    pset.SID.push_back(sidCount);
	    pset.POS.push_back(isHole ? posCount-- : posCount++);
	    pset.X.push_back((Sfloat)it->X / scaleFactor);
	    pset.Y.push_back((Sfloat)it->Y / scaleFactor);

	    it++;
	}

	/* advance to the next contour */
	pn = pn->GetNext();
    }
} 

#ifndef STANDALONE
/*-----------------------------------------------------------------------------
  PolySetToR:

  Author:  Nicholas Boers (Mar. 2013)

  Notes:
  Given a PolySet structure, produce a data frame for R.  The protectCount
  argument is for recording the number of times that we call PROTECT so that
  we can subsequently UNPROTECT upon exit.
  ---------------------------------------------------------------------------*/
static SEXP
PolySetToR (PolySet &pset, int &protectCount)
{
    /* final result (eventually a data frame) */
    SEXP res = R_NilValue;

    /* columns for the data frame */
    SEXP rPID, rSID, rPOS, rX, rY;

    /* attributes for the data frame */
    SEXP attrClass, attrNames, attrRowNames;

    /* pointers for quickly accessing vectors */
    Sint *rPIDptr, *rSIDptr, *rPOSptr;
    Sfloat *rXptr, *rYptr;

    /* the number of vertices in the PolySet */
    long count = pset.PID.size ();

    /* obtain the new R objects for the data frame and its columns */
    PROTECT(res = NEW_LIST(5));
    PROTECT(rPID = NEW_INTEGER(count));
    PROTECT(rSID = NEW_INTEGER(count));
    PROTECT(rPOS = NEW_INTEGER(count));
    PROTECT(rX = NEW_NUMERIC(count));
    PROTECT(rY = NEW_NUMERIC(count));
    protectCount += 6;

    /* add the vectors to the data frame */
    SET_ELEMENT(res, 0, rPID);
    SET_ELEMENT(res, 1, rSID);
    SET_ELEMENT(res, 2, rPOS);
    SET_ELEMENT(res, 3, rX);
    SET_ELEMENT(res, 4, rY);

    /* obtain the attributes */
    PROTECT(attrClass = NEW_STRING(1));
    PROTECT(attrNames = NEW_STRING(5));
    PROTECT(attrRowNames = NEW_INTEGER(count));
    protectCount += 3;

    /* initialize attributes */
    SET_STRING_ELT(attrClass, 0, mkChar("data.frame"));
    SET_STRING_ELT(attrNames, 0, mkChar("PID"));
    SET_STRING_ELT(attrNames, 1, mkChar("SID"));
    SET_STRING_ELT(attrNames, 2, mkChar("POS"));
    SET_STRING_ELT(attrNames, 3, mkChar("X"));
    SET_STRING_ELT(attrNames, 4, mkChar("Y"));
    for (int i = 0; i < count; i++)
	INTEGER_POINTER(attrRowNames)[i] = i;

    /* attach the attributes */
    SET_ATTR(res, R_RowNamesSymbol, attrRowNames);
    SET_NAMES(res, attrNames);
    classgets(res, attrClass);

    /* set up our pointers for loading the actual data */
    rPIDptr = INTEGER_POINTER(rPID);
    rSIDptr = INTEGER_POINTER(rSID);
    rPOSptr = INTEGER_POINTER(rPOS);
    rXptr = NUMERIC_POINTER(rX);
    rYptr = NUMERIC_POINTER(rY);

    /* load the data */
    for (unsigned long i = 0; i < pset.PID.size(); i++) {
	*rPIDptr++ = pset.PID[i];
	*rSIDptr++ = pset.SID[i];
	*rPOSptr++ = pset.POS[i];
	*rXptr++ = pset.X[i];
	*rYptr++ = pset.Y[i];
    }

    /* return the result */
    return (res);
}
#endif /* not defined STANDALONE */

/*-----------------------------------------------------------------------------
  countPolygons:

  Author:  Nicholas Boers (Mar. 2013)

  Notes:
  Count the polygons in a PolySet based on PID/SID values; this procedure
  counts each hole as a polygon.  If onlyPID is true, use only the PID field
  when counting.
  ---------------------------------------------------------------------------*/
static long
countPolygons (Sint *pid, Sint *sid, Sint n, int onlyPID)
{
    Sint lastPID, lastSID, count;

    /* return 0 if given no vertices */
    if (n == 0)
	return 0;

    /* iterate through the vertices and count */
    lastPID = pid[0];
    lastSID = sid[0];
    count = 1;
    for (Sint i = 1; i < n; i++) {
	if (lastPID != pid[i] || (!onlyPID && lastSID != sid[i])) {
	    lastPID = pid[i];
	    lastSID = sid[i];
	    count++;
	}
    }

    /* return the count */
    return (count);
}

/*-----------------------------------------------------------------------------
  determineScale:

  Author:  Nicholas Boers (Mar. 2013)

  Notes:
  Determine the number of bits we can (left) shift by when converting
  floating-point coordinates to integer coordinates; later, we'll
  multiply by 2**shift rather than shift (given the floating-point
  coordinates).
  ---------------------------------------------------------------------------*/
static int
determineScale (const Sfloat *f, Sint n)
{
    ulong64 mask = ~(0LL);
    int count = 0;

    /* if n == 0; won't clear any bits in mask and it will return 63 */

    /* go through each floating-point value, convert it to an unsigned
       64-bit integer, and clear the value's set bits from 'mask'; in the
       end, the set bits in mask were never needed by a value */
    for (int i = 0; i < n; i++) {
	ulong64 val = llabs(*f++);
	mask &= ~val;
    }

    /* count the most-significant set bits */
    while (mask & ((ulong64)1 << 63)) {
	count++;
	mask <<= 1;
    }

    /* -1 because we'll store scaled floats within *signed* integers */
    return count > 0 ? count - 1 : 0;
}

/*-----------------------------------------------------------------------------
  getScaleFactor:

  Author:  Nicholas Boers (Mar. 2013)

  Notes:
  Given all of the X/Y coordinates of both subject and clip polygons,
  determine the single scale factor that covers all of them.
  ---------------------------------------------------------------------------*/
static ulong64
getScaleFactor (Sfloat *sXptr, int sXlen, Sfloat *sYptr, int sYlen,
		Sfloat *cXptr, int cXlen, Sfloat *cYptr, int cYlen)
{
    int scaleBits = 64;
    int tmp;

    tmp = determineScale(sXptr, sXlen);
    if (tmp < scaleBits)
	scaleBits = tmp;
    tmp = determineScale(sYptr, sYlen);
    if (tmp < scaleBits)
	scaleBits = tmp;
    tmp = determineScale(cXptr, cXlen);
    if (tmp < scaleBits)
	scaleBits = tmp;
    tmp = determineScale(cYptr, cYlen);
    if (tmp < scaleBits)
	scaleBits = tmp;

    /* subtract 1 for rounding, e.g.,
       suppose 6-bit values, 7.999999999999 => 000 111; scale
       bits will be 2; 2**2 = 4; 7.99999999999 * 4 might be 32;
       which is 100 000 <= shifted one bit too far */
    scaleBits -= 1;

    /* given the code here and in determineScale, the maximum shift is
       62, which does not cause problems for a 64-bit integer */
    return (ulong64)1 << scaleBits;
}

#ifndef STANDALONE
/*-----------------------------------------------------------------------------
  joinPolys:

  Author:  Nicholas Boers (Mar. 2013)

  Notes:
  This code is the entry point for R.

  Regarding arguments, the "s" represents subject and "c" clip for the two
  PolySets.
  ---------------------------------------------------------------------------*/
extern "C" SEXP
joinPolys(SEXP operation,
	  SEXP sPID, SEXP sSID, SEXP sPOS, SEXP sX, SEXP sY,
	  SEXP cPID, SEXP cSID, SEXP cPOS, SEXP cX, SEXP cY)
{
    /* for recording the number of calls to PROTECT so that upon termination
       we can UNPROTECT all of them */
    int protectCount = 0;

    /* for the final result */
    SEXP res = R_NilValue;	/* final result to return */

    /* for determining appropriate case */
    Sint nSubject, nClip;
    enum ClipType op;

    /* pointers for quickly accessing arguments */
    Sint *sPIDptr, *sSIDptr, *cPIDptr, *cSIDptr;
    Sfloat *sXptr, *sYptr, *cXptr, *cYptr;

    /* polygons for Clipper library */
    Polygons subj, clip, temp;	/* input for Clipper */
    PolyTree result;		/* output from Clipper */

    /* scale factor to use when interacting with the Clipper library */
    ulong64 scaleFactor;

    /* for building our result; it's easier to build it in this vector
       and convert it to an R data type than build in an R type from
       the start */
    PolySet resultset;

    if (DEBUG) Rprintf("Clipper: dbg: built %s at %s\n", __DATE__, __TIME__);

    /* convert the incoming vectors to the correct types */
    PROTECT(sPID = AS_INTEGER(sPID));
    PROTECT(sSID = AS_INTEGER(sSID));
    PROTECT(sX = AS_NUMERIC(sX));
    PROTECT(sY = AS_NUMERIC(sY));

    PROTECT(cPID = AS_INTEGER(cPID));
    PROTECT(cSID = AS_INTEGER(cSID));
    PROTECT(cX = AS_NUMERIC(cX));
    PROTECT(cY = AS_NUMERIC(cY));

    protectCount += 8;

    /* set the pointers for quickly accessing the arguments */
    sPIDptr = INTEGER_POINTER(sPID);
    sSIDptr = INTEGER_POINTER(sSID);
    sXptr = NUMERIC_POINTER(sX);
    sYptr = NUMERIC_POINTER(sY);

    cPIDptr = INTEGER_POINTER(cPID);
    cSIDptr = INTEGER_POINTER(cSID);
    cXptr = NUMERIC_POINTER(cX);
    cYptr = NUMERIC_POINTER(cY);
    
    /* determine scale factor for converting floating-point coordinates
       to integer coorindates */
    scaleFactor = getScaleFactor (sXptr, LENGTH(sX), sYptr, LENGTH(sY),
				  cXptr, LENGTH(cX), cYptr, LENGTH(cY));

    /* we'll use the (a) number of subject polys, (b) number of clip
       polys, and (c) the operation to determine PID/SID numbering */
    nSubject = countPolygons(sPIDptr, sSIDptr, LENGTH(sPID), TRUE);
    nClip    = countPolygons(cPIDptr, cSIDptr, LENGTH(cPID), TRUE);
    switch (INTEGER_VALUE(operation)) {
    case 0: op = ctIntersection; break;
    case 1: op = ctUnion; break;
    case 2: op = ctDifference; break;
    case 3: op = ctXor; break;
    default: error("Unrecognized operation.\n"); return res;
    }

    if (DEBUG) Rprintf("Clipper: dbg: nSubject: %d\n", nSubject);
    if (DEBUG) Rprintf("Clipper: dbg: nClip: %d\n", nClip);

    /* create our subject and clip PolySetState structures that will later
       use to iterate through all of the polygons */
    struct PolySetState sStat = { sPIDptr, sSIDptr, sXptr, sYptr,
				  LENGTH(sPID), 0 };
    struct PolySetState cStat = { cPIDptr, cSIDptr, cXptr, cYptr,
				  LENGTH(cPID), 0 };
    Sint currentPID, noSint;
    
    /* we will have one of five cases when we proceed (as per the
       joinPolys documentation)... */
    if (nSubject > 1 && nClip == 0) {
	/* (1) multiple subject and NO clip polygons:
           sequentially apply to subject polys (((A op B) op C) ...);
	   assigns single PID to the output */
	temp = getNextPolygon(sStat, scaleFactor, currentPID);
	clip = getNextPolygon(sStat, scaleFactor, currentPID);
	while (thereAreMorePolygons(sStat)) {
	    join (temp, op, clip, temp);
	    clip = getNextPolygon(sStat, scaleFactor, currentPID);
	}
	/* for last clip, result goes into a PolyTree */
	join (temp, op, clip, result);
	appendToResult(resultset, result, scaleFactor, 1);
    } else if (nSubject > 1 && nClip == 1) {
	/* (2) multiple subject and one clip:
           perform operation between each subject and single clip;
	   maintains PIDs of subject polys */
	clip = getNextPolygon (cStat, scaleFactor, currentPID);
	do {
	    subj = getNextPolygon (sStat, scaleFactor, currentPID);
	    join (subj, op, clip, result);
	    appendToResult(resultset, result, scaleFactor, currentPID);
	} while (thereAreMorePolygons(sStat));
    } else if (nSubject > 1 && nClip > 1) {
	/* (3) multiple subject and multiple clip polygons:
           for subject polys A and B and clip polys C and D,
	   concatenates A op C, B op C, A op D, B op D;
	   assign PIDs 1 to n where n is |subject| * |clip| */
	currentPID = 1;
	while (thereAreMorePolygons(cStat)) {
	    clip = getNextPolygon (cStat, scaleFactor, noSint);
	    /* reset the subject state */
	    sStat.nextStart = 0;
	    while (thereAreMorePolygons(sStat)) {
		subj = getNextPolygon (sStat, scaleFactor, noSint);
		join (subj, op, clip, result);
		appendToResult(resultset, result, scaleFactor, currentPID);
		currentPID++;
	    }
	}
    } else if (nSubject == 1 && op == ctDifference) {
	/* (4) one subject and multiple clip *and* DIFF operation:
           perform (((subject DIFF clip[0]) DIFF clip[1]) ...);
	   maintains single PID of subject */
	temp = getNextPolygon (sStat, scaleFactor, currentPID);
	clip = getNextPolygon (cStat, scaleFactor, noSint);
	while (thereAreMorePolygons(cStat)) {
	    join (temp, op, clip, temp);
	    clip = getNextPolygon (cStat, scaleFactor, noSint);
	}
	/* for last clip, result goes into a PolyTree */
	join (temp, op, clip, result);
	appendToResult(resultset, result, scaleFactor, currentPID);
    } else if (nSubject == 1 && nClip >= 1) {
	/* (5) one subject (A) and multiple clip (B and C) *and*
           _NOT_ DIFF operation:
           concatenates A op B, A op C;
	   maintains PIDs of clip */
	subj = getNextPolygon (sStat, scaleFactor, noSint);
	while (thereAreMorePolygons(cStat)) {
	    clip = getNextPolygon (cStat, scaleFactor, currentPID);
	    join (subj, op, clip, result);
	    appendToResult(resultset, result, scaleFactor, currentPID);
	}
    } else {
	/* unhandled case */
	error ("Unhandled\n");
    }

    /* convert our result vectors to an R PolySet */
    if (resultset.PID.size() > 0) {
	res = PolySetToR(resultset, protectCount);
    }

    UNPROTECT(protectCount);
    return (res);
}
#endif /* not defined STANDALONE */

#ifdef STANDALONE
/*=============================================================================
  Code placed within this section is for compiling a standalone executable
  for quickly testing various components.
  ===========================================================================*/
struct shiftTest {
    Sfloat inp;
    int exp;
};

int main ()
{
    shiftTest individual[] = {
	{ 0, 63 },
	{ 1, 62 },
	{ 2, 61 },
	{ 3, 61 },
	{ 3.99, 61 },
	{ 4, 60 },
	{ 4.01, 60 },
	{ 5, 60 },
	{ 6, 60 },
	{ 7.99, 60 },
	{ 0, 0 },
    };
    shiftTest *testIter = &individual[0];

    printf ("Testing determineScale...\n");
    while (testIter->inp != 0 || testIter->exp != 0) {
	int act = determineScale(&(testIter->inp), 1);
	printf ("%s: exp: %d, act: %d\n",
		act == testIter->exp ? "OK" : "FAIL",
		testIter->exp, act);
	testIter++;
    }

    return 0;
}
#endif /* STANDALONE */
