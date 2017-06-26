#include "globals.h"
#include <R.h>
#include <Rinternals.h>

void calcOrientation(PBSINT *inID, double *inXY, PBSINT *inVerts,
     PBSINT *outID, double *outOrientation, PBSINT *outVerts, PBSINT *status);
void clip(PBSINT *inID, double *inXY, PBSINT *inVerts, PBSINT *polygons,
     double *limits, PBSINT *outID, double *outXY, PBSINT *outVerts, PBSINT *status);
void rollupPolys(PBSINT *inID, double *inPOS, double *inXY, PBSINT *inVerts,
     PBSINT *outID, double *outXY, PBSINT *outVerts, PBSINT *rollupMode, 
     PBSINT *exteriorCCW, PBSINT *closedPolys, PBSINT *addRetrace, PBSINT *status);
void calcArea(PBSINT *inID, double *inXY, PBSINT *inVerts, PBSINT *outID,
     double *outArea, PBSINT *outVerts, PBSINT *status);
void calcCentroid(PBSINT *inID, double *inXY, PBSINT *inVerts, PBSINT *outID,
     double *outXY, PBSINT *outVerts, PBSINT *status);
void closePolys(PBSINT *inID, double *inXY, PBSINT *inVerts, double *limits, 
     PBSINT *outID, double *outXY, PBSINT *outVerts, PBSINT *status);
void convUL(double *inXY, PBSINT *inVerts, PBSINT *toUTM, PBSINT *zone,
     PBSINT *southern, double *outXY, PBSINT *outVerts, PBSINT *status);
void findCells(double *inPt, PBSINT *inPts, double *inBrk, PBSINT *inBrks,
     PBSINT *outCell, PBSINT *outBdry, PBSINT *status);
void findPolys(PBSINT *inEventsID, double *inEventsXY, PBSINT *inEvents,
     PBSINT *inPolysID, double *inPolysXY, PBSINT *inPolys, PBSINT *outID,
     PBSINT *outIDs, PBSINT *status);
void isConvex(PBSINT *inID, double *inXY, PBSINT *inVerts, PBSINT *outID,
     PBSINT *outResult, PBSINT *outVerts, PBSINT *status);
void isIntersecting(PBSINT *inID, double *inXY, PBSINT *inVerts, 
     PBSINT *numericResult, PBSINT *outID, PBSINT *outResult, 
     PBSINT *outVerts, PBSINT *status);
void thickenPolys(PBSINT *inID, double *inXY, PBSINT *inVerts, double *tolerance,
     PBSINT *filter, PBSINT *units, PBSINT *keepOrig, PBSINT *close, 
     PBSINT *outID, double *outXY, PBSINT *outVerts, PBSINT *status);
void thinPolys(PBSINT *inID, PBSINT *inXY, PBSINT *inVerts, double *tolerance,
     PBSINT *filter, PBSINT *units, PBSINT *outID, PBSINT *outXY, 
     PBSINT *outVerts, PBSINT *status);
SEXP joinPolys(SEXP operation,
     SEXP sPID, SEXP sSID, SEXP sPOS, SEXP sX, SEXP sY,
     SEXP cPID, SEXP cSID, SEXP cPOS, SEXP cX, SEXP cY);
SEXP importGSHHS(SEXP gshhsFileName, SEXP clipLimits, SEXP levels, SEXP minVerts);


