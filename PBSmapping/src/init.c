/* Thank-you Roger S. Bivand */
#include "registerPBSmapping.h"
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

static const R_CMethodDef CEntries[]  = {
    /* PBSmapping.c */
    {"calcOrientation", (DL_FUNC) &calcOrientation, 7},  /* line 1421 */
    {"clip",            (DL_FUNC) &clip, 9},             /* line  368 */
    {"rollupPolys",     (DL_FUNC) &rollupPolys, 12},     /* line  512 */
    {"calcArea",        (DL_FUNC) &calcArea, 7},         /* line 1227 */
    {"calcCentroid",    (DL_FUNC) &calcCentroid, 7},     /* line 1323 */
    {"closePolys",      (DL_FUNC) &closePolys, 8},       /* line  771 */
    {"convUL",          (DL_FUNC) &convUL, 8},           /* line 1154 */
    {"findCells",       (DL_FUNC) &findCells, 7},        /* line  964 */
    {"findPolys",       (DL_FUNC) &findPolys, 9},        /* line 1028 */
    {"isConvex",        (DL_FUNC) &isConvex, 7},         /* line 1499 */
    {"isIntersecting",  (DL_FUNC) &isIntersecting, 8},   /* line 1583 */
    {"thickenPolys",    (DL_FUNC) &thickenPolys, 12},    /* line 1660 */
    {"thinPolys",       (DL_FUNC) &thinPolys, 10},       /* line 1761 */
    {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
    /* clipperWrapper.cpp */
    {"joinPolys",   (DL_FUNC) &joinPolys, 11},    /* clipperWrapper.cpp: 535 */
    {"importGSHHS", (DL_FUNC) &importGSHHS, 4},   /* convGSHHS.c:        208 */
    {NULL, NULL, 0}
};

void 
#ifdef HAVE_VISIBILITY_ATTRIBUTE
__attribute__ ((visibility ("default")))
#endif
R_init_PBSmapping(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

