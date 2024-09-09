/*=============================================================================
  Copyright (C) 2003-2024 Fisheries and Oceans Canada

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

/* Thank-you Roger S. Bivand (used maptools as an example) */

#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include "PBSmapping.h"
#include "convGSHHS.h"
#include "clipperWrapper.h"

static const R_CMethodDef CEntries[]  = {
    /* PBSmapping.c */
    {"calcArea",        (DL_FUNC) &calcArea, 7},         /* line 1087 */
    {"calcCentroid",    (DL_FUNC) &calcCentroid, 7},     /* line 1160 */
    {"calcOrientation", (DL_FUNC) &calcOrientation, 7},  /* line 1234 */
    {"clip",            (DL_FUNC) &clip, 9},             /* line  357 */
    {"closePolys",      (DL_FUNC) &closePolys, 8},       /* line  680 */
    {"convUL",          (DL_FUNC) &convUL, 8},           /* line 1036 */
    {"findCells",       (DL_FUNC) &findCells, 7},        /* line  864 */
    {"findPolys",       (DL_FUNC) &findPolys, 9},        /* line  919 */
    {"isConvex",        (DL_FUNC) &isConvex, 7},         /* line 1298 */
    {"isIntersecting",  (DL_FUNC) &isIntersecting, 8},   /* line 1362 */
    {"rollupPolys",     (DL_FUNC) &rollupPolys, 12},     /* line  461 */
    {"thickenPolys",    (DL_FUNC) &thickenPolys, 12},    /* line 1429 */
    {"thinPolys",       (DL_FUNC) &thinPolys, 10},       /* line 1516 */
    {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
    {"joinPolys",   (DL_FUNC) &joinPolys, 11},    /* clipperWrapper.cpp: 524 */
    {"importGSHHS", (DL_FUNC) &importGSHHS, 4},   /* convGSHHS.c:        206 */
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

