/*-----------------------------------------------------------------------------
 * File:   convGSHHS.c
 *
 * This program is modified from original code from Paul Wessel
 * which was downloaded from 
 * http://www.ngdc.noaa.gov/mgg/shorelines/data/gshhs/version1.5/
 * and made available under GNU GPL.
 *
 * Modified by Alex Couture-Beil (alex at mofo dot ca)
 * Original file header below
 --------------------------------------------------------------------------------
 * AUTHOR:	Paul Wessel (pwessel@hawaii.edu)
 * CREATED:	JAN. 28, 1996
 * PURPOSE:	To extract ASCII data from binary shoreline data
 *		as described in the 1996 Wessel & Smith JGR Data Analysis Note.
 * VERSION:	1.1 (Byte flipping added)
 *		1.2 18-MAY-1999:
 *		   Explicit binary open for DOS systems
 *		   POSIX.1 compliant
 *		1.3 08-NOV-1999: Released under GNU GPL
 *		1.4 05-SEPT-2000: Made a GMT supplement; FLIP no longer needed
 *		1.5 14-SEPT-2004: Updated to deal with latest GSHHS database (1.3)
 *
 *	Copyright (c) 1996-2004 by P. Wessel and W. H. F. Smith
 *	See COPYING file for copying and redistribution conditions.
 *
 *	This program is free software; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation; version 2 of the License.
 *
 *	This program is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	Contact info: www.soest.hawaii.edu/pwessel 
 ---------------------------------------------------------------------------*/
#include <string.h>
#include <R.h>
#include <Rdefines.h>
#include <errno.h>
#include "globals.h"
#include "floating.h"
#include "polygons.h"
#include "gshhs.h"

/* initial memory allocation size */
#define POLYDATA_INIT_SPACE 1000
#define POLYSET_INIT_SPACE  100000


void resize(double **x, double **y, int **pos, int **pid, int new_n)
{
    *x = (double*)realloc(*x, sizeof(double)*new_n);
    *y = (double*)realloc(*y, sizeof(double)*new_n);
    *pos = (int*)realloc(*pos, sizeof(int)*new_n);
    *pid = (int*)realloc(*pid, sizeof(int)*new_n);
    if (*pid==NULL || *pos==NULL || *y==NULL || *x==NULL)
	error("out of memory");
}


void convGSHHS(char **inFileName, char **outFileName, char **outDataFileName,
               int *PBSformat, int *polyLevelFilter, int *minVert,
               double *locLimits, int *retVal)
{
    Rprintf("deprecated - use importGSHHS instead");
    return;
}

/* **************************************************************************** */

int extracGSHHS(const char *inFileName, int *polyLevelFilter, int minVert,
		double *locLimits, int **p_PID, int **p_POS, double **p_X,
		double **p_Y, int maxSpace, int **pd_PID, int **pd_level,
		int **pd_source, int **pd_greenwich, int *pd_space)
{
    double w, e, s, n, area, lon, lat;
    char source;
    FILE	*fpIn;
    int	k, max_east = 270000000, n_read, flip, level, version, greenwich, src;
    struct	POINT p;
    struct GSHHS h;
	
    double *inX, *inY, *outX, *outY;
    int *inPOS, *outPOS, nVerts;
	
    int p_ind=0, pd_ind=0;
	
    /* Open input and output files */
    if ((fpIn = fopen(inFileName, "rb")) == NULL ) {
	error("%s: %s\n", inFileName, strerror(errno));
	return -1;
    }
	
    n_read = fread((void *)&h, (size_t)sizeof(struct GSHHS), (size_t)1, fpIn);
    version = (h.flag >> 8) & 255;
    /* Take as sign that byte-swabbing is needed */
    flip = (version != GSHHS_DATA_VERSION);

    while(n_read == 1) {
	if (flip) {
	    h.id = swabi4 ((unsigned int)h.id);
	    h.n  = swabi4 ((unsigned int)h.n);
	    h.west  = swabi4 ((unsigned int)h.west);
	    h.east  = swabi4 ((unsigned int)h.east);
	    h.south = swabi4 ((unsigned int)h.south);
	    h.north = swabi4 ((unsigned int)h.north);
	    h.area  = swabi4 ((unsigned int)h.area);
	    h.flag  = swabi4 ((unsigned int)h.flag);
	}
	level = h.flag & 255;
	version = (h.flag >> 8) & 255;
	greenwich = (h.flag >> 16) & 255;
	src = (h.flag >> 24) & 255;
	w = h.west  * 1.0e-6;	/* Convert from microdegrees to degrees */
	e = h.east  * 1.0e-6;
	s = h.south * 1.0e-6;
	n = h.north * 1.0e-6;
	/* Either WVS or CIA (WDBII) pedigree */
	source = (src == 1) ? 'W' : 'C';
	area = 0.1 * h.area;	/* Now im km^2 */
		
	/* Skip data if polygon isn't wanted */
	if (!polyLevelFilter[level-1] || h.n<=minVert) {
	    fseek(fpIn, (long)(h.n * sizeof(struct POINT)), SEEK_CUR);
	}
	else {
	    /* createSpace to store polygon */
	    inX = (double*)malloc(sizeof(double)*h.n);
	    inY = (double*)malloc(sizeof(double)*h.n);
	    nVerts = 2*h.n;
	    outX = (double*)malloc(sizeof(double)*nVerts);
	    outY = (double*)malloc(sizeof(double)*nVerts);
			
	    /* inPOS not used but needed to call func */
	    inPOS = (int*)malloc(sizeof(int)*h.n);
	    outPOS = (int*)malloc(sizeof(int)*2*h.n);			
			
	    if (inX==NULL || inY==NULL || outX==NULL || outY==NULL
		|| inPOS==NULL || outPOS==NULL) {
		    error("out of memory");
		}
	
	    /* changed for(..< h.n to h.n-1 and used fseek to skip
	       first point which is repeated at the end */
	    fseek(fpIn, (long)(sizeof(struct POINT)), SEEK_CUR);
	    for(k = 0; k < h.n-1; k++) {
		if (fread((void *)&p, (size_t)sizeof(struct POINT), (size_t)1,
			  fpIn) != 1) {
		    /* fprintf(stderr, "%s: Error reading file %s for
		       polygon %d, point %d.\n", programName, inFileName,
		       h.id, k); */
		    fclose(fpIn);
		    return -1;
		}
		if (flip) {
		    p.x = swabi4((unsigned int)p.x);
		    p.y = swabi4((unsigned int)p.y);
		}
		lon = (greenwich && p.x > max_east) 
		    ? p.x * 1.0e-6 - 360.0 : p.x * 1.0e-6;
		lat = p.y * 1.0e-6;

		/* save polygon for later clipping */
		inX[k]=lon;
		inY[k]=lat;
	    }
			
	    /* clip */
	    clipPolygon(inX,
			inY,
			inPOS, /* POS data not used or initialized */
			h.n-1,
			outX,
			outY,
			outPOS,
			&nVerts,
			locLimits,
			1);
	    if (nVerts<0) {
		fclose(fpIn);
		error("Out of memory in clipPolygon");
		return -1;
	    }
			
	    /* save poly header info */
	    if (nVerts>0) {
		if (pd_ind >= *pd_space) {
		    /* resize memory by factor of 2 */
		    *pd_space *= 2;
		    *pd_PID = (int*)realloc(*pd_PID,
					    sizeof(int)*(*pd_space));
		    *pd_level = (int*)realloc(*pd_level,
					      sizeof(int)*(*pd_space));
		    *pd_source = (int*)realloc(*pd_source,
					       sizeof(int)*(*pd_space));
		    *pd_greenwich = (int*)realloc(*pd_greenwich,
						  sizeof(int)*(*pd_space));
		    if (*pd_PID==NULL || *pd_level==NULL || *pd_source==NULL
			|| *pd_greenwich==NULL)
			error("out of memory");
		}
		(*pd_PID)[pd_ind]=h.id;
		(*pd_level)[pd_ind]=level;
		(*pd_source)[pd_ind]=source;
		(*pd_greenwich)[pd_ind]=greenwich;
		pd_ind++;
	    }

	    /* save polygon points */
	    for(k=0;k<nVerts;k++) {
		if (p_ind >= maxSpace) {
		    resize(p_X, p_Y, p_POS, p_PID, maxSpace*2);
		    maxSpace *= 2;
		}
		(*p_PID)[p_ind]=h.id;
		(*p_POS)[p_ind]=k+1;
		(*p_X)[p_ind]=outX[k];
		(*p_Y)[p_ind]=outY[k];
		p_ind++;
	    }
	    free(inX);
	    free(inY);
	    free(outX);
	    free(outY);
	    free(inPOS);
	    free(outPOS);
	}
	max_east = 180000000;	/* Only Eurasiafrica needs 270 */
	n_read = fread((void *)&h, (size_t)sizeof (struct GSHHS), (size_t)1,
		       fpIn);
    }
	
    fclose(fpIn);
    *pd_space = pd_ind;
    return p_ind;
}



/* 
   gshhsFileName - character file name
   clipLimits    - numeric vector {x,X,y,Y}

*/
SEXP importGSHHS(SEXP gshhsFileName, SEXP clipLimits, SEXP levels,
		 SEXP minVerts)
{
    SEXP list, x,y,pid,pos, list_names, polyData, level, source, greenwich;
    double *p_x, *p_y;
    int *p_pid, *p_pos;
    unsigned int rows=POLYSET_INIT_SPACE;
    int i, *levelFilter;
    double *locLimits;
	
    int *pd_PID, *pd_level, *pd_source, *pd_greenwich, 
	pd_space=POLYDATA_INIT_SPACE;
	
    /* get some C pointers */
    locLimits = NUMERIC_POINTER(clipLimits);
    levelFilter = INTEGER_POINTER(levels);
	
    /* create list which will be base of polyset data.frame */
    PROTECT(list = allocVector(VECSXP, 4));

    /* set list names of polyset */
    PROTECT(list_names = allocVector(STRSXP, 4));
    SET_STRING_ELT(list_names, 0,  mkChar("PID"));
    SET_STRING_ELT(list_names, 1,  mkChar("POS"));
    SET_STRING_ELT(list_names, 2,  mkChar("X"));
    SET_STRING_ELT(list_names, 3,  mkChar("Y"));
    /* set attribute and unprotect since its now a protected attr */
    setAttrib(list, R_NamesSymbol, list_names);
    UNPROTECT(1);
	
    /* create polydata list */
    PROTECT(polyData=allocVector(VECSXP, 4));
	
    /* set list names of polyset */
    PROTECT(list_names = allocVector(STRSXP, 4));
    SET_STRING_ELT(list_names, 0,  mkChar("PID"));
    SET_STRING_ELT(list_names, 1,  mkChar("LEVEL"));
    SET_STRING_ELT(list_names, 2,  mkChar("SOURCE"));
    SET_STRING_ELT(list_names, 3,  mkChar("GREENWICH"));
    /* set attribute and unprotect since its now a protected attr */
    setAttrib(polyData, R_NamesSymbol, list_names);
    UNPROTECT(1);
	

    /* allocate memory for polygons */
    p_x = (double*)malloc(rows*sizeof(double));
    p_y = (double*)malloc(rows*sizeof(double));
    p_pos = (int*)malloc(rows*sizeof(int));
    p_pid = (int*)malloc(rows*sizeof(int));
    if (p_x==NULL || p_y==NULL || p_pos==NULL || p_pid==NULL)
	error("out of memory");
    /* allocate memory for polydata */
    pd_PID = (int*)malloc(pd_space*sizeof(int));
    pd_level = (int*)malloc(pd_space*sizeof(int));
    pd_source = (int*)malloc(pd_space*sizeof(int));
    pd_greenwich = (int*)malloc(pd_space*sizeof(int));
    if (pd_PID==NULL || pd_level==NULL || pd_source==NULL
	|| pd_greenwich==NULL)
	error("out of memory");
	
    /* extract GSHHS DB filename */
    i = extracGSHHS(CHAR(STRING_ELT(gshhsFileName,0)), levelFilter, 0, 
		    locLimits, &p_pid, &p_pos, &p_x, &p_y, rows, &pd_PID,
		    &pd_level, &pd_source, &pd_greenwich, &pd_space);
    if (i < 0) {
	UNPROTECT(2);
	return R_NilValue;
    }

    /* create R storage for returned polygons */
    PROTECT(x = allocVector(REALSXP, i));
    PROTECT(y = allocVector(REALSXP, i));
    PROTECT(pos = allocVector(INTSXP, i));
    PROTECT(pid = allocVector(INTSXP, i));
	
    /* copy C data into R data */
    memcpy(NUMERIC_POINTER(x), p_x, sizeof(double)*i);
    memcpy(NUMERIC_POINTER(y), p_y, sizeof(double)*i);
    memcpy(INTEGER_POINTER(pos), p_pos, sizeof(int)*i);
    memcpy(INTEGER_POINTER(pid), p_pid, sizeof(int)*i);

    /* attach R vectors onto the polyset list */
    SET_VECTOR_ELT(list, 0, pid);
    SET_VECTOR_ELT(list, 1, pos);
    SET_VECTOR_ELT(list, 2, x);
    SET_VECTOR_ELT(list, 3, y);
	
    UNPROTECT(4);

    /* free temp C polyset storage */
    free(p_x);
    free(p_y);
    free(p_pos);
    free(p_pid);


    /* create R vectors for polydata */
    PROTECT(pid = allocVector(INTSXP, pd_space));
    PROTECT(level = allocVector(INTSXP, pd_space));
    PROTECT(source = allocVector(INTSXP, pd_space));
    PROTECT(greenwich = allocVector(INTSXP, pd_space));

    /* copy C mem into R mem */
    memcpy(INTEGER_POINTER(pid), pd_PID, sizeof(int)*pd_space);
    memcpy(INTEGER_POINTER(level), pd_level, sizeof(int)*pd_space);
    memcpy(INTEGER_POINTER(source), pd_source, sizeof(int)*pd_space);
    memcpy(INTEGER_POINTER(greenwich), pd_greenwich, sizeof(int)*pd_space);

    /* attach R vectors onto the polydata list */
    SET_VECTOR_ELT(polyData, 0, pid);
    SET_VECTOR_ELT(polyData, 1, level);
    SET_VECTOR_ELT(polyData, 2, source);
    SET_VECTOR_ELT(polyData, 3, greenwich);	
	
    UNPROTECT(4);

    /* free temp C polyData storage */
    free(pd_PID);
    free(pd_level);
    free(pd_source);
    free(pd_greenwich);

	
    /* attach polyData to polySet as "PolyData" attribute */
    setAttrib(list, install("PolyData"), polyData);
	
    UNPROTECT(2);
    return list;
}
