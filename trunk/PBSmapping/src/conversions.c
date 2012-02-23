/*=============================================================================
  Copyright (C) 2003-2006  Fisheries and Oceans Canada

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
  File: conversions.c
   
  Routines for converting units/projections.

  History:
    ?? Jul 2001 [Chris Grandin]
      - version 1.0

    ?? May 2003 [Nicholas Boers]
      - version 1.1

    11 Jul 2003 [Nicholas Boers]
      - converted to stand-alone file for separate compilation
      - converted to ANSI C (rather than C++)
      - added ability to use different UTM zones
      - version 1.2

    31 Jul 2003 [Nicholas Boers]
      - tried running `Splint' on it; it found undefined behaviour in some
        of the long formulas
      - added `static' to functions that are not exported
      - version 1.21

    17 Jun 2004 [Nicholas Boers]
      - cleaned up comments
  
    For further history, see repository log.

  References:

    [Ord10] Ordnance Survey. 2010. A guide to coordinate systems
      in Great Britain. Report D00659 (v2.1). Southampton, UK.
      http://www.ordnancesurvey.co.uk/oswebsite/gps/docs/
           A_Guide_to_Coordinate_Systems_in_Great_Britain.pdf

  ---------------------------------------------------------------------------*/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

#include "globals.h"
#include "conversions.h"

/* constants of the earth (Ellipsoidal) and conversion factors */
#define _a	6378137.0000		/* major axis of ellipsoid */
#define _b	6356752.3141		/* minor axis of ellipsoid */
#define _e2	(((_a*_a) - (_b*_b)) / (_a*_a)) /* eccentricity squared; B1 */
#define _n	((_a - _b) / (_a + _b))	/* C1 */
#define _n2	(_n * _n)		/* _n^2 */
#define _n3	(_n2 * _n)		/* _n^3 */
#define _phi0	0.0			/* true origin, latitude */
/*      lambda0 (-177+((utmZone-1)*6.0))   true origin, longitude */
#define _N0	0.0			/* true origin, northings (metres) */
#define _E0	500000.0                /* true origin, eastings (meters) */
#define _F0	0.9996			/* scale factor */

/* getNu: get radius of curvature at lat. 'phi' perpendicular to a meridian */
#define getNu(phi) (_a * _F0 * pow(1 - _e2 * pow(sin(phi), 2), -0.5))
/* getRho: get a radius of curvature of a meridian at latitude 'phi' */
#define getRho(nu,phi) (_a * _F0 * (1 - _e2) * pow(1 - _e2		\
						   * pow(sin(phi), 2), -1.5))
/* getEta2 */
#define getEta2(nu,rho) ((nu / rho) - 1.0)
/* getM: get developed arc of a meridian from 'phi' to zero */
#define getM(phi) ( (_b * _F0)						\
		    * ( ( (1 + _n + 5.0/4.0*_n2 + 5.0/4.0*_n3)		\
			  * (phi - _phi0) )				\
			- ( (3*_n + 3*_n2 + 21.0/8.0*_n3)		\
			    * (sin(phi - _phi0) * cos(phi + _phi0)) )	\
			+ ( (15.0/8.0*_n2 + 15.0/8.0*_n3)		\
			    * (sin(2*(phi - _phi0)) * cos(2*(phi + _phi0))) ) \
			- ( (35.0/24.0*_n3)				\
			    * (sin(3*(phi - _phi0)) * cos(3*(phi + _phi0))) ) ) )

/*------------------------------------------------------------------------------
 * lonlat_to_utm:
 *   Convert coordinates in longitude (lambda)/latitude (phi) to
 *   eastings/northings.
 * 
 * Source: 
 *   [Ord10, Appendix C, Section C.1]
 *
 * See note for utm_to_lonlat.
 *----------------------------------------------------------------------------*/
void lonlat_to_utm(double lambda, double phi, struct pair *utm, int utmZone)
{
#if !defined(_N0) || !defined(_E0) || !defined(_F0) || !defined(_phi0)
#error "One or more of the required constants is undefined."
#endif

    double lambda0 = (-177 + ((utmZone - 1) * 6.0)) * DEG_TO_RAD;

    double nu, rho, eta2;
    nu = getNu (phi);
    rho = getRho (nu, phi);
    eta2 = getEta2 (nu, rho);					/* C2 */

    double M;
    M = getM (phi);						/* C3 */

    double I, II, III, IIIA, IV, V, VI;
    double cosphi = cos(phi), tanphi = tan(phi), sinphi=sin(phi);
    I = M + _N0;
    II = (nu / 2.0) * sinphi * cosphi;
    III = ( (nu / 24.0) * sinphi * pow(cosphi, 3)
	    * (5.0 - pow(tanphi, 2) + 9.0 * eta2) );
    IIIA = ( (nu / 720.0) * sinphi * pow(cosphi, 5)
	     * (61.0 - 58.0 * pow(tanphi, 2) + pow(tanphi, 4)) );
    IV = nu * cosphi;
    V = ( (nu / 6.0) * pow(cosphi, 3) * ((nu / rho) - pow(tanphi, 2)) );
    VI = ( (nu / 120.0) * pow(cosphi, 5)
	   * (5.0 - 18.0 * pow(tanphi, 2)
	      + pow(tanphi, 4) + 14.0 * eta2 
	      - 58.0 * pow(tanphi, 2) * eta2) );
    
    utm->y = I + II * pow(lambda - lambda0, 2)
	+ III * pow(lambda - lambda0, 4)
	+ IIIA * pow(lambda - lambda0, 6);			/* C4 */
    utm->x = _E0 + IV * (lambda - lambda0)
	+ V * pow(lambda - lambda0, 3)
	+ VI * pow(lambda - lambda0, 5);			/* C5 */
}

/*------------------------------------------------------------------------------
 * utm_to_lonlat:
 *
 * Source: 
 *   [Ord10, Appendix C, Section C.2]
 *
 * Note:
 * For clarity, this (2012) version doesn't attempt to reduce calls to
 * trigonometric functions as much as previously -- and we haven't
 * noticed a significant performance hit by removing many of the
 * earlier "optimizations."
 *----------------------------------------------------------------------------*/
void utm_to_lonlat(double E, double N, struct pair *lonlat,
                   int utmZone)
{
#if !defined(_a) || !defined(_b) || !defined(_e2) || !defined(_N0) || \
    !defined(_F0) || !defined(_phi0)
#error "One or more of the required constants is undefined."
#endif

    /* calculate M and adjust phiPrime in the process */
    double phiPrime, M;
    phiPrime = ((N - _N0) / (_a * _F0)) + _phi0;		/* C6 */
 redo:
    M = getM (phiPrime);					/* C3 */
    /* 0.00001 m = 0.01 mm */
    if (fabs(N - _N0 - M) >= 0.00001) {
	phiPrime += ((N - _N0 - M) / (_a * _F0));		/* C7 */
	goto redo;
    }

    /* (N - _N0 - M) < 0.00001 */
    double nu, rho, eta2;
    nu = getNu (phiPrime);
    rho = getRho (nu, phiPrime);
    eta2 = getEta2 (nu, rho);					/* C2 */

    double VII, VIII, IX, X, XI, XII, XIIA;
    double nu2=nu*nu, nu3=nu2*nu, nu4=nu2*nu2, nu5=nu4*nu, nu6=nu4*nu2,
	nu7=nu6*nu;
    VII = tan(phiPrime) / (2.0 * rho * nu);
    VIII = ( (tan(phiPrime) / (24.0 * rho * nu3))
	     * (5.0 + 3.0 * pow(tan(phiPrime), 2)
		+ eta2
		- 9 * pow(tan(phiPrime), 2) * eta2));
    IX = ( (tan(phiPrime) / (720.0 * rho * nu5))
	   * (61.0
	      + 90.0 * pow(tan(phiPrime), 2)
	      + 45.0 * pow(tan(phiPrime), 4)) );
    X = (1.0 / cos(phiPrime)) / nu;
    double secPhiPrime = (1.0 / cos(phiPrime));
    XI = ( (secPhiPrime / (6.0 * nu3))
	   * ((nu / rho)
	      + 2.0 * pow(tan(phiPrime), 2)) );
    XII = ( (secPhiPrime / (120.0 * nu5))
	    * (5.0
	       + 28.0 * pow(tan(phiPrime), 2)
	       + 24.0 * pow(tan(phiPrime), 4)) );
    XIIA = ( (secPhiPrime / (5040.0 * nu7))
	     * (61.0
		+ 662.0 * pow(tan(phiPrime), 2)
		+ 1320.0 * pow(tan(phiPrime), 4)
		+ 720.0 * pow(tan(phiPrime), 6)) );
    
    double lambda0 = (-177 + ((utmZone - 1) * 6.0)) * DEG_TO_RAD;
    lonlat->y = ( phiPrime
		  - VII * pow(E - _E0, 2)
		  + VIII * pow(E - _E0, 4)
		  - IX * pow(E - _E0, 6) );			/* C8 */
    lonlat->x = ( lambda0
		  + X * (E - _E0)
		  - XI * pow(E - _E0, 3)
		  + XII * pow(E - _E0, 5)
		  - XIIA * pow(E - _E0, 7) );			/* C9 */
}
