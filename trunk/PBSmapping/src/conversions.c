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
  
  Algorithm source:
    National Mapping Agency of Great Britain Ordnance Survey
    <http://www.ordsvy.gov.uk>
  ---------------------------------------------------------------------------*/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include "globals.h"
#include "conversions.h"

/* constants of the earth (Ellipsoidal) and conversion factors */
#define A_PRE   6378137.0                 /* major axis of ellipsoid */
#define B_PRE   6356752.3141              /* minor axis of ellipsoid */
#define SF      0.9996                    /* scale factor */
#define A       (A_PRE * SF)              /* major after scaled */
#define B       (B_PRE * SF)              /* minor after scaled */
#define N       ((A - B) / (A + B))       /* ratio used in calculations */
#define E       sqrt(((A*A)-(B*B))/(A*A)) /* eccentricity */
#define E0      500000.0                  /* grid eastings of true origin */

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE 1
#endif

/*-----------------------------------------------------------------------------
  get_nu:
   
  Description:
  - get a radius of curvature at latitude 'phi' perpendicular to a meridian
  ---------------------------------------------------------------------------*/
static double get_nu(double phi)
{
  double sin1, sin2;

  sin1 = sin(phi);
  sin2 = sin1 * sin1;

  return (A / (sqrt(1.0 - (E * E * sin2))));
}

/*-----------------------------------------------------------------------------
  get_rho:
   
  Description:
  - get a radius of curvature of a meridian at latitude 'phi'
  ---------------------------------------------------------------------------*/
static double get_rho(double nu, double phi)
{
  double sin1, sin2;

  sin1 = sin(phi);
  sin2 = sin1 * sin1;

  return ((nu * (1.0 - E * E)) / (1.0 - (E * E * sin2)));
}

/*-----------------------------------------------------------------------------
  get_eta:
  ---------------------------------------------------------------------------*/
static double get_eta(double nu, double rho)
{
  return (sqrt((nu / rho) - 1.0));
}

/*-----------------------------------------------------------------------------
  get_M:

  Description:
  - get developed arc of a meridian from 'phi' to zero
  ---------------------------------------------------------------------------*/
static double get_M(double phi)
{
  double c1, c2, c3, c4, n2, n3, sin1, cos1, M;

  c1 = 5.0/4.0;
  c2 = 21.0/8.0;
  c3 = 15.0/8.0;
  c4 = 35.0/24.0;
  n2 = N*N;
  n3 = n2*N;
  sin1 = sin(phi);
  cos1 = cos(phi);
  M =     ((1.0 + N + (c1*n2) + (c1*n3)) * phi);
  M = M - (((3.0*N) + (3.0*n2) + (c2*n3)) * sin(phi) * cos(phi));
  M = M + (((c3*n2) + (c3*n3)) * sin(2.0*phi) * cos(2.0*phi));
  M = M - ((c4*n3) * sin(3.0*phi) * cos(3.0*phi));

  return (B*M);
}

/*-----------------------------------------------------------------------------
  lonlat_to_utm:
  ---------------------------------------------------------------------------*/
void lonlat_to_utm(double lon, double lat, struct pair *eastingNorthing,
                   int utmZone)
{
  double sin1, cos1, cos3, cos5, tan1, tan2, tan4, nu, rho, eta, eta2;
  double P, P2, P3, P4, P5, P6, I, II, III, IIIA, IV, V, VI;
  int lonOrig = -177 + ((utmZone - 1) * 6);

  sin1 = sin(lat);
  cos1 = cos(lat);
  cos3 = cos1*cos1*cos1;
  cos5 = cos3*cos1*cos1;
  tan1 = tan(lat);
  tan2 = tan1*tan1;
  tan4 = tan2*tan2;
  nu = get_nu(lat);
  rho = get_rho(nu,lat);
  eta = get_eta(nu,rho);
  eta2 = eta*eta;
  P = lon - (lonOrig * DEG_TO_RAD);
  P2 = P*P;
  P3 = P2*P;
  P4 = P3*P;
  P5 = P4*P;
  P6 = P5*P;
  I = get_M(lat);
  II = (nu/2.0)*sin1*cos1;
  III = (nu/24.0)*sin1*cos3*(5.0-tan2+9.0*eta2);
  IIIA = (nu/720.0)*sin1*cos5*(61.0-58.0*tan2+tan4);
  IV = nu*cos1;
  V = (nu/6.0)*cos3*((nu/rho)-tan2);
  VI = (nu/120.0)*cos5*(5.0-18.0*tan2+tan4+14.0*eta2-58.0*tan2*eta2);

  eastingNorthing->y = I+P2*II+P4*III+P6*IIIA;
  eastingNorthing->x = E0+P*IV+P3*V+P5*VI;
}

/*-----------------------------------------------------------------------------
  utm_to_lonlat:
  ---------------------------------------------------------------------------*/
void utm_to_lonlat(double easting, double northing, struct pair *lonlat,
                   int utmZone)
{
  double E1, E2, E3, E4, E5, E6, E7, lat1, M, nu, rho, eta, eta2;
  double tan1, tan2, tan4, tan6, sec1, nu3, nu5, nu7, VII, VIII;
  double IX, X, XI, XII, XIIA;
  int lonOrig = -177 + ((utmZone - 1) * 6);
  int i;

  E1 = easting - E0;
  E2 = E1*E1;
  E3 = E2*E1;
  E4 = E3*E1;
  E5 = E4*E1;
  E6 = E5*E1;
  E7 = E6*E1;
  lat1 = northing/A;
  M = get_M(lat1);

  for(i=0;i<10;i++){
    lat1 += (northing-M)/A;
    M = get_M(lat1);
  }

  nu = get_nu(lat1);
  rho = get_rho(nu,lat1);
  eta = get_eta(nu,rho);
  eta2 = eta*eta;
  tan1 = tan(lat1);
  tan2 = tan1*tan1;
  tan4 = tan2*tan2;
  tan6 = tan4*tan4;
  sec1 = 1.0/(cos(lat1));
  nu3 = nu*nu*nu;
  nu5 = nu3*nu*nu;
  nu7 = nu5*nu*nu;
  VII = tan1/(2.0*rho*nu);
  VIII = tan1/(24.0*rho*nu3)*(5.0+3.0*tan2+eta2-9.0*tan2*eta2);
  IX = tan1/(720.0*rho*nu5)*(61.0+90.0*tan2+45.0*tan4);
  X = sec1/nu;
  XI = sec1/(6.0*nu3)*(nu/rho+2.0*tan2);
  XII = sec1/(120.0*nu5)*(5.0+28.0*tan2+24.0*tan4);
  XIIA = sec1/(5040.0*nu7)*(61.0+662.0*tan2+1320.0*tan4+720.0*tan6);

  /* 11 Jul 2003 [Nicholas Boers]
     Swapped the x and y, because values being returned were backwards. */
  lonlat->x = E1*X-E3*XI+E5*XII-E7*XIIA + (lonOrig * DEG_TO_RAD);
  lonlat->y = lat1 - E2*VII+E4*VIII-E6*IX;
}
