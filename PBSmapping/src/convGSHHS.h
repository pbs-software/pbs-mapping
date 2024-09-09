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

#ifndef _CONVGSHHS_H_
#define _CONVGSHHS_H_

#include <R.h>
#include <Rdefines.h>

/*-----------------------------------------------------------------------------
  importGSHHS:
    This function is called by R to extract a region.

   Arguments:
     gshhsFileName	file name (string)
     clipLimits		longitude/latitude limits for extraction
     levels		maximum level to extract
     minVerts		minimum vertices in an extracted polygon
  ---------------------------------------------------------------------------*/
SEXP importGSHHS(SEXP gshhsFileName, SEXP clipLimits, SEXP levels, SEXP minVerts);

#endif /* _CONVGSHHS_H_ */
