Utils - PBSmapping command line utilities
-----------------------------------------

Stand-alone command-line mapping utilities that can handle large data sets.
These utilities may process files that are too large for the R working environment.
Original code by Nicholas Boers, modified by Alex Couture-Beil.

Compilation Directions for Windows Users
----------------------------------------

The batch file makeUtils.bat has been provided to help windows users compile the
three command-line mapping utilities. The batch file requires that you install
minGW (Minimalist GNU for Windows)

1) download the latest minGW installer (currently MinGW-5.1.3.exe at the time)
from http://sourceforge.net/project/showfiles.php?group_id=2435&package_id=82721

2) When asked to choose components to install, you must select
   * g++ compiler
   * MinGW Make

3) Choose a destination folder. We suggest c:\Utils\MinGW

4) After MinGW is installed, edit makeUtils.bat and update the MINGW_PATH value.
   If you chose "c:\Utils\MinGW", then you should set the path value to 
   "c:\Utils\MinGW\bin". Note that you will have to append "bin" to the 
   choosen installation folder to form the correct path.

5) run makeUtils.bat

Alternatively you may wish to look into installing bloodshed dev-c++ which is
a free IDE (integrated development environment) that includes MinGW.


Command Line Utilities
----------------------

clipPolys
	Clips a PolySet, where each (PID, SID) set describes a unique polygon.

convUL
	Converts coordinates between UTM (km) and Lon/Lat.

findPolys
	Finds the polygons in a PolySet that contain events specified by EventData.

See PBSmapping-UG.pdf (p.21-23) located in root directory of PBSmapping library.
