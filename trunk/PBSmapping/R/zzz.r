.First.lib <- function(lib, pkg)
{
  library.dynam("PBSmapping", pkg, lib);
  cat(
"PBS Mapping 2.59.01  Copyright (C) 2003-2009 Fisheries and Oceans Canada\
\
PBS Mapping comes with ABSOLUTELY NO WARRANTY; for details see the\
file COPYING.  This is free software, and you are welcome to redistribute\
it under certain conditions, as outlined in the above file.\
\
A complete user's guide 'PBSmapping-UG.pdf' appears\
in the '.../library/PBSmapping/doc' folder.
\
To see demos, type '.PBSfigs()'.\
\
Built on Jul 30, 2009\
Pacific Biological Station, Nanaimo\n\n")
}

