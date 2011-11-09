.onLoad <- function(lib, pkg)
{
	library.dynam("PBSmapping", pkg, lib);
	pkg_info <- utils::sessionInfo( package="PBSmapping" )$otherPkgs$PBSmapping
	if( is.character( pkg_info$Packaged ) )
		pkg_date <- strsplit( pkg_info$Packaged, " " )[[1]][1]
	else
		pkg_date  <- "unkown"
	
	userguide_path <- system.file( "doc/PBSmapping-UG.pdf", package = "PBSmapping")
	
	cat("
-----------------------------------------------------------
PBS Mapping", pkg_info$Version, "-- Copyright (C) 2003-2011 Fisheries and Oceans Canada

PBS Mapping comes with ABSOLUTELY NO WARRANTY;
for details see the file COPYING.
This is free software, and you are welcome to redistribute
it under certain conditions, as outlined in the above file.

A complete user guide 'PBSmapping-UG.pdf' is located at 
", userguide_path, "

To see demos, type '.PBSfigs()'.

Packaged on", pkg_date, "
Pacific Biological Station, Nanaimo
-----------------------------------------------------------


")
}

