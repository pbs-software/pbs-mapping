## PBSmapping: Mapping fisheries data and spatial analysis tools ##
&copy; Fisheries and Oceans Canada (2003-2024)

This software has evolved from fisheries research conducted at the Pacific Biological Station (PBS) in Nanaimo, British Columbia, Canada. It extends the R language to include two-dimensional plotting features similar to those commonly available in a Geographic Information System (GIS). Embedded C code speeds algorithms from computational geometry, such as finding polygons that contain specified point events or converting between longitude-latitude and Universal Transverse Mercator (UTM) coordinates. Additionally, we include C++ code developed by Angus Johnson for the Clipper library, data for a global shoreline, and other data sets in the public domain.

**PBSmapping** represents just one of a <a href="https://github.com/pbs-software">series of R packages</a> developed at the Pacific Biological Station (<a href="http://www.pac.dfo-mpo.gc.ca/science/facilities-installations/index-eng.html#pbs">PBS</a>) in Nanaimo, British Columbia. Users of PBSmapping wanting a stable release should obtain it from <a href="https://CRAN.R-project.org/package=PBSmapping">CRAN</a>.

This GitHub site assists developers in tracking issues, and may offer a more advanced version of **PBSmapping** than that on CRAN. Evolving packages (Windows binary and source tarball) are built after using CRAN's rigorous `R CMD check --as-cran` routine (using R-devel on a **Windows 10** 64-bit system). Most of the time, the current revision on <a href="https://github.com/pbs-software/pbs-mapping">GitHub</a> can be built in R using `devtools::install_github("pbs-software/pbs-mapping/PBSmapping")`; however, not every revision has been checked for CRAN worthiness.

New features in PBSmapping since its first inception:
<ul style="list-style-type:disc;">
  <li>hexagonal grid cells (default = rectangular cells)</li>
  <li>clockwise map rotation (though coordinates become relative)</li>
  <li>compass rose</li>
</ul> 

As with any freely available product, there is no warranty or promise that **PBSmapping** will perform adequately for all circumstances. Additionally, coding errors are possible, and users should contact the package maintainer if bugs are detected.

**Note** that **PBSmapping** version 2.73.4 on CRAN lacks the function `importShapefile` because the function depended on the `maptools` package by Roger Bivand and company, which was removed voluntarily from CRAN Oct 16, 2023. Roger has given permission to use his C code, but this will depend on finding someone capable of incorporating `Rshapeget`.

Maintainer: <a href="mailto:rowan.haigh@dfo-mpo.gc.ca">Rowan Haigh</a>

<p align="right"><img src="DFOlogo_small.jpg" alt="DFO logo" style="height:30px;"></p> 
