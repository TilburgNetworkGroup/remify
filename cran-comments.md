# remify 3.2.5
 
* Date: 2024-02-05
* This is the 3.2.5 version of the package. In this version, events are dropped from the edgelist only if at least one missing value is found in at least one of the columns: "time", "actor1", "actor2", "type", "weight". Bug resolved when "ncores > 1" with function "getEventComposition()". Bug correction in 'getDyadID' method.
 
## Test environments 
* Local macOS Sonoma 14.0, R version 4.3.1 
* Local Windows 10, R version 4.3.1 
* rhub check: Fedora Linux, R-devel, clang, gfortran
* rhub check: Ubuntu Linux 20.04.1 LTS, R-release, GCC
* rhub check: Windows Server 2022, R-devel, 64 bit 
 
## R CMD check results
There were no ERRORs, no WARNINGs, no NOTEs.
