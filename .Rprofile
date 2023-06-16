lib_path <- paste0(dirname(getwd()), "/r_packages")
# I have a set of packages and specific versions that work with this proejct.
# These can be found in the current_packages.csv file.

.libPaths(lib_path) # set the package directory

setwd("R") # set the directory to the R folder


r <- getOption("repos") # hard code the UK repo for CRAN
r["CRAN"] <- "https://www.stats.bris.ac.uk/R/"
options(repos = r)
rm(r)