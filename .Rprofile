setwd("R") # set the directory to the R folder

r <- getOption("repos") # hard code the UK repo for CRAN
r["CRAN"] <- "https://www.stats.bris.ac.uk/R/"
options(repos = r)
rm(r)

source(file.path(Sys.getenv(
  if (.Platform$OS.type == "windows") "USERPROFILE" else "HOME"
), ".vscode-R", "init.R"))


# options(languageserver.formatting_style = function(options) {
#     styler::tidyverse_style(scope = "indention", indent_by = options$tabSize)
# })
