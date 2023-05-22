# Look at pension wealth for those who retired

library(data.table)
library(dplyr)
library(stringr)

# Set the correct working directory.
# Note you need to run the whole script to get this to work.
# I.e crtl+shift+enter.
cur_dir <- getSrcDirectory(function(x) {
    x
})
setwd(cur_dir)
stop() # Remove if running the whole thing through

setwd(dirname(rstudioapi::getSourceEditorContext()$path))


pension_files <- dir("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/",
    pattern = "wave",
    full.names = T
) %>%
    grep("pen", ., value = T)
# There are two types of pension file
# Lets figure out what the differnces are.
head(pension_files)


fread(pension_files[[1]]) %>%
    names() %>%
    grep("", ., value = T)

# Join across waves I think. Is there a unique joiner in each household
lapply(financial_files, \(x) "idauniq" %in% names(fread(x)))

ReadAndSetWave <- function(filename) {
    wave <- str_extract("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/wave_5_financial_derived_variables.tab", "wave_\\d{1}")
    data <- fread(filename)
    data[, wave := wave]
    return(data)
}

all_pension <- lapply(pension_files, ReadAndSetWave) %>%
    rbindlist(fill = T, use.names = T)

# there is partner income

head(all_pension)

# do annuity rates change in the data

# was eric french's value function iteration paper based on elsa data
# could i do that?
