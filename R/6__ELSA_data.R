# This script will sort out the ELSA data.

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

# I don't think there is consumption data but there is income data
# there must be consumption data
# And there is pension data and hopefully annuity data as well
if (!dir.exists("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/")) {
    readLines("Do you want to unzip the ELSA files")
    unzip(
        zipfile = "../../data/ELSA/5050tab_BDDEA8F799D95FB6A9F6FB5ADBE43D55F7B50718D8615C88A14F94298AADF043_V1.zip",
        exdir = "../../data/ELSA/elsa_unziped"
    )
}


financial_files <- dir("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/",
    pattern = "wave",
    full.names = T
) %>%
    grep("financial_", ., value = T)



fread(financial_files[[1]]) %>%
    names() %>%
    grep("exp", ., value = T)

# Join across waves I think. Is there a unique joiner in each household
lapply(financial_files, \(x) "idauniq" %in% names(fread(x)))

ReadAndSetWave <- function(filename) {
    wave <- str_extract("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/wave_5_financial_derived_variables.tab", "wave_\\d{1}")
    data <- fread(filename)
    data[, wave := wave]
    return(data)
}

all_finance <- lapply(financial_files, ReadAndSetWave) %>%
    rbindlist(fill = T, use.names = T)



all_finance

## aim is to figure out what change there was in the spending habit of retirees.
# where is the documentation?
