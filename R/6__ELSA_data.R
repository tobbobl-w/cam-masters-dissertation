# This script will sort out the ELSA data.

library(data.table)
library(dplyr)

# Set the correct working directory.
# Note you need to run the whole script to get this to work.
# I.e crtl+shift+enter.
cur_dir <- getSrcDirectory(function(x) {
    x
})
setwd(cur_dir)
stop() # Remove if running the whole thing through


# I don't think there is consumption data but there is income data
# And there is pension data and hopefully annuity data as well
if (!dir.exists("Data/ELSA/elsa_unziped/UKDA-5050-tab/tab/")) {
    readLines("Do you want to unzip the ELSA files")
    unzip(
        zipfile = "Data/ELSA/5050tab_BDDEA8F799D95FB6A9F6FB5ADBE43D55F7B50718D8615C88A14F94298AADF043_V1.zip",
        exdir = "Data/ELSA/elsa_unziped"
    )
}

financial_files <- dir("Data/ELSA/elsa_unziped/UKDA-5050-tab/tab/",
    pattern = "wave",
    full.names = T
) %>%
    grep("financial_", ., value = T)

fread(financial_files[[1]]) %>%
    names() %>%
    grep("exp", ., value = T)
# Join across waves I think. Is there a unique joiner in each household


lapply(financial_files, \(x) "idauniq" %in% names(fread(x)))
