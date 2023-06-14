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
stop() # Remove if you want to run from source.

source("__ELSA_functions.R")

ELSA_dir("wave_1")

wave_8_data <- ReadAndSetWave("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/wave_9_elsa_data_eul_v1.tab")

names(wave_8_data)

search_names <- function(data, string) {
    grep(string, names(data), value = TRUE)
}

search_names(wave_8_data, "capam")

# These are social care related expenditures.
wave_8_data$capam[wave_8_data$capam > 0]
wave_8_data$cahsc[wave_8_data$cahsc > 0]


# Slight aside
# to make searching for variables in the documentation easier I will
# convert rtf files to excel
# this isn't working
dir("../../data/ELSA/elsa_unziped/UKDA-5050-tab/mrdoc/ukda_data_dictionaries/elsa_eol_w3_archive_v1_ukda_data_dictionary.rtf")

ReadRTF("../../data/ELSA/elsa_unziped/UKDA-5050-tab/mrdoc/ukda_data_dictionaries/elsa_eol_w3_archive_v1_ukda_data_dictionary.rtf")


Lines <- readLines("../../data/ELSA/elsa_unziped/UKDA-5050-tab/mrdoc/ukda_data_dictionaries/elsa_eol_w3_archive_v1_ukda_data_dictionary.rtf") # nolint
pat <- "^\\\\f0.*\\\\cf0 "
g <- grep(pat, Lines, value = TRUE)
noq <- gsub("\\\\'", "'", g)

head(Lines, n = 20)

clean <- sub("\\\\.*", "", sub(pat, "", noq))
return(clean)
