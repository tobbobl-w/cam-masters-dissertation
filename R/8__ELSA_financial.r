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

ReadAndSetWave <- function(filename) {
    wave <- str_extract(filename, "wave_\\d{1}")
    data <- fread(filename)
    data[, wave := wave]
    return(data)
}


# Compile finance files

financial_files <- dir("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/",
    pattern = "wave",
    full.names = TRUE
) %>%
    grep("financial_", ., value = TRUE)

# Join across waves I think. Is there a unique joiner in each household
lapply(financial_files, \(x) "idauniq" %in% names(fread(x)))


all_finance <- lapply(financial_files, ReadAndSetWave) %>%
    rbindlist(fill = TRUE, use.names = TRUE)

all_ifs <- dir("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/",
    pattern = "wave",
    full.names = TRUE
) %>%
    grep("ifs", ., value = TRUE) %>%
    lapply(ReadAndSetWave) %>%
    rbindlist(fill = TRUE, use.names = TRUE)


all_pension <- dir("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/",
    pattern = "wave",
    full.names = TRUE
) %>%
    grep("pen", ., value = TRUE) %>%
    lapply(ReadAndSetWave) %>%
    rbindlist(fill = TRUE, use.names = TRUE)


fwrite(all_pension, "../../data/ELSA/elsa_to_use/elsa_pensions.csv")
fwrite(all_ifs, "../../data/ELSA/elsa_to_use/elsa_pensions.csv")
fwrite(all_finance, "../../data/ELSA/elsa_to_use/elsa_pensions.csv")

head(all_finance$idauniq)

full_join(all_pension, all_ifs)

sum(all_pension[wave == "wave_1"]$idauniq %in%
    all_pension[wave == "wave_2"]$idauniq)
# does idauniq link across waves











# ppen_bu_i - BU private pensions (iappei/iappmo) - value (incl. imputed values)
# indobyr - year of birth
# age
# idauniq - unique wave identifier

all_finance[, .(ppen_bu_i, indobyr, age)]



all_finance[, ]
