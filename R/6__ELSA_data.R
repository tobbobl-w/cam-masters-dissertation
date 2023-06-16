# This script will unzip the elsa zip file

library(data.table)
library(dplyr)
library(stringr)

install.packages("stringr")



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
