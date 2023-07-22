# This script will unzip the elsa zip file
# All the other scripts should work after it.

if (!dir.exists("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/")) {
    readLines("Do you want to unzip the ELSA files")
    unzip(
        zipfile = "../../data/ELSA/5050tab_BDDEA8F799D95FB6A9F6FB5ADBE43D55F7B50718D8615C88A14F94298AADF043_V1.zip",
        exdir = "../../data/ELSA/elsa_unziped"
    )
}
