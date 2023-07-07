library(data.table)
library(dplyr)
library(stringr)

source("__ELSA_functions.R")

dir("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/",
    pattern = "core",
    full.names = TRUE
) %>%
    grep("", ., value = TRUE)

# %>%
# lapply(ReadAndSetWave) %>%
# rbindlist(fill = TRUE, use.names = TRUE)

eol_data <- fread("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/h_elsa_eol_a2.tab")

ncol(eol_data)
nrow(eol_data)

rad_vars <- search_names(eol_data, "rad")

lapply(rad_vars, function(name) unique(eol_data[[name]]))

rad_vars[24]

eol_data$radage %>% unique()
eol_data$radexpec %>% unique()
eol_data$rad
eol_data$mortstat

# is this the combined data or something like that?
# looks like it could be.
# i only have year of birth and month-year of interview
# I think radage  is age at death.
# check it against the harmonised data set to be sure.


# check whether.
eol_data$radyear %>% unique()

eol_data$radage


eol_data %>% names()

search_names(harm, "radyear")

# lets take some ids and check

eol_check <- eol_data %>%
    select(
        idauniq,
        eol_radage = radage
    )

harm_check <- harm %>%
    select(idauniq, radyear, rabyear) %>%
    filter(!is.na(radyear)) %>%
    mutate(harm_radage = radyear - rabyear) %>%
    select(idauniq, harm_radage)

# completely different. maybe the eol data is completely different
full_join(eol_check, harm_check)



# lets check all the tab files for the right variable name


# EIDATEY
files_with_death <- dir("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/",
    pattern = "",
    full.names = TRUE
) %>%
    lapply(function(filename) names(fread(filename, nrows = 1))) %>%
    lapply(function(col_names) grep("eida", col_names, value = T, ignore.case = T)) %>%
    sapply(length) %>%
    sapply(\(x) x != 0)

dir("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/",
    pattern = "",
    full.names = TRUE
)[files_with_death]

# cool in here.
# lets just use it
#


# lets use the harmonised file death year.
# not many of them
# we have age at interview
# month of interview
# and year of birth
# we can then narrow down a bit when they are born
# once we have this we can estimate rough probs for death.
# and then we can adjust ons death probs a bit.


harm %>% names()

long_harm <- fread("../../data/ELSA/elsa_to_use/harmonised_data_long.csv")
names(long_harm)
long_harm$age_at_interview

# logic for birthday
# assume interviews are mid month
# interivew date gives us upper and lower bound on birthday
# or do we just use birth year and death year
# probably not gonna gain much
# have a look at their stata code to see how they update use death rates
# they reg hazard on a cubic of age
#
