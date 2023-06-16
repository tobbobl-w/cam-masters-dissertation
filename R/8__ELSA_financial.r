library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)

source("__ELSA_functions.R")

# Compile finance files
# What is in the finance files?
all_finance <- dir("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/",
    pattern = "wave",
    full.names = TRUE
) %>%
    grep("financial_", ., value = TRUE) %>%
    lapply(ReadAndSetWave) %>%
    rbindlist(fill = TRUE, use.names = TRUE)

# state pension mean to check weekly or monthly
all_finance[spen_r_i > 0, mean(spen_r_i, na.rm = T)] # cool yes must be weekly

# Mean of annuity income
all_finance[anin_r_i > 0, mean(anin_r_i, na.rm = T)] # weekly?
all_finance[, mean(anin_r_i == 0, na.rm = T)] # wow most of sample has none.



# mean of annuity pension wealth variable
all_finance[ppinc_bu_s > 0, mean(ppinc_bu_s, na.rm = T)] # not sure how this is different to annuity then?

all_finance[, mean(ppinc_bu_s > 0, na.rm = T)]
# not sure how this is different to annuity then?

# anin_r_i anin_p_i wppp_r_i wppp_p_i

# ppinc_bu_s this is called total annuitised income.
# private plus other?
# "BU tot annuitised income (priv pen + oth annuity income) - summary var
# But I don't actually know what that means


# let's split these up by cohort, age and year
# see if we get any nice differences

# IFS derived variables
# I should probably use these if i don't expect to make
# any changes to the raw files
# these have demographic info and some other stuff which is useful,
# doesn't seem to have income variables etc though.

all_ifs <- dir("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/",
    pattern = "wave",
    full.names = TRUE
) %>%
    grep("ifs", ., value = TRUE) %>%
    lapply(ReadAndSetWave) %>%
    rbindlist(fill = TRUE, use.names = TRUE)

# Add interview date
all_ifs[, intdate := paste0(intdaty, "-", intdatm)]
all_ifs[, intdate := lubridate::ym(intdate)]
hist(all_ifs$intdate,
    breaks = 50.0
)


all_finance$file_wave %>% head()
nrow(all_ifs)
nrow(all_finance)
head(all_ifs$wave)
# I think these are the pension data that is in the pension grid documentation
# I haven't had a proper look at the code yet though
# These seem to be more about where the pensions were from in terms of which employer etc

all_pension <- dir("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/",
    pattern = "wave",
    full.names = TRUE
) %>%
    grep("pen", ., value = TRUE) %>%
    lapply(ReadAndSetWave) %>%
    rbindlist(fill = TRUE, use.names = TRUE)


# join ifs, pension and financial by wave and identifier

search_names(all_finance, "idauniq")

all_joined_data <- Reduce(
    function(x, y) full_join(x, y, by = c("file_wave", "idauniq")),
    list(all_finance, all_ifs)
)

names_to_trim <- search_names(all_joined_data, "\\.(x|y)") %>%
    sort() %>%
    str_split("\\.") %>% # ie split along the period
    lapply(function(x) x[1]) %>% # only get the string before the dot
    unlist() %>% # unlist to get a vector
    unique() # only want unique

for (name in names_to_trim) {
    newname <- name
    oldname <- paste0(name, ".x")
    stopifnot(oldname %in% names(all_joined_data))

    names_to_remove <- search_names(all_joined_data, paste0(name, "."))
    setnames(all_joined_data, oldname, newname)
    for (remove_name in names_to_remove) {
        all_joined_data[, ":="(NAME = NULL), env = list(NAME = remove_name)]
    }
}


fwrite(all_joined_data, "../../data/ELSA/elsa_to_use/elsa_ifs_finance.csv")
