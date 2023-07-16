library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(readxl)
library(tidyr)
library(lubridate)
library(haven)
library(patchwork)

source("__ELSA_functions.R")

# this is the distance that we use on the asset grid in Julia
grid_distance <- 250


reg_data <- fread(
    "../../data/ELSA/elsa_to_use/elsa_reg_data.csv"
)

## now for julia we need to round financial assets
# what to do with negative assets
# reg_data$fin_wealth %>% hist()


reg_data[, round_fin_wealth :=
    round(fin_wealth / grid_distance, 0) * grid_distance] # round to nearest grid point

# do we need some other things?
# I am not sure...
dt_for_julia <- reg_data[, .(id_wave,
    gender = ragender, age = age_at_interview,
    round_fin_wealth, public_pension,
    ever_dc_pen, ever_db_pen,
    year = date_year
)]

dt_for_julia[, gender := fcase(
    gender == 1, "male",
    gender == 2, "female"
)]

dt_for_julia[, public_pension := round(public_pension / 100, 0) * 100] # round pension to the nearest 100

summary(dt_for_julia$round_fin_wealth)

# , ever_dc_pen, ever_db_pen
gender_age_year_unique <- dt_for_julia[!is.na(public_pension), .(gender, age, year)] %>%
    unique()

# check no NAs in any columns
stopifnot(!any(sapply(gender_pension_age_unique, \(x) any(is.na(x)))))

fwrite(
    gender_age_year_unique,
    "../../data/ELSA/elsa_to_use/gender_pension_age_unique.csv"
)
# We want to run the life cycle model for each of these gender-pension-age pairs.



fwrite(
    dt_for_julia,
    "../../data/ELSA/elsa_to_use/for_julia.csv"
)


dir("../../data/ELSA/lifecycle_outputs/", full.names = T) %>%
    length()
# its not actually that slow!

time_made <- dir("../../data/ELSA/lifecycle_outputs/", full.names = T) %>%
    lapply(file.info) %>%
    sapply(\(x) x$ctime) %>%
    sort()

# takes about a second
summary(time_made - lag(time_made), na.rm = T)
# could I speed up with parrellisation
# run for a few different levels of annuitization


# So what do we want to do next.
# read in the jsons
# then simulate consumption with assets
# also run with bequest motive
# How do I change the annuity amount without
#  having to run this function thousands of times
#  add another field as income and then add it to the matrix algebra?
#  dont think it would be too much slower but it could take a while to get right


## Ok now lets read in and see the rate of annuitisation we would expect
dir("../../data/ELSA/lifecycle_outputs/", full.names = T)

library(jsonlite)

read_json("../../data/ELSA/lifecycle_outputs/84_beqfalse_male_objective_2016")
