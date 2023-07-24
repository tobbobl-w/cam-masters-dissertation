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

reg_data <- fread(
    "../../data/ELSA/elsa_to_use/elsa_reg_data.csv"
)

# reg_data$retirement_year
# reg_data$retired_age

dt_for_julia <- reg_data[, .(id_wave,
    gender = ragender, age = age_at_interview,
    retired_age,
    retirement_year,
    public_pension,
    ever_dc_pen,
    ever_db_pen,
    year = date_year,
    fin_wealth,
    dc_pot
)]

dt_for_julia[, sum(dc_pot > fin_wealth, na.rm = T)]

dt_for_julia[dc_pot > fin_wealth]
dt_for_julia[fin_wealth > dc_pot]


nrow(dt_for_julia)

dt_for_julia[, gender := fcase(
    gender == 1, "male",
    gender == 0, "female"
)]


# , ever_dc_pen, ever_db_pen
gender_age_year_unique <- dt_for_julia[, .(gender, age, year)] %>%
    unique()

# check no NAs in any columns
stopifnot(!any(sapply(gender_age_year_unique, \(x) any(is.na(x)))))

fwrite(
    gender_age_year_unique,
    "../../data/ELSA/elsa_to_use/gender_age_year_unique.csv"
)
# We want to run the life cycle model for each of these gender-pension-age pairs.


fwrite(
    dt_for_julia,
    "../../data/ELSA/elsa_to_use/for_julia.csv"
)

fwrite(
    select(dt_for_julia, id_wave, age, gender, year),
    "../../data/ELSA/elsa_to_use/for_julia_if_age_gender_year.csv"
)
#

# ids to copy


# we also want to add any other source of income
# higher the income the less likely annuitisation



# So what do we want to do next.
# read in the jsons
# then simulate consumption with assets
# also run with bequest motive
# How do I change the annuity amount without
#  having to run this function thousands of times
#  add another field as income and then add it to the matrix algebra?
#  dont think it would be too much slower but it could take a while to get right


# ------------ prep subjective life expectancies -----------

subjective_life_probs <- fread(
    "../../data/ELSA/subjective_tables/subjective_survival_probs.csv"
)

life_probs_we_need <- subjective_life_probs[id_wave %in% dt_for_julia$id_wave]

## Small comparison between subjective and objective for one person
sub_for_plot <- life_probs_we_need[dt_for_julia[, .(id_wave, year, age, gender)], on = c("id_wave", "age")] %>%
    filter(id_wave == "100005-5") %>% # male born who is 64 in 2011
    mutate(type = "subjective") %>%
    mutate(age = expected_age - 1) %>%
    select(age, cum_alive = prob, type)

# compare to objective for a male aged 64 in 2011 (this is characteristics of )

obj_dat <- fread("../../data/ONS/males_transition_probs.csv")

obj_for_plot <- obj_dat %>%
    select(age, death_prob = `1947`) %>%
    filter(age >= 64) %>%
    mutate(alive_prob = 1 - death_prob) %>%
    mutate(cum_alive = cumprod(alive_prob)) %>%
    mutate(type = "objective") %>%
    select(age, cum_alive, type)

all_for_plot <- rbind(sub_for_plot, obj_for_plot)

# ggplot(all_for_plot, aes(x = age, y = cum_alive, group = type)) +
#     geom_point()

## cool this looks about right
# lets save objective data as year by year death probs as well
# functions are designed to take death prob


# --------- transform subjective probs ---------
# reverse the cumprod operation by flipping
# want to have year by year death rate
life_probs_to_save <- subjective_life_probs %>%
    mutate(start_age = age) %>%
    mutate(age = expected_age - 1) %>%
    rename(cum_alive_prob = prob) %>%
    mutate(cum_death_prob = 1 - cum_alive_prob) %>%
    select(id_wave, start_age, age, cum_alive_prob, cum_death_prob) %>%
    group_by(id_wave) %>%
    mutate(year_death_prob = (lag(cum_alive_prob) - cum_alive_prob) / lag(cum_alive_prob)) %>% # i.e. prob you die in a year conditional on being born that year
    mutate(year_death_prob = ifelse(is.na(year_death_prob), cum_death_prob, year_death_prob)) %>%
    mutate(year_alive_prob = 1 - year_death_prob) %>%
    group_by(id_wave) %>%
    mutate(cum_alive_prob_2 = cumprod(year_alive_prob)) #

# Check that retransforming the new death probs creates the same
# alive probs
life_probs_to_save %>%
    summarise(
        diff = mean(cum_alive_prob_2 - cum_alive_prob),
        equal = all.equal(cum_alive_prob_2, cum_alive_prob)
    ) %>%
    summarise(mean(diff), mean(equal)) # nice all equal
# ok these look like rounding errors
#

sub_death_probs <- life_probs_to_save %>%
    select(id_wave, age, death_prob = year_death_prob)


fwrite(
    sub_death_probs,
    "../../data/ELSA/subjective_tables/subjective_death_probs_for_julia.csv"
)
# fread("../../data/ELSA/subjective_tables/subjective_death_probs_for_julia.csv") %>% head()
