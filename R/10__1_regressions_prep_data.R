library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)

# so we are comparing pre and post retirement consumption
# both sides of the reform

# so new retirees in 2011-12 and new retirees in 2014-15


# small things
# 1. consumption data
# 2. interview age
# 3. interview year
# 3. expected retirement age
# 4. couple or single


pension_wealth <- fread(
    "../../data/ELSA/elsa_to_use/elsa_dc_pen_wealth_predictions.csv"
)
setnames(pension_wealth, "year", "int_year")

class(pension_wealth$dc_pot)

harm_long <- fread(
    "../../data/ELSA/elsa_to_use/harmonised_data_long.csv"
)[in_wave == 1]

harm_long[, int_year := year(int_month_date)]

harm_long <- merge.data.table(
    harm_long,
    pension_wealth,
    by = c("idauniq", "int_year"),
    all.x = TRUE,
    all.y = FALSE
)

harm_long[, dc_pot := as.numeric(dc_pot)]

# set expected retirement age
harm_long[, ":="(
    first_exp_ret_age = expected_retired_age[!is.na(expected_retired_age)][1],
    no_exp_ret_age = all(is.na(expected_retired_age))),
by = .(idauniq)
]


harm_long[, retirement_year := rabyear + retired_age]

# Set to treatment or control. I.e. in years prior to reform or years before
harm_long[, pre_post_ref := case_when(
    retirement_year %in% c(2013, 2012, 2011) ~ "control",
    retirement_year %in% c(2015, 2016, 2017) ~ "treat"
)]
table(harm_long$pre_post_ref)



# want to find people who have had dc pension at some point
# crete the following variable
# ever have a dc pension
# 1 dc, 2 db, 3 either

harm_long[, has_dc_pension := case_when(
    pension_type_job1 == 1 ~ "yes",
    pension_type_job2 == 1 ~ "yes",
    pension_type_job3 == 1 ~ "yes",
    T ~ "no"
)]

harm_long[, ever_dc_pen := fifelse(
    any(has_dc_pension == "yes"),
    "yes", "no"
),
by = .(idauniq)
]

harm_long[, has_db_pension := case_when(
    pension_type_job1 == 2 ~ "yes",
    pension_type_job2 == 2 ~ "yes",
    pension_type_job3 == 2 ~ "yes",
    T ~ "no"
)]

harm_long[, ever_db_pen := fifelse(
    any(has_db_pension == "yes"),
    "yes", "no"
),
by = .(idauniq)
]


harm_long[, years_since_retirement := age_at_interview - retired_age]



# Now we want people in the three years after they have retired.
AddYearsAround <- function(year, add = 2) {
    if (is.na(year)) {
        NA
    } else {
        seq.int(year, year + add, by = 1)
    }
}

harm_long$fuzzy_retirement <- lapply(harm_long$retired_age, AddYearsAround, 2)

harm_long[, ":="(
    ret_in = mapply("%in%", age_at_interview, fuzzy_retirement),
    ret_equals = retired_age == age_at_interview), by = .(idauniq)]

harm_long[ret_in & ret_equals, .(retired_age, age_at_interview, ret_in)]



# We want recent retirees in the three years after they have retired.
data_for_regressions <- harm_long[ret_in == TRUE & !is.na(pre_post_ref)]


data_for_regressions[, id_wave := paste0(idauniq, "-", wave)]


data_for_regressions[, ragender := ragender - 1] # make it binary rather than 1,2
# Check that this is the case
stopifnot(all(data_for_regressions$ragender %in% c(0, 1)))



data_for_regressions[, ever_dc_pen_bin := ever_dc_pen == "yes"]
data_for_regressions[, ever_db_pen_bin := ever_db_pen == "yes"]
data_for_regressions[, exp_real_ret_age := first_exp_ret_age - retired_age]



fwrite(
    data_for_regressions,
    "../../data/ELSA/elsa_to_use/elsa_reg_data.csv"
)
