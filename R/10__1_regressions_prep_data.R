library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)

# so we are comparing pre and post retirement consumption
# both sides of the reform

# so new retirees in 2011-12 and new retirees in 2014-15


# small things
# 1. couple or single

Policy_year <- 2014


# ------- Pension wealth data --------------
pension_wealth_dt <- fread(
  "data/ELSA/elsa_to_use/elsa_dc_pen_wealth_predictions.csv"
)

# ------- Life expectancy data -----------
obj_life_exps_dt <- fread("data/ONS/objective_life_expectancies.csv") |>
  _[, gender_string := fcase(
    gender == "Males", "male",
    gender == "Females", "female"
  )]

sub_life_exps_dt <- fread("data/ELSA/subjective_tables/subjective_life_expectancies.csv")


# ---------- ELSA data -----------
harm_long <- fread(
  "data/ELSA/elsa_to_use/harmonised_data_long.csv"
)[in_wave == 1]

harm_long[, int_year := year(int_month_date)]
harm_long[, birth_year := int_year - age_at_interview]
harm_long[, id_wave := paste0(idauniq, "-", wave)]
harm_long[, ragender := fcase(
  ragender == 1, 0,
  ragender == 2, 1
)]


harm_long[, gender_string := fcase(
  ragender == 0, "male",
  ragender == 1, "female" # one is female
)]

harm_long[pension_wealth_dt, dc_pot := dc_pot, on = c("idauniq", "int_year" = "year")]

harm_long[obj_life_exps_dt, obj_life_exps := life_exp,
  on = c("age_at_interview" = "age", "gender_string", "birth_year")
]

harm_long[sub_life_exps_dt, sub_life_exps := subjective_life_exp,
  on = c("id_wave")
]

harm_long[, dc_pot := as.numeric(dc_pot)]

# set expected retirement age
harm_long[, ":="(
  first_exp_ret_age = expected_retired_age[!is.na(expected_retired_age)][1],
  no_exp_ret_age = all(is.na(expected_retired_age))),
by = .(idauniq)
]

harm_long[, retirement_year := rabyear + retired_age]

# Set to treatment or control. I.e. in years prior to reform or years before
years_after <- seq.int(Policy_year + 1, Policy_year + 3)
years_before <- seq.int(Policy_year - 1, Policy_year - 3)

harm_long[, pre_post_ref := case_when(
  retirement_year %in% years_before ~ "control",
  retirement_year %in% years_after ~ "treat"
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

names(harm_long)

harm_long[, .N, expected_retired_age]
harm_long[, expected_retired_year := rabyear + expected_retired_age]

harm_long[, hist(rabyear)]
setorder(harm_long, int_year)

harm_long[, .(
  expected_retired_year_nm =
    expected_retired_year[!is.na(expected_retired_year)][1]
), by = .(idauniq)]

wide_dt <- dcast(harm_long,
  idauniq + expected_retired_year + rabyear ~ int_year,
  value.var = c("total_monthly_consumption", "public_pension")
)

wide_dt[, rv_ex := 2013 - expected_retired_year]
wide_dt[, rv_dob := rabyear - (2013 - 65)]

MakePlot <- function(rv_variable) {
  wide_dt[!is.na(rv) & rv >= -3 & rv <= 3,
    lapply(.SD, \(x) list(mean = mean(x, na.rm = TRUE), non_missing = sum(!is.na(x)))),
    .SDcols = patterns("total_monthly_consumption"),
    by = .(rv),
    env = list(rv = rv_variable)
  ]
}

MakePlot("rv_dob")



# Ran all of my regressions wrong oops

# We want recent retirees in the three years after they have retired.
data_for_regressions <- harm_long[ret_in == TRUE & !is.na(pre_post_ref)] %>%
  # One value of 450,000 for consumption
  filter(total_monthly_consumption < 3500)


data_for_regressions[, id_wave := paste0(idauniq, "-", wave)]

data_for_regressions[, ever_dc_pen_bin := as.integer(ever_dc_pen == "yes")]
data_for_regressions[, ever_db_pen_bin := as.integer(ever_db_pen == "yes")]
data_for_regressions[, exp_real_ret_age := first_exp_ret_age - retired_age]


hist(data_for_regressions$monthly_leisure)


# Filtering data
data_for_regressions <- data_for_regressions[monthly_leisure < 1000 | is.na(monthly_leisure)] # 3 rows
data_for_regressions <- data_for_regressions[monthly_food_in < 500 | is.na(monthly_food_in)] # 4 rows
data_for_regressions <- data_for_regressions[monthly_utility < 800 | is.na(monthly_utility)] # 1 row

table(data_for_regressions$pre_post_ref)
# drop early retirees
data_for_regressions <- data_for_regressions[retired_age >= 55]

fwrite(
  data_for_regressions,
  "../../data/ELSA/elsa_to_use/elsa_reg_data.csv"
)
