library(data.table)
library(dplyr)
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

harm_long <- fread(
    "../../data/ELSA/elsa_to_use/harmonised_data_long.csv"
)[in_wave == 1]

harm_long[, ":="(first_exp_ret_age = expected_retired_age[!is.na(expected_retired_age)][1],
    no_exp_ret_age = all(is.na(expected_retired_age))),
by = .(idauniq)
]

# harm_long$no_exp_ret_age %>% mean()
# quite a few

# table(harm_long$date_year)
# ggplot(data = data.frame(x = harm_long$date_year)) +
#     geom_histogram(
#         aes(x = x),
#         bins = length(unique(harm_long$date_year))
#     )

harm_long[, pre_post_ref := case_when(
    date_year %in% c(2012, 2011, 2010) ~ "control",
    date_year %in% c(2014, 2015, 2016) ~ "treat"
)]

# table(harm_long$pre_post_ref, useNA = "ifany")

# Ok that is slightly better
# this is control group
# do we only want unique ids?



# if we are doing diff in diff
# then we need consump at time before

harm_long[, has_dc_pension := case_when(
    pension_type_job1 == 1 ~ "yes",
    pension_type_job2 == 1 ~ "yes",
    pension_type_job3 == 1 ~ "yes",
    T ~ "no"
)]

table(harm_long$has_dc_pension)

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


# Don't use either because it doesnt give us much info.

# ever have a dc pension
# 1 dc, 2 db, 3 either
# want to find people who have had dc pension at some point


harm_long[, years_since_retirement := age_at_interview - retired_age]


# now also save financial wealth
# pension wealth
# state pension
# age
# and gender
# then we read these into julia and solve the lifecycle models






harm_long[, int_month_date := lubridate::dmy(paste0("01-", date_month, "-", date_year))]


AddYearsAround <- function(year, add = 2) {
    if (is.na(year)) {
        NA
    } else {
        seq.int(year, year + add, by = 1)
    }
}

harm_long$fuzzy_retirement <- lapply(harm_long$retired_age, AddYearsAround, 2)


harm_long[, ":="(ret_in = mapply("%in%", age_at_interview, fuzzy_retirement),
    ret_equals = retired_age == age_at_interview), by = .(idauniq)]

harm_long[ret_in & ret_equals, .(retired_age, age_at_interview, ret_in)]
# ok this works


data_for_regressions <- harm_long[ret_in == TRUE & !is.na(pre_post_ref)]

data_for_regressions[, id_wave := paste0(idauniq, "-", wave)]

# nice good number of people in both.
data_for_regressions[, table(ever_db_pen, ever_dc_pen)]

fwrite(
    data_for_regressions,
    "../../data/ELSA/elsa_to_use/elsa_reg_data.csv"
)

data_for_regressions[, unique(rabyear)] %>% sort()

nrow(data_for_regressions)


#### Plot consumption
consump_time_in <- harm_long[ret_in == TRUE,
    .(mean_consump = mean(total_monthly_consumption, na.rm = T)),
    by = .(date_year)
]

consump_time_eq <- harm_long[ret_equals == TRUE,
    .(mean_consump = mean(total_monthly_consumption, na.rm = T)),
    by = .(date_year)
]

ggplot() +
    geom_point(
        data = consump_time_in,
        aes(x = date_year, y = mean_consump, colour = "red")
    ) +
    geom_point(
        data = consump_time_eq,
        aes(x = date_year, y = mean_consump, colour = "blue")
    )


# control for age at retirement, retirement wealth
# and age from retirement?
# lets get retirement wealth now.


## Event study?
## No this is a diff in diff basically says that individuals are
# similar apart from for the reform (after controlling for vars)
lm(total_monthly_consumption ~ pre_post_ref,
    data = data_for_regressions
)

lm(total_monthly_consumption ~ pre_post_ref + age_at_interview,
    data = data_for_regressions
)

lm(
    total_monthly_consumption ~ pre_post_ref + age_at_interview + fin_wealth +
        ragender + ever_dc_pen + ever_dc_pen * pre_post_ref + years_since_retirement,
    data = data_for_regressions
)
# ok this is encouraging
# what actually is the method though?
# what assumptions does this rely on

# & ever_dc_pen == TRUE

# also works without dc filter
# affects are prob large enough to be ok after adjusting for cpi inflation


# what about couples??
# lets try and solve subjective code
# done this



# Lets also run a diff in diff where the control group are retirees without a db pension
