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


# harmonised data from ucla.
# This is harmonised with HRS.

names_harm <- read_dta(
    "../../data/ELSA/elsa_unziped/UKDA-5050-tab/code/stata_output_files/H_ELSA_g2_stata12.dta",
    n_max = 1
) %>%
    names()
writeLines(names_harm, "name_check.txt")
###

# Old code to read in dta and save as csv.
# harm <- read_dta(
#     "../../data/ELSA/elsa_unziped/UKDA-5050-tab/code/stata_output_files/H_ELSA_g2_stata12.dta"
# )
# setDT(harm)
# fwrite(
#     harm,
#     "../../data/ELSA/elsa_unziped/UKDA-5050-tab/code/stata_output_files/H_ELSA_g2_stata12.csv"
# )

harm <- fread(
    "../../data/ELSA/elsa_unziped/UKDA-5050-tab/code/stata_output_files/H_ELSA_g2_stata12.csv"
)


# need to make a years till retirement variable.
# R1RETEMP 1 for retired and 0 if not.
# R1RETAGE retirement age, only for those who are retired.
# R2WRETAGE expected retirement age, for those not retired.
# R9RPLNYA expected retirement year -- wave 9 only.
# would be good to compare these two variables.
# RwLIV10 probablility of living for twn years.

# make data long.
# multiple columns at once.
search_names(harm, "radyear")
class(harm$r1ptyp1_e)
table(harm$r1ptyp1_e, useNA = "ifany")

harm[inw1 == 1, .(head(r1iwindm), head(r1iwindy))]


harm[, table(radyear)]
harm[, unique(radyear)]
harm[, sum(!is.na(radyear))]


harm_long <- melt(
    harm,
    id.vars = c("idauniq", "pn", "ragender", "rabyear", "radyear"),
    measure = patterns(
        "r\\d{1}retemp", "r\\d{1}ptyp1_e",
        "r\\d{1}ptyp2_e", "r\\d{1}ptyp3_e",
        "r\\d{1}retage", "r\\d{1}wretage",
        "r\\d{1}agey", "inw\\d{1}$",
        "r\\d{1}iwindm", "r\\d{1}iwindy",
        "hh\\d{1}ctot1m"
    ),
    value.name = c(
        "retired", "pension_type_job1",
        "pension_type_job2", "pension_type_job3",
        "retired_age", "expected_retired_age",
        "age_at_interview", "in_wave",
        "date_month", "date_year",
        "total_monthly_consumption"
    )
)

fwrite(harm_long, "../../data/ELSA/elsa_to_use/harmonised_data_long.csv")

harm_long[, unique(variable)] # what is this variable?

harm_long[, .(number_of_waves = sum(in_wave)), by = .(idauniq)][order(number_of_waves)]

sum_long <- harm_long[, .(
    always_retired = all(retired == 1), # this is slow
    number_of_waves = sum(in_wave)
), by = .(idauniq)][order(number_of_waves)]

sum_long

harm_long[, unique(retired, na.rm = F)]
# how do I make plots showing the number of people with
# dc and db pension plans.
# what am I trying to do.


table(harm_long$pn)
names(harm_long)
ncol(harm_long)
head(harm_long)
table(harm_long$retired, useNA = "ifany")
table(harm_long$pension_type_job1, useNA = "ifany")


are_ret_expec_correct <- function() {
    # lets see if retirement is the same as expectations.
    # compare expected retirement age to actual.
    # Plot the proportion of individuals whose retirement
    # expectations align with the actual year of retirement.


    # First find the people who are at not retired and then retire.
    exp_vs_real_ret_age <- harm_long[, .(
        not_retired = any(!is.na(expected_retired_age)),
        retired = any(!is.na(retired_age))
    ), by = .(idauniq)]

    # Return the IDS of those who meet the condition.
    ids_to_check <- exp_vs_real_ret_age[
        not_retired == TRUE & retired == TRUE
    ][, idauniq]

    # Get the first expected retirement age.
    # Get the age of retirement.
    retirement_ages_dt <- harm_long[idauniq %in% ids_to_check][, .(
        expected_retirement_age =
            expected_retired_age[!is.na(expected_retired_age)][1],
        real_retirement_age =
            retired_age[!is.na(retired_age)][1]
    ), by = .(idauniq)]

    # percentage of individuals who retire at the age they expect to
    retirement_ages_dt[, mean(expected_retirement_age == real_retirement_age)]

    percentage_retire <- function(thick_years) {
        # Work out the proportion for a given age range.
        # Thick years adds space around the real retirement age.
        retirement_ages_dt[, retiree_within_year := expected_retirement_age %in%
            seq(real_retirement_age - thick_years,
                real_retirement_age + thick_years,
                by = 1
            ),
        by = .(idauniq)
        ]

        return(retirement_ages_dt[, mean(retiree_within_year)])
    }

    years_to_add <- seq.int(0, 10, 1)

    data_for_plot <- sapply(years_to_add, percentage_retire)

    plot_df <- data.frame(
        years = years_to_add,
        percentage_within = data_for_plot
    )

    ggplot(plot_df, aes(
        x = years,
        y = percentage_within
    )) +
        geom_point() +
        geom_line() +
        labs(
            title =
                "Proportion of individuals who retire within x years of their original expectation",
            x = "Years",
            y = ""
        ) +
        scale_y_continuous(labels = scales::percent)
}

are_ret_expec_correct()
# Ok there are bits and bobs that are a bit weird but
# over 50% of people are correct within 2 years.


ret_by_year <- function() {
    # Now plot total retirements in the sample by year
    # and plot expected retirements by year on the same graph.
    exp_ret_year_dt <- harm_long[,
        .(
            exp_ret_age = expected_retired_age[!is.na(expected_retired_age)][1],
            ret_age = retired_age[!is.na(retired_age)][1]
        ),
        by = .(idauniq, rabyear)
    ][, ":="(exp_ret_year = exp_ret_age + rabyear,
        ret_year = ret_age + rabyear)][order(rabyear)]
    # Now from this calculate retirements and expected retirements by year.
    # Can we do this at once?

    exp_and_ret_dt <- rbindlist(
        list(
            exp_ret_year_dt[!is.na(exp_ret_year), .(count = .N),
                by = .(year = exp_ret_year)
            ][, type := "expected_retirements"],
            exp_ret_year_dt[!is.na(ret_year), .(count = .N),
                by = .(year = ret_year)
            ][, type := "real_retirements"]
        )
    )

    # Plot count of real retirements and
    # expected retirements on the same graph.
    # this doesnt tell us whether expected retirements are generally later than
    # real retirements or not.
    exp_and_ret_dt[year %in% c(2005:2020)] %>%
        ggplot(aes(x = year, y = count, colour = type)) +
        geom_point() +
        geom_line()
}

ret_by_year()

# Want a histogram of differences between actual year of retirement
# and expected first year of retirement.


harm_long[]

retirement_ages_dt <- harm_long[, .(
    expected_retirement_age =
        expected_retired_age[!is.na(expected_retired_age)][1],
    real_retirement_age =
        retired_age[!is.na(retired_age)][1]
), by = .(idauniq)]

retirement_ages_dt[, .(
    mean(is.na(expected_retirement_age)),
    mean(is.na(real_retirement_age)),
    mean(!is.na(expected_retirement_age) & !is.na(real_retirement_age))
)]
# Only 15% of observations have both an expected.
# and real retirement age.

ret_age_differences <- function() {
    ret_differences <- retirement_ages_dt[
        !is.na(expected_retirement_age) & !is.na(real_retirement_age)
    ][, difference := expected_retirement_age - real_retirement_age]

    ggplot(ret_differences, aes(x = difference)) +
        geom_histogram(bins = 60) +
        labs(
            title = "Density of expected age minus real retirement age",
            x = "Expected retirement age minus real retirement age"
        )
    # strong right tail where expected age is greater than real
}
ret_age_differences()


# Is there consumption data in here as well?

harm_long[, head(total_monthly_consumption)]
# what are we regressing
# just monthly consumption on lots of controls, one of which is the
# only really care about values near the kink.
# and therfore the interview must have been conducted in that time period.
# It would be good to have the theoeretical model
# because there are predictions about consumption paths as well of time since retirement.

# and then simulations from lockwood paper and the restd paper recently



# Pension differences
# check people who aren't retired.

harm_long %>% names()

year_of_ret_summary <- function() {
    # does year of retirement change?
    # seems to much to deal with tbh.
    # probably worth checking though.
    # want within variance.


    ret_age_summary <- harm_long[retired == 1 & !is.na(retired_age)][, .(
        sd_ret = sd(retired_age),
        max_ret = max(retired_age),
        min_ret = min(retired_age),
        med_ret = median(retired_age),
        count_ret = length(unique(retired_age))
    ), by = .(idauniq)]
    # [sort(count_ret)]
    # histograms for each one about from sd.

    library(patchwork)

    max_hist <- ggplot(
        ret_age_summary,
        aes(x = max_ret)
    ) +
        geom_histogram()
    # surely not?

    min_hist <- ggplot(
        ret_age_summary,
        aes(x = min_ret)
    ) +
        geom_histogram()

    med_hist <- ggplot(
        ret_age_summary,
        aes(x = min_ret)
    ) +
        geom_histogram()

    sd_hist <- ggplot(
        ret_age_summary[sd_ret > 0],
        aes(x = sd_ret)
    ) +
        geom_histogram() +
        labs(title = "Histogram of SDs for ")




    (max_hist + min_hist) /
        (med_hist + sd_hist + (wrap_elements(gridExtra::tableGrob(data.frame(table(ret_age_summary$count_ret) / nrow(ret_age_summary) * 100)))
        )
        )
    # Nice most are just one and all those distributions look ok.
}

year_of_ret_summary()




# Plot pension types over time by age.
# want number of people whos primary pension is different types

pen_type_eve_retirment <- function() {
    # Pension type is only there for people who are working
    # So we just look at the last non-missing pension type for each individual
    # first filtering IDs by them being retired.

    harm_long[, ":="(
        retired_in_sample = as.integer(any(retired == 1, na.rm = T)),
        first_retirement_age = retired_age[!is.na(retired_age)][1]
    ),
    by = .(idauniq)
    ]
    pension_before_retirement <-
        harm_long[retired_in_sample == 1][!is.na(pension_type_job1)][, .SD[c(.N)], by = idauniq]

    pension_before_retirement[, head(first_retirement_age)]

    pension_before_retirement[, .(count_pensions = .N),
        by = .(date_year, pension_type_job1 = factor(pension_type_job1))
    ] %>%
        ggplot(aes(x = date_year, y = count_pensions, colour = pension_type_job1)) +
        geom_point() +
        geom_line()
    # so much missing pension data.
    # I think I need to read the pension documentation
    # so that I have a better idea of what is actually going on.
    # not sure if counting by interview date is a good idea.
}

harm_long[!is.na(pension_type_job1), length(unique(idauniq))] / length(unique(harm_long$idauniq))
# 28% I have some sort of pension data for
table(harm_long$pension_type_job1, useNA = "always")


# Is it the type of thing they only do once
# No it is collected for people who have a job.


harm_long[!is.na(pension_type_job1)][, .(num_waves = sum(in_wave)), by = .(idauniq)]
harm_long[idauniq == 100054]

table(harm_long[, pension_type_job1], useNA = "ifany")

harm_long[, .(mean(total_monthly_consumption, na.rm = T), sd(total_monthly_consumption, na.rm = T)),
    by = .(date_month)
][order(date_month)]
