library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(readxl)
library(tidyr)
library(lubridate)
library(haven)

source("__ELSA_functions.R")


# harmonised data from ucla.
# This is harmonised with HRS.
harm <- read_dta(
    "../../data/ELSA/elsa_unziped/UKDA-5050-tab/code/stata_output_files/H_ELSA_g2_stata12.dta"
)


setDT(harm)

nrow(harm)
ncol(harm)

table(harm$pn)

"R1PTYP1_E" %in% names(harm)
"r1ptyp1_e" %in% names(harm)
"r1ptyp1_e" %in% names(harm)
grep("ptyp\\d{1}_e", names(harm), value = T)
# This is type of pension fot a given job.
# We can observe this in the period before retirement.

table(harm$r1ptyp1_e)
head(names(harm))

writeLines(names(harm), "name_check.txt")

# need to make a years till retirement variable.
# R1RETEMP 1 for retired and 0 if not.
# R1RETAGE retirement age, only for those who are retired.
# R2WRETAGE expected retirement age, for those not retired.
# R9RPLNYA expected retirement year -- wave 9 only.
# would be good to compare these two variables.
# RwLIV10 probablility of living for twn years.

# make data long.
# multiple columns at once.
search_names(harm, "rabyear")

harm[inw1 == 1, .(head(r1iwindm), head(r1iwindy))]

harm_long <- melt(
    harm,
    id.vars = c("idauniq", "pn", "ragender", "rabyear"),
    measure = patterns(
        "r\\d{1}retemp", "r\\d{1}ptyp1_e",
        "r\\d{1}ptyp2_e", "r\\d{1}ptyp3_e",
        "r\\d{1}retage", "r\\d{1}wretage",
        "r\\d{1}agey", "inw\\d{1}$",
        "r\\d{1}iwindm", "r\\d{1}iwindy"
    ),
    value.name = c(
        "retired", "pension_type_job1",
        "pension_type_job2", "pension_type_job3",
        "retired_age", "expected_retired_age",
        "age_at_interview", "in_wave",
        "date_month", "date_year"
    )
)

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



# But this is actually only a problem if someone switches around 2012.
# How many people switched around that time.

exp_ret_year_dt <- harm_long[!is.na(expected_retired_age),
    .(exp_ret_age = expected_retired_age[1]),
    by = .(idauniq, rabyear)
][, exp_ret_year := exp_ret_age + rabyear][order(rabyear)]

exp_ret_year_dt[, .(expected_retirements = .N),
    by = .(exp_ret_year)
][order(expected_retirements)][exp_ret_year %in% c(2005:2020)] %>%
    ggplot(aes(x = exp_ret_year, y = expected_retirements)) +
    geom_point() +
    geom_line()
# bit weird that we get the drop and then the peak in 2013.
# we only have expected retirement age for people who have worked.
# does this change the situation and generate the bump shape.


# now check actual retirements
ret_year_dt <- harm_long[!is.na(retired_age),
    .(ret_age = retired_age[1]),
    by = .(idauniq, rabyear)
][, ret_year := ret_age + rabyear][order(rabyear)]

ret_year_dt[, .(retirements = .N),
    by = .(ret_year)
][order(retirements)][ret_year %in% c(2005:2020)] %>%
    ggplot(aes(x = ret_year, y = retirements)) +
    geom_point() +
    geom_line()
# could have just plotted these on the same graph.
# real retirements follows similar trend, but the hump is a bit earlier i think.
