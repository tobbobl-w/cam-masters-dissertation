library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)

source("__ELSA_functions.R")

# Characteristics and financial variables
elsa <- fread("../../data/ELSA/elsa_to_use/elsa_ifs_finance.csv")


# Have a look at attrition in ELSA
elsa[, .(idauniq, wave)][, .(num_waves = length(unique(wave))), by = .(idauniq)] %>%
    ggplot(aes(x = num_waves)) +
    geom_histogram()

# plot percent with annuity income and avg annuity inc
# by wave and age of respondent.
annuity_age_wave_means <- elsa[, .(
    mean_annuity_inc =
        mean(anin_r_i[anin_r_i > 0], na.rm = T),
    per_annuity =
        mean(anin_r_i > 0, na.rm = T)
), by = .(age, file_wave)]
head(annuity_age_wave_means$file_wave)

# Plot percentage who have any annuity income against age shaded by wave.
ggplot(
    annuity_age_wave_means[
        age > 60
    ][file_wave %in% c("wave_1", "wave_8")],
    aes(x = age, y = per_annuity, colour = file_wave)
) +
    geom_point() +
    geom_line()
# ok nice it looks like fewer people do have annuities
# but still a small portion of the sample have any annuity income.
# can we do a cross tab


ggplot(
    annuity_age_wave_means[
        age > 60
    ][file_wave %in% c("wave_1", "wave_8")],
    aes(x = age, y = mean_annuity_inc, colour = file_wave)
) +
    geom_point() +
    geom_line()




# waves are every two years starting in 2002
# so 6th wave was 2014 and 4th was 2010
# we would expect to start to see the difference from the 4th
# wave onwards then.
# ppinc_bu_s - total annuitised income

search_names(elsa, "anin_")

length(names(elsa))

# Just take the first row of each household unit.


elsa[age == 65][, .(prop_annuity_inc = mean(anin_r_i + anin_p_i > 0, na.rm = T)),
    by = .(intdaty)
] %>%
    ggplot(., aes(x = intdaty, y = prop_annuity_inc)) +
    geom_line() +
    geom_point()
.libPaths()

# quite variable
# should i condition on pension wealth?
# or some other cohort indicator.

# ppen_bu_i - BU private pensions (iappei/iappmo) - value (incl. imputed values)
# indobyr - year of birth
# age
# idauniq - unique wave identifier

# questions for the data
# wealth measures
# pension wealth and household wealth
# should i plot data by household or individual level?
##### I think household level
# What discount rate should I use to equivalise money across waves?
#
# wppp_r_i -- private pension income currently received
# guessing this means income from db/dc pensions that they have accumulated.
# as opposed to annuities that they purchased.

head(elsa$idahhw1)

elsa[idahhw1 == 10001, .(anin_r_i, anin_p_i, wppp_r_i, wppp_p_i)]

grep("idahh", names(elsa), value = T)

search_names(elsa, "idahh")



# now plot annuities conditional on wealth
# what is the wealth variable that I should use
# netfw_bu_s net financial wealth of the household unit.


head(elsa$netfw_bu_s)

# Get a household data set.
# For first run we should just take the first row for every household. - done
# then add together both annuities - done
# then find whether have annuity conditional on net wealth - doing
# household-wave
elsa[, household_wave := paste0(fuid, "-", file_wave)]

elsa_hh <- elsa[rowid(household_wave) <= 1]


elsa_hh[, ":="(hh_anin = anin_r_i + anin_p_i)]

elsa_hh_prop_an <- elsa_hh[, .(prop_anin = mean(hh_anin > 0, na.rm = T)), by = .(file_wave, age)]
# plot proportion recieveing annuity income at ages 60, 65 and 70 with
# grouped bars for all the means

ggplot(
    elsa_hh_prop_an[age %in% c(60, 65, 70)],
    aes(x = age, fill = file_wave, y = prop_anin)
) +
    geom_col(position = "dodge")
# relatively large changes at later ages.


# how to introduce wealth into this.
# does wealth include the pdv of annuity income.
# i dont think so
# netfw_bu_s

search_names(elsa_hh, "anin")

ggplot(elsa_hh[netfw_bu_s > 0][netfw_bu_s < 100000], aes(x = netfw_bu_s)) +
    geom_histogram()

elsa_hh[, .(pension_inc = mean(wppp_bu_i > 0, na.rm = T)), by = .(file_wave, age)][age %in% c(55, 60, 65, 70)] %>%
    ggplot(
        aes(x = age, fill = file_wave, y = pension_inc)
    ) +
    geom_col(position = "dodge")
# I want to see what happened to pension wealth and what happened to pension income

elsa_hh[netfw_bu_s > 10000][, .(prop_anin = mean(hh_anin > 0, na.rm = T)), by = .(file_wave, age)][age %in% c(55, 60, 65, 70)] %>%
    ggplot(
        aes(x = age, fill = file_wave, y = prop_anin)
    ) +
    geom_col(position = "dodge")

elsa_hh[, .(count = .N), by = .(age, wave)] %>%
    ggplot(aes(x = age, y = count, group = wave)) +
    geom_point() +
    geom_line()


# Ok all waves have above 150 at around age 63.
# but still not many of those people would
# how many older people are in fes?
# my typing has actually got much better.

# First look at household unit.
# Are these matched across waves?
# No only individuals are matched across waves.
# We dont really care about the longitudinal nature of the data atm


# I guess the research question is how did consumption in early retirement change
# as a result of the annuity reform.
# Some difference in difference/reg discontinuity might be the best thing to do
# Maybe a theoretical model as well

# could this be a test for the bequest motive being the driver of non-annuitisation.
# if consumption stays the same then people are saving for bequests. -- could look at saving as well.
# if it increases it is because people want to consume more early.

# so one theoretical model is the
# UK is a good place to study this since out-of-pocket medical expenses are low and
# due to the introduction of the cap on long-term-care costs they are still expected
# to stay low.

# look at annuity rates conditional on having enough private pension wealth to
# be eligible to be forced to buy one.

# savings rates?
#

# could the models tell us explicitly what the change in savings should be for someone who
# was constrained and is now not constrained. If we treat all changes in
# consumption behaviour as a result of the annuity law change then we can
# compare to optimal behaviour from a model.

# first five years of retirement.

# steps would be 1. estimate the change in behaviour -- reduced form estimates of policy change on consumption
# 2. predictions from different life-cycle models
# 3. compare these to the reduced form estimates from part 1.

# also present summary stats
# I would need to edit the set up of the models a bit.


# savings rate or asssets?
# I think savings rate. dont consider house prices.
# non-housing wealth.


# what method to use?
# diff in diff or reg discontinuity seem the best options
# or use the policy change as an iv that pushes some people away from
# diff in diff using people with DB pensions as the control group.
# would that just be people who have a majority of their money in a DB pension.
# Yes -- some cutoff to get reasonable size. And then we group together waves.
