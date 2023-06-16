library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)

source("__ELSA_functions.R")

elsa <- fread("../../data/ELSA/elsa_to_use/elsa_ifs_finance.csv")

elsa[, head(futype)]
# all_pension leave out the pension data set for now
# since I am not sure I need it.

elsa[, .(idauniq, wave)
][, .(num_waves = length(unique(wave))), by = .(idauniq)]   %>% 
ggplot(aes(x = num_waves)) + 
geom_histogram()

search_names(elsa, "ida")

# plot average by wave and age.

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
# For first run we should just take the first row for every household. 
# then add together both annuities
# then find whether have annuity conditional on net wealth
# household-wave
elsa[, household_wave := paste0()]




# First look at household unit.
# Are these matched across waves?
# I am not sure.
#

