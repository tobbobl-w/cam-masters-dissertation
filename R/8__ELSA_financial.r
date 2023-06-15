library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)

# Set the correct working directory.
# Note you need to run the whole script to get this to work.
# I.e crtl+shift+enter.
cur_dir <- getSrcDirectory(function(x) {
    x
})
setwd(cur_dir)
stop() # Remove if running the whole thing through

ReadAndSetWave <- function(filename) {
    # Read a tab file and set a wave number.

    # Extract wave number from filename
    filename_wave <- str_extract(filename, "wave_\\d{1}")
    data <- fread(filename)
    # Set wave in data
    data[, file_wave := filename_wave]
    return(data)
}


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

all_finance[]

# anin_r_i anin_p_i wppp_r_i wppp_p_i

# ppinc_bu_s this is called total annuitised income.
# private plus other?
# "BU tot annuitised income (priv pen + oth annuity income) - summary var
# But I don't actually know what that means


# let's split these up by cohort, age and year to see if we get any nice differences

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
    breaks = 50
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

all_joined_data <- Reduce(
    function(x, y) full_join(x, y, by = c("file_wave", "idauniq")),
    list(all_finance, all_ifs)
)
# all_pension leave out the pension data set for now
# since I am not sure I need it.


# plot average by wave and age.

annuity_age_wave_means <- all_joined_data[, .(
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

all_joined_data[age == 70][, .(annuity_inc = mean(anin_r_i > 0, na.rm = T)),
    by = .(intdaty)
]
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
# What discount rate should I use to equivalise money across waves?
#
