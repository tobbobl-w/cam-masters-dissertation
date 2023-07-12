library(data.table)
library(dplyr)
library(stringr)

source("__ELSA_functions.R")


check_files_for_variable_name <- function(var_name) {
    # returns the files which contain the variable name we are looking for
    # Just helpful for seeing where variables originally came from
    files_with_var_name <- dir("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/",
        pattern = "",
        full.names = TRUE
    ) %>%
        lapply(\(filename) names(fread(filename, nrows = 1))) %>%
        lapply(function(col_names) {
            grep(var_name,
                col_names,
                value = T,
                ignore.case = T
            )
        }) %>%
        sapply(length) %>%
        sapply(\(x) x != 0)

    dir("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/",
        pattern = "",
        full.names = TRUE
    )[files_with_var_name]
}

check_files_for_variable_name("eida")
# year of death
check_files_for_variable_name("exlo")
# life expec

search_names(
    fread(
        "../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/wave_6_elsa_data_v2.tab",
        nrows = 1
    ),
    "exlo",
    ignore.case = TRUE
)
# "ExLo80" "ExLo90"

expectation_cols <- c(
    "idauniq",
    "ExLo80",
    "ExLo90"
)

## get expectation data
files_with_exp <- check_files_for_variable_name("exlo")

data_with_exp <- lapply(files_with_exp, ReadAndSetWave) %>%
    lapply(\(data) setnames(data, str_to_lower)) %>%
    rbindlist(use.names = T, fill = T) %>%
    select(idauniq, file_wave, exlo80, exlo90) %>%
    mutate(wave = as.numeric(str_extract(file_wave, "\\d{1}"))) # make wave numeric so that we can join later


# ok this is the data.
# need to do some cleaning on it
# get rid of negative values and set values of 0 and 1
# to be just above and below respectively

# use scaled life expectancy as the third point of data.
# I think we need rough estimates to plug into the life cycle model.

# can get ons life exp from the table I made earlier
# These are the parameters that I used to select the life probs
start_year <- 2005

life_exp_list <- list(
    female = fread("../../data/ONS/Females_transition_probs.csv"),
    male = fread("../../data/ONS/Males_transition_probs.csv")
)

ons_life_exp <- mapply(
    function(data, gender_name) {
        # Melt the data to long, and set the gender
        melt(data,
            id.vars = "age",
            variable.name = "birth_year",
            value.name = "death_rate"
        ) %>%
            mutate(gender = gender_name) %>%
            mutate(birth_year = str_extract(birth_year, "\\d{4}")) %>%
            mutate(birth_year = as.integer(birth_year))
    },
    life_exp_list,
    names(life_exp_list),
    SIMPLIFY = FALSE
) %>%
    rbindlist()
## Calculate prob of being alive at 110

ons_p110 <- ons_life_exp %>%
    mutate(surv_prob = 1 - death_rate) %>%
    filter(!is.na(surv_prob)) %>%
    group_by(gender, birth_year) %>%
    arrange(-age) %>%
    mutate(prob110 = lag(cumprod(surv_prob))) %>% # cumulative prov of being alive
    mutate(prob110 = fifelse(age == 110, 1, prob110)) %>%
    select(age, gender, birth_year, prob110)



#### READ IN MAIN DATA
# get age and gender from here
long_harm <- fread("../../data/ELSA/elsa_to_use/harmonised_data_long.csv")
long_harm[ragender %in% c(1, 2), gender := fifelse(ragender == 1, "male", "female")]

# Join long harmonized with ons life exp and subjective life exp
joined_harm <- left_join(
    long_harm,
    ons_p110,
    by = c(
        "rabyear" = "birth_year",
        "age_at_interview" = "age",
        "gender"
    )
) %>%
    left_join(
        data_with_exp,
        by = c("idauniq", "wave")
    )


"exlo80" # prob live to certain age
# age in question depended on the age of the individual.
# are expectations of death in here as well?

library(tidyr)

joined_harm %>%
    select(idauniq, exlo80, exlo90, prob110, wave, age = age_at_interview) %>%
    filter(!is.na(exlo80) & !is.na(exlo90) & !is.na(prob110)) %>%
    filter(exlo80 >= 0, exlo80 <= 100) %>%
    filter(exlo90 >= 0, exlo90 <= 100) %>%
    mutate(across(c("exlo80", "exlo90"), ~
        case_when(
            .x == 100 ~ 99,
            .x == 0 ~ 1,
            .default = .x
        ) / 100)) %>%
    filter(wave <= 7) %>%
    rename(ex1 = exlo80, ex2 = exlo90, ex3 = prob110) %>%
    mutate(t1 = case_when(
        age <= 65 ~ 75 - age,
        age > 65 ~ 80 - age
    )) %>%
    mutate(t2 = 85 - age) %>%
    mutate(t3 = 110 - age) %>%
    filter(!is.na(t1) & !is.na(t2) & !is.na(t3)) %>%
    # generate some intial parameter values
    mutate(k = log(log(ex1) / log(ex2)) / (log(t1) - log(t2))) %>%
    mutate(lambda = (1 / t1) * ((-log(ex1))^(1 / k))) %>%
    mutate(invlambda = lambda^(-1)) %>% 
    filter(!is.infinite(lambda))

nl ( exlo =  exp(-( {lambda}*t)^{k})) if id==`id', initial(lambda `initlambda' k `initk')    
nls

# gen     t1 = 75-age if age<66
# replace t1 = 80-age if age>65
# gen     t2 = 85-age
# gen t3 = 110 - age

# gen k = ln(ln(exlo80)/ln(exlo90))/(ln(t1)-ln(t2))
# gen lambda = (1/t1)*((-ln(exlo80))^(1/k))
# gen invlambda = 1/lambda
# *replace invlambda=. if invlambda>5000

# /**********************************************************************************************/
# /* Now, impose third point and then fit Weibull according to least squares					  */
# /**********************************************************************************************/

# * We are going to merge in the individual's probability of reaching age 110, according to life tables *
# * We treat this as a third "answer" that the individual gives. *










# logic for birthday
# assume interviews are mid month
# interivew date gives us upper and lower bound on birthday
# or do we just use birth year and death year
# probably not gonna gain much
# have a look at their stata code to see how they update use death rates
# they reg hazard on a cubic of age
#

# for now lets set birthday to be mid year,
# and also death dat
# now we want to calculate subjective life expectancy again
# what are the subjective life expec variables


# now we use non-linear least squares to set
# subjective survival curves

# actually we need to scale the ONS life exp
# how?
# calculate mortality rates and then compare to ons
# then scale ons


# out of those who respond in a given year how many die

long_harm[in_wave == 1, ]


# I think I should just use ONS and only scale if I have tie
# Before that though I need to have ONS until 110 not just 100
