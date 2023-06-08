library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)

# Join consumption data from the various waves of data
# Hopefully this isn't too difficult

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

dir("../../data/household_files")

test_hh <- fread("../../data/derived_files/2014_dvhh_ukanon.tab")
test_person <- fread("../../data/derived_files/2014_dvper_ukanon.tab")


# So we need the age variable from the person files
# And the consumption variable from the household files

# Is case still the joining variable



consumption_data <- test_hh[, .(case, total_expend = P600)]
age_data <- test_person[Person == 1, .(case, age = a005p)] # Select head of household

joined_data <- full_join(consumption_data, age_data,
    by = "case"
)

## Ok so we want to do the aboce for all years

derived_files <- dir("../../data/derived_files")

str_extract_all(derived_files, "\\d{4}")
# Do 2019-20 data refer to 2019 or 2020 - always the lower year.

JoinYear <- function(year_of_data = 2004) {
    files_for_year <- grep(year_of_data, derived_files, value = T)

    full_name_for_files <- paste0("../../data/derived_files/", files_for_year)

    # household file
    hh_file <- grep("dvhh", full_name_for_files, value = T)
    hh_data <- fread(hh_file)
    setnames(hh_data, str_to_lower)
    hh_consump <- hh_data[, list(case, total_expend = p600)] # nolint: object_usage_linter.

    hh_consump[, data_wave := year_of_data] # nolint: object_usage_linter.

    # person files
    per_file <- grep("dvper", full_name_for_files, value = TRUE)
    per_data <- fread(per_file)
    setnames(per_data, str_to_lower)
    per_age <- per_data[person == 1, list(
        case, # nolint: object_usage_linter.
        age = a005p, # nolint: object_usage_linter.
        annuity_income = b346 # nolint: object_usage_linter.
    )]

    consump_age <- full_join(hh_consump, per_age, by = "case")
    return(consump_age)
}

JoinYear(year_of_data = 2004)

all_data <- lapply(
    2004:2020,
    JoinYear
) %>%
    rbindlist() %>%
    mutate(birth_year = data_wave - age) %>%
    mutate(age_in_2014 = birth_year - 2014)

if (!dir.exists("../../data/clean_data")) dir.create("../../data/clean_data")

fwrite(
    all_data,
    "../../data/clean_data/joined_household_individual.csv"
)
