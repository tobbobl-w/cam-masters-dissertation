library(data.table)
library(dplyr)
library(stringr)
library(readxl)

source("__ELSA_functions.R")

# I want to make survival curves from ONS

# Also could make survival curves from annutitant tables that
# the institute and faculty of actuaries makes produces annuity tables.
# one in 2008 vs the latest could make for an interesting comparison

# How do we make cumulative death probabilities?
### qx is the mortality rate between age x and (x +1),
# that is the probability that a person aged x exact
# will die before reaching age (x +1).


# sheets_to_get <- excel_sheets("../../data/ONS/nationallifetables3yruk.xlsx") %>%
#     grep("\\d{4}", ., value = TRUE)

# what form shall we put it in?
# json?
# or long csv with year, age and gender?

# ons_lifeexp <- lapply(sheets_to_get, function(sheet_name) {
#     suppressMessages({
#         data <- read_excel("../../data/ONS/nationallifetables3yruk.xlsx",
#             sheet = sheet_name,
#             skip = 5
#         ) %>%
#             select(-7) %>% # drop gap in the sheet between m & f lifexp
#             setnames(col_names) %>%
#             mutate(years = sheet_name)
#     })
#     return(data)
# })


# all_data <- rbindlist(ons_lifeexp)

# qx is the prob of reaching the next age, conditional on being that age.
# so if we have a constant birth year
# we can trance an age through and get all of their conditional life expectancies.


# we really want predictions though? not real life expectancy
# these are mortality predictions

# predictions_12 <- read_excel("../../data/ONS/wukprincipal12qxr.xls")
# excel_sheets("../../data/ONS/wukprincipal12qxr.xls")

# I think i want period lx and then we trace up the diagonal


# return_prob_for_age_year <- function(pred, year, age_to_get) {
#     # This function takes the data and selects the transition
#     # prob from an age in a year to the next age
#     col_name <- paste0("period_", year)

#     pred[age == age_to_get][, COL, env = list(COL = col_name)]
# }

# return_all_probs <- function(pred, start_year, start_age) {
#     # This finds all the transition probs
#     # from a start age in a given start year
#     # all the way to age 100
#     ages_to_get <- seq.int(
#         from = start_age,
#         to = 100
#     )

#     years_to_get <- seq.int(
#         from = start_year,
#         length.out = length(ages_to_get)
#     )

#     age_probs <- mapply(
#         return_prob_for_age_year,
#         pred = list(pred),
#         year = years_to_get,
#         age_to_get = ages_to_get
#     )

#     return(age_probs)
# }

# return_death_probs <- function(gender) {
#     stopifnot(gender %in% c("Males", "Females"))

#     pred_data <- read_excel("../../data/ONS/wukprincipal12qxr.xls",
#         sheet = paste0(gender, " period qx"), # gender sets the sheet to gets
#         skip = 6
#     ) %>%
#         setnames(\(x) paste0("period_", x)) %>%
#         setnames("period_age (years)", "age") %>%
#         setDT() %>%
#         mutate(age = as.numeric(age)) %>%
#         filter(!is.na(age))

#     mapply(
#         return_all_probs,
#         pred = list(pred_data),
#         start_year = 2005,
#         start_age = seq.int(from = 50, to = 70)
#     )
# }

# male_transition_probs <- return_death_probs("Males")
# female_transition_probs <- return_death_probs("Females")

# ## why didn't i just get year of birth and then all the probs
# ## only need 10 sets of probabilities, from 2005 but for all ages

# # list of vectors of transition probs
# # can I convert into a matrix?
# # each column is age in 2005
# # each row is year after that
# # so basically stack them side by side


# # first make all of the vectors the same length
# add_nas_function <- function(probs) {
#     length_of_vec <- length(probs < 51) # 51 is the max number of elements
#     if (length_of_vec < 51) {
#         # Put NAs at the front
#         probs <- c(rep(NA, 51 - length_of_vec), probs)
#     }
#     return(probs)
# }

# male_transition_probs <- lapply(
#     male_transition_probs,
#     add_nas_function
# )

# female_transition_probs <- lapply(
#     female_transition_probs,
#     add_nas_function
# )


# # bind together into a matrix
# male_mat <- do.call("cbind", male_transition_probs) / 100000
# female_mat <- do.call("cbind", female_transition_probs) / 100000

# ## Now save and manipulate in Julia
# fwrite(male_mat, "../../data/ONS/male_transition_probs.csv")
# fwrite(female_mat, "../../data/ONS/female_transition_probs.csv")


# ## compare this data with the odea and sturrock life exp data

# change_numeric_names <- function(name) {
#     if (nchar(name) == 4) {
#         paste0("birth_yr_", name)
#     } else {
#         name
#     }
# }

# odea_ons_data <- read_excel(
#     "../../Misc docs/odea_sturrock_code/ReStatReplication 15022021/ONS data/ONS life tables 1841 onwards.xls",
#     sheet = "Males cohort lx",
#     skip = 8
# ) %>%
#     rename_with(Vectorize(change_numeric_names)) %>%
#     rename(age = `age (years)`)



# # pick birth year 1955 and compare to birth year 1955 from above


# byear_1955 <- odea_ons_data %>%
#     select(age, alive = birth_yr_1955) %>%
#     mutate(deaths = lead(lag(alive) - alive)) %>% # number of deaths from age to age + 1
#     mutate(death_rate = deaths / alive)


# library(ggplot2)

# ggplot(data = byear_1955, aes(x = age, y = death_rate)) +
#     geom_path() +
#     geom_path(data = data.frame(
#         age = c(50:100),
#         death_rate = male_mat[, 1]
#     ), aes(x = age, y = death_rate))
## mm my ons data is sub lower than the other predictions

# lets just use these ons tables
# so I need to rewrite my code.

#### USE LIFE TABLES WITH AGE UNTIL 110 IN


create_death_prob_matrix <- function(gender) {
    # Function that creates the transition matrix from ons data.

    sheet_to_get <- paste0(gender, " cohort lx")

    ons_surv <- read_excel(
        "../../data/ONS/ONS life tables 1841 onwards.xls",
        sheet = sheet_to_get,
        skip = 8
    ) %>%
        # rename_with(Vectorize(change_numeric_names)) %>%
        rename(age = `age (years)`) %>%
        tidyr::pivot_longer(
            -age,
            values_to = "alive",
            names_to = "birth_year"
        )

    # birth years needed 1935 to 1955
    # want death rates

    ons_surv <- ons_surv %>%
        mutate(birth_year = as.numeric(birth_year)) %>%
        group_by(birth_year) %>%
        arrange(age) %>%
        mutate(deaths = lead(lag(alive) - alive)) %>% # alive yesterday minus alive today, shifted back to today
        mutate(death_rate = deaths / alive) %>%
        filter(birth_year >= 1935, birth_year <= 1955) %>%
        mutate(year = age + birth_year) %>%
        filter(year >= 2005) %>%
        filter(age >= 50, age <= 110)

    ons_surv_matrix <- ons_surv %>%
        tidyr::pivot_wider(
            id_cols = age,
            names_from = birth_year,
            values_from = death_rate
        )

    fwrite(
        ons_surv_matrix,
        paste0("../../data/ONS/", gender, "_transition_probs.csv")
    )
}

sapply(c("Males", "Females"), create_death_prob_matrix)
