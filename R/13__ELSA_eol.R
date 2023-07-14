library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(nls2)
library(jsonlite)
library(tidyr)

source("__ELSA_functions.R")


## This file does a couple of thins
# 1. Get expectation data from raw elsa files
# 2. Join it with demographic data from harmonized data
# 3. Get prob live until 110 from ONS
# 4. Run NLS to find subjective survival curves
# 5. Join all back together and save


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

# find out which file the data is in
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

# These are file names
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
## Calculate real prob of being alive at 110 using ONS lifetables

ons_p110 <- ons_life_exp %>%
    mutate(surv_prob = 1 - death_rate) %>%
    filter(!is.na(surv_prob)) %>%
    group_by(gender, birth_year) %>%
    arrange(-age) %>%
    mutate(prob110 = lag(cumprod(surv_prob))) %>% # cumulative prov of being alive
    mutate(prob110 = fifelse(age == 110, 1, prob110)) %>%
    select(age, gender, birth_year, prob110)




#### READ IN MAIN DATA ########
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

ready_for_nls <- joined_harm %>%
    select(idauniq, exlo80, exlo90, prob110, wave, age = age_at_interview) %>%
    filter(!is.na(exlo80) & !is.na(exlo90) & !is.na(prob110)) %>%
    filter(exlo80 >= 0, exlo80 <= 100) %>% # do not allow expectations out of the unit interval
    filter(exlo90 >= 0, exlo90 <= 100) %>%
    mutate(across(c("exlo80", "exlo90"), ~
        case_when( # change certainty to almost certainty
            .x == 100 ~ 99,
            .x == 0 ~ 1,
            .default = .x
        ) / 100)) %>%
    filter(wave <= 7) %>%
    rename(ex1 = exlo80, ex2 = exlo90, ex3 = prob110) %>%
    mutate(t1 = case_when( ## rules for the age that the probability is for
        age <= 65 ~ 75 - age,
        age > 65 ~ 80 - age
    )) %>%
    mutate(t2 = 85 - age) %>%
    mutate(t3 = 110 - age) %>%
    filter(!is.na(t1) & !is.na(t2) & !is.na(t3)) %>%
    filter(ex1 > ex2) # make the expectation

# convert to long, answer level data. I.e. one expectation per row
long_ready <- ready_for_nls %>%
    melt(
        measure.vars = patterns(
            t = "t",
            exlo = "ex"
        ),
        variable.name = "names"
    )

long_ready[, id_wave := paste0(idauniq, "-", wave)]



# First step is to generate some realistic starting values
# which we will then use in NLS

# Make a grid of initial values
pars <- expand.grid(
    k = seq(1, 5, len = 100),
    lambda = seq(0.1, 50, len = 100)
)

# Run for a random id_wave and then use the correct
#   answer to this as the default starting point
random_id <- long_ready[, id_wave[floor(
    runif(1, min = 1, max = nrow(long_ready))
)]]

temp_dt <- long_ready[id_wave == random_id]

res_rand <- nls2(exlo ~ exp(-(t / lambda)^k),
    data = temp_dt,
    start = pars,
    algorithm = "brute-force"
)

output_rand <- nls(
    exlo ~ exp(-((t / lambda)^k)),
    data = temp_dt,
    start = as.list(coef(res_rand))
)

top_start_values <- as.list(coef(output_rand))
# then set the starting values for the others to this


BruteForce <- function(id_wave_to_try,
                       end_k = 5,
                       end_lam = 40,
                       grid_len = 100) {
    ## make a grid
    pars <- expand.grid(
        k = seq(0.1, end_k, len = grid_len),
        lambda = seq(0.1, end_lam, len = grid_len)
    )

    # try all points on the grid
    res <- nls2(exlo ~ exp(-(t / lambda)^k),
        data = long_ready[id_wave == id_wave_to_try],
        start = pars,
        algorithm = "brute-force"
    )
    as.list(coef(res))
}

# set expanding values
# make grid larger and finer. This will make evaluation of it slower
# but makes finding a good starting value more likely
grid_types_to_try <- data.frame(
    end_k = seq(5, 40, len = 7),
    end_lam = seq(40, 100, len = 7),
    grid_len = seq(50, 100, len = 7)
)


## Create directory for jsons
if (!dir.exists("../../data/ELSA/subjective_tables/jsons")) {
    dir.create(
        "../../data/ELSA/subjective_tables/jsons",
        recursive = TRUE
    )
}


# function to read in json data
read_json_idwave <- function(id_wave) {
    read_json(paste0(
        "../../data/ELSA/subjective_tables/jsons/",
        id_wave, ".json"
    ), simplifyVector = T)
}

# checks the ids that we have already made
check_ids_done <- function() {
    ids_done_already <- str_extract(
        dir("../../data/ELSA/subjective_tables/jsons/"),
        "\\d{6}-\\d{1}"
    )
    return(ids_done_already)
}


# This step drastically speeds up evaluation ############
#######################################
# My advice is to randomly run 300 or so then read in the values,
# and then we get a much better set of starting values to try
# Now that a few have done, we read in all the parameters that were run
# and then use different pairings of the distribution of lambda and k
read_json_get_params <- function() {
    # Note this can only be run if we have have found some jsons already
    json_dir <- dir("../../data/ELSA/subjective_tables/jsons", full.names = T)

    # If no jsons (i.e. havent run the code below yet) we just return start values
    if (length(json_dir) == 0) {
        return(data.table(
            k = rep(top_start_values$k, 5),
            lambda = rep(top_start_values$lambda)
        ))
    }

    # get 200 of the jsons at random
    random_ints <- floor(runif(200, 1, length(json_dir)))

    params <- lapply(json_dir[random_ints], fromJSON)

    params_dt <- rbindlist(params)

    params_dt %>%
        ggplot(aes(x = k, y = lambda)) +
        geom_point()

    # want 5 sets of starting values
    # use k means clustering to group
    # then return

    clusters <- kmeans(params_dt, 5, iter.max = 10, nstart = 1)

    return(clusters$centers)
}

typical_values <- read_json_get_params()
ids_that_are_done <- check_ids_done()


run_nls_function <- function(id_wave_to_try,
                             starting = top_start_values,
                             typval_df = typical_values) {
    # print(id_wave_to_try) # useful for debugging

    # Loop with different starting values
    # try a couple of escalating grids
    counter <- 1

    while (TRUE) {
        output <- NULL

        # first try initial random guess
        try(output <- nls(exlo ~ exp(-((t / lambda)^k)),
            data = long_ready[id_wave == id_wave_to_try],
            start = starting
        )) # does not stop in the case of error

        if (!is.null(output)) break # if nls works, then quit from the loop

        if (counter <= 5) {
            # try a different starting value from an id that didnt work
            starting <- as.list(typval_df[counter, ])
        } else if (counter > 5) {
            # Now try the expanding values of the grid
            grid_deets <- as.list(grid_types_to_try[counter - 5, ])

            # create new grid
            starting <- BruteForce(
                id_wave_to_try,
                end_k = grid_deets$end_k,
                end_lam = grid_deets$end_lam,
                grid_len = grid_deets$grid_len
            )
        } else if (counter > 12) { # stop if try too many times
            stop()
        }

        # iterate counter
        counter <- counter + 1
    }

    # save as json so when we run we dont have to do the ones we have already found
    # when running check if the json exists
    write_json(
        as.list(coef(output)),
        paste0(
            "../../data/ELSA/subjective_tables/jsons/",
            id_wave_to_try, ".json"
        ),
        simplifyVector = FALSE
    )
}

# Only run for ids that we havent done before
ids_to_do <- long_ready[, unique(id_wave)][!long_ready[, unique(id_wave)] %in% ids_that_are_done]

length(ids_to_do)

outputs <- lapply(
    ids_to_do,
    run_nls_function
)
# wow this is so much faster once we have done a couple of hundred




# read back the data
dt_with_params <- lapply(
    long_ready[, unique(id_wave)],
    read_json_idwave
) %>%
    rbindlist()

dt_with_params$id_wave <- long_ready[, unique(id_wave)]


# Now how do we get lifetables out?
# I guess just the pdf of these for different ages?

# take the pdf and find the probs that it implies for different age groups
MakeLifeTable <- function(params = params,
                          years_into_the_future = 60) {
    sub_life <- sapply(
        c(1:years_into_the_future),
        \(t)exp(-(t / params$lambda)^params$k)
    )

    return(sub_life)
}

# create list for the sub life tables
sub_life_tab_list <- vector("list", length = length(dt_with_params$id_wave))

# populate list
for (i in seq_along(dt_with_params$id_wave)) {
    sub_life_tab_list[[i]] <- MakeLifeTable(as.list(dt_with_params[i, ]))
}

# probably save this is long form
# although if working in julia we should use matrices
# lets save in both long and in matrices
# for each id wave we want the age they are
# and then we can have prob of surviving until age x

sub_life_dt <- mapply(
    function(prob_vec, id) {
        data.table(
            prob = prob_vec,
            id_wave = id
        )
    },
    prob_vec = sub_life_tab_list,
    id = dt_with_params$id_wave,
    SIMPLIFY = FALSE
) %>%
    rbindlist()

sub_life_dt[, years_into_future := rep(c(1:60), length(dt_with_params$id_wave))]

# give age back to id-waves
sub_life_dt <- merge.data.table(
    sub_life_dt,
    joined_harm[!is.na(age_at_interview), .(
        id_wave = paste0(idauniq, "-", wave),
        age = age_at_interview
    )],
    by = "id_wave",
    all.x = TRUE
)

# add years into the future onto current age
sub_life_dt[, expected_age := age + years_into_future]

# ok great we now have subjective done
fwrite(
    sub_life_dt,
    "../../data/ELSA/subjective_tables/objective_survival_probs.csv"
)
