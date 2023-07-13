library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(nls2)
library(jsonlite)
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

ready_for_nls <- joined_harm %>%
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
    filter(!is.infinite(lambda)) %>%
    filter(ex1 > ex2)


hist(ready_for_nls$k)

ready_for_nls[, mean(ex1 < ex2)]



# convert to long, answer level
long_ready <- ready_for_nls %>%
    melt(
        measure.vars = patterns(
            t = "t",
            exlo = "ex"
        ),
        variable.name = "names"
    )


long_ready[, id_wave := paste0(idauniq, "-", wave)]
# "100007-3" top id

# ok increasing doesn't make sense # could set to first one if is?
############## check what odea did to the above

# First step is to generate some realistic starting values
# which we will then use in NLS
pars <- expand.grid(
    k = seq(1, 5, len = 100),
    lambda = seq(0.1, 50, len = 100)
)

temp_dt <- long_ready[id_wave == long_ready[, id_wave[1]]]

res_top <- nls2(exlo ~ exp(-(t / lambda)^k),
    data = temp_dt,
    start = pars,
    algorithm = "brute-force"
)

output_top <- nls(
    exlo ~ exp(-((t / lambda)^k)),
    data = temp_dt,
    start = as.list(coef(res_top))
)

top_start_values <- as.list(coef(output_top))
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


if (!dir.exists("../../data/ELSA/subjective_tables/jsons")) {
    dir.create(
        "../../data/ELSA/subjective_tables/jsons",
        recursive = TRUE
    )
}

read_json_idwave <- function(id_wave) {
    read_json(paste0(
        "../../data/ELSA/subjective_tables/jsons/",
        id_wave, ".json"
    ), simplifyVector = T)
}

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
    jsons_to_get <- dir("../../data/ELSA/subjective_tables/jsons", full.names = T)
    # get 20% of the jsons at random
    params <- lapply(jsons_to_get[runif(length(jsons_to_get)) > 0.8], fromJSON)

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
    ##########################################
    ## first check if we have done that ID already
    print(id_wave_to_try)

    # Loop with different starting values
    # try a couple of escalating grids
    counter <- 1

    while (TRUE) {
        output <- NULL

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


ids_to_do <- long_ready[, unique(id_wave)][!long_ready[, unique(id_wave)] %in% ids_that_are_done]

length(ids_to_do)
outputs <- lapply(
    ids_to_do,
    run_nls_function
)
# wow this is so much faster


################
### Individual testing for ids that still fail

# read in data


dt_with_params <- lapply(
    long_ready[, unique(id_wave)],
    read_json_idwave
) %>%
    rbindlist()
dt_with_params$id_wave <- long_ready[, unique(id_wave)]


# Now how do we get lifetables out?
# I guess just the pdf of these for different ages?
dt_with_params

params <- dt_with_params[1, ] %>% as.list()

MakeLifeTable <- function(params = params,
                          years_into_the_future = 60) {
    sub_life <- sapply(
        c(1:years_into_the_future),
        \(t)exp(-(t / params$lambda)^params$k)
    )

    return(sub_life)
}

MakeLifeTable(params = params)

sub_life_tab_list <- vector("list", length = length(dt_with_params$id_wave))

for (i in seq_along(dt_with_params$id_wave)) {
    sub_life_tab_list[[i]] <- MakeLifeTable(as.list(dt_with_params[i, ]))
}

# probably save this is long form
# although if working in julia we should use matrices
# lets save in both long and in matrices

# after this lets move locations....
