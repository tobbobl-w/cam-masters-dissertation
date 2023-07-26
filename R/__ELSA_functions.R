ReadAndSetWave <- function(
    filename,
    select_cols = "all") {
    # Read a tab file and set a wave number.

    # Extract wave number from filename
    WAVE_num <- str_extract(filename, "wave_\\d{1}")

    # get data, if we know the cols we want then we just select those

    if (select_cols[1] == "all") { # bit sloppy
        data <- fread(filename)
    } else {
        data <- fread(filename,
            select = select_cols
        )
    }
    # Set wave in data
    data[, file_wave := WAVE_num]

    return(data)
}

ELSA_dir <- function(string = "*") {
    elsa_dir <- dir("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/",
        pattern = "wave",
        full.names = TRUE
    ) %>%
        grep(string, ., value = TRUE)

    return(elsa_dir)
}

ReadRTF <- function(filename) {
    Lines <- readLines(filename)
    pat <- "^\\\\f0.*\\\\cf0 "
    g <- grep(pat, Lines, value = TRUE)
    noq <- gsub("\\\\'", "'", g)

    clean <- sub("\\\\.*", "", sub(pat, "", noq))
    return(clean)
}

search_names <- function(data, string, ...) {
    grep(
        string,
        names(data),
        value = TRUE,
        ...
    )
}


# List of names for the summary table
clean_names <- list(
    "Gender" = "ragender",
    "RetirementYear" = "retirement_year",
    "InterviewYear" = "int_year",
    "YearsSinceRetirement" = "years_since_retirement",
    "RetiredAge" = "retired_age",
    "AgeAtInterview" = "age_at_interview",
    "ExpectedRetiredAge" = "first_exp_ret_age",
    "DifferenceAge" = "exp_real_ret_age",
    "FinWealth(£000s)" = "fin_wealth",
    "DCPension" = "ever_dc_pen_bin",
    "DCValue(£000s)" = "dc_pot",
    "DBPension" = "ever_db_pen_bin",
    "StatePension" = "public_pension",
    "OwnsHouse" = "owns_house",
    "HouseValue(£000s)" = "house_value",
    "ObjectiveLifeExp" = "obj_life_exps",
    "SubjectiveLifeExp" = "sub_life_exps",
    "TotalConsump" = "total_monthly_consumption",
    "FoodConsump" = "monthly_food_total",
    "FoodConsumpIn" = "monthly_food_in",
    "FoodConsumpOut" = "monthly_food_out",
    "ClothingConsump" = "monthly_clothing",
    "LeisureConsump" = "monthly_leisure",
    "UtilityConsump" = "monthly_utility"
)

ReturnCleanName <- function(short_name) {
    names(clean_names)[clean_names == short_name]
}


VariableOrder <- function(name_vector) {
    # takes a vector of names
    # returns in the order specified in clean names
    stopifnot(all(name_vector %in% unlist(clean_names)))

    name_vector[order(match(name_vector, unlist(clean_names)))]
}
