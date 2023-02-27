library(stringr)
library(data.table)

household_info_names <- list.files(
    "Data",
    recursive = T,
    pattern = "dvhh"
)


# So I want to create a csv file with folder name and then the year of the data that that refers to

GuessYear <- function(zipfilename) {
    file_info <- unzip(
        zipfilename,
        list = TRUE
    )
    # Check year for the household file
    household_file <- basename(grep("dvhh", file_info$Name, value = T))

    year_guess <- unique(str_extract(household_file, "\\d{4}"))

    # Stop if we don't have a unique guess
    stopifnot(length(year_guess) == 1)

    output <- data.frame(
        filename = basename(zipfilename),
        year = year_guess
    )
    return(output)
}

files_and_year <- lapply(
    dir("../../data/ZIP_files/", full.names = T),
    GuessYear
) %>%
    rbindlist()

# Great same numer of years as files
length(files_and_year$year) == length(dir("../../data/ZIP_files"))

data_start_year <- 2003
data_end_year <- 2020

all(files_and_year$year == c(data_start_year:data_end_year))

# Now we can extract
# First clean up the data folder
dir.create("../../data/unziped_data")

files_and_year[, long_name := paste0("../../data/ZIP_files/", filename)]
files_and_year[, newfolder_name := paste0("../../data/unzipped_data/", year)]

for (i in seq_along(files_and_year$long_name)) {
    unzip(
        zipfile = files_and_year$long_name[i],
        exdir = files_and_year$newfolder_name[i],
        overwrite = T
    )
}

# Now we have a folder with all the unzipped data in

# We can go and extract the dvhh file we need

# Let's copy and paste them all into one folder so then we can easily check if needed

# Rough list of dervived household variables
dvhh_filenames <- list.files("../../data/unzipped_data",
    recursive = TRUE,
    full.names = TRUE
) %>%
    grep("dvhh_(uk|UK)anon", ., value = T) %>%
    grep("\\.tab$", ., value = T) %>%
    .[!grepl("2015q1", .)]

# Missing 2005

dir.create("../../data/derived_files/")
file.copy(dvhh_filenames, "../../data/derived_files/", overwrite = TRUE)

# Rough list of dervived person level files
pers_filenames <- list.files("../../data/unzipped_data",
    recursive = TRUE,
    full.names = TRUE
) %>%
    grep("dvper_(uk|UK)anon", ., value = T) %>%
    grep("\\.tab$", ., value = T) %>%
    .[!grepl("2015_quarter_1", .)]

file.copy(pers_filenames, "../../data/derived_files/", overwrite = TRUE)
