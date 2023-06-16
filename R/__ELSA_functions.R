
ReadAndSetWave <- function(filename) {
    # Read a tab file and set a wave number.

    # Extract wave number from filename
    WAVE_num <- str_extract(filename, "wave_\\d{1}")
    data <- fread(filename)
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

search_names <- function(data, string) grep(string, names(data), value = T)
