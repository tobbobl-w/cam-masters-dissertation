# Look at pension wealth for those who retired

library(data.table)
library(dplyr)
library(stringr)
library(readxl)

source("__ELSA_functions.R")

pension_grid <- dir("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/",
    pattern = "wave",
    full.names = TRUE
) %>%
    grep("pension(_grid|grid)", ., value = T) %>%
    lapply(ReadAndSetWave)

#  %>%
# rbindlist(fill = T, use.names = T)

mapply(search_names, pension_grid, "wpapu", ignore.case = TRUE)

pension_grid

head(pension_grid[[1]])

pension_grid1 <- pension_grid[[1]]

head(pension_grid1)

# oh maybe this is when the pension mapping grid becomes useful.
# get pension grid lookups
grid_lookups <- read_excel(
    "../../data/ELSA/elsa_unziped/UKDA-5050-tab/mrdoc/excel/5050_wave_2_pension_grid_corresponding_variables.xls"
)

names(grid_lookups)
head(grid_lookups)
# lets have a look at the excel files.
# pension variables seem to be different in each

search_files <- function(string, ...) {
    dir("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/",
        pattern = "wave",
        full.names = TRUE
    ) %>%
        grep(string, ., value = T, ...)
}

dir("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/",
    pattern = "wave",
    full.names = TRUE
) %>%
    grep("pension(_wealth|wealth)", ., value = T)
# Only up to wave 5

search_files("fin")

grid_5 <- fread("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/wave_5_pension_wealth.tab")

fin_7 <- fread("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/wave_7_financial_derived_variables.tab")

names(grid_5)[names(grid_5) %in% names(fin_7)]
names(grid_5)
names(fin_7)
head(grid_5)

fin_6 <- fread

fread(pension_files[[1]]) %>%
    names() %>%
    grep("", ., value = T)

fread("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/wave_9_elsa_pensiongrid_eul_v1.tab") %>%
    names() %>%
    grep("", ., value = T)

# Join across waves I think. Is there a unique joiner in each household
lapply(financial_files, \(x) "idauniq" %in% names(fread(x)))



all_pension <- lapply(pension_files, ReadAndSetWave) %>%
    rbindlist(fill = T, use.names = T)

# there is partner income

head(all_pension)

# do annuity rates change in the data

# was eric french's value function iteration paper based on elsa data
# could i do that?

# for that I need medical spending data as well, is that in ELSA?
# No it is not in elsa but it is in fes I think
# so then I could estimate a model of medical spending outside of the model
# and then match on asset distribution as de nardi et al do?

# Why is this research interesting -- no satisfactory life-cycle model that explains the
# dec


pension_wealth <- fread("../../data/ELSA/UKDA-8502-tab/tab/wave_2_pension_wealth.tab")
