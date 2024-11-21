# Look at pension wealth for those who retired
library(data.table)
library(dplyr)
library(stringr)
library(readxl)

source("__ELSA_functions.R")

search_files <- function(string, ...) {
  dir("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/",
    pattern = "wave",
    full.names = TRUE
  ) %>%
    grep(string, ., value = T, ...)
}

# ---------------- Get Pension Wealth files ----------------
pension_wealth_files <- search_files("(3|4|5)_pension_wealth") %>%
  lapply(fread) %>%
  Reduce(function(x, y) full_join(x, y, by = "idauniq"), .) %>% # bind together
  setnames(str_to_lower)

pw_long <- melt(
  pension_wealth_files,
  id.vars = c("idauniq", "file_wave"),
  measure = patterns(
    "totpenw_\\d{2}", "inreceipt_\\d{2}", "currentdc_\\d{2}",
    "currentdb_\\d{2}", "retaineddc_\\d{2}", "retaineddb_\\d{2}",
    "stpenw_\\d{2}", "bsp_\\d{2}", "addpen_\\d{2}", "pripenw_\\d{2}"
  ),
  value.name = c(
    "total_pension_wealth", "in_receipt",
    "current_dc", "current_db",
    "retained_dc", "retained_db",
    "state_pw", "bsp_wealth",
    "add_pw", "private_pw"
  ),
  variable.name = "file_index"
)

pw_long[, wave := fcase(
  file_index == 1, 3,
  file_index == 2, 4,
  file_index == 3, 5
)]

# really we only use individuals from waves 3, 4, 5
# check file 1 pension wealth files are the same

# Assume pension pots accrue at a rate of 3% a year
# find last size of dc pension then
# for every year after that just increase by three percent
accural_rate <- 1.03


id_date <- fread(
  "../../data/ELSA/elsa_to_use/harmonised_data_long.csv",
  select = c("idauniq", "int_month_date", "wave")
)

id_date <- id_date[wave == 5][, int_year := year(int_month_date)]

wave_5_dt <- pw_long[wave == 5][, .(idauniq, init_total_dc = retained_dc + current_dc)]

wave_5_dt <- merge.data.table(id_date, wave_5_dt, by = c("idauniq"))

# Create a dt going nine years into the future
future_years <- data.table(
  idauniq = unlist(lapply(wave_5_dt$idauniq, function(id) rep(id, 10))),
  years_into_future = rep(c(0:9), length(wave_5_dt$idauniq))
) %>%
  merge.data.table(wave_5_dt, by = c("idauniq"))

future_years[, dc_pot := init_total_dc * (accural_rate^years_into_future)]
future_years[, year := int_year + years_into_future]

future_years[init_total_dc > 0, .(dc_pot = mean(dc_pot)), by = .(year)]
# Looks good
# Do we only care about the size of DC pots?
# Yes i think so.
# Then we can just save this

future_years <- future_years[, .(idauniq, year, dc_pot)]



# match on idauniq
fwrite(pw_long, "../../data/ELSA/elsa_to_use/elsa_pension_wealth.csv")

# match on interview year and id
fwrite(future_years, "../../data/ELSA/elsa_to_use/elsa_dc_pen_wealth_predictions.csv")




# -------------------------- OLD --------------------------


# pension_grid <- dir("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/",
#     pattern = "wave",
#     full.names = TRUE
# ) %>%
#     grep("pension(_grid|grid)", ., value = T) %>%
#     lapply(ReadAndSetWave)

# #  %>%
# # rbindlist(fill = T, use.names = T)

# mapply(search_names, pension_grid, "wpapu", ignore.case = TRUE)

# pension_grid

# head(pension_grid[[1]])

# pension_grid1 <- pension_grid[[1]]

# head(pension_grid1)

# # oh maybe this is when the pension mapping grid becomes useful.
# # get pension grid lookups
# grid_lookups <- read_excel(
#     "../../data/ELSA/elsa_unziped/UKDA-5050-tab/mrdoc/excel/5050_wave_2_pension_grid_corresponding_variables.xls"
# )

# names(grid_lookups)
# head(grid_lookups)
# # lets have a look at the excel files.
# # pension variables seem to be different in each


# dir("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/",
#     pattern = "wave",
#     full.names = TRUE
# ) %>%
#     grep("pension(_wealth|wealth)", ., value = T)
# # Only up to wave 5

# search_files("fin")

# grid_5 <- fread("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/wave_5_pension_wealth.tab")

# fin_7 <- fread("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/wave_7_financial_derived_variables.tab")

# names(grid_5)[names(grid_5) %in% names(fin_7)]
# names(grid_5)
# names(fin_7)
# head(grid_5)

# fin_6 <- fread

# fread(pension_files[[1]]) %>%
#     names() %>%
#     grep("", ., value = T)

# fread("../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/wave_9_elsa_pensiongrid_eul_v1.tab") %>%
#     names() %>%
#     grep("", ., value = T)

# # Join across waves I think. Is there a unique joiner in each household
# lapply(financial_files, \(x) "idauniq" %in% names(fread(x)))



# all_pension <- lapply(pension_files, ReadAndSetWave) %>%
#     rbindlist(fill = T, use.names = T)

# # there is partner income

# head(all_pension)

# # do annuity rates change in the data

# # was eric french's value function iteration paper based on elsa data
# # could i do that?

# # for that I need medical spending data as well, is that in ELSA?
# # No it is not in elsa but it is in fes I think
# # so then I could estimate a model of medical spending outside of the model
# # and then match on asset distribution as de nardi et al do?


# pension_wealth <- fread(
#     "../../data/ELSA/elsa_unziped/UKDA-5050-tab/tab/wave_2_pension_wealth.tab")


# getwd()
