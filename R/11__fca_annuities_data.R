library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(readxl)
library(tidyr)
library(lubridate)

dir("../../data/FCA_annuities", full.names = T)

annuity_data_2018 <- read_excel("../../data/FCA_annuities/psd-ri-2018.xlsx",
    sheet = "Quarterly ",
    range = "B9:BD28"
)

# get data up to 2015
annuity_data_2015 <- read_excel("../../data/FCA_annuities/ri-2015-data.xlsx",
    sheet = "Quarterly ",
    range = "B7:AR26"
)

names(annuity_data_2015)[2] <- "product_type"

table(annuity_data_2015$product_type)
# Make the data long
annuity_data_2015_long <- annuity_data_2015 %>%
    filter(product_type %in% c("Annuities", "Income Drawdown")) %>%
    pivot_longer(
        cols = -c(1, 2), names_to = "date",
        values_to = "num_sales"
    ) %>%
    mutate(date = paste0(
        str_extract(date, "Q\\d{1}"),
        "-",
        str_extract(date, "\\d{4}")
    )) %>%
    select(product_type, date, num_sales)


# Read in 2020 data. It is already long.
annuity_data_2020_long <- read_excel("../../data/FCA_annuities/product-sales-data-retail-investments-2020.xlsx") %>%
    select(c(3, 4, 5)) %>%
    filter(`Sub Category` %in% c("Annuities", "Income drawdown"))


#

joined_data <- rbindlist(list(annuity_data_2015_long, annuity_data_2020_long),
    use.names = FALSE
) %>%
    mutate(
        date = gsub("Q1", "30-03", date),
        date = gsub("Q2", "30-06", date),
        date = gsub("Q3", "30-09", date),
        date = gsub("Q4", "30-12", date)
    ) %>%
    mutate(date = dmy(date)) %>%
    mutate(product_type = str_to_lower(product_type))
# Q1 30 march, Q2 , Q3


ggplot(joined_data, aes(x = date, y = num_sales, colour = product_type)) +
    geom_point() +
    geom_line()

# massive drop in 2013 as well.
# So if retirement expectation was before 2013 then untreated, if retirement expectation was after 2013 then the cohort was treated
# what types of things will i need to control for
# probably quite alot. but hopefully there are not too many differences
# between these two groups apart from the fact one was impacted and one not
# then the second strategy will be to look at
