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
    mutate(product_type = str_to_lower(product_type)) %>%
    filter(date > "2009-01-01")
# Q1 30 march, Q2 , Q3


pen_freedons_announce <- dmy("19 March 2014")
pen_freedoms_force <- dmy("1 April 2015")

label_df <- data.frame(
    date = c(pen_freedons_announce - 50, pen_freedoms_force + 50),
    y = c(100000, 80000),
    labels = c("Announcement", "Fully implemented")
)

annuity_plot <- ggplot(joined_data) +
    geom_point(aes(x = date, y = num_sales, colour = product_type)) +
    geom_line(aes(x = date, y = num_sales, colour = product_type)) +
    scale_x_date(date_breaks = "years", date_labels = "%Y") +
    theme_bw() +
    labs(
        x = "", y = "Sales", title = "Select Product Sales Data",
        colour = "Product type"
    ) +
    geom_vline(
        xintercept = pen_freedons_announce,
        linetype = "dotted"
    ) +
    geom_vline(
        xintercept = pen_freedoms_force,
        linetype = "dotted"
    ) +
    geom_text(data = label_df, aes(x = date, y = y, label = labels))


if (!dir.exists("../Texfiles/figures")) {
    dir.create("../Texfiles/figures")
}

ggsave(
    "../Texfiles/figures/annuity_overtime.pdf",
    annuity_plot,
)


pot_size_dt <- fread("../../data/FCA_annuities/2122_accessed_by_size_pension.csv") %>%
    melt("Pot size",
        variable.name = "Product type",
        value.name = "Pots"
    )

pot_size_dt %>%
    group_by(`Product type`) %>%
    summarise(
        sum_pots = sum(Pots)
    ) %>%
    mutate(prop_pots = sum_pots / sum(sum_pots))

# Get the order of pot size smallest to largest
pots_order <- fread("../../data/FCA_annuities/2122_accessed_by_size_pension.csv")[, 1] %>%
    unlist()

pot_size_dt[, `Pot size` := factor(`Pot size`, pots_order)]

annuity_sizes_plot <- ggplot(
    pot_size_dt,
    aes(
        x = `Pot size`,
        y = Pots,
        fill = `Product type`
    )
) +
    geom_col() +
    coord_flip() +
    labs(title = "Number of pots accessed by size, 2021-22") +
    theme_bw()

ggsave(
    "../Texfiles/figures/annuity_pot_sizes.pdf",
    annuity_sizes_plot,
    height = 5,
    width = 11,
    units = "in"
)
