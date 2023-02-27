setwd(dirname(rstudioapi::getSourceEditorContext()$path))
library(data.table)
library(dplyr)
library(ggplot2)

all_data <- fread("../../data/clean_data/joined_household_individual.csv")
names(all_data)

# Plot mean annuity income
# Plot proportion who have an annuity income
income <- all_data %>%
    mutate(any_annuity_inc = fifelse(annuity_income > 0, 1, 0)) %>%
    group_by(age, birth_year) %>%
    summarise(
        prop_annuity = mean(any_annuity_inc),
        mean_annuity = mean(annuity_income[any_annuity_inc == 1])
    )

income %>%
    filter(birth_year == 1945)

# Let's plot any annuity income for over 65s

plot_data <- income %>%
    filter(age == 65)

ggplot(
    plot_data,
    aes(
        x = birth_year,
        y = prop_annuity
    )
) +
    geom_point()


