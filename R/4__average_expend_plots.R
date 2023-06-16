library(data.table)
library(dplyr)
library(ggplot2)

all_data <- fread("../../data/clean_data/joined_household_individual.csv")
names(all_data)

summarised_data <- all_data %>%
    filter(data_wave != 2020) %>% # Filter 2020 because it has strange data
    group_by(data_wave, age) %>%
    summarise(mean_expend = mean(total_expend)) %>%
    mutate(birth_year = data_wave - age) %>%
    mutate(age_in_2014 = 2014 - birth_year) %>%
    mutate(turned_65_post_reform = fifelse(
        data_wave >= 2014 & age_in_2014 <= 65,
        "Turned 65 after reform",
        "65 before the reform"
    )) %>%
    mutate(birth_year = as.character(birth_year)) %>%
    mutate(age = as.integer(age))


# Plot births in 1950 to 1960

by_data_wave <- ggplot(
    summarised_data %>% filter(birth_year %in% c(1950:1960)),
    aes(
        x = data_wave,
        y = mean_expend,
        colour = birth_year
    )
) +
    geom_point() +
    geom_line() +
    labs(
        x = "Year",
        y = "Average expenditure",
        title = "Average expenditure by birth year",
        colour = "Birth Year"
    )
if (!dir.exists("plots")) dir.create("plots")
ggsave("../../plots/avg_expend_data_wave.pdf", plot = by_data_wave, )

# Plot consumption at age 65 for all cohorts

consumption_at_65 <- ggplot(
    summarised_data %>% filter(age == 65),
    aes(
        x = birth_year,
        y = mean_expend,
        colour = turned_65_post_reform,
    )
) +
    geom_point() +
    labs(
        x = "Birth year",
        y = "Average expenditure at age 65",
        title = "Average expenditure at age 65",
        colour = "Birth Year"
    )

ggsave(
    "../../plots/average_expend_at_65.pdf",
    consumption_at_65
)


# Distribution of expenditre over 60s
# Plot by age, colour by year of birth
plot_data <- summarised_data %>%
    filter(birth_year %in% c(1947:1953)) %>%
    filter(age >= 60) %>%
    filter(age <= 70)

expiture_by_age_and_birth_year <- ggplot(
    plot_data,
    aes(
        x = age,
        y = mean_expend,
        colour = birth_year
    )
) +
    geom_point() +
    geom_line() +
    labs(
        x = "Age",
        y = "Average expenditure",
        title = "Average expenditure by age and birth year",
        colour = "Birth Year"
    )

ggsave(
    "../../plots/expenditure_by_age_and_birth_year.pdf",
    expiture_by_age_and_birth_year
)

# maybe I should look at expenditure on consumables only




# Now look at just an average?
# Average of consumer spending over 60s for people turning 65 in 2010-2015
# And the same for people turning 65 in 2015-2020.

head(summarised_data)

age_plot_pre_post <- summarised_data %>%
    ungroup() %>%
    mutate(pre_post_reform_group = case_when(
        age_in_2014 >= 65 & age_in_2014 <= 70 ~ "Pre-Reform",
        age_in_2014 <= 65 & age_in_2014 > 60 ~ "Post-Reform"
    )) %>%
    filter(!is.na(pre_post_reform_group)) %>%
    group_by(pre_post_reform_group, age) %>%
    summarise(mean_expend = mean(mean_expend))


look <- ggplot(age_plot_pre_post, aes(
    x = age, y = mean_expend,
    colour = pre_post_reform_group
)) +
    geom_point() +
    geom_line()

difference_plot <- age_plot_pre_post %>%
    tidyr::pivot_wider(
        names_from = pre_post_reform_group,
        values_from = mean_expend
    ) %>%
    mutate(difference = `Post-Reform` - `Pre-Reform`)

look <- ggplot(difference_plot, aes(x = age, y = difference)) +
    geom_point() +
    geom_line()



ggsave("../../plots/look.pdf", look)

# Ok that is kind of interesting.
# The biggest difference in consumption spending comes at age 65


# Can i write out a value function





?case_when
