library(data.table)
library(dplyr)


sub_dt <- fread("../../data/ELSA/subjective_tables/subjective_survival_probs.csv")

base_pop <- 100000

sub_dt[, number_alive := base_pop * prob]

setorder(sub_dt, -expected_age)

sub_dt[, life_years_left := cumsum(number_alive),
    by = .(id_wave)
]

sub_dt[, life_exp := life_years_left / number_alive]

sub_life_exp <- sub_dt %>%
    filter(age == expected_age - 1) %>% # just want life expectancy at interview date
    mutate(
        age = expected_age - 1,
        subjective_life_exp = life_exp
    ) %>%
    select(id_wave, age, subjective_life_exp)

fwrite(
    sub_life_exp,
    "../../data/ELSA/subjective_tables/subjective_life_expectancies.csv"
)
