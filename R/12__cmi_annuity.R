# calculate annuity prices using the IofAs continuous mortality series data
# this is the actual death probs of annuitants,
# so we can add a loading factor to the implied price and use
# that.


library(readxl)
library(dplyr)
library(data.table)
library(stringr)
library(ggplot2)

# annuitant rates

# this is the 2008 annuitants mortality rates
# Table	        Description
# LML08	        Life annuities Male Lives
# LFL08	        Life annuities Female Lives

# annuity life tables in 2008
ann_lt_08 <- read_excel(
    "../../data/CMI/raw/92190-cmiwp81-final08series-annuities-rates.xlsx",
    sheet = "08 annuities qx",
    skip = 4
) %>%
    setnames(str_to_lower) %>%
    select(age = "age x", "lml08", "lfl08") %>% # get annuitant life tables
    filter(age >= 60) %>% # only have data for those people who are over 60
    mutate(year = 2008) %>%
    rename(
        male_prob = lml08,
        female_prob = lfl08
    )



###### 2016 data ######
# Table name	Product type	Gender	Weighting
# PML16	        All products	Male	Lives
# PFL16	        All products	Female	Lives

ann_lt_16 <- read_excel(
    "../../data/CMI/raw/Final 16 Series pension annuitant mortality rates v01 2020-07-10.xlsx",
    sheet = "q",
    skip = 6
) %>%
    setnames(str_to_lower) %>%
    select(age, pml16, pfl16) %>%
    filter(age >= 60) %>%
    mutate(year = 2016) %>%
    rename(
        male_prob = pml16,
        female_prob = pfl16
    )

# these are just probs of reaching next year
#   conditional on being a certain age.
# I need to adjust if I am just considering from age 60.
#


both_years <- rbindlist(list(ann_lt_08, ann_lt_16)) %>%
    tidyr::pivot_longer(
        c(male_prob, female_prob),
        names_to = "gender",
        values_to = "prob"
    ) %>%
    mutate(gender = str_remove(gender, "_prob"))

both_years %>%
    mutate(gender_year = paste0(gender, "-", year)) %>%
    ggplot(aes(
        x = age, y = prob,
        colour = gender_year
    )) +
    geom_point() +
    geom_line()

fwrite(
    both_years,
    "../../data/CMI/raw/cmi_annuitant_lifetables.csv"
)

# I will then use these in Julia to create realistic product costs
