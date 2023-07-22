library(knitr)
library(kableExtra)
library(data.table)
library(dplyr)
library(stringr)

rm(list = ls())
source("__ELSA_functions.R")

# Covariate Balance
# need to add home ownership
# could have a look at the retirement paper that we did in s130
# what about pre retirement consumption
# also different consumptions
# lets hammer out the empirical work today and tomorrow
# and then focus on the other models on sunday


if (!dir.exists("../Texfiles/tables")) {
    dir.create("../Texfiles/tables")
}

data_for_regressions <- fread("../../data/ELSA/elsa_to_use/elsa_reg_data.csv") %>%
    mutate(
        fin_wealth = fin_wealth / 1000,
        house_value = house_value / 1000,
        dc_pot = dc_pot / 1000
    )
data_for_regressions$year_of %>% table()

search_names(data_for_regressions, "year")

# List of names for the summary table
clean_names <- list(
    "Gender" = "ragender",
    "RetirementYear" = "retirement_year",
    "InterviewYear" = "int_year",
    "YearsSinceRetirement" = "years_since_retirement",
    "RetiredAge" = "retired_age",
    "ExpectedRetiredAge" = "first_exp_ret_age",
    "DifferenceAge" = "exp_real_ret_age",
    "FinancialWealth (thousands)" = "fin_wealth",
    "DCPension" = "ever_dc_pen_bin",
    "DCValue (thousands)" = "dc_pot",
    "DBPension" = "ever_db_pen_bin",
    "OwnsHouse" = "owns_house",
    "HouseValue (thousands)" = "house_value",
    "ObjectiveLifeExp" = "obj_life_exps",
    "SubjectiveLifeExp" = "sub_life_exps",
    "TotalConsump" = "total_monthly_consumption",
    "FoodConsumpInHouse" = "monthly_food_in",
    "FoodConsumpOutHouse" = "monthly_food_out",
    "ClothingConsump" = "monthly_clothing",
    "LeisureConsump" = "monthly_leisure",
    "UtilityConsump" = "monthly_utility"
)


ReturnCleanName <- function(short_name) {
    names(clean_names)[clean_names == short_name]
}

summary_cols <- unname(unlist(clean_names))
consumption_cols <- summary_cols[grepl("monthly", summary_cols)]
non_consump_cols <- summary_cols[!grepl("monthly", summary_cols)]


ss_stat_out <- data_for_regressions %>%
    group_by(pre_post_ref) %>%
    mutate(across(all_of(summary_cols), ~ as.numeric(.x))) %>%
    summarise(across(
        all_of(summary_cols),
        list(
            mean = \(x) mean(x, na.rm = T),
            sd = \(x) sd(x, na.rm = T),
            min = \(x) min(x, na.rm = T),
            max = \(x) max(x, na.rm = T),
            median = \(x) median(x, na.rm = T),
            non_missing = \(x) sum(!is.na(x))
        ),
        .names = "{.col}__{.fn}"
    )) %>%
    data.table() %>%
    melt(id.vars = c("pre_post_ref")) %>%
    mutate(row_name = str_extract(variable, ".+(?=__)")) %>%
    # i.e. extract everything before the double underscore
    mutate(function_name = str_extract(variable, "(?<=__).+")) %>%
    mutate(function_treatment = paste0(function_name, "_", pre_post_ref)) %>%
    dcast(row_name ~ function_treatment, value.var = "value") %>%
    mutate(row_name = unlist(lapply(row_name, ReturnCleanName))) %>%
    mutate(across(matches("sd|mean"), ~ sprintf("%.3f", .x))) %>%
    mutate(across(matches("min|max|median"), ~ sprintf("%.1f", .x)))


# set order out
ss_stat_out <- ss_stat_out[order(match(ss_stat_out$row_name, names(clean_names)))]

function_names <- names(ss_stat_out) %>%
    str_remove("_(control|treat)") %>%
    str_replace("_|(row_name)", " ") %>%
    str_to_title() %>%
    unique()

col_names <- names(ss_stat_out) %>%
    str_extract("name|control|treat") %>%
    str_replace("name", "") %>%
    str_to_title()


header_to_add <- c(1, rep(2, length(function_names) - 1))

names(header_to_add) <- function_names

# TODO
ss_table <- kbl(ss_stat_out,
    col.names = col_names,
    booktabs = TRUE,
    format = "latex",
    caption = "Summary statistics \\label{tab:sum_stats}"
) %>%
    add_header_above(header_to_add)

writeLines(
    ss_table,
    "../Texfiles/tables/r_summary_stats.tex"
)



# table not separated by treated and control
# all_ss_stat <- data_for_regressions %>%
#     mutate(across(all_of(summary_cols), ~ as.numeric(.x))) %>%
#     summarise(across(
#         all_of(summary_cols),
#         list(
#             mean = \(x) mean(x, na.rm = T),
#             sd = \(x) sd(x, na.rm = T),
#             min = \(x) min(x, na.rm = T),
#             max = \(x) max(x, na.rm = T),
#             missing = \(x) mean(is.na(x))
#         ),
#         .names = "{.col}__{.fn}"
#     )) %>%
#     data.table() %>%
#     melt(id.vars = integer()) %>%
#     mutate(row_name = str_extract(variable, ".+(?=__)")) %>% # i.e. extract everything before the double underscore
#     mutate(function_name = paste0("all_data", "_", str_extract(variable, "(?<=__).+"))) %>%
#     dcast(row_name ~ function_name, value.var = "value") %>%
#     mutate(across(matches("sd|mean"), ~ sprintf("%.3f", .x))) %>%
#     mutate(across(matches("min|max"), ~ sprintf("%.1f", .x)))




CreateCovBalanceFormulas <- function(variable_name) {
    string_linear <- paste0(
        variable_name,
        " ~ pre_post_ref + rabyear + rabyear*pre_post_ref"
    )

    formula_to_use <- formula(string_linear)

    return(formula_to_use)
}


RunCovariateBalance <- function(variable_name) {
    print(variable_name)
    formula_to_use <- CreateCovBalanceFormulas(variable_name)

    output <- lm(formula_to_use, data_for_regressions)
    sum_output <- summary(output)

    # Extract coefficient on pre_post_ref and its sd
    coef <- sum_output$coefficients[rownames(sum_output$coefficients) ==
        "pre_post_reftreat", "Estimate"]

    std_error <- sum_output$coefficients[rownames(sum_output$coefficients) ==
        "pre_post_reftreat", "Std. Error"]

    return(list(
        var_name = variable_name,
        coef = coef,
        std_error = std_error
    ))
}


cov_balance <- lapply(non_consump_cols, RunCovariateBalance) %>%
    rbindlist() %>%
    mutate(
        coef = sprintf("%.3f", coef),
        std_error = sprintf("%.3f", std_error)
    ) %>%
    mutate(var_name = sapply(var_name, ReturnCleanName))


cov_balance <- cov_balance[order(match(
    cov_balance$var_name,
    names(clean_names[clean_names %in% non_consump_cols])
))]

cov_balance <- cov_balance
col_names <- c("", "Pt. est.", "SE")

coef_table <- kbl(cov_balance,
    col.names = col_names,
    booktabs = TRUE,
    format = "latex",
    caption = "Covariate Balance \\label{tab:cov_balance}"
)

writeLines(
    coef_table,
    "../Texfiles/tables/cov_balance.tex"
)

# this seems quite non-sensical



## Now save these variables


# use stargazer for now because I know how it works
# save as tex files in the latex file






# 0.3 of a year longer

# do this for all the above variables
# basically want to copy the table from MD





# MD has an example of a fuzzy RD that could be good to copy.
