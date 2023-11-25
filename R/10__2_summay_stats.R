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
        dc_pot = dc_pot / 1000,
        public_pension = public_pension / 1000
    )

search_names(data_for_regressions, "pen")
search_names(data_for_regressions, "year")

data_for_regressions$ragender %>% table()



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
            # sd = \(x) sd(x, na.rm = T),
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
stop()
align_string <- function(col_names, alignment = "r") {
    # Left align first col
    # right align others
    right <- rep(alignment, length(col_names) - 1)
    # first collapse the vector
    # then join with l
    full_string <- paste0("l", paste0(right, collapse = ""))

    stopifnot(nchar(full_string) == length(col_names))

    return(full_string)
}

ss_table <- kbl(ss_stat_out,
    col.names = col_names,
    booktabs = TRUE,
    format = "latex",
    caption = "Summary statistics \\label{tab:sum_stats} ",
    align = align_string(names(ss_stat_out))
) %>%
    add_header_above(header_to_add) %>%
    kable_styling(font_size = 10)

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
        " ~ pre_post_ref + rv + rv:pre_post_ref "
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
    caption = "Covariate Balance \\label{tab:cov_balance}",
    align = align_string(col_names, alignment = "d")
)

??systemfit()
library(data.table, verbose = T)


install.packages("systemfit")
installed.packages()
writeLines(
    coef_table,
    "../Texfiles/tables/cov_balance.tex"
)




# I want to write my own F-stat function
# takes multiple models and covariance matrices
# then calcs F stat for the variable that I care

model_1 = lm(
    ragender ~  pre_post_ref + rv + rv:pre_post_ref , 
    data = data_for_regressions
)

# i.e. constrain that pre_post_ref == 0
model_2 = lm(
    ragender ~  rv + rv:pre_post_ref , 
    data = data_for_regressions
)

sigma_hat = 1/nrow(data_for_regressions)*(sum(model_1$residuals^2))
sigma_tilda = 1/nrow(data_for_regressions)*(sum(model_2$residuals^2))

n = nrow(data_for_regressions)
coefs = length(model_1$coefficients)

F_stat = (sigma_tilda - sigma_hat)/(sigma_hat/(n - coefs))

coeftest(model_1)
F_stat * 4

library(lmtest)

install.packages("coeftest")

install.packages("lmtest")
names(model_1)

model_1  %>% 
lapply(head)
model_1$qr  %>% names

model_1$qr$rank


