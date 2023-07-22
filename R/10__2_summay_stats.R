library(knitr)
library(kableExtra)


# Covariate Balance
# need to add home ownership
# could have a look at the retirement paper that we did in s130
# what about pre retirement consumption
# also different consumptions
# lets hammer out the empirical work today and tomorrow
# and then focus on the other models on sunday

rm(list = ls())

if (!dir.exists("../Texfiles/tables")) {
    dir.create("../Texfiles/tables")
}

data_for_regressions <- fread("../../data/ELSA/elsa_to_use/elsa_reg_data.csv")

# List of names for the summary table
clean_names <- list(
    "Gender" = "ragender",
    "YearsSinceRetirement" = "years_since_retirement",
    "RetiredAge" = "retired_age",
    "ExpectedRetiredAge" = "first_exp_ret_age",
    "DifferenceAge" = "exp_real_ret_age",
    "FinancialWealth" = "fin_wealth",
    "DCPension" = "ever_dc_pen_bin",
    "DBPension" = "ever_db_pen_bin",
    "HouseValue" = "house_value"
)


ReturnCleanName <- function(short_name) {
    names(clean_names)[clean_names == short_name]
}

summary_cols <- unname(unlist(clean_names))

# , "retirement_year"

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
            non_missing = \(x) sum(!is.na(x))
        ),
        .names = "{.col}__{.fn}"
    )) %>%
    data.table() %>%
    melt(id.vars = c("pre_post_ref")) %>%
    mutate(row_name = str_extract(variable, ".+(?=__)")) %>% # i.e. extract everything before the double underscore
    mutate(function_name = str_extract(variable, "(?<=__).+")) %>%
    mutate(function_treatment = paste0(function_name, "_", pre_post_ref)) %>%
    dcast(row_name ~ function_treatment, value.var = "value") %>%
    mutate(row_name = unlist(lapply(row_name, ReturnCleanName)))

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
# Life expectancies
# Flip table

ss_table <- kbl(ss_stat_out,
    col.names = col_names,
    booktabs = TRUE,
    format = "latex"
) %>%
    add_header_above(header_to_add)

writeLines(
    ss_table,
    "../Texfiles/tables/r_summary_stats.tex"
)



# table not separated by treated and control
all_ss_stat <- data_for_regressions %>%
    mutate(across(all_of(summary_cols), ~ as.numeric(.x))) %>%
    summarise(across(
        all_of(summary_cols),
        list(
            mean = \(x) mean(x, na.rm = T),
            sd = \(x) sd(x, na.rm = T),
            min = \(x) min(x, na.rm = T),
            max = \(x) max(x, na.rm = T),
            missing = \(x) mean(is.na(x))
        ),
        .names = "{.col}__{.fn}"
    )) %>%
    data.table() %>%
    melt(id.vars = integer()) %>%
    mutate(row_name = str_extract(variable, ".+(?=__)")) %>% # i.e. extract everything before the double underscore
    mutate(function_name = paste0("all_data", "_", str_extract(variable, "(?<=__).+"))) %>%
    dcast(row_name ~ function_name, value.var = "value")




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

cov_balance <- lapply(summary_cols, RunCovariateBalance) %>%
    rbindlist()
# %>%
# mutate(var_name = unlist(lapply(var_name, ReturnCleanName)))

cov_balance <- cov_balance[order(match(cov_balance$var_name, names(clean_names)))]

col_names <- c("", "Pt. est.", "SE")

coef_table <- kbl(cov_balance,
    col.names = col_names,
    booktabs = TRUE,
    format = "latex",
    caption = "Covariate Balance \\label{tab:cov_balance}",
    label = "another_lab"
)

writeLines(
    coef_table,
    "../Texfiles/tables/cov_balance.tex"
)



## Now save these variables


# use stargazer for now because I know how it works
# save as tex files in the latex file






# 0.3 of a year longer

# do this for all the above variables
# basically want to copy the table from MD





# MD has an example of a fuzzy RD that could be good to copy.
