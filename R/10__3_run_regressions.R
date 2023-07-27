library(dplyr)
library(ggplot2)
library(modelsummary)

source("./__ELSA_functions.R")

data_for_regressions <- fread(
    "../../data/ELSA/elsa_to_use/elsa_reg_data.csv"
) %>%
    mutate(
        fin_wealth = fin_wealth / 1000,
        house_value = house_value / 1000,
        dc_pot = dc_pot / 1000,
        public_pension = public_pension / 1000
    ) %>%
    mutate(
        pre_post_ref_bin = fcase(
            pre_post_ref == "treat", 1,
            pre_post_ref == "control", 0
        )
    )

clean_names <- append(clean_names, c("PostReform" = "pre_post_ref_bin"), 0)

# #### Plot consumption
# consump_time_in <- harm_long[ret_in == TRUE,
#     .(mean_consump = mean(total_monthly_consumption, na.rm = T)),
#     by = .(date_year)
# ]

# consump_time_eq <- harm_long[ret_equals == TRUE,
#     .(mean_consump = mean(total_monthly_consumption, na.rm = T)),
#     by = .(date_year)
# ]

# ggplot() +
#     geom_point(
#         data = consump_time_in,
#         aes(x = date_year, y = mean_consump, colour = "red")
#     ) +
#     geom_point(
#         data = consump_time_eq,
#         aes(x = date_year, y = mean_consump, colour = "blue")
#     )


## Event study?
## No this is a diff in diff basically says that individuals are
# similar apart from for the reform (after controlling for vars)


lm(total_monthly_consumption ~ pre_post_ref_bin,
    data = data_for_regressions
)

lm(total_monthly_consumption ~ pre_post_ref_bin + age_at_interview,
    data = data_for_regressions
)

lm(
    total_monthly_consumption ~ pre_post_ref_bin + age_at_interview + fin_wealth +
        ragender + ever_dc_pen_bin + ever_dc_pen_bin * pre_post_ref_bin + years_since_retirement + owns_house,
    data = data_for_regressions
)


lm(
    total_monthly_consumption ~ pre_post_ref_bin + age_at_interview + fin_wealth +
        ragender + dc_pot + years_since_retirement + owns_house + public_pension,
    data = data_for_regressions[ever_dc_pen == "yes"]
)

# Define a function that we can use to create and run models
# Pass the lefthand side variable and an expression to filter the data with (i.e. if we only want dc pensioners)
RunRegression <- function(string_form, filter_exp) {
    lm(
        formula(string_form),
        data = data_for_regressions[eval(filter_exp)]
    )
}


# change so that we have c(unclean_name = clean_name)
unclean_names <- unname(unlist(clean_names))
unclean_name_vector <- names(clean_names)
names(unclean_name_vector) <- unclean_names

# select stats to have at the bottom
bottom_stats <- modelsummary::gof_map %>%
    filter(raw %in% c(
        "nobs", "r.squared", "adj.r.squared"
    ))

align_string_ms <- function(col_names, type_alignment = "d") {
    # Left align first col
    # right align others
    multiplier <- 1

    right <- rep(
        type_alignment,
        (length(col_names) * multiplier)
    )
    # first collapse the vector
    # then join with l
    full_string <- paste0("l", paste0(right, collapse = ""))

    return(full_string)
}


# --------- dc only models ---------


rhs <- "pre_post_ref_bin + retired_age + fin_wealth +
ragender + dc_pot + years_since_retirement + owns_house + public_pension"

lhs <- c(
    "total_monthly_consumption", "monthly_food_in", "monthly_food_out",
    "monthly_food_total", "monthly_clothing"
) %>% VariableOrder()

consump_forms <- paste0(lhs, " ~ ", rhs)

out_models_only_dc <- lapply(
    consump_forms,
    RunRegression,
    expression(ever_dc_pen == "yes")
)
names(out_models_only_dc) <- sapply(lhs, ReturnCleanName)


modelsummary(
    out_models_only_dc,
    output = "../Texfiles/tables/elsa_results_only_dc.tex",
    title = "DC Only \\label{tab:DcOnlyRes}",
    coef_rename = unclean_name_vector,
    estimate = "{estimate}",
    statistic = "({std.error})",
    align = align_string_ms(names(out_models_only_dc)),
    gof_map = bottom_stats,
    vcov = ~pre_post_ref_bin,
    notes = "I use robust standard errors clustered at the treatment level
    since standard errors are
    likely correlated within these groups."
)



# ------------ all data with interaction -------
rhs <- "pre_post_ref_bin + pre_post_ref_bin*ever_dc_pen_bin + pre_post_ref_bin*ever_db_pen_bin + retired_age + fin_wealth +
ragender + dc_pot + years_since_retirement + owns_house + public_pension + ever_db_pen_bin"

lhs <- c(
    "total_monthly_consumption", "monthly_food_in", "monthly_food_out",
    "monthly_food_total", "monthly_clothing"
) %>% VariableOrder()

consump_forms <- paste0(lhs, " ~ ", rhs)

out_models_all_data <- lapply(
    consump_forms,
    RunRegression,
    expression(TRUE)
)

names(out_models_all_data) <- sapply(lhs, ReturnCleanName)

modelsummary(
    out_models_all_data,
    output = "../Texfiles/tables/elsa_results_all_with_interact.tex",
    title = "All individuals with interaction \\label{tab:ElsaAllData}",
    coef_rename = unclean_name_vector,
    estimate = "{estimate}",
    statistic = "({std.error})",
    align = align_string_ms(names(out_models_all_data)),
    gof_map = bottom_stats,
    vcov = ~pre_post_ref_bin,
    notes = "I use robust standard errors clustered at the treatment level
    since standard errors are
    likely correlated within these groups."
)

# ----------- Interaction with pension pot size --------------

rhs <- "pre_post_ref_bin + pre_post_ref_bin:dc_pot + retired_age + fin_wealth +
ragender + dc_pot + years_since_retirement + owns_house + public_pension"

lhs <- c(
    "total_monthly_consumption", "monthly_food_in", "monthly_food_out",
    "monthly_food_total", "monthly_clothing"
) %>% VariableOrder()

consump_forms <- paste0(lhs, " ~ ", rhs)

out_models_dc_pot_interaction <- lapply(
    consump_forms,
    RunRegression,
    expression(ever_dc_pen == "yes")
)
names(out_models_dc_pot_interaction) <- sapply(lhs, ReturnCleanName)

modelsummary(
    out_models_dc_pot_interaction,
    output = "../Texfiles/tables/elsa_results_dc_pot_interact.tex",
    title = "DC Pension Size interaction \\label{tab:DcOnlyInteract}",
    coef_rename = unclean_name_vector,
    estimate = "{estimate}",
    statistic = "({std.error})",
    align = align_string_ms(names(out_models_dc_pot_interaction)),
    gof_map = bottom_stats,
    vcov = ~pre_post_ref_bin,
    notes = "I use robust standard errors clustered at the treatment level
    since standard errors are
    likely correlated within these groups."
)



# ----------- Interaction with financial wealth --------------

rhs <- "pre_post_ref_bin + pre_post_ref_bin:fin_wealth + retired_age + fin_wealth +
ragender  + years_since_retirement + owns_house + public_pension"

lhs <- c(
    "total_monthly_consumption", "monthly_food_in", "monthly_food_out",
    "monthly_food_total", "monthly_clothing"
) %>% VariableOrder()

consump_forms <- paste0(lhs, " ~ ", rhs)

out_models_fin_wealth_interaction <- lapply(
    consump_forms,
    RunRegression,
    expression(ever_dc_pen == "yes")
)
names(out_models_fin_wealth_interaction) <- sapply(lhs, ReturnCleanName)

modelsummary(
    out_models_fin_wealth_interaction,
    output = "../Texfiles/tables/elsa_results_fin_wealth_interact.tex",
    title = "DC Financial Wealth interaction \\label{tab:DcOnlyFinWealthInteract}",
    coef_rename = unclean_name_vector,
    estimate = "{estimate}",
    statistic = "({std.error})",
    align = align_string_ms(names(out_models_fin_wealth_interaction)),
    gof_map = bottom_stats,
    vcov = ~pre_post_ref_bin,
    notes = "I use robust standard errors clustered at the treatment level
    since standard errors are
    likely correlated within these groups."
)



# --------- dc only models expected_ret == real ret ---------

rhs <- "pre_post_ref_bin + retired_age + fin_wealth +
ragender + dc_pot + years_since_retirement + owns_house + public_pension"

lhs <- c(
    "total_monthly_consumption", "monthly_food_in", "monthly_food_out",
    "monthly_food_total", "monthly_clothing"
) %>% VariableOrder()

consump_forms <- paste0(lhs, " ~ ", rhs)

out_models_only_dc_only_retexp <- lapply(
    consump_forms,
    RunRegression,
    expression(ever_dc_pen == "yes" & abs(exp_real_ret_age) <= 1)
)
names(out_models_only_dc_only_retexp) <- sapply(lhs, ReturnCleanName)

# Ok lets write code that makes a nice table from this
# and then put into latex
# and then chat about it!


modelsummary(
    out_models_only_dc_only_retexp,
    output = "../Texfiles/tables/rob_elsa_results_only_dc_only_exp.tex",
    title = "Robustness: DC only and retire in year expected  \\label{tab:DcOnlyExpOnlyRes}",
    coef_rename = unclean_name_vector,
    estimate = "{estimate}",
    statistic = "({std.error})",
    align = align_string_ms(names(out_models_only_dc_only_retexp)),
    gof_map = bottom_stats,
    vcov = ~pre_post_ref_bin,
    notes = "I use robust standard errors clustered at the treatment level
    since standard errors are
    likely correlated within these groups."
)


# --------- DC only drop 2012 retirees

rhs <- "pre_post_ref_bin + retired_age + fin_wealth +
ragender + dc_pot + years_since_retirement + owns_house + public_pension"

lhs <- c(
    "total_monthly_consumption", "monthly_food_in", "monthly_food_out",
    "monthly_food_total", "monthly_clothing"
) %>% VariableOrder()

consump_forms <- paste0(lhs, " ~ ", rhs)

out_models_only_dc_only_retexp <- lapply(
    consump_forms,
    RunRegression,
    expression(ever_dc_pen == "yes" & retirement_year > 2012)
)
names(out_models_only_dc_only_retexp) <- sapply(lhs, ReturnCleanName)

modelsummary(
    out_models_only_dc_only_retexp,
    output = "../Texfiles/tables/rob_elsa_results_only_dc_only_late_ret_year.tex",
    title = "Robustness: DC only and retired later than 2012 \\label{tab:DcOnlyNot2012}",
    coef_rename = unclean_name_vector,
    estimate = "{estimate}",
    statistic = "({std.error})",
    align = align_string_ms(names(out_models_only_dc_only_retexp)),
    gof_map = bottom_stats,
    vcov = ~pre_post_ref_bin,
    notes = "I use robust standard errors clustered at the treatment level
    since standard errors are
    likely correlated within these groups."
)
