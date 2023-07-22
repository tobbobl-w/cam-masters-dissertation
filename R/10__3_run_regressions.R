library(dplyr)
library(ggplot2)
library(modelsummary)

data_for_regressions <- fread(
    "../../data/ELSA/elsa_to_use/elsa_reg_data.csv"
) %>%
    mutate(
        fin_wealth = fin_wealth / 1000,
        house_value = house_value / 1000,
        dc_pot = dc_pot / 1000,
        public_pension = public_pension / 1000
    )





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


lm(total_monthly_consumption ~ pre_post_ref,
    data = data_for_regressions
)

lm(total_monthly_consumption ~ pre_post_ref + age_at_interview,
    data = data_for_regressions
)

lm(
    total_monthly_consumption ~ pre_post_ref + age_at_interview + fin_wealth +
        gender + ever_dc_pen + ever_dc_pen * pre_post_ref + years_since_retirement + owns_house,
    data = data_for_regressions
)


lm(
    total_monthly_consumption ~ pre_post_ref + age_at_interview + fin_wealth +
        gender + dc_pot + years_since_retirement + owns_house + public_pension,
    data = data_for_regressions[ever_dc_pen == "yes"]
)

# ------- Other consumption variables -------

rhs <- "pre_post_ref + retired_age + fin_wealth +
gender + dc_pot + years_since_retirement + owns_house + public_pension"

lhs <- c(
    "total_monthly_consumption", "monthly_food_in", "monthly_food_out",
    "monthly_food_total", "monthly_clothing"
) %>% VariableOrder()


consump_forms <- paste0(lhs, " ~ ", rhs)

# Pass the lefthand side variable and an expression to filter the data with
RunRegression <- function(string_form, filter_exp) {
    lm(
        formula(string_form),
        data = data_for_regressions[eval(filter_exp)]
    )
}

out_models_only_dc <- lapply(
    consump_forms,
    RunRegression,
    expression(ever_dc_pen == "yes")
)

names(out_models_only_dc) <- sapply(lhs, ReturnCleanName)

# Ok lets write code that makes a nice table from this
# and then put into latex
# and then chat about it!

# --------- dc only models ---------

# change so that we have c(unclean_name = clean_name)

unclean_names <- unname(unlist(clean_names))
unclean_name_vector <- names(clean_names)
names(unclean_name_vector) <- unclean_name_vector

modelsummary(
    out_models_only_dc,
    output = "../Texfiles/tables/elsa_results_only_dc.tex",
    vcov = "robust",
    title = "DC Only \\label{tab:DcOnlyRes}",
    coef_rename = unclean_name_vector
)


# ------------ all data with interaction -------


out_models_only_dc[[1]]

#
out_models_all_data <- lapply(
    consump_forms,
    RunRegression,
    expression(TRUE)
)






# & ever_dc_pen == TRUE

# also works without dc filter
# affects are prob large enough to be ok after adjusting for cpi inflation


# what about couples??
# lets try and solve subjective code
# done this



# Lets also run a diff in diff where the control group are retirees without a db pension
