library(dplyr)
library(ggplot2)


data_for_regressions <- fread(
    "../../data/ELSA/elsa_to_use/elsa_reg_data.csv"
)

# #
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
        ragender + ever_dc_pen + ever_dc_pen * pre_post_ref + years_since_retirement,
    data = data_for_regressions
)

# ok this is encouraging
# what actually is the method though?
# what assumptions does this rely on

# & ever_dc_pen == TRUE

# also works without dc filter
# affects are prob large enough to be ok after adjusting for cpi inflation


# what about couples??
# lets try and solve subjective code
# done this



# Lets also run a diff in diff where the control group are retirees without a db pension
