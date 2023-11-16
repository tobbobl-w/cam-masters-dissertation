library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(modelsummary)
library(kableExtra)

source("./__ELSA_functions.R")
clean_names <- append(clean_names, c("PostReform" = "pre_post_ref_bin"), 0)

# Reading data for regressions

bequest_consump_dt <- fread("../../data/ELSA/lifecycle_outputs/bequest_consumption_simulation.csv") %>%
    rename(
        consump_beq_ann = annuity_consump,
        consump_beq_no_ann = no_annuity_consump
    )

standard_consump_dt <-
    fread("../../data/ELSA/lifecycle_outputs/standard_model_consumption_simulation.csv") %>%
    rename(
        consump_stan_ann = annuity_consump,
        consump_stan_no_ann = no_annuity_consump
    ) %>%
    select(id_wave, consump_stan_ann, consump_stan_no_ann)

subjective_consump_dt <-
    fread("../../data/ELSA/lifecycle_outputs/subjective_model_consumption_simulation.csv") %>%
    rename(
        consump_sub_ann = annuity_consump,
        consump_sub_no_ann = no_annuity_consump
    ) %>%
    select(id_wave, consump_sub_ann, consump_sub_no_ann)


# standard_consump_dt[, .(annuity_cost, annuity_payment)]
# bequest_consump_dt[, .(annuity_cost, annuity_payment)]

reg_dt <- fread(
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


# change so that we have c(unclean_name = clean_name)
unclean_names <- unname(unlist(clean_names))
unclean_name_vector <- names(clean_names)
names(unclean_name_vector) <- unclean_names


# select stats to have at the bottom
bottom_stats <- modelsummary::gof_map %>%
    filter(raw %in% c(
        "nobs", "r.squared", "adj.r.squared", "std.errors",
        "se_type"
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





#  ------------ join data together ------------
joint_dt <- reg_dt %>%
    merge.data.table(
        bequest_consump_dt,
        by = "id_wave"
    ) %>%
    merge.data.table(
        standard_consump_dt,
        by = "id_wave"
    ) %>%
    merge.data.table(
        subjective_consump_dt,
        by = "id_wave"
    ) %>%
    # for 'treated' consumption is without annuity
    # for 'control' consumption is with annuity
    mutate(bequest_consumption = fifelse(
        pre_post_ref == "control" & ever_dc_pen == "yes", consump_beq_ann,
        consump_beq_no_ann
    )) %>%
    mutate(standard_consumption = fifelse(
        pre_post_ref == "control" & ever_dc_pen == "yes",
        consump_stan_ann,
        consump_stan_no_ann
    )) %>%
    mutate(subjective_consumption = fifelse(
        pre_post_ref == "control" & ever_dc_pen == "yes",
        consump_sub_ann,
        consump_sub_no_ann
    )) %>%
    mutate(rich_enough = fifelse(
        unlist(mapply(
            function(x, y) all.equal(x, y) == TRUE,
            consump_beq_ann,
            consump_beq_no_ann
        )),
        "no", "yes"
    )) %>%
    mutate(
        bequest_consumption = bequest_consumption / 12,
        standard_consumption = standard_consumption / 12,
        subjective_consumption = subjective_consumption / 12
    )


joint_dt %>%
    filter(retirement_year == 2012) %>%
    filter(int_year == 2014) %>%
    filter(fin_wealth > 100) %>%
    filter(fin_wealth < 150) %>%
    filter(public_pension > 5) %>%
    select(id_wave)



# which ids do we NOT want
full_directory <- dir("../../data/ELSA/lifecycle_outputs/id_wave/", full.names = T)
files_to_keep <- joint_dt$id_wave %>%
    lapply(function(idwave) grep(idwave, full_directory, value = T)) %>%
    lapply(function(x) x[1]) %>%
    unlist()

files_to_remove <- full_directory[!full_directory %in% files_to_keep]
file.remove(files_to_remove)

# --------------- Save some summary statistics ---------------

mean_consumption_out <- joint_dt %>%
    filter(ever_dc_pen == "yes") %>%
    group_by(pre_post_ref) %>%
    summarise(across(
        c("bequest_consumption", "standard_consumption", "subjective_consumption", "total_monthly_consumption"),
        list(
            mean = \(x) mean(x, na.rm = TRUE),
            sd = \(x) sd(x, na.rm = TRUE),
            non_missing = \(x) sum(!is.na(x))
        )
    )) %>%
    data.table() %>%
    melt(
        id.vars = "pre_post_ref"
    ) %>%
    mutate(
        function_type = str_extract(variable, "mean|sd|non_missing"),
        data_type = str_extract(variable, ".+(?<=_cons)")
    ) %>%
    dcast(data_type ~ function_type + pre_post_ref) %>%
    mutate(data_type = str_remove(data_type, "_cons")) %>%
    mutate(data_type = str_to_title(str_replace(data_type, "_", " "))) %>%
    mutate(
        data_type =
            fifelse(data_type == "Total Monthly", "Total Monthly (ELSA)", data_type)
    )

function_names <- names(mean_consumption_out) %>%
    str_remove("_(control|treat)") %>%
    str_replace("_|(row_name)|(data_type)", " ") %>%
    str_to_title() %>%
    unique()

col_names <- names(mean_consumption_out) %>%
    str_extract("data_type|control|treat") %>%
    str_replace("data_type", "") %>%
    str_to_title()

header_to_add <- c(1, rep(2, length(function_names) - 1))

names(header_to_add) <- function_names

# TODO
consumption_out <- kbl(mean_consumption_out,
    col.names = col_names,
    booktabs = TRUE,
    format = "latex",
    caption = "Model consumption predictions \\label{tab:simulation_prediction}"
) %>%
    add_header_above(header_to_add)

writeLines(
    consumption_out,
    "../Texfiles/tables/simulation_summary_stats.tex"
)







# -------------- Run regression models on simulated data --------------
models <- list(
    dc_only = list(
        filter_expression = expression(ever_dc_pen == "yes"),
        rhs = "pre_post_ref_bin + retired_age + fin_wealth +
            ragender + dc_pot + years_since_retirement + owns_house + public_pension"
    ),
    all_data_interaction = list(
        filter_expression = expression(TRUE),
        rhs = "pre_post_ref_bin + pre_post_ref_bin*ever_dc_pen_bin + pre_post_ref_bin*ever_db_pen_bin + retired_age + fin_wealth +
ragender + dc_pot + years_since_retirement + owns_house + public_pension + ever_db_pen_bin"
    ),
    dc_only_pen_pot = list(
        filter_expression = expression(ever_dc_pen == "yes"),
        rhs = "pre_post_ref_bin + pre_post_ref_bin:dc_pot + retired_age + fin_wealth +
ragender + dc_pot + years_since_retirement + owns_house + public_pension"
    ),
    dc_only_fin_interact = list(
        filter_expression = expression(ever_dc_pen == "yes"),
        rhs = "pre_post_ref_bin + pre_post_ref_bin:fin_wealth + retired_age + fin_wealth +
ragender  + years_since_retirement + owns_house + public_pension"
    )
)

RunModelsFunction <- function(filter_and_model_list, lhs) {
    formula_to_use <- paste0(lhs, " ~ ", filter_and_model_list$rhs) %>%
        as.formula()

    output <- lm(
        formula_to_use,
        data = joint_dt[eval(filter_and_model_list$filter_expression)]
    )

    return(output)
}

col_smart_names <- c(
    "dc_only" = "DCOnly",
    "all_data_interaction" = "AllDataDCInteraction",
    "dc_only_pen_pot" = "DCOnlyPensionInt",
    "dc_only_fin_interact" = "DCOnyFinancialInt"
)




# --------- bequest motive models -----------

reg_variable <- "bequest_consumption"

bequest_outmodels <- lapply(
    models,
    RunModelsFunction,
    reg_variable
)

names(bequest_outmodels) <- unname(col_smart_names)

modelsummary(
    bequest_outmodels,
    output = "../Texfiles/tables/beq_life_cycle.tex",
    vcov = "robust",
    title = "Simulated bequest lifecycle models \\label{tab:BeqLifeCycle}",
    coef_rename = unclean_name_vector,
    statistic = NULL,
    align = align_string_ms(names(bequest_outmodels)),
    gof_map = bottom_stats
)


# ------ standard life cycle models -------------


reg_variable <- "standard_consumption"

standard_outmodels <- lapply(
    models,
    RunModelsFunction,
    reg_variable
)
names(standard_outmodels) <- unname(col_smart_names)

modelsummary(
    standard_outmodels,
    output = "../Texfiles/tables/standard_life_cycle.tex",
    vcov = "robust",
    title = "Simulated standard lifecycle models \\label{tab:StandardLifeCycle}",
    coef_rename = unclean_name_vector,
    statistic = NULL,
    align = align_string_ms(names(standard_outmodels)),
    gof_map = bottom_stats
)




# ------ subjective prob life cycle models -------------

reg_variable <- "subjective_consumption"

subjective_outmodels <- lapply(
    models,
    RunModelsFunction,
    reg_variable
)
names(subjective_outmodels) <- unname(col_smart_names)

modelsummary(
    subjective_outmodels,
    output = "../Texfiles/tables/subjective_life_cycle.tex",
    vcov = "robust",
    title = "Simulated subjective lifecycle models \\label{tab:SubjectiveLifeCycle}",
    coef_rename = unclean_name_vector,
    statistic = NULL,
    align = align_string_ms(names(subjective_outmodels)),
    gof_map = bottom_stats
)

# -------------- all models one table

all_models <- list(
    bequest_outmodels[!grepl("PensionIn", names(bequest_outmodels))],
    standard_outmodels[!grepl("PensionIn", names(standard_outmodels))],
    subjective_outmodels[!grepl("PensionIn", names(subjective_outmodels))]
) %>%
    unlist(recursive = F)

new_names <- names(all_models) %>%
    str_replace("AllDataDCInteraction", "DCInt") %>%
    str_replace("DCOnyFinancialInt", "FinInt")

names(all_models) <- new_names

# two layers of names
# top layer is model type
# bottom layer is

top_names <- c(" " = 1, "Bequest" = 3, "Standard" = 3, "Subjective" = 3)
tab_out <- modelsummary(
    all_models,
    output = "latex",
    booktabs = T,
    vcov = "robust",
    title = "Empirical models with simulated consumption data \\label{tab:SubjectiveLifeCycle}",
    coef_rename = unclean_name_vector,
    statistic = NULL,
    align = align_string_ms(names(all_models)),
    gof_map = bottom_stats,
    fmt = fmt_sprintf("%.2f"),
)

output <- "../Texfiles/tables/all_simulated_models.tex"

# customize table with `kableExtra`
tab_out_fin <- tab_out %>%
    # column labels
    add_header_above(top_names) %>%
    kable_styling(font_size = 10)

writeLines(
    tab_out_fin,
    output
)

ss_table <- kbl(all_modles,
    col.names = col_names,
    booktabs = TRUE,
    format = "latex",
    caption = "Empirical models with simulated consumption data \\label{tab:simulation} ",
    align = align_string(names(ss_stat_out))
) %>%
    add_header_above(top_names) %>%
    kable_styling(font_size = 10)

writeLines(
    ss_table,
    "../Texfiles/tables/r_summary_stats.tex"
)



#  ---------------------- random notes -----------

joint_dt %>%
    filter(pre_post_ref == "control") %>%
    mutate(diff = consump_beq_no_ann - consump_beq_ann) %>%
    mutate(diff_stan = consump_beq_no_ann - consump_stan_no_ann) %>%
    summarise(
        mean(diff_stan, na.rm = TRUE),
        mean(diff, na.rm = TRUE),
        mean(rich_enough == "yes" & ever_dc_pen == "no", na.rm = TRUE)
    )
# 20% of retirees do not have enough wealth

# what impact do other variables have on the difference in consumption

joint_dt %>%
    filter(!is.na(consump_beq_no_ann)) %>%
    mutate(is_equal = unlist(mapply(all.equal, consump_beq_ann, consump_beq_no_ann))) %>%
    summarise(mean(is_equal == TRUE))


lm(
    bequest_consumption ~ pre_post_ref_bin + retired_age + fin_wealth +
        ragender + dc_pot + years_since_retirement + owns_house + public_pension,
    data = joint_dt[ever_dc_pen == "yes"]
)

# wow shows up as an increase


lm(
    bequest_consumption ~ pre_post_ref_bin + pre_post_ref_bin * ever_dc_pen_bin + pre_post_ref_bin * ever_db_pen_bin + retired_age + fin_wealth +
        ragender + dc_pot + years_since_retirement + owns_house + public_pension + ever_db_pen_bin,
    data = joint_dt
)


lm(
    bequest_consumption ~ pre_post_ref_bin + pre_post_ref_bin:dc_pot + retired_age + fin_wealth +
        ragender + dc_pot + years_since_retirement + owns_house + public_pension,
    data = joint_dt[ever_dc_pen == "yes"]
)

lm(
    bequest_consumption ~ pre_post_ref_bin + pre_post_ref_bin:fin_wealth + retired_age + fin_wealth +
        ragender + years_since_retirement + owns_house + public_pension,
    data = joint_dt[ever_dc_pen == "yes"]
)



# write code to output these models

# quick check on number of ids we have done subjective ones for

json_names <- dir("../../data/ELSA/lifecycle_outputs/id_wave")

ids_done <- str_extract(json_names, "\\d{6}-\\d{1}")
head(ids_done)
mean(joint_dt$id_wave %in% ids_done)
# nice this has gone up a tiny bit now.

ids_not_done <- joint_dt$id_wave[!joint_dt$id_wave %in% ids_done]

ids_that_we_can_do <- dir("../../data/ELSA/subjective_tables/jsons/") %>%
    str_extract("\\d{6}-\\d{1}")


length(intersect(ids_not_done, ids_that_we_can_do))


all_sub_jsons <- dir("../../data/ELSA/subjective_tables/jsons/")

files_to_copy <- paste0(
    "../../data/ELSA/subjective_tables/jsons/",
    ids_not_done,
    ".json"
)

new_names <- paste0("../../../../temp_data_dir/", ids_not_done, ".json")

file.copy(files_to_copy, new_names, overwrite = TRUE)

length(files_to_copy) == length(new_names)
