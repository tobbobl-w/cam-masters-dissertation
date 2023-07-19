
include("0__functions.jl")

ann_lf_df = CSV.File("../data/CMI/raw/cmi_annuitant_lifetables.csv") |>
            DataFrame

# get vectors for all diff years and genders
ann_lf_male_08 = ann_lf_df[(ann_lf_df.gender.=="male").&(ann_lf_df.year.==2008), :][!, :prob]
ann_lf_female_08 = ann_lf_df[(ann_lf_df.gender.=="female").&(ann_lf_df.year.==2008), :][!, :prob]
ann_lf_male_16 = ann_lf_df[(ann_lf_df.gender.=="male").&(ann_lf_df.year.==2016), :][!, :prob]
ann_lf_female_16 = ann_lf_df[(ann_lf_df.gender.=="female").&(ann_lf_df.year.==2016), :][!, :prob]


function annuity_payment_function_using_annuitant_mortality(
    annuity_cost;
    loading_factor=0.95,
    age=60,
    gender,
    ann_life_table_year)

    # if older than 60 the annuity should be priced according to mortality from that point
    start_index = age - 60 + 1

    if ann_life_table_year == 2008 && gender == "male"
        death_probs = ann_lf_male_08
    elseif ann_life_table_year == 2008 && gender == "female"
        death_probs = ann_lf_female_08
    elseif ann_life_table_year == 2016 && gender == "male"
        death_probs = ann_lf_male_16
    elseif ann_life_table_year == 2016 && gender == "female"
        death_probs = ann_lf_female_16
    end

    # now want p we reach ages given we are age
    alive_probs = 1 .- death_probs[start_index:end]

    # Prob of reaching 61 is just top
    # prob of reaching 62 is top two multiplied together
    # So we take cumulative product of alive probability
    cum_alive = cumprod(alive_probs)

    loading_factor * (annuity_cost / (sum([cum_alive[age] * 1 / (((1 + r)^age)) for age in eachindex(cum_alive)])))
end
# cool now we can use this function.. 

annuity_payment_function(100000, gender="male", age=61, ann_life_table_year=2016)
## males get more which is right # is this roughly inline for a nominal non esclating pension

annuity_payment_function(100000, gender="male", age=65, ann_life_table_year=2016)
# also annuities increase with age which is good



function Annuity_GeneralDeathProbs(annuity_cost, loading_factor, death_probs)
    # annuity_cost - how much to spend on an annuity
    # loading_factor - discount the annuity
    # death_probs - vector of death probabilities for each age

    alive_probs = 1 .- death_probs

    cum_alive = cumprod(alive_probs)

    loading_factor * (annuity_cost / (sum([cum_alive[age] * 1 / (((1 + r)^age)) for age in eachindex(cum_alive)])))

end
