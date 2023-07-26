
# so we want to track consumption in early retirement for individuals in ELSA given 
# take retirement year info

include("0__functions.jl")
include("1__lifecycle_with_income_function.jl")

elsa_reg = GetELSAData()

sum(elsa_reg.fin_wealth .> 500000)
sum(elsa_reg.fin_wealth .< 0)

sum(elsa_reg.fin_wealth .> 500000 .&& elsa_reg.ever_dc_pen .== "yes")

# for each retirement year, retirement age, gender, 
# get the optimal policy function 
# and then we can use the asset path function 


println(names(elsa_reg))

elsa_filtered = elsa_reg[elsa_reg.fin_wealth .<= 500000 .&&
 elsa_reg.fin_wealth .>= 0 .&& elsa_reg.retired_age .>= 55, :]


names_vec = vec(["id_wave", 
"annuity_consump",
"no_annuity_consump",
"annuity_cost",
"annuity_payment"])



# ------------------ Life cycle plots ------------------
# ret_year = 2012
# int_year = 2014
# fin_wealth = 120,000 (mean) => 120 grid 
# pension = 6,000 => 12
# id_wave 107132-7
# gender = "female"
# ret_age = 65
# birth_year = 1947

# No bequests, objective life probs
stan_pol_func = RetrievePolicyFunction(
    false, "objective", 1947, "female",  "107132-7"
)

beq_pol_func = RetrievePolicyFunction(
    true, "objective", 1947, "female",  "107132-7"
)

sub_pol_func = RetrievePolicyFunction(
    false, "subjective", 1947, "female",  "107132-7"
)

# the other policy functions are too long so we trim them down so that it starts at 
# retirement

# maybe also trim them a few years
beq_pol_func = beq_pol_func[:, :, 10:end]
stan_pol_func = stan_pol_func[:, :, 10:end]

# identical in every other respect
using Plots

plot([AssetPathFunction(stan_pol_func, 120, 12).cons AssetPathFunction(beq_pol_func, 120, 12).cons AssetPathFunction(sub_pol_func, 120, 12).cons]) 


Annuity_GeneralDeathProbs(
    60000,
    0.90, 
    GetObjectiveDeathProbs(gender = "female", year = 2012, age = 65)    
)
# So move down 60 asset grids
# and move up 7 income grids

# ---- Consumption Plot ----------
consump_paths = hcat(
    AssetPathFunction(stan_pol_func, 120, 12).cons, 
    AssetPathFunction(stan_pol_func, 60, 19).cons,
    AssetPathFunction(beq_pol_func, 120, 12).cons,
    AssetPathFunction(beq_pol_func, 60, 19).cons, 
    AssetPathFunction(sub_pol_func, 120, 12).cons, 
    AssetPathFunction(sub_pol_func, 60, 19).cons
)
lables = ["Standard" "Standard with annuity" "Bequest" "Bequest with annuity" "Subjective" "Subjective with annuity"]

all_ages = [64 + i for i in 1:size(sub_pol_func, 3)]

pre100_consump_paths = consump_paths[all_ages .<= 100, :]
pre100_ages = all_ages[all_ages .<= 100] 

cons_plot = plot(pre100_ages, 
pre100_consump_paths[:, [1,3, 5]], 
label = lables[:, [1,3, 5]], 
ls = :solid, title = "Consumption Path of Median Retiree")

xlabel!("Age")
ylabel!("Consumption")

plot!(pre100_ages, pre100_consump_paths[:, [2,4, 6]], 
label = lables[:, [2,4,6]], 
ls = :dash)

savefig(cons_plot, 
"Texfiles/figures/consumption_plot_median_retiree.pdf")

# asset path of median retiree
# ----------- Asset plot -----------

asset_paths = hcat(
    AssetPathFunction(stan_pol_func, 120, 12).assets, 
    AssetPathFunction(stan_pol_func, 60, 19).assets,
    AssetPathFunction(beq_pol_func, 120, 12).assets,
    AssetPathFunction(beq_pol_func, 60, 19).assets, 
    AssetPathFunction(sub_pol_func, 120, 12).assets, 
    AssetPathFunction(sub_pol_func, 60, 19).assets
)
lables = ["Standard" "Standard with annuity" "Bequest" "Bequest with annuity" "Subjective" "Subjective with annuity"]

all_ages = [64 + i for i in 1:(size(sub_pol_func, 3) + 1)]

pre100_asset_paths = asset_paths[all_ages .<= 100, :]
pre100_ages = all_ages[all_ages .<= 100] 

asset_plot = plot(pre100_ages, 
pre100_asset_paths[:, [1,3, 5]], 
label = lables[:, [1,3, 5]], 
ls = :solid, title = "Asset Path of Median Retiree")

xlabel!("Age")
ylabel!("Assets (1000s)")

plot!(pre100_ages, pre100_asset_paths[:, [2,4, 6]], 
label = lables[:, [2,4,6]], 
ls = :dash)

savefig(asset_plot, 
"Texfiles/figures/asset_plot_median_retiree.pdf")





# ---------- First bequests, objective life expectancy -------------
# --------- Bequest models ---------------

objective_beq_consumptions = []

for row_index in 1:nrow(elsa_filtered)

    row_to_get = elsa_filtered[row_index, :]

    println((ret_age = row_to_get["retired_age"],
     ret_year= row_to_get["retirement_year"], 
     int_year = row_to_get["year"]))

    outputs = ReturnConsumptionWithAnnuity(
        true, 
    "objective", 
       
    
        row_to_get["gender"],
        row_to_get["id_wave"],
        row_to_get["retired_age"],
        row_to_get["retirement_year"],
        row_to_get["year"],
        row_to_get["ass_grid"],
        row_to_get["pen_grid"]
    )
    append!(objective_beq_consumptions, outputs)
end

bequest_motive_data = reshape(objective_beq_consumptions, 5, Int(7240/5)) |>
permutedims |>
Tables.table |>
DataFrame

rename!(bequest_motive_data, Symbol.(names_vec))

CSV.write("../data/ELSA/lifecycle_outputs/bequest_consumption_simulation.csv",
bequest_motive_data)



# ------------ no bequest, objective life expecancy -------------

objective_consumptions = []

for row_index in 1:nrow(elsa_filtered)

    row_to_get = elsa_filtered[row_index, :]

    println((ret_age = row_to_get["retired_age"],
     ret_year= row_to_get["retirement_year"], 
     int_year = row_to_get["year"]))

    outputs = ReturnConsumptionWithAnnuity(
        false, 
        "objective", 
        row_to_get["gender"],
        row_to_get["id_wave"],
        row_to_get["retired_age"],
        row_to_get["retirement_year"],
        row_to_get["year"],
        row_to_get["ass_grid"],
        row_to_get["pen_grid"]
    )
    append!(objective_consumptions, outputs)
end

standard_theory_data = reshape(
    objective_consumptions, 5, Int(7240/5)) |>
permutedims |>
Tables.table |>
DataFrame

rename!(standard_theory_data, Symbol.(names_vec))

CSV.write(
    "../data/ELSA/lifecycle_outputs/standard_model_consumption_simulation.csv",
    standard_theory_data)




#  --------------- subjetive life expectancies ---------------


# check which ids we have run and only ask to get those

ids_in_folder =  elsa_filtered.id_wave[CheckInFolder.(elsa_filtered.id_wave)]
# must have been running the wrong ids
# hopefully will speed up when I have fewer things running

subjective_consumption = []

for wave_id in ids_in_folder

    row_to_get = elsa_filtered[elsa_filtered.id_wave .== wave_id, :][1, :]

    println((ret_age = row_to_get["retired_age"],
     ret_year= row_to_get["retirement_year"], 
     int_year = row_to_get["year"]))

    outputs = ReturnConsumptionWithAnnuity(
        false, 
        "subjective", 
        row_to_get["gender"],
        row_to_get["id_wave"],
        row_to_get["retired_age"],
        row_to_get["retirement_year"],
        row_to_get["year"],
        row_to_get["ass_grid"],
        row_to_get["pen_grid"]
    )
    append!(subjective_consumption, outputs)
end


subjective__theory_data = reshape(
    subjective_consumption, 5, Int(length(subjective_consumption)/5)) |>
    permutedims |>
    Tables.table |>
    DataFrame

rename!(subjective__theory_data, Symbol.(names_vec))

CSV.write(
    "../data/ELSA/lifecycle_outputs/subjective_model_consumption_simulation.csv",
    subjective__theory_data)



# ------------ Also get retirement asset plot ------------
# for each person trace whole consumption and asset path
# and then plot those




# RunLoop()
# pwd()

# elsa_reg[elsa_reg.id_wave.=="116751-5", :]

# for row_index in 1:nrow(elsa_reg)
#     print(row_index)
# end

# inc_grid_gap

# histogram(elsa_reg.ass_grid)

# asset_grid_points
# inc_grid_points


# ok sometimes we are going to be really far off 
# should probably have specified a finer income grid
# what setting a slightly higher moneys worth and always rounding down