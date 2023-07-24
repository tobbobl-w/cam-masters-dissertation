
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


# ---------- First no bequests, objective life expectancy -------------
# --------- Bequest models ---------------

objective_consumptions = []

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
    append!(objective_consumptions, outputs)
end

objective_consumptions

bequest_motive_data = reshape(objective_consumptions, 5, Int(7240/5)) |>
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




#  ---------------subjetive life expectancies ---------------


# check which ids we have run and only ask to get those



CheckInFolder = function(id_wave) 
    folder_name = "../Data/ELSA/lifecycle_outputs/id_wave/"
    regex_search = Regex(id_wave)

    files_in_folder = readdir(folder_name)
    vector_of_matches = .!(isnothing.(match.(regex_search, files_in_folder)))

    if sum(vector_of_matches) == 1
        return true
    else
        return false
    end 
end 

ids_in_folder =  elsa_filtered.id_wave[CheckInFolder.(elsa_filtered.id_wave)]
# must have been running the wrong ids
# hopefully will speed up when I have fewer things running

objective_consumptions = []

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
    append!(objective_consumptions, outputs)
end



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
    birth_year = ret_year - ret_age
    
    ret_age = row_to_get["retired_age"]

    pol_func = RetrievePolicyFunction(
        false, # bequests
        "subjective", # life exp type
        0,
        "male",
        "112968-8")




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