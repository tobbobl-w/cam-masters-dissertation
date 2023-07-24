### In this script we use the functions developed in 0 and 1
using JSON3

include("0__functions.jl")
include("1__lifecycle_with_income_function.jl")

# ----------------- SUBJECTIVE --------------------

# First read in the idwaves that we have flagged for running
col_types = Dict(:id_wave => String, :age => Int128, :year => Int128)

idwave_df = CSV.File("../data/ELSA/elsa_to_use/for_julia.csv", types=col_types) |>
            DataFrame



# Subjective death comes from "0__functions.jl"
ids_we_have_probs = unique(subjective_death_df[!, :id_wave])
# So for each one of these we want to run the lifecycle solver if we have a id-wave for them

# these are ids that we will use in the regression
ids_we_need_lifecycle = idwave_df[:, :id_wave]

# we only want to run ids we care about and that we have subjective life probs for
ids_to_run = intersect(
    ids_we_need_lifecycle,
    ids_we_have_probs)


# Now run all 
RunIdsFunction = function (group_of_ids)
    for id_wave in group_of_ids

        id_wave_row = idwave_df[idwave_df.id_wave.==id_wave, :]

        age, gender, year = id_wave_row[!, :age][1], id_wave_row[!, :gender][1], id_wave_row[!, :year][1]

        println(id_wave)

        RetirementLifecycleWithIncome(
            bequests=false,
            age=age,
            gender=gender,
            life_prob_types="subjective",
            year=year,
            id_wave=id_wave
        )
    end
end


rev_ids_to_run = reverse(ids_to_run)

RunIdsFunction(rev_ids_to_run)