### In this script we use the functions developed in 0 and 1

include("0__functions.jl")
include("1__lifecycle_with_income_function.jl")


# # This is the combo of demographic info that I solve the model for 
# gender_age_year_combos = CSV.File("../data/ELSA/elsa_to_use/gender_age_year_unique.csv") |>
#                          Tables.matrix


# # This will take a while I think
# # Depends on grid size 
# for row in eachrow(gender_age_year_combos)
#     println(row)
#     RetirementLifecycleWithIncome(
#         bequests=false,
#         age=row[2],
#         gender=row[1],
#         life_prob_types="objective",
#         year=row[3]
#     )
# end



# ----------------- SUBJECTIVE --------------------

# First read in the idwaves that we have flagged for running
col_types = Dict(:id_wave => String, :age => Int128, :year => Int128,
    :ever_dc_pen => String, :ever_db_pen => String, :public_pension => Float64)

idwave_df = CSV.File("../data/ELSA/elsa_to_use/for_julia.csv", types=col_types) |>
            DataFrame


# Subjective death comes from "0__functions.jl"
ids_we_have_probs = unique(subjective_death_df[!, :id_wave])
# So for each one of these we want to run the lifecycle solver if we have a id-wave for them

# these are ids that we will use in the regression
ids_we_need_lifecycle = idwave_df[:, :id_wave]

# we only want to run ids we care about and that we have subjective life probs for
ids_to_run = intersect(ids_we_need_lifecycle, ids_we_have_probs)


# Now run all 
for id_wave in ids_to_run

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
# this is too long, I think we need fewer grid points in the states

# GetSubjectiveDeathProbs(id_wave_to_get="100021-5")



# Nice this is working now!!
# different life expectancies & different years and genders (these are kinda the same)

# get a unique df with all the types we will need


# ################################# TO DO #################################
# # 

# # then take individuals from elsa and estimate what consumption should be
# # under the different model types i.e. conditional on assets, age, object live probs and 
# # then both bequests and subjective life probs

# # then we can run regressions on the impact of forced annuitisation

# # there is annuity price data vs income drawdown on fca website which is cool 


# using Statistics
