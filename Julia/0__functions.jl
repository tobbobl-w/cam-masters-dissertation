using CSV
using LinearAlgebra
using DataFrames
using Plots
using JSON3


# I will not change these so they can be in functions
# Set parameters
const β = 0.97
const r = 1 / β - 1
const σ = 2.5
const terminal_age = 110


function GetObjDeathProbMatrices()
    # this function returns a function that gives death probs

    # First get some meta info
    first_two_lines = CSV.File("../data/ONS/males_transition_probs.csv", limit=2, header=false) |>
                      Tables.matrix

    header_info = first_two_lines[1, :]
    # first column is age 
    # next columns are year of birth

    # The second element of first column is the min age
    min_age = parse(Int, first_two_lines[2, 1])

    # Max year
    max_year = Int(header_info[2])


    # Objective transition probs from ons
    male_lftb = CSV.File("../data/ONS/males_transition_probs.csv") |>
                Tables.matrix

    female_lftb = CSV.File("../data/ONS/females_transition_probs.csv") |>
                  Tables.matrix
    # Each colum is a year of birth
    # each row is an age 
    return (male=male_lftb, female=female_lftb,
        top_year=max_year, min_age=min_age)
end

death_prob_matrices = GetObjDeathProbMatrices()

# ONS life tables
function GetObjectiveDeathProbs(; gender, year, age)
    if gender == "male"
        data = death_prob_matrices.male
    elseif gender == "female"
        data = death_prob_matrices.female
    end

    birth_year = year - age

    if birth_year < death_prob_matrices.top_year | age < death_prob_matrices.min_age
        println("Person is too young, we dont have death prob")
    end

    # plus 2 because first column is ages, and plus one because of indexing
    col_index = 2 + death_prob_matrices.top_year - birth_year # selects the correct column
    # second column of data is birth year 1975, so if if birth_year = 1975 we will select it

    start_row_index = 1 + age - death_prob_matrices.min_age # first row of interest

    death_probs = data[start_row_index:end, col_index]

    return death_probs
end



# --------- Read in subjective life expectancies ---------------
# Have to set column types
death_column_types = Dict(:id_wave => String, :age => Int128, :death_prob => Float64)

subjective_death_df = CSV.File("../data/ELSA/subjective_tables/subjective_death_probs_for_julia.csv",
    types=death_column_types) |>
                      DataFrame

# Function takes an id-wave and returns the subjective death probs
GetSubjectiveDeathProbs = function (df = subjective_death_df; id_wave_to_get = id_wave_to_get)
    df[df.id_wave.==id_wave_to_get, :][!, :death_prob]
end


# utility function
function utility(c)
    if c > 0.0001
        (c^(1 - σ)) / (1 - σ)
    else
        -Inf
    end
end



# bequest function
# from lockwood RED 2012
bequest_lock(b; c0=5000, m=0.96) = (m / (1 - m))^σ * (((m / (1 - m)) * c0) + b)^(1 - σ) / (1 - σ)
# but won't including bequests make utility lower



# asset accumulation
# budget constraint
bc_inc(a, a′, income) = a * (1 + r) - a′ + income


CreateConsumptionArray = function (asset_grid=asset_grid, income_grid=income_grid)
    consumption_array = ones(Float64, length(asset_grid), length(asset_grid), length(income_grid))
    # Initialize a matrix to store consumption values

    # Nested loop to calculate consumption values for each combination of assets
    for z in eachindex(income_grid)
        income = income_grid[z]
        for x in eachindex(asset_grid)
            cur_assets = asset_grid[x]
            for y in eachindex(asset_grid)
                next_assets = asset_grid[y]
                consumption_array[x, y, z] = bc_inc(cur_assets, next_assets, income)
            end
        end
    end
    return consumption_array

end


CreateUtilityArray = function (asset_grid=asset_grid, income_grid=income_grid)

    utility_array = ones(Float64, length(asset_grid), length(asset_grid), length(income_grid))
    # Initialize a matrix to store utility values

    # Loop to calculate utility values for each consumption value in the consumption matrix
    for i in eachindex(utility_array)
        utility_array[i] = utility(consumption_array[i])
    end

    return utility_array
end


# ----------------- Post model run functions -----------------

RetrievePolicyFunction = function (
    bequests=false,
    life_prob_types="objective",
    age=65,
    year=2014,
    gender="male")

    json_filename = string("../data/ELSA/lifecycle_outputs/", age, "_beq", bequests, "_", gender, "_", life_prob_types, "_", year, ".json")

    # check file exists
    isfile(json_filename)
    # Read in json
    json_in = JSON3.read(json_filename)

    # Multi dimensional arrays were saved as long vectors so we reshape them  
    pol_func_readin = reshape(json_in["pol_func"], inc_grid_points, asset_grid_points, terminal_age - age)

    return pol_func_readin
end


function AssetPathFunction(opt_policy_function, asset_start_point, income_grid_point)

    # Return asset paths given a set of optimal policy functions
    # and a starting value of wealth

    number_years = size(opt_policy_function)[2]
    println(number_years)
    # Intialise
    asset_path = Vector{Int}(undef, number_years + 1)

    # set intial assets
    asset_path[1] = asset_start_point
    
    # Get optimal asset paths from optimal policy matrix
    for age in 1:(number_years)
        asset_path[age+1] = opt_policy_function[:, age][asset_path[age]]
    end

    # Create consumption path
    consumption_path = zeros(number_years)
    for age in eachindex(consumption_path)
        consumption_path[age] = bc_inc(asset_grid[asset_path[age]], asset_grid[asset_path[age+1]], income_grid[income_grid_point])
    end

    return (assets=asset_path, cons=consumption_path)

end


RetrieveValueFunction = function (
    bequests=false,
    life_prob_types="objective",
    age=65,
    year=2014,
    gender="male")

    json_filename = string("../data/ELSA/lifecycle_outputs/",
        age, "_beq", bequests, "_", gender, "_",
        life_prob_types, "_", year, ".json")

    # check file exists
    isfile(json_filename)
    # Read in json
    json_in = JSON3.read(json_filename)

    # Multi dimensional arrays were saved as long vectors so we reshape them  
    value_func = reshape(json_in["value_func"], inc_grid_points, asset_grid_points, terminal_age - age)

    return value_func
end


# ---------- Annuity code ------------
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

# annuity_payment_function(100000, gender="male", age=61, ann_life_table_year=2016)
## males get more which is right # is this roughly inline for a nominal non esclating pension

# annuity_payment_function(100000, gender="male", age=65, ann_life_table_year=2016)
# also annuities increase with age which is good


function Annuity_GeneralDeathProbs(annuity_cost, loading_factor, death_probs)
    # annuity_cost - how much to spend on an annuity
    # loading_factor - discount the annuity
    # death_probs - vector of death probabilities for each age

    alive_probs = 1 .- death_probs

    cum_alive = cumprod(alive_probs)

    loading_factor * (annuity_cost / (sum([cum_alive[age] * 1 / (((1 + r)^age)) for age in eachindex(cum_alive)])))

end

function FindClosestGridPoint(x, grid_gap, num_grid_point)
    # assume grid starts from 0
    # we want to return the index of the grid that someone is at
    grid_point = round(Int, x / grid_gap)



    # Assume for now that no one actually is at 0
    if grid_point > 0
        return grid_point
        
        # Also stop individuals considering points outside of the grid
    elseif grid_point > num_grid_point
        num_grid_point
    else 
        return 1
    end
end

