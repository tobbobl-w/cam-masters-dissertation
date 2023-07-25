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

GetJson = function (
    bequests=false,
    life_prob_types="objective",
    birth_year=1950,
    gender="male",
    id_wave="")

    if life_prob_types == "objective"

        folder_name = "../Data/ELSA/lifecycle_outputs/beq_testing/"

        json_string = string("birth_year_", birth_year, "_", bequests, "_", gender, "_objective_20000.json")
        full_file_name = folder_name * json_string

    elseif life_prob_types == "subjective"

        folder_name = "../Data/ELSA/lifecycle_outputs/id_wave/"
        regex_search = Regex(id_wave)

        files_in_folder = readdir(folder_name)
        vector_of_matches = .!(isnothing.(match.(regex_search, files_in_folder)))

        # there should be a unique match
        if sum(vector_of_matches) == 0

            error("Not a match for this json string")
        end

        json_string = files_in_folder[vector_of_matches][1]
        full_file_name = folder_name * json_string
    end

    if isfile(full_file_name)
        json_in = JSON3.read(full_file_name)
        return json_in
    else
        error_message = string(
            "cant find file: ", full_file_name
        )
        error(error_message)
    end
end

RetrievePolicyFunction = function (
    bequests=false,
    life_prob_types="objective",
    birth_year=1950,
    gender="male",
    id_wave="")

    # Read in json
    json_in = GetJson(
        bequests,
        life_prob_types,
        birth_year,
        gender,
        id_wave
    )

    # asset and income dimensions always the same. 
    year_dimension = Int(length(json_in["pol_func"]) / (inc_grid_points * asset_grid_points))

    # Multi dimensional arrays were saved as long vectors so we reshape them
    # this is an array of dimension income states, asset states and age states
    pol_func_readin = reshape(
        json_in["pol_func"],
        inc_grid_points,
        asset_grid_points,
        year_dimension)

    return pol_func_readin
end


AssetPathFunction = function (
    opt_policy_function,
    asset_start_point,
    income_grid_point)

    # Return asset paths given a set of optimal policy functions
    # Asset path tracks the  
    # and a starting value of wealth

    opt_policy_function_assets = opt_policy_function[income_grid_point, :, :]

    number_years = size(opt_policy_function)[3]

    # Intialise
    asset_path = Vector{Int}(undef, number_years + 1)
    # set intial assets
    asset_path[1] = asset_start_point

    # Get optimal asset paths from optimal policy matrix
    for age in 1:(number_years)
        asset_path[age+1] = opt_policy_function_assets[:, age][asset_path[age]]
    end


    # Create consumption path
    consumption_path = zeros(number_years)
    for age in eachindex(consumption_path)
        # If asset grid is 0 then set assets next period to 0
        # I did not allow individuals to have 0 assets on the grid.  
        if asset_path[age+1] == 0
            consumption_path[age] = bc_inc(
                asset_grid[asset_path[age]],
                0,
                income_grid[income_grid_point])
        else
            consumption_path[age] = bc_inc(
                asset_grid[asset_path[age]],
                asset_grid[asset_path[age+1]],
                income_grid[income_grid_point])
        end
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

function GetELSAData()
    # first get data
    col_types = Dict(:id_wave => String, :age => Int128, :year => Int128,
        :ever_dc_pen => String, :ever_db_pen => String, :public_pension => Float64,
        :fin_wealth => Float64)

    idwave_df = CSV.File("../data/ELSA/elsa_to_use/for_julia.csv", types=col_types) |>
                DataFrame

    idwave_df = idwave_df[Bool.(.!ismissing.(idwave_df.fin_wealth)).&Bool.(.!ismissing.(idwave_df.public_pension)), :]

    idwave_df.pen_grid = FindClosestGridPoint.(
        idwave_df.public_pension, inc_grid_gap, inc_grid_points)

    idwave_df.ass_grid = FindClosestGridPoint.(
        idwave_df.fin_wealth, asset_grid_gap, asset_grid_points)

    return idwave_df
end





OptimalAnnuityAmount_beq = function (
    income_start_point, asset_start_point, loading_factor=0.85;
    value_func, gender="male", age=65, year=2015)

    # comparing different starting values on the income-asset array
    # and the loading factor gives the cost of the tradeoff. I.e. how far we move on each one
    # value func should just be a matrix 
    value_of_start = value_func[income_start_point, asset_start_point]

    death_probs = GetObjectiveDeathProbs(age=age, year=year, gender=gender)

    # make a rough matrix of grid points to move on assets vs income
    annuity_income_vec = [Annuity_GeneralDeathProbs(assets, loading_factor, death_probs) for assets in asset_grid]

    # I am not going to allow very small annuity purchaes since basically no one does this anyway
    # get rid of assets below 10000
    # This is how many grid points of income each annuity is associated with
    annuity_income_grid_points = FindClosestGridPoint.(annuity_income_vec, inc_grid_gap, inc_grid_points)

    # number_asset_grid_points by 2 matrix. 1st row is asset point
    assets_annuity_movement_mat = [Int.(asset_grid / asset_grid_gap) annuity_income_grid_points]

    how_close_can_annuity_amount_be = 70
    # only allowed to pick a point that is within x of a grid point otherwise can get better than actruarily fair
    select_vec = abs.(FindClosestGridPoint.(annuity_income_vec, inc_grid_gap, inc_grid_points) * inc_grid_gap - annuity_income_vec) .< how_close_can_annuity_amount_be

    # we want to evalute the value of these points and pick the max
    # the first column is starting point on the asset grid
    # second column is starting point on the income grid
    assets_annuity_movement_mat = assets_annuity_movement_mat[select_vec, :]

    value_of_points = []

    for row_index in 1:sum(select_vec)

        # Move up the income grid
        income_point = income_start_point + assets_annuity_movement_mat[row_index, 2]

        # Move down the asset grid
        asset_point = asset_start_point - assets_annuity_movement_mat[row_index, 1]

        # cannot have negative assets
        if asset_point < 1
            continue
        end

        if income_point > inc_grid_points
            # stop individuals going outside of the grid
            income_point = inc_grid_points
        end
        if asset_point < 0
            # do not allow negative asset holdings. 
            append!(value_of_points, -Inf)
        else
            append!(value_of_points, value_func[income_point, asset_point])
        end
    end

    if value_of_points == []
        return "too poor to annuitise"
    end

    max_value_and_index = findmax(value_of_points)

    if max_value_and_index[1] > value_of_start
        # return the optimal amount to spend on annuities and the
        to_return = (
            value=max_value_and_index[1],
            move_down_assets=assets_annuity_movement_mat[max_value_and_index[2], 1],
            move_up_income=assets_annuity_movement_mat[max_value_and_index[2], 2])

        return to_return
    elseif max_value_and_index[1] < value_of_start
        return ("do not annuitise", value_of_start)
    end

end




# Ok now we want to check optimal annuity rates in our real life data. 
# Need a function that takes year, age, wealth and income 
# and calculates optimal annuity amount, and consumption 
ToAnnuitiseOrNot = function (year, age, gender, state_pension, wealth,
    loading_factor, id_wave="none")

    if id_wave == "none"
        birth_year = year - age
        json_in = GetJson(birth_year, gender)
        to_add = 1
    elseif id_wave != "none"
        json_in = GetJSON_IDWave(id_wave)
        to_add = 0
    end

    value_func = json_in["value_func"]

    years_of_life_left = Int(length(value_func) / (inc_grid_points * asset_grid_points))

    value_func = reshape(value_func, inc_grid_points, asset_grid_points, years_of_life_left)

    # need the to add because in the early subjective models I just had 
    # (terminal_age - years_of_life) dimensions, rather than (1 + terminal_age - years_of_life)  
    first_age = to_add + terminal_age - years_of_life_left

    # Now we select the year of the value function where they have to make an annuitisation decision
    decision_year_index = 1 + age - first_age
    println(decision_year_index)

    # This is the value matrix in the year we care about
    # Now we see if moving along the income dimension makes up for going down the asset dimension
    value_func_at_year = value_func[:, :, decision_year_index]

    if size(value_func_at_year) != (inc_grid_points, asset_grid_points)
        println("Wrong dimensions: stopping")
        return "Wrong dimensions: stopping"
    end


    opt_annuity = OptimalAnnuityAmount_beq(
        state_pension, wealth, loading_factor, value_func=value_func_at_year,
        gender=gender, age=age, year=year)

    return opt_annuity
end




ReturnConsumptionWithAnnuity = function (
    bequests=true,
    life_prob_types="objective",
    gender="male",
    id_wave="",
    ret_age=62,
    ret_year=2012,
    int_year=2013,
    asset_start_point=250,
    income_start_point=50;
    prop_of_wealth=0.5)

    # id_wave is identification
    # gender is gender
    # ret_age and ret_year are retirement age and retirement year respectively
    # int_year is interview year
    # asset_start_point and income_start_point are assets and income start points in the grid respectively

    # prop_of_wealth controls the proportion of financial wealth that 
    #       individuals are foced to annutise

    birth_year = ret_year - ret_age

    pol_func = RetrievePolicyFunction(
        bequests, # bequests
        life_prob_types, # life exp type
        birth_year,
        gender,
        id_wave)


    # for subjective models because there are no bequests the policy function 
    # only has years_of_retirement - 1 since we do not need to know decision in last period

    years_solved_for = size(pol_func)[3]
    years_of_retirment_for_individual = terminal_age - ret_age

    if life_prob_types == "subjective"
        years_of_retirment_for_individual = years_of_retirment_for_individual - 1
    end

    start_index = 1 + years_solved_for - years_of_retirment_for_individual

    pol_func_at_start_of_retirement = pol_func[:, :, start_index:end]


    con_path = AssetPathFunction(
        pol_func_at_start_of_retirement,
        asset_start_point,
        income_start_point)

    # For obejective: we want consumption from interview year rather than retirement year
    # For subjective: want consumption at time the death probs were elicited. 

    if life_prob_types == "subjective"
        year_of_int_index = 1 + int_year - ret_year
    elseif life_prob_types == "objective"
        year_of_int_index = 1
    end

    consumption_no_ann = con_path.cons[year_of_int_index]

    # now annuitise part of wealth 
    # half and a quarter
    # then cal the increase in come, round down 

    death_probs = GetObjectiveDeathProbs(
        gender=gender,
        year=ret_year,
        age=ret_age
    )


    annuity_cost_grid = floor(Int, asset_start_point * prop_of_wealth)

    annuity_payment = Annuity_GeneralDeathProbs(
        annuity_cost_grid * asset_grid_gap,
        0.9,
        death_probs
    )

    # income grid points to move up
    # if 0 then we assume the individual is not wealthy enough to  
    #  purchase an annuity and we just return normal consumption
    annuity_payment_grid = floor(Int, annuity_payment / inc_grid_gap)

    if annuity_payment_grid == 0
        with_annuity_cons = consumption_no_ann
    else
        # asset path with annuity 
        with_annuity = AssetPathFunction(
            pol_func,
            asset_start_point - annuity_cost_grid,
            income_start_point + annuity_payment_grid)

        with_annuity_cons = with_annuity.cons[year_of_int_index]
    end

    # return id-wave, consumption with annuity, consumption without annuity, annuity cost, annuity_payment
    data_to_return = (
        id_wave=id_wave,
        annuity_consump=with_annuity_cons,
        no_annuity_consump=consumption_no_ann,
        annuity_cost=annuity_cost_grid * asset_grid_gap,
        annuity_payment=annuity_payment_grid * inc_grid_gap)

    return data_to_return
end




# ---------- testing --------------

# RetrievePolicyFunction(
#     true, # bequests
#     "objective", # life exp type
#     1942,
#     "female")
#     years_solved_for = 47
#     years_of_retirement = 



