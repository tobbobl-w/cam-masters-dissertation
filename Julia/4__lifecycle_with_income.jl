using LinearAlgebra
using DelimitedFiles
using CSV
using DataFrames
using Plots
using JSON3

# Ok I think I should make this into a module and export the functions 

# Set parameters
const β = 0.97
const r = 1 / β - 1
const σ = 2.5
const terminal_age = 110

const asset_grid_points = 100
const asset_grid_gap = 1000
# define asset grid
const asset_grid = [i * asset_grid_gap for i in 1:asset_grid_points]

# define income grid
const inc_grid_points = 100
const inc_grid_gap = 500
const income_grid = [i * inc_grid_gap for i in 1:inc_grid_points]

include("0__functions.jl")

consumption_array = ones(Float64, asset_grid_points, asset_grid_points, inc_grid_points)
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

utility_array = ones(Float64, asset_grid_points, asset_grid_points, inc_grid_points)
# Initialize a matrix to store utility values

# utility function
function utility(c)
    if c > 0.0001
        (c^(1 - σ)) / (1 - σ)
    else
        -Inf
    end
end

# Loop to calculate utility values for each consumption value in the consumption matrix
for i in eachindex(utility_array)
    utility_array[i] = utility(consumption_array[i])
end


retirement_lifecycle_with_income = function (;
    bequests=false,
    age=60,
    gender="male",
    life_prob_types="objective",
    year=2015)


    # set file name to save json to. 
    json_filename = string(age, "_beq", bequests, "_", gender, "_", life_prob_types, "_", year, ".json")
    filename_with_dir = "../data/ELSA/lifecycle_outputs/" * json_filename

    ## So now for each level of income and current assets we find best consumption next period
    terminal_age = 110
    start_age = age

    death_probs = death_probabilities(gender=gender, age=age, year=year)

    # Dimensions are the state variables
    # Income, current assets and age
    value_array = ones(Float64, inc_grid_points, asset_grid_points, terminal_age - start_age)
    policy_array = ones(Float64, inc_grid_points, asset_grid_points, terminal_age - start_age)

    # for each income level find the optimal asset policy function
    for inc in eachindex(income_grid)

        utility_matrix = utility_array[:, :, inc]

        # If bequests then we get utility at end of life from certain death
        if bequests == true
            terminal_value = [bequest_lock(final_assets) for final_assets in asset_grid]
        else
            terminal_value = [0 for final_assets in asset_grid]
        end

        # Initialize a vector to store the terminal values for optimal consumption
        temp_value = zeros(asset_grid_points, terminal_age - start_age)
        # Initialize a matrix to store temporary values

        temp_value[:, terminal_age-start_age] = terminal_value
        # Set the terminal values in the temporary value matrix

        # each colum is an age
        # each row amount of assets next period. 
        opt_policy_function = zeros(asset_grid_points, terminal_age - start_age)
        # Initialize a matrix to store the optimal policy function

        # solve model backwards
        t = 1
        for t in 1:(terminal_age-start_age-1)
            # age moves one back
            cur_age = terminal_age - t
            value_index = cur_age - start_age

            # prob of death is year by year
            prob_of_death = death_probs[value_index]

            ev_g = if bequests == true
                temp_value[:, value_index+1] * (1 - prob_of_death) +
                prob_of_death * broadcast(bequest_lock, opt_policy_function[:, value_index+1])
            else
                temp_value[:, value_index+1] * (1 - prob_of_death)
            end
            # Optimal assets are a function of age and cur assets
            # we are in age already so just need to return 
            ev_g
            # a vector as long as assets, then optimal a prime, and max value as well. 
            # then we can do something better with it after. 

            outputs = zeros(asset_grid_points, 3)
            # col 1 is a index
            # col 2 is value
            # col 3 is a prime index

            for x in eachindex(asset_grid)

                # Then maximise returning Value and the best asset index
                obj_g = utility_matrix[x, :] + (ev_g * β)

                max_and_index = findmax(obj_g)
                # First element is the max
                # second element is the index that achieves the max
                # therefore second element is the policy function.

                outputs[x, :] = [x, max_and_index[1], max_and_index[2]]
            end

            # save value for this period. 
            temp_value[:, value_index] = outputs[:, 2]

            # save policy function from this period
            opt_policy_function[:, value_index] = outputs[:, 3]

        end

        value_array[inc, :, :] = temp_value
        # save value array for a given income

        policy_array[inc, :, :] = opt_policy_function
        # save policy function from this period

    end

    outputs_to_save = (pol_func=policy_array,
        value_func=value_array,
        beq=bequests,
        age=age,
        gender=gender,
        life_prob_types=life_prob_types,
        year=year)

    open(filename_with_dir, "w") do io
        JSON3.pretty(io, outputs_to_save)
    end
end

# @time retirement_lifecycle_with_income()

# How many times does this need to run? 200 not too bad
# each run will take about 50 mins

# Now we want to plot consumption paths
# Dont have a consumption floor but I dont think this matters because we dont have stochastic income?

# I should do a run with a finer grid and see how much longer it takes
# I think I should only need to run for one set of year-age combos.
# surely 



retrieve_policy_function = function (
    bequests=false,
    life_prob_types="objective",
    age=65,
    year=2014,
    gender="male")

    json_filename = string("../data/ELSA/lifecycle_outputs/", age, "_beq", bequests, "_", gender, "_", life_prob_types, "_", year, ".json")

    isfile(json_filename)
    json_in = JSON3.read(json_filename)

    pol_func_readin = reshape(json_in["pol_func"], inc_grid_points, asset_grid_points, terminal_age - age)

    return pol_func_readin
end


pol_func = retrieve_policy_function()
pol_func[income_grid_point, :, :]
# now we trace through this



function asset_path_function(opt_policy_function, asset_start_point, income_grid_point)

    # Return asset paths given a set of optimal policy functions
    # and a starting value of wealth

    number_years = size(opt_policy_function)[2]
    println(number_years)
    # Intialise
    asset_path = Vector{Int}(undef, number_years + 1)

    # set intial assets
    asset_path[1] = asset_start_point
    asset_path[end] = 1 # i.e. assets at age 111 are 0

    # Get optimal asset paths from optimal policy matrix
    for age in 1:(number_years-1)
        println(age + 1)
        asset_path[age+1] = opt_policy_function[:, age][asset_path[age]]
    end

    # Create consumption path
    consumption_path = zeros(number_years)
    for age in eachindex(consumption_path)
        consumption_path[age] = bc_inc(asset_grid[asset_path[age]], asset_grid[asset_path[age+1]], income_grid[income_grid_point])
    end

    return (assets=asset_path, cons=consumption_path)

end

for_plots_60 = asset_path_function(
    retrieve_policy_function(false, "objective", 60, 2010, "male")[50, :, :],
    70,
    60)

for_plots.assets[6]

for_plots_65 = asset_path_function(
    retrieve_policy_function(false, "objective", 65, 2015, "male")[50, :, :],
    55,
    60)


death_probabilities(gender="male", year=2010, age=60)[6:end]

death_probabilities(gender="male", year=2015, age=65)
## these are different and they should not be. 
## a 60 year old in 2010 should have the same expected mortality 
# as one a 65 year old in 2015 

plot([for_plots_60.assets[6:end] for_plots_65.assets])

plot(for_plots.cons)




# questions: how to see if they prefer annuities
# time increase if we make the grid finer

