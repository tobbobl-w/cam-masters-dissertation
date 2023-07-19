using LinearAlgebra
using DelimitedFiles
using CSV
using DataFrames
using Plots
using JSON3

# Ok I think I should make this into a module and export the functions 

## Set grid points
const asset_grid_points = 500
const asset_grid_gap = 1000
# define asset grid
const asset_grid = [i * asset_grid_gap for i in 1:asset_grid_points]

# define income grid
const inc_grid_points = 100
const inc_grid_gap = 500
const income_grid = [i * inc_grid_gap for i in 1:inc_grid_points]


consumption_array = CreateConsumptionArray()
utility_array = CreateUtilityArray()

RetirementLifecycleWithIncome = function (;
    bequests=false,
    age=60,
    gender="male",
    life_prob_types="objective",
    year=2015,
    id_wave="")

    # two ways to run this function
    # either with objective probs given by age, gender and year
    # or subjective probs given by id_wave

    if life_prob_types == "objective"
        # set file name to save json to. 
        json_filename = string(age, "_beq", bequests, "_", gender, "_", life_prob_types, "_", year, ".json")
        filename_with_dir = "../data/ELSA/lifecycle_outputs/" * json_filename

        death_probs = GetObjectiveDeathProbs(gender=gender, age=age, year=year)

    elseif life_prob_types == "subjective"
        dir_name = "../data/ELSA/lifecycle_outputs/id_wave/"
        if !isdir(dir_name)
            mkdir(dir_name)
        end
        json_filename = string(age, "_beq", bequests, "_", gender, "_", life_prob_types, "_", year, "_", id_wave, ".json")
        filename_with_dir = dir_name * json_filename

        death_probs = GetSubjectiveDeathProbs(id_wave_to_get=id_wave)

    end


    # Check if file exists already
    if isfile(filename_with_dir)
        return "Already done file"
    end


    ## So now for each level of income and current assets we find best consumption next period
    terminal_age = 110
    start_age = age

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
        year=year,
        id_wave)

    open(filename_with_dir, "w") do io
        JSON3.pretty(io, outputs_to_save)
    end
end

# @time RetirementLifecycleWithIncome()

# How many times does this need to run? 200 not too bad
# each run will take about 50 mins

# Now we want to plot consumption paths
# Dont have a consumption floor but I dont think this matters because we dont have stochastic income?

# I should do a run with a finer grid and see how much longer it takes
# I think I should only need to run for one set of year-age combos.
# surely 

