include("0__functions.jl")

# Ok I think I should make this into a module and export the functions 

# Set parameters
const β = 0.97
const r = 1 / β - 1
const σ = 2.5
const grid_points = 100
const grid_gap = 1000
const terminal_age = 110

# define consumption grid
const asset_grid = [i * grid_gap for i in 1:grid_points]


GetObjectiveDeathProbs(gender="male", year=2010, age=63)
GetObjectiveDeathProbs(gender="female", year=2010, age=63)
GetObjectiveDeathProbs(gender="female", year=2015, age=63)




# Pricing of annuity
# sum of payments divided by prob of death
# divided by loading factor
# times interest rate
# then we calc utility


function LifeCycleSolve(
    annuity_cost;
    bequests=false,
    pension=0,
    age,
    gender,
    life_prob_types="objective",
    year)
    # Function definition with parameters:
    # - annuity_cost: the cost of the annuity
    # - moneysworth: a factor used to calculate the annual annuity payment
    # - bequests: a boolean indicating whether bequests are considered
    # - pension: a fixed pension amount
    # - age: age to run the lifecycle model from, this impacts years of life left and annuity rates


    # set file name to save json to. 
    json_filename = string(age, "_beq", bequests, "_", gender, "_", life_prob_types, "_", year, "_pen", pension)
    filename_with_dir = "../data/ELSA/lifecycle_outputs/" * json_filename

    # If we have a json with those combos already then we skip
    # otherwise run the life cycle model as usual
    if isfile(filename_with_dir)
        return "json_run_already"
    end


    # set death probabilities.
    if life_prob_types == "objective"
        death_probs = GetObjectiveDeathProbs(gender=gender, age=55, year=year)
    elseif life_prob_types == "subjective"
        death_probs = "" # come and define later
    end
    # - death_probs: an array of cumulative death probabilities that are used in the optimisation stage



    # Retirement cohort
    if year > 2013
        ret_cohort = 2016
    else
        ret_cohort = 2008
    end
    # - ret_cohort: either pre 2014 or post 2014 cohort of data. this impacts annuity rates

    # using annuitant life tables estimate annuity payments 
    # that are realistic
    annual_annuity_payment = annuity_payment_function(
        annuity_cost, age=age,
        gender=gender,
        ann_life_table_year=ret_cohort
    )

    consumption_matrix = zeros(grid_points, grid_points)
    # Initialize a matrix to store consumption values

    # Nested loop to calculate consumption values for each combination of assets
    for x in eachindex(asset_grid)
        assets = asset_grid[x]
        for y in eachindex(asset_grid)
            next_assets = asset_grid[y]
            consumption_matrix[x, y] = bc(assets, next_assets, annual_annuity_payment, pension)
        end
    end


    utility_matrix = zeros(grid_points, grid_points)
    # Initialize a matrix to store utility values

    # Loop to calculate utility values for each consumption value in the consumption matrix
    for i in eachindex(utility_matrix)
        utility_matrix[i] = utility(consumption_matrix[i])
    end

    # If bequests then we get utility at end of life from certain death
    if bequests == true
        terminal_value = [bequest_lock(final_assets) for final_assets in asset_grid]
    else
        terminal_value = [0 for final_assets in asset_grid]
    end

    # Initialize a vector to store the terminal values for optimal consumption
    terminal_age = 110
    start_age = age
    temp_value = zeros(grid_points, terminal_age - start_age)
    # Initialize a matrix to store temporary values

    temp_value[:, terminal_age-start_age] = terminal_value
    # Set the terminal values in the temporary value matrix

    # each colum is an age
    # each row amount of assets next period. 
    opt_policy_function = zeros(grid_points, terminal_age - start_age)
    # Initialize a matrix to store the optimal policy function

    t = 1
    for t in 1:(terminal_age-start_age-1)
        # age moves one back
        cur_age = terminal_age - t
        value_index = cur_age - start_age
        prob_of_death = (death_probs[value_index+1] - death_probs[value_index]) / (1 - death_probs[value_index])

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

        outputs = zeros(grid_points, 3)
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

        opt_policy_function[:, value_index] = outputs[:, 3]

    end




    outputs_to_save = (pol_func=opt_policy_function,
        value_func=temp_value,
        pen=pension,
        ann_payment=annual_annuity_payment,
        beq=bequests,
        age=age,
        gender=gender,
        life_prob_types=life_prob_types,
        year=year)


    open(filename_with_dir, "w") do io
        JSON3.pretty(io, outputs_to_save)
    end

    # Looks correct!
    return "Done"

end




# This is starting wealth. For annuitisation we want to change this depending 
# on how large the annuitisation is
function AssetPathFunction(opt_policy_function, start_point)

    # Return asset paths given a set of optimal policy functions
    # and a starting value of wealth

    # Intialise
    asset_path = Vector{Int}(undef, terminal_age - start_age)

    # set intial assets
    asset_path[1] = start_point

    # Get optimal asset paths from optimal policy matrix
    for age in 1:(terminal_age-start_age-1)
        asset_path[age+1] = opt_policy_function[:, age][asset_path[age]]
    end

    # Create consumption path
    consumption_path = zeros(terminal_age - start_age)
    for age in 1:(terminal_age-start_age-1)
        consumption_path[age] = bc(asset_grid[asset_path[age]], asset_grid[asset_path[age+1]])
    end

    return (assets=asset_path, cons=consumption_path)

end


# make directory for lifecycle outputs
if !isdir("../data/ELSA/lifecycle_outputs")
    mkdir("../data/ELSA/lifecycle_outputs")
end
