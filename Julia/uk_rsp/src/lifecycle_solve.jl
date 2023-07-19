
# define key parameters
β = 0.97
r = 1 / β - 1
σ = 2.5
grid_points = 2000
grid_gap = 250 # 1000 pound increments
terminal_age = 110
start_age = 65

# define consumption grid
asset_grid = [i * grid_gap for i in 1:grid_points]

# asset accumulation
# assuming constant nominal annuity
bc(a, a′, annuity=0, state_pension=5000) = a * (1 + r) - a′ + annuity + state_pension


#u(bc(10000, 10000, 2000, 5000)) - u(bc(10000, 10000, 1000, 5000)) > 0
# this makes sense 

# utility function
utility = function (c)
    if c > 0.0001
        (c^(1 - σ)) / (1 - σ)
    else
        -Inf
    end
end


# bequest function
# from lockwood RED 2012
bequest_lock(b; c0=18, m=0.96) = (m / (1 - m))^σ * (((m / (1 - m)) * c0) + b)^(1 - σ) / (1 - σ)
# but won't including bequests make utility lower

# Pricing of annuity
# sum of payments divided by prob of death
# divided by loading factor
# times interest rate
# then we calc utility

function annuity_payment_function(annuity_cost; company_wedge=0.15)
    annuity_payment = sum([cum_death_probs[age] * 1 / (((1 + r)^age) * (1 + company_wedge)) for age in eachindex(cum_death_probs)])
    return annuity_cost / (annuity_payment)
end
annuity_payment_function(1000)
# cool now we can use this function.. 
# where can I get real annuity prices from?

# now check value annuitising a proportion of wealth
# prop of wealth to annuitise. 
# need to solve function for different levels of annuitisied wealth

# can we just solve with annuitisation 
# and then compare with starting on the grid higher or lower. 

# Ok so run with annuity incomes of 


LifeCycleSolve = function (annuity_cost, bequests=false)


    # Take the annuity cost and get the rough annual payment 
    annual_annuity_payment = annuity_payment_function(annuity_cost)

    # set government pension
    pension = 5000

    # Intialise consumption grid
    consumption_matrix = zeros(grid_points, grid_points)

    # Now for each level of assets return how much consumption individuals get
    for x in eachindex(asset_grid)
        assets = asset_grid[x]
        for y in eachindex(asset_grid)
            next_assets = asset_grid[y]
            # assets this period are 1st dimension
            # assets next period are 2nd dimension
            consumption_matrix[x, y] = bc(assets, next_assets, annual_annuity_payment, pension)
        end
    end


    # # Check that low assets this period and high the next implies negative consumption
    # # and visa versa
    # consumption_matrix[1, 500]
    # consumption_matrix[1000, 1]

    utility_matrix = zeros(grid_points, grid_points)

    for i in eachindex(utility_matrix)
        utility_matrix[i] = utility(consumption_matrix[i])
    end


    # set terminal values for optimal consumption, value and next assets.
    terminal_value = [0 for final_assets in asset_grid]
    # u(final_assets)

    terminal_age = 110
    start_age = 65

    temp_value = zeros(grid_points, terminal_age - start_age)

    temp_value[:, terminal_age-start_age] = terminal_value

    # each colum is an age
    # each row is the optimal policy function -- i.e assets next period. 
    opt_policy_function = zeros(grid_points, terminal_age - start_age)

    t = 1
    for t in 1:(terminal_age-start_age-1)
        # age moves one back
        age = terminal_age - t

        # this is the column in the data frame that refers to that age. 
        # we work backwards from the terminal age. 
        value_index = age - start_age

        prob_of_death = (cum_death_probs[value_index+1] - cum_death_probs[value_index]) / (1 - cum_death_probs[value_index])

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

    non_asset_consumption = pension + annual_annuity_payment

    # Lets also return consumption policy function

    # Looks correct!
    return opt_policy_function, temp_value, non_asset_consumption

end




# This is starting wealth. For annuitisation we want to change this depending 
# on how large the annuitisation is
AssetPathFunction = function (opt_policy_function, start_point)

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

    return asset_path, consumption_path

end
