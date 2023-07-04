using LinearAlgebra
using DelimitedFiles
using CSV
using DataFrames
using Plots

# need life probabilities. 
# copied for now from lockwood


cum_death_probs = CSV.File("Julia/death_probs.csv") |>
                  Tables.matrix |>
                  vec

plot(cum_death_probs)
pdf_death_probs = [cum_death_probs[i+1] - cum_death_probs[i] for i in 1:45]
plot(pdf_death_probs)

# Define values
β = 0.97
r = 1 / β - 1
σ = 2.5

grid_points = 1000 # 
grid_gap = 250 # 1000 pound increments

# Pricing of annuity
# sum of payments divided by prob of death
# divided by loading factor
# times interest rate
# then we calc utility

annuity_payment = 1000
annuity_cost = sum([cum_death_probs[age] * annuity_payment / ((1 + r)^age) for age in eachindex(cum_death_probs)])
# think this is correct
# so this much wealth will buy an annuity for rest of life of 1000 a year.
# how do we use this with the grid points
# maybe this is why having a grid for consumption is better

# now check value annuitising a proportion of wealth
# prop of wealth to annuitise. 
# need to solve function for different levels of annuitisied wealth

θ = 0.5


# define consumption grid
asset_grid = [i * grid_gap for i in 1:grid_points]

# asset accumulation
# assuming constant nominal annuity
bc(a, a′, annuity=0, state_pension=5000) = a * (1 + r) - a′ + annuity + state_pension
# then annuity effects the starting position on the grid. 



# utility function
u(c) = (c^(1 - σ) - 1) / (1 - σ)

# Intialise consumption grid
consumption_matrix = zeros(grid_points, grid_points)

for x in eachindex(asset_grid)
    assets = asset_grid[x]
    for y in eachindex(asset_grid)
        next_assets = asset_grid[y]
        # assets this period are 1st dimension
        # assets next period are 2nd dimension
        consumption_matrix[x, y] = bc(assets, next_assets)
    end
end

# Check that low assets this period and high the next implies negative consumption
# and visa versa
consumption_matrix[1, 500]
consumption_matrix[1000, 1]

utility_matrix = zeros(grid_points, grid_points)

for i in eachindex(utility_matrix)
    if consumption_matrix[i] > 0.0001 # negative consumption should have -Inf utility and never be picked. 
        utility_matrix[i] = u(consumption_matrix[i])
    else
        utility_matrix[i] = -Inf # Never pick negative consumption. 
    end
end

for x in 0:500:50
    println("x = ", x)
end

# try first a couple of annuitisation values
# want value if they annuitise 0:500:50 points

solve_life_cycle = function (annuitisation)

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

        ev_g = temp_value[:, value_index+1] * (1 - prob_of_death)

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

    return [temp_value, opt_policy_function]
end

temp_value, opt_policy_function = solve_life_cycle()



opt_policy_function
temp_value
plot(temp_value[:, 10])
# Looks correct!

# Now give starting assets and we can trace asset decumulation
# whatever y is this period, next period that is x and then we use the function again. 

start_point = 200

asset_path = Vector{Int}(undef, terminal_age - start_age)
asset_path[1] = start_point
asset_path[1]

for age in 1:(terminal_age-start_age-1)
    asset_path[age+1] = opt_policy_function[:, age][asset_path[age]]
end


consumption_path = zeros(terminal_age - start_age)
for age in 1:(terminal_age-start_age-1)
    consumption_path[age] = bc(asset_grid[asset_path[age]], asset_grid[asset_path[age+1]])
end

plot(consumption_path)
plot(asset_path)
# also want to return utility



asset_path[15]

consumption_matrix[300, 500] # This shouldnt be possible
utility_matrix[300, 500]
# consumption floor?

bc(asset_grid[300], asset_grid[501])

asset_grid[300] * (1 + r)

# I guess it is fuzzy because risk of death is not smooth. 
# what happens if I set it to always be 3%
# smoother when always 60% chance of death. 
# Hitting 0 consumption is really feared. 

# Jobs to do: 

# exp_next_period = function(a, t)
#     # expectation of next period
#     # conditional on t and a
#     # with some prob we get balue next period
#     # and then with some other prob die and get 0. 
#     prob_of_death = (cum_death_probs[t+1] - cum_death_probs[t]) / (1 - cum_death_probs[t])

#     return prob_of_death * 0 + (1 - prob_of_death) * value_function(a, t)
# end

# utility_matrix[1, :]

# findmax(utility_matrix[700, :])

# for x in eachindex(asset_grid)
#     print(findmax(utility_matrix[x, :]))
# end
# this looks weird. I would expect more stuff in between. 


# value_function = function (a, t)
#     # return decision on a′ (index of grid) for each a


#     # pick a level of assets next period for each level of assets this period. 
#     for x in eachindex(asset_grid)
# value_vector = [utility_matrix[x, i] + β * exp_next_period() for i in eachindex(asset_grid)

#         findmax(utility_matrix[x, :] )
#     end

# end
# # mmmm I don't think this is the way to solve this type of problem 
# # 


