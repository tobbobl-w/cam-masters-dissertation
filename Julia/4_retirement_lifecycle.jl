using LinearAlgebra
using DelimitedFiles
using CSV
using DataFrames
# wow have no idea

# formulate problem

# do we need to use quadrature to take expectations? 
# No because prob of death is on outside not inside 
# of the function
# 


# at age 110 the person dies
# optimal to consume all assets



# need life probabilities. 
# could just make them up. 

cum_death_probs = [0
    0.018740306
    0.038815572
    0.060225799
    0.08304727
    0.107305414
    0.13310194
    0.160538561
    0.189602563
    0.220281232
    0.252561853
    0.286520711
    0.322170519
    0.359320569
    0.397703868
    0.437129707
    0.477483663
    0.518651308
    0.560441936
    0.602563125
    0.644658886
    0.68624609
    0.726765327
    0.765669896
    0.802387673
    0.836397386
    0.867254049
    0.894652529
    0.918376688
    0.938401098
    0.954802044
    0.967795662
    0.977712513
    0.985023012
    0.990223002
    0.993782897
    0.996160399
    0.997698782
    0.998665039
    0.999262593
    0.999605869
    0.999796577
    0.999898289
    0.999949144
    0.999974572
    0.999987286]
using Plots
plot(cum_death_probs)
pdf_death_probs = [cum_death_probs[i+1] - cum_death_probs[i] for i in 1:45]
plot(pdf_death_probs)

# why the fuck cant i do even this. 
# ahh silly we want prob of death given alive. 
# so need to 

# should have a transition matrix since these probs change with age. 
# can we just use bayes rule to update though
# column is age now and 

# need some sensible starting values for assets and 
# consumption grids. 


β = 0.97
r = 1 / β
σ = 2.5

grid_points = 1000
grid_gap = 150

# define consumption grid
asset_grid = [i * grid_gap for i in 1:grid_points]
starting_assets = 100000
# we can put some heterogeneity on this later I think

bc(a, a′) = a′ / (1 + r) - a

# consumption
u(c) = (c^(1 - σ) - 1) / (1 - σ)

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

consumption_matrix

utility_matrix = zeros(grid_points, grid_points)

for i in eachindex(utility_matrix)
    if consumption_matrix[i] > 0.0001 # negative consumption should have -Inf utility and never be picked. 
        utility_matrix[i] = u(consumption_matrix[i])
    else
        utility_matrix[i] = -Inf
    end
end



# set terminal values for optimal consumption, value and next assets. c_opt, temp_V, and net_y
terminal_value = [u(final_assets) for final_assets in asset_grid]
# do we need to know anything else
# I dont think so?

terminal_age = 110
start_age = 65

axes(temp_value, 2) |> typeof

temp_value = zeros(grid_points, terminal_age - start_age)

temp_value[:, terminal_age-start_age] = terminal_value


for col in axes(temp_value, 2)
    println(temp_value[:, col])
end

t = 1

for t in 1:(terminal_age-start_age-1)
    # age moves one back
    age = terminal_age - t
    value_index = terminal_age - start_age - t

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
        obj_g = utility_matrix[x, :] + ev_g

        max_and_index = findmax(obj_g)

        outputs[x, :] = [x, max_and_index[1], max_and_index[2]]
    end

    outputs


end

# not dead
rand(2)
typeof(findmax(rand(2)))

outputs = zeros(grid_points, 3)
x = 1
max_and_index = findmax(rand(2))

outputs[x, :] = [x, max_and_index[1], max_and_index[2]]


value_assets = Val(100)
# Tuple{Float64,Int64}

append!(value_assets, findmax(rand(2)))

typeof(value_assets)

[test1 test2] = [1 2]

Val(100)
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


