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
grid_gap = 50 # 1000 pound increments

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


# define consumption grid
cons_grid = [i * grid_gap for i in 1:grid_points]

# asset accumulation
# assuming constant nominal annuity
# budget constraint function
bc(a, c, annuity, pension=0) = a * (1 + r) - c + annuity + pension
# then annuity effects the starting position on the grid. 

# utility function
u(c) = (c^(1 - σ) - 1) / (1 - σ)


# Just have a utility vector now. 
# Then a value vector associated with each final value of consumption
# but this depends on assets
# do we need starting assets in order to calculate this 
# we need some way of keeping track of the state variables
# That is what i dont understand about the lockwood code

# define a cash on hand grid...
# this makes 

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



# set terminal values for optimal consumption, value and next assets.
terminal_value = [u(final_assets) for final_assets in asset_grid]

terminal_age = 110
start_age = 65

temp_value = zeros(grid_points, terminal_age - start_age)

temp_value[:, terminal_age-start_age] = terminal_value

# each colum is an age
# each row is the optimal policy function -- i.e assets next period. 
opt_policy_function = zeros(grid_points, terminal_age - start_age)
