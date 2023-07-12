using LinearAlgebra
using DelimitedFiles
using CSV
using DataFrames
using Plots

# need life probabilities. 
# copied for now from lockwood
# Ok I think I should make this into a module and export the functions 
# then I can use revise for a better workflow. 
# Things to do: 
# add gender and maybe health status into the model 
# then find the expectancies and bequest motives which 
# generate similar sized proportions of annuitisation

# I can also make my life cycle code faster I think although not sure how

# to

cum_death_probs = CSV.File("Julia/death_probs.csv") |>
                  Tables.matrix |>
                  vec
# Increase every prob of death by 0.01

# Mortality data from the ONS
const base_year = 2005
const base_age = 50

male_data = CSV.File("../data/ONS/male_transition_probs.csv") |>
            Tables.matrix

female_data = CSV.File("../data/ONS/female_transition_probs.csv") |>
              Tables.matrix
# Each column is the individuals age in 2010. 
# each row is a year after that, transition probs from 
# age to age + 1

# these people are aged 50 in 2005
plot([male_data[:, 21] male_data[:, 1]])

# So then if we want to pdf of probs in a given age 
# ONS life tables
function death_probabilities(data, year=2012, age=65)
    # return probs from this age and year onwards
    col_index = year - (base_year - 1) # selects the correct column
    start_row_index = age - base_age # first row of interest

    death_probs = data[start_row_index:51, col_index]

    return death_probs
end

# If I dont update 
death_probabilities(male_data, 2010, 63)
death_probabilities(female_data, 2010, 63)
# these finish at age 100. and start at the age specified


# ok so now I can set gender and use realistic lifetables
# I could have a look at annuitant tables as well and then use a 
# scaling factor on the implied fair price to calucalte the price
# that normal people face. 

# can set wealth from elsa data as well 





function constrained_add(a, b)
    # This is just so I can play around with subjective 
    # probabilities and get strictly increasing probs. 
    if (a + b) > (a + (1 - a) / 2)
        return a + (1 - a) / 2
    else
        return a + b
    end
end

sub_death_probs = [constrained_add(prob, 0.1) for prob in cum_death_probs]

plot([cum_death_probs sub_death_probs])


const β = 0.97
const r = 1 / β - 1
const σ = 2.5
const grid_points = 2000
const grid_gap = 250 # 1000 pound increments
const terminal_age = 110
const start_age = 65

# define consumption grid
const asset_grid = [i * grid_gap for i in 1:grid_points]

# asset accumulation
# assuming constant nominal annuity
bc(a, a′, annuity=0, state_pension=5000) = a * (1 + r) - a′ + annuity + state_pension


#u(bc(10000, 10000, 2000, 5000)) - u(bc(10000, 10000, 1000, 5000)) > 0
# this makes sense 

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

# Pricing of annuity
# sum of payments divided by prob of death
# divided by loading factor
# times interest rate
# then we calc utility

function annuity_payment_function(annuity_cost; moneysworth=0.8)
    moneysworth * (annuity_cost / (sum([cum_death_probs[age] * 1 / (((1 + r)^age)) for age in eachindex(cum_death_probs)])))
end
# cool now we can use this function.. 
# where can I get real annuity prices from?

# now check value annuitising a proportion of wealth
# prop of wealth to annuitise. 
# need to solve function for different levels of annuitisied wealth

# can we just solve with annuitisation 
# and then compare with starting on the grid higher or lower. 

# Ok so run with annuity incomes of 

function LifeCycleSolve(annuity_cost; moneysworth=0.75, bequests=false, death_probs=cum_death_probs, pension=0)

    # Take the annuity cost and get the rough annual payment 
    annual_annuity_payment = annuity_payment_function(annuity_cost, moneysworth=moneysworth)

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

    non_asset_consumption = pension + annual_annuity_payment

    # Lets also return consumption policy function

    # Looks correct!
    return opt_policy_function, temp_value, non_asset_consumption

end




# This is starting wealth. For annuitisation we want to change this depending 
# on how large the annuitisation is
function asset_path_function(opt_policy_function, start_point)

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



# Could run this for each 

# Now compare value with and without
# Value at start should reflect it.
(opt_policy_function_ann_beq, temp_value_ann_beq, ann_consump_beq) = LifeCycleSolve(100000, bequests=true)
(opt_policy_function_ann, temp_value_ann, ann_consump) = LifeCycleSolve(100000, bequests=false)

(opt_policy_function_no_ann, temp_value_no_ann, no_ann_consump) = LifeCycleSolve(0, bequests=false)
(opt_policy_function_no_ann_beq, temp_value_no_ann_beq, no_ann_consump_beq) = LifeCycleSolve(0, bequests=true)




temp_value_ann[200, 1] - temp_value_no_ann[600, 1]
# So value with annuity is higher as expected.

temp_value_ann_beq[200, 1] - temp_value_no_ann_beq[600, 1]
# with bequests the individual prefers not to annuitise!!


(asset_path_ann, consumption_path_ann) = asset_path_function(opt_policy_function_ann, 200)
(asset_path_ann_beq, consumption_path_ann) = asset_path_function(opt_policy_function_ann, 200)


(asset_path, consumption_path) = asset_path_function(opt_policy_function_no_ann, 600)
(asset_path_beq, consumption_path_beq) = asset_path_function(opt_policy_function_no_ann_beq, 600)


consumption_path_ann = consumption_path_ann .+ ann_consump
consumption_path = consumption_path .+ no_ann_consump

plot([consumption_path_ann consumption_path])
# also want to return utility
plot([asset_path_ann asset_path])

plot([asset_path asset_path_beq])

opt_policy_function_no_ann_beq



# Do I need to increase the size of the grid?

## Chart showing the proportion of annuititised wealth would 
## be nice to have. 

## Now we want to compare the bequest function and the predicted life expectancy

obj_outputs = LifeCycleSolve(0, bequests=false, death_probs=cum_death_probs)
sub_outputs = LifeCycleSolve(0, bequests=false, death_probs=sub_death_probs)

sub_outputs[1]
# Not really sure if i believe the end 
obj_outputs[3]

obj_assets_consump = asset_path_function(obj_outputs[1], 600)
sub_assets_consump = asset_path_function(sub_outputs[1], 600)

sub_assets_consump.assets


plot([sub_assets_consump.assets obj_assets_consump.assets])

sub_assets_consump.assets

# how is consumption 5000 a year here. 
plot([sub_assets_consump.cons obj_assets_consump.cons])
# subjective consumption is higher at the start but lower later.



# Now compare subjective probs with bequest consumption
sub_assets_consump = asset_path_function(sub_outputs[1], 600)

# with forced 
sub_outputs_ann = LifeCycleSolve(100000, bequests=false, death_probs=sub_death_probs)
obj_outputs_beq_ann = LifeCycleSolve(100000, bequests=true, death_probs=cum_death_probs)

# without annuity
sub_outputs = LifeCycleSolve(0, bequests=false, death_probs=sub_death_probs)
obj_outputs_beq = LifeCycleSolve(0, bequests=true, death_probs=cum_death_probs)
obj_outputs_ann = LifeCycleSolve(100000, bequests=false, death_probs=cum_death_probs)

# now get mean consumption 

obj_assets_consump_beq_ann = asset_path_function(obj_outputs_beq_ann[1], 200)
sub_assets_consump_ann = asset_path_function(sub_outputs_ann[1], 200)

obj_assets_consump_beq = asset_path_function(obj_outputs_beq[1], 600)
sub_assets_consump = asset_path_function(sub_outputs[1], 600)

# wow bequest function might be too strong?
# need to compare to data to find out probably

plot([obj_assets_consump_beq_ann[1] obj_assets_consump_beq[1] sub_assets_consump_ann[1] sub_assets_consump[1]])

plot([(obj_assets_consump_beq_ann[2] .+ annuity_payment_function(100000, moneysworth=0.8)) obj_assets_consump_beq[2] (sub_assets_consump_ann[2] .+ annuity_payment_function(100000, moneysworth=0.8)) sub_assets_consump[2]])



# was intuition correct?
# 1 and 3 are with annuities and have larger consumption?
# 2 and 4 are without annuities and have smaller consumption
# I thought consumption would rise here?


## compare values to check if people want the annuity?
obj_outputs_beq[2][2000, 1] - obj_outputs_beq_ann[2][300, 1]
# People want annuity


function check_annuity_equivalence(wealth)
    obj_outputs_beq[2][2000, 1] - obj_outputs_beq_ann[2][wealth, 1]
end

to_plot = [check_annuity_equivalence(i) for i in 1:2000]

p1 = plot(to_plot)
hline!(p1, [0])

mean(obj_outputs_beq[2] .> obj_outputs_beq_ann[2])
# no matter what wealth annuity is still too good value

## see if subjective people still purchase annuities

function check_annuity_equivalence(wealth)
    sub_outputs[2][2000, 1] - sub_outputs_ann[2][wealth, 1]
end

to_plot = [check_annuity_equivalence(i) for i in 1:2000]

p1 = plot(to_plot)
hline!(p1, [0])
# bigger gap than for bequest types
# Means individual would pay more for the annuity


function check_annuity_equivalence(wealth)
    obj_outputs[2][2000, 1] - obj_outputs_ann[2][wealth, 1]
end

to_plot = [check_annuity_equivalence(i) for i in 1:2000]

p1 = plot(to_plot)
hline!(p1, [0])
# looks so similar to subjective annuity

# still seem too attractive. 
# need value to be lower

# can up the bequest motive or make the subjective expectancy data worse

### is there point playing around more







################################# TO DO #################################
# 

# then take individuals from elsa and estimate what consumption should be
# under the different model types i.e. conditional on assets, age, object live probs and 
# then both bequests and subjective life probs

# then we can run regressions on the impact of forced annuitisation

# there is annuity price data vs income drawdown on fca website which is cool 


using Statistics
