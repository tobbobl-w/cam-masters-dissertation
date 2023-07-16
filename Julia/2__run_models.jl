### In this script we use the functions developed in 4
include("4__lifecycle_with_income.jl")

# Could run this for each 

# annuity_cost;
# moneysworth=0.95,
# bequests=false,
# pension=0,
# age,
# gender,
# life_prob_types="objective",
# year)

# mmm in 2012 gender discrimination started in the annuity market in the UK
# that is kinda fucked becasuse that will change the price of annuities. 
# Is there something clever I could do to get around this?
# Only compare 2012 and 2013 retirements ??

gender_age_year_combos = CSV.File("../data/ELSA/elsa_to_use/gender_pension_age_unique.csv") |>
                         Tables.matrix
# This is the combo of demographic info we need to run it for

# drop those under 60 for now. Need to change annuity function to just be life probs with some loading factor
sum([row[2] < 60 for row in eachrow(gender_age_year_combos)])
gender_age_year_combos = gender_age_year_combos[gender_age_year_combos[:, 2].>=60, :]


gender_age_year_combos

gender = "male"
age = 64
year = 2011


# This will take a while I think
# I wonder how long
for row in eachrow(gender_age_year_combos)
    println(row)
    retirement_lifecycle_with_income(
        bequests=false,
        age=row[2],
        gender=row[1],
        life_prob_types="objective",
        year=row[3]
    )

end






# Nice this is working now!!
# Just need to run for different level of forced annuities
# different life expectancies 
# different amount of state pension income
# different years and genders 
# get a unique df with all the types we will need

# (opt_policy_function_ann, temp_value_ann, ann_consump) = LifeCycleSolve(100000, bequests=false)

# (opt_policy_function_no_ann, temp_value_no_ann, no_ann_consump) = LifeCycleSolve(0, bequests=false)
# (opt_policy_function_no_ann_beq, temp_value_no_ann_beq, no_ann_consump_beq) = LifeCycleSolve(0, bequests=true)




# temp_value_ann[200, 1] - temp_value_no_ann[600, 1]
# # So value with annuity is higher as expected.

# temp_value_ann_beq[200, 1] - temp_value_no_ann_beq[600, 1]
# # with bequests the individual prefers not to annuitise!!


# (asset_path_ann, consumption_path_ann) = asset_path_function(opt_policy_function_ann, 200)
# (asset_path_ann_beq, consumption_path_ann) = asset_path_function(opt_policy_function_ann, 200)


# (asset_path, consumption_path) = asset_path_function(opt_policy_function_no_ann, 600)
# (asset_path_beq, consumption_path_beq) = asset_path_function(opt_policy_function_no_ann_beq, 600)


# consumption_path_ann = consumption_path_ann .+ ann_consump
# consumption_path = consumption_path .+ no_ann_consump

# plot([consumption_path_ann consumption_path])
# # also want to return utility
# plot([asset_path_ann asset_path])

# plot([asset_path asset_path_beq])

# opt_policy_function_no_ann_beq



# # Do I need to increase the size of the grid?

# ## Chart showing the proportion of annuititised wealth would 
# ## be nice to have. 

# ## Now we want to compare the bequest function and the predicted life expectancy

# obj_outputs = LifeCycleSolve(0, bequests=false, death_probs=cum_death_probs)
# sub_outputs = LifeCycleSolve(0, bequests=false, death_probs=sub_death_probs)

# sub_outputs[1]
# # Not really sure if i believe the end 
# obj_outputs[3]

# obj_assets_consump = asset_path_function(obj_outputs[1], 600)
# sub_assets_consump = asset_path_function(sub_outputs[1], 600)

# sub_assets_consump.assets


# plot([sub_assets_consump.assets obj_assets_consump.assets])

# sub_assets_consump.assets

# # how is consumption 5000 a year here. 
# plot([sub_assets_consump.cons obj_assets_consump.cons])
# # subjective consumption is higher at the start but lower later.



# # Now compare subjective probs with bequest consumption
# sub_assets_consump = asset_path_function(sub_outputs[1], 600)

# # with forced 
# sub_outputs_ann = LifeCycleSolve(100000, bequests=false, death_probs=sub_death_probs)
# obj_outputs_beq_ann = LifeCycleSolve(100000, bequests=true, death_probs=cum_death_probs)

# # without annuity
# sub_outputs = LifeCycleSolve(0, bequests=false, death_probs=sub_death_probs)
# obj_outputs_beq = LifeCycleSolve(0, bequests=true, death_probs=cum_death_probs)
# obj_outputs_ann = LifeCycleSolve(100000, bequests=false, death_probs=cum_death_probs)

# # now get mean consumption 

# obj_assets_consump_beq_ann = asset_path_function(obj_outputs_beq_ann[1], 200)
# sub_assets_consump_ann = asset_path_function(sub_outputs_ann[1], 200)

# obj_assets_consump_beq = asset_path_function(obj_outputs_beq[1], 600)
# sub_assets_consump = asset_path_function(sub_outputs[1], 600)

# # wow bequest function might be too strong?
# # need to compare to data to find out probably

# plot([obj_assets_consump_beq_ann[1] obj_assets_consump_beq[1] sub_assets_consump_ann[1] sub_assets_consump[1]])

# plot([(obj_assets_consump_beq_ann[2] .+ annuity_payment_function(100000, moneysworth=0.8)) obj_assets_consump_beq[2] (sub_assets_consump_ann[2] .+ annuity_payment_function(100000, moneysworth=0.8)) sub_assets_consump[2]])



# # was intuition correct?
# # 1 and 3 are with annuities and have larger consumption?
# # 2 and 4 are without annuities and have smaller consumption
# # I thought consumption would rise here?


# ## compare values to check if people want the annuity?
# obj_outputs_beq[2][2000, 1] - obj_outputs_beq_ann[2][300, 1]
# # People want annuity


# function check_annuity_equivalence(wealth)
#     obj_outputs_beq[2][2000, 1] - obj_outputs_beq_ann[2][wealth, 1]
# end

# to_plot = [check_annuity_equivalence(i) for i in 1:2000]

# p1 = plot(to_plot)
# hline!(p1, [0])

# mean(obj_outputs_beq[2] .> obj_outputs_beq_ann[2])
# # no matter what wealth annuity is still too good value

# ## see if subjective people still purchase annuities

# function check_annuity_equivalence(wealth)
#     sub_outputs[2][2000, 1] - sub_outputs_ann[2][wealth, 1]
# end

# to_plot = [check_annuity_equivalence(i) for i in 1:2000]

# p1 = plot(to_plot)
# hline!(p1, [0])
# # bigger gap than for bequest types
# # Means individual would pay more for the annuity


# function check_annuity_equivalence(wealth)
#     obj_outputs[2][2000, 1] - obj_outputs_ann[2][wealth, 1]
# end

# to_plot = [check_annuity_equivalence(i) for i in 1:2000]

# p1 = plot(to_plot)
# hline!(p1, [0])
# # looks so similar to subjective annuity

# # still seem too attractive. 
# # need value to be lower

# # can up the bequest motive or make the subjective expectancy data worse

# ### is there point playing around more







# ################################# TO DO #################################
# # 

# # then take individuals from elsa and estimate what consumption should be
# # under the different model types i.e. conditional on assets, age, object live probs and 
# # then both bequests and subjective life probs

# # then we can run regressions on the impact of forced annuitisation

# # there is annuity price data vs income drawdown on fca website which is cool 


# using Statistics
