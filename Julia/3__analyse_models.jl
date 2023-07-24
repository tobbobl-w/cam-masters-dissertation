using CSV
using LinearAlgebra
using DataFrames
using Plots
using JSON3

include("0__functions.jl")
include("1__lifecycle_with_income_function.jl")

## analyse models 
# want to read in jsons and compare consumption with and without forced annuitisation

# here we need to decide on the price of the annuity that individuals would be forced to buy
# use population life expectancies (weighted by number of people)


# first compare someone with objective and subjective models
# for a given forced annuity price what are consumptions

col_types = Dict(:id_wave => String, :age => Int128, :year => Int128,
    :ever_dc_pen => String, :ever_db_pen => String, :public_pension => Float64,
    :fin_wealth => Float64)

idwave_df = CSV.File("../data/ELSA/elsa_to_use/for_julia.csv", types=col_types) |>
            DataFrame


# Currently these functions return just a vector 
# we could get them to return a df. 
for_plots_inc50 = AssetPathFunction(
    RetrievePolicyFunction(false, "objective", 60, 2010, "male")[50, :, :],
    70,
    60)


for_plots_inc70 = AssetPathFunction(
    RetrievePolicyFunction(false, "objective", 65, 2015, "male")[50, :, :],
    50,
    60)


GetObjectiveDeathProbs(gender="male", year=2010, age=60)[6:end]
GetObjectiveDeathProbs(gender="male", year=2015, age=65)
# ## these are different and they should not be. 
# ## a 60 year old in 2010 should have the same expected mortality 
# # as one a 65 year old in 2015 


plot([for_plots_60.assets[6:end] for_plots_65.assets])
# Yeah that works, so we only need to trace once!

plot([for_plots_60.cons[end] for_plots_65.cons])
# and consumption is the same


# questions: how to see if they prefer annuities
# time increase if we make the grid finer

# lets do annuities
# so an annuity costs a certain amount of grid space 
# does moving down this grid and up the income grid improve value

Annuity_GeneralDeathProbs(100000, 0.85,
    GetObjectiveDeathProbs(gender="female", age=60, year=2011))


asset_grid[100]
income_grid[9]
# does moving 9 points on the income grid
# make up for moving 100 points on the asset grid

AssetPathFunction(
    RetrievePolicyFunction(false, "objective", 60, 2011, "female")[50, :, :],
    50,
    60)


# Show if annuities are demanded for different loading factors

RetrieveValueFunction(false, "objective", 60, 2011, "male")[50, 100, 1] # with annuity
RetrieveValueFunction(false, "objective", 60, 2011, "male")[40, 200, 1] # withou


# ----------- 1 what loading factor makes our population buy so few annuities.
# I want to read in all demographic info
# then with diff loading factors show different levels of annuitisation
# looking for around 6
# other income should include all other income including from db/dc pension bit



# should get measure of wealth aswell
idwave_df = idwave_df[Bool.(.!ismissing.(idwave_df.fin_wealth)).&Bool.(.!ismissing.(idwave_df.public_pension)), :]


# so we want gender age year and grid points
# take these and predict how many people annuitise for different levels of loading factor

# work on these when I am home

OptimalAnnuityAmount = function (loading_factor=0.85; gender="male", age=65, year=2015, asset_start_point=200, income_start_point=50)

    # comparing different starting values on the income-asset array
    # and the loading factor gives the cost of the tradeoff. I.e. how far we move on each one

    value_func = RetrieveValueFunction(false, "objective", age, year, gender)[:, :, 1]
    value_of_start = value_func[income_start_point, asset_start_point]

    death_probs = GetObjectiveDeathProbs(age=age, year=year, gender=gender)

    # make a rough matrix of grid points to move on assets vs income
    ann_inc_vec = [Annuity_GeneralDeathProbs(assets, loading_factor, death_probs) for assets in asset_grid]

    # I am not going to allow very small annuity purchaes since basically no one does this anyway
    # get rid of assets below 10000
    ann_inc_vec_grid_points = FindClosestGridPoint.(ann_inc_vec, 500)

    ann_asset_grid_mat = [Int.(asset_grid / 1000) ann_inc_vec_grid_points]

    # only allowed to pick a point that is within 100 of a grid point
    select_vec = abs.(FindClosestGridPoint.(ann_inc_vec, 500) * 500 - ann_inc_vec) .< 100

    # we want to evalute the value of these points and pick the max
    points_to_evaluate = ann_asset_grid_mat[select_vec, :]

    value_of_points = []

    for row_index in 1:sum(select_vec)

        # Move up the income grid
        income_point = income_start_point + points_to_evaluate[row_index, 2]

        # Move down the asset grid
        asset_point = asset_start_point - points_to_evaluate[row_index, 1]

        # cannot have negative assets
        if asset_point < 1
            continue
        end

        append!(value_of_points, value_func[income_point, asset_point])
    end

    max_value_and_index = findmax(value_of_points)

    if max_value_and_index[1] > value_of_start
        # return the optimal amount to spend on annuities and the
        return max_value_and_index
    elseif max_value_and_index[1] < value_of_start
        return ("do not annuitise", value_of_start)
    end

end




# what do I want to show
# under normal assumptions people want to annutise anyway. 
# do I actually want to show that though
# Lets come back to this. for now we want to find the bequest motive that makes only 6% of people annuitise their wealth
# can pick m and c0

# what did lockwood pick for m and c0
# m = 0.96
# c0 is 18 but I know he adjusted the consumption grid
# I can keep m the same and change c0
# could try with 1 age to get c0s that make people indifferent 
# and then try again using this c0 with everyone. 




out = OptimalAnnuityAmount(0.85, gender="male", age=65, year=2015, asset_start_point=200, income_start_point=50)
typeof(out)

df = DataFrame(A=Float64[], B=Int64[])


potential_loading_factors = [i / 100 for i in 50:100]


append!(df, out)
# now see what factor people stop demanding annuities at 
for fac in potential_loading_factors
    OptimalAnnuityAmount(fac; gender="male", age=65, year=2015, asset_start_point=200, income_start_point=50)
end
# why is this so slow
