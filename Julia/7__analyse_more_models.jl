# analyse bequest runs

# For each year-age-pension-wealth lets find if someone should annuitise

include("0__functions.jl")
include("1__lifecycle_with_income_function.jl")

# first get data
col_types = Dict(:id_wave => String, :age => Int128, :year => Int128,
    :ever_dc_pen => String, :ever_db_pen => String, :public_pension => Float64,
    :fin_wealth => Float64)

idwave_df = CSV.File("../data/ELSA/elsa_to_use/for_julia.csv", types=col_types) |>
            DataFrame
idwave_df = idwave_df[Bool.(.!ismissing.(idwave_df.fin_wealth)) .& Bool.(.!ismissing.(idwave_df.public_pension)), :]
      
idwave_df.pen_grid = FindClosestGridPoint.(
    idwave_df.public_pension, inc_grid_gap, inc_grid_points)

idwave_df.ass_grid = FindClosestGridPoint.(
    idwave_df.fin_wealth, asset_grid_gap, asset_grid_points)

    sum(idwave_df.pen_grid .<= 100 )
    sum(idwave_df.ass_grid. <=500  .& idwave_df.pen_grid .<= 100)


good_assets = idwave_df.ass_grid .<=500   
good_inc = idwave_df.pen_grid .<= 100

# filter too wealthy and too high income
idwave_df = idwave_df[good_assets .& good_inc, :]
 

optimal_asset = []

for row in eachrow(idwave_df)
    println([row["pen_grid"] row["ass_grid"]])
    output_val = ToAnnuitiseOrNot(
        row["year"],
        row["age"], 
        row["gender"],
        row["pen_grid"], 
        row["ass_grid"])

    push!(optimal_asset, output_val)
end 



sum(optimal_asset .==  "do not annuitise")

not_worked_for = optimal_asset[optimal_asset .== "too poor to annuitise"]

worked_for =  optimal_asset[optimal_asset .!= "too poor to annuitise"]

ids_worked = idwave_df.id_wave[1:length(optimal_asset)][optimal_asset .!= "too poor to annuitise"]

worked_dt = DataFrame(worked_for)

worked_dt.id_wave = ids_worked

idwave_df[:, "year"][1]

joined_df = innerjoin(worked_dt, idwave_df, on = :id_wave)

joined_df.annuity_spend = joined_df.move_down_assets .* asset_grid_gap

joined_df.annuity_income = joined_df.move_up_income .* inc_grid_gap

joined_df.age
joined_df.gender
joined_df.year

# CSV.write(
#     "../data/ELSA/lifecycle_outputs/first_outputs_from_bequest_models.csv", 
#     joined_df)


histogram(joined_df.pen_grid)
histogram(joined_df.ass_grid)
histogram(joined_df.move_down_assets)
histogram(joined_df.move_up_income)


# wow 66% annuitisation with bequest motive
sum(joined_df.move_down_assets)/sum(joined_df.ass_grid)

# Income goes up 90%
sum(joined_df.move_up_income)/sum(joined_df.pen_grid)

# And we have people who are too poor to annutise their optimal amount

# is this vecuase people ahve not reached state pension age






# still get quite alot of annuitisation
# we havent accounted for inflation
# havent tried with housing

# shall we try consumption analysis


# now we have optimal annuity decisions we 
# can make consumption decisions




# mmmmm
# shall i try use these functions on 
# the subjective annuities?
# yeah that sounds fun
