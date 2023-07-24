
json_idwave_folder = readdir("../data/ELSA/lifecycle_outputs/id_wave/")

GetJSON_IDWave = function (id_wave)
    file_to_get = json_idwave_folder[findfirst(occursin.(id_wave, json_idwave_folder))]

    json_filename = "../data/ELSA/lifecycle_outputs/id_wave/" *
                    file_to_get

    JSON3.read(json_filename)
end

# now we can use similar code to the bequest one

# first get data
col_types = Dict(:id_wave => String, :age => Int128, :year => Int128,
    :ever_dc_pen => String, :ever_db_pen => String, :public_pension => Float64,
    :fin_wealth => Float64)

idwave_df = CSV.File("../data/ELSA/elsa_to_use/for_julia.csv", types=col_types) |>
            DataFrame

idwave_df = idwave_df[Bool.(.!ismissing.(idwave_df.fin_wealth)).&Bool.(.!ismissing.(idwave_df.public_pension)), :]

idwave_df.pen_grid = FindClosestGridPoint.(
    idwave_df.public_pension, inc_grid_gap, inc_grid_points)

idwave_df.ass_grid = FindClosestGridPoint.(
    idwave_df.fin_wealth, asset_grid_gap, asset_grid_points)

sum(idwave_df.pen_grid .<= 100)
# sum(idwave_df.ass_grid. <=500  .& idwave_df.pen_grid .<= 100)


json_idwave_folder

good_assets = idwave_df.ass_grid .<= 500
good_inc = idwave_df.pen_grid .<= 100

# filter too wealthy and too high income
idwave_df = idwave_df[good_assets.&good_inc, :]

# make sure we have run the id_wave
CheckOccursIn(id_wave) = sum(occursin.(id_wave, json_idwave_folder)) == 1

idwave_df = idwave_df[CheckOccursIn.(idwave_df.id_wave), :]

optimal_asset = []


for row in eachrow(idwave_df)

    output_val = ToAnnuitiseOrNot(
        row["year"],
        row["age"],
        row["gender"],
        row["pen_grid"],
        row["ass_grid"],
        0.8, # Loading factor
        row["id_wave"])

    push!(optimal_asset, output_val)
end

# "do not annuitise"
optimal_asset[length.(optimal_asset).==2]
# 67 


not_worked_for = optimal_asset[optimal_asset.=="too poor to annuitise"]
# too poor 

worked_for = optimal_asset[length.(optimal_asset).==3]
ids_worked = idwave_df.id_wave[1:length(optimal_asset)][length.(optimal_asset).==3]

worked_dt = DataFrame(worked_for)

worked_dt.id_wave = ids_worked

idwave_df[:, "year"][1]

joined_df = innerjoin(worked_dt, idwave_df, on=:id_wave)

joined_df.annuity_spend = joined_df.move_down_assets .* asset_grid_gap

joined_df.annuity_income = joined_df.move_up_income .* inc_grid_gap

joined_df.age
joined_df.gender
joined_df.year

CSV.write(
    "../data/ELSA/lifecycle_outputs/first_outputs_from_subjective_models.csv",
    joined_df)


histogram(joined_df.pen_grid)
histogram(joined_df.ass_grid)
histogram(joined_df.move_down_assets)
histogram(joined_df.move_up_income)


# wow 78% annuitisation with subjective 
sum(joined_df.move_down_assets) / sum(joined_df.ass_grid)


# Income goes up 130%
sum(joined_df.move_up_income) / sum(joined_df.pen_grid)

# I could see what happens when I increase even further the loading facotr



# ok we want consumption
# for objective models we can find the solved model at retirement age
# then solve with an annuity or without on




