

include("0__functions.jl")
include("1__lifecycle_with_income_function.jl")
include("5__bequest_functions.jl")

years = cat(repeat([2005], 26), [i for i in 2006:2015], dims=1)
ages = cat([i for i in 50:75], repeat([50], 10), dims=1)

year_ages_to_run_bequest_model = [years ages]

birth_years = year_ages_to_run_bequest_model[:, 1] - year_ages_to_run_bequest_model[:, 2]

birth_years

println(year_ages_to_run_bequest_model)

for row in eachrow(year_ages_to_run_bequest_model)
    year = row[1]
    age = row[2]
    print(row)
    # RetirementLifecycleWithIncomeBequests(
    #         bequests=true,
    #         age=age,
    #         gender="male",
    #         life_prob_types="objective",
    #         year=year,
    #         id_wave="", 
    #         c0 = 20000)
end


for row in eachrow(year_ages_to_run_bequest_model)
    year = row[1]
    age = row[2]
    RetirementLifecycleWithIncomeBequests(
        bequests=true,
        age=age,
        gender="female",
        life_prob_types="objective",
        year=year,
        id_wave="",
        c0=20000)
end


# going to quickly change the names to birth-year gender 
# as opposed to year and age

# pwd()

# cp() # copy files
# mv() # move and rename files


# Ok now we want to check optimal annuity rates in our real life data. 
# Need a function that takes year, age, wealth and income 
# and calculates optimal annuity amount, and consumption 

# first step in this is getting the json
# to do this we calc birth year
# and then see which of the bequest files line up with a year-age that was allowed

GetJson = function (
    birth_year=1955,
    gender="female",
    bequests=true,
    life_prob_types="subjective")

    if bequests == true
        folder_name = "../Data/ELSA/lifecycle_outputs/beq_testing/"
        if life_prob_types == "subjective"
            json_string = string("birth_year_", birth_year, "_true_", gender, "_objective_20000.json")
            full_file_name = folder_name * json_string
            println(full_file_name)
            if isfile(full_file_name)
                json_in = JSON3.read(full_file_name)
            else
                return "cant find file"
            end
        end
    end
end



ToAnnuitiseOrNot = function (year, age, gender, state_pension, wealth)

    birth_year = year - age
    json_in = GetJson(birth_year, gender)

    value_func = json_in["value_func"]

    years_of_life_left = Int(length(value_func) / (inc_grid_points * asset_grid_points))

    value_func = reshape(value_func, inc_grid_points, asset_grid_points, years_of_life_left)

    first_age = 1 + terminal_age - years_of_life_left

    # Now we select the year of the value function where they have to make an annuitisation decision
    decision_year_index = 1 + age - first_age
    println(decision_year_index)

    # This is the value matrix in the year we care about
    # Now we see if moving along the income dimension makes up for going down the asset dimension
    value_func_at_year = value_func[:, :, decision_year_index]

    if size(value_func_at_year) != (inc_grid_points, asset_grid_points)
        println("Wrong dimensions: stopping")
        return "Wrong dimensions: stopping"
    end


    opt_annuity = OptimalAnnuityAmount_beq(
        state_pension, wealth, value_func=value_func_at_year,
        gender=gender, age=age, year=year)

    return opt_annuity
end

# ToAnnuitiseOrNot(2012, 65, "male", 30, 250)
# test = ToAnnuitiseOrNot(2012, 75, "male", 30, 250)

# # do those rough annuity amounts make sense

# test.move_down_assets 
# test.move_up_income 

# Annuity_GeneralDeathProbs(
#     test.move_down_assets*1000,
#     0.85,
#     GetObjectiveDeathProbs(gender = "male", year = 2012, age = 75))
# 7*500
#     # maybe I should make it closer 



# 2012 - 65
# 111 - 65

# Now for individuals in ELSA

