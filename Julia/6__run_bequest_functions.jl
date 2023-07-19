

include("0__functions.jl")
include("1__lifecycle_with_income_function.jl")
include("5__bequest_functions.jl")

years = cat(repeat([2005], 26) , [i for i in 2006:2015], dims =1 )
ages = cat([i for i in 50:75], repeat([50], 10), dims =1 )

year_ages_to_run_bequest_model = [years ages] 



for row in eachrow(year_ages_to_run_bequest_model)
    year = row[1]
    age = row[2]
    RetirementLifecycleWithIncomeBequests(
            bequests=true,
            age=age,
            gender="male",
            life_prob_types="objective",
            year=year,
            id_wave="", 
            c0 = 20000)
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
            c0 = 20000)
end 

file


# going to quickly change the names to birth-year gender 
# as opposed to year and age

pwd()

cp() # copy files
mv() # move and rename files

json_files = readdir(
    "../Data/ELSA/lifecycle_outputs/beq_testing")
occursin("birth_year", "safasdfasfbirth_year")

ChangeJsonName = function(
        json_file_name, 
        folder_name = "../Data/ELSA/lifecycle_outputs/beq_testing/")
    
    if occursin(".json", json_file_name) 
        if occursin("birth_year", json_file_name)
                return "done already"
        else
        println(json_file_name)
        year_regex = r"((?<=objective_)\d{4})" # first occurence 
        age_regex = r"^\d{2}"

        year_out = match(year_regex, json_file_name).match
        age_out = match(age_regex, json_file_name).match
        age =  parse(Int, String(age_out))
        year = parse(Int, String(year_out))
        birth_year = year - age  

        new_string = replace(json_file_name, year_regex => s"")
        new_string = replace(new_string, age_regex => s"")
        new_string = string("birth_year_", birth_year)* new_string
        new_string = replace(new_string, "__" => "_")
        
        mv(folder_name * json_file_name, folder_name * new_string)
        
        return "done"
        end 
    else 
        return "not a json"
    end 
end 

ChangeJsonName.(json_files)

# Ok now we want to check optimal annuity rates in our real life data. 
# Need a function that takes year, age, wealth and income 
# and calculates optimal annuity amount, and consumption 

# first step in this is getting the json
# to do this we calc birth year
# and then see which of the bequest files line up with a year-age that was allowed

GetJson = function (
    birth_year=1955,
    gender="female" ,
    bequests=true,
    life_prob_types="subjective")

    if bequests == true
        folder_name = "../Data/ELSA/lifecycle_outputs/beq_testing/"
        if life_prob_types == "subjective"
            json_string = string("birth_year_", 1930,"_beqtrue_", gender, "_objective_20000.json")
            full_file_name = folder_name * json_string
            if isfile(full_file_name)
                json_in = JSON3.read(full_file_name)
            else 
                return "cant find file"
            end 
        end 
    end 
end

GetJson(1930)
json_in1 = GetJson(1950)

value_func1 = json_in1["value_func"]
Int(length(value_func1)/(inc_grid_points*asset_grid_points))

# For each year-age-pension-wealth lets find if someone should annuitise


ToAnnuitiseOrNot = function(year, age, gender, state_pension, wealth)

    birth_year = year - age
    json_in = GetJson(birth_year, gender)

    value_func = json_in["value_func"]

    years_of_life_left = Int(length(value_func)/(inc_grid_points*asset_grid_points))
    
    value_func = reshape(value_func, inc_grid_points, asset_grid_points, years_of_life_left)
    
    first_age = 1 + terminal_age - years_of_life_left

    # Now we select the year of the value function where they have to make an annuitisation decision
    decision_year_index = 1 + age - first_age
println(decision_year_index)
    # This is the value matrix in the year we care about
    # Now we see if moving along the income dimension makes up for going down the asset dimension
    value_func_at_year = value_func[:, :, decision_year_index]

    if size(value_func_at_year) != ( inc_grid_points, asset_grid_points)
        println("Wrong dimensions: stopping")
        return "Wrong dimensions: stopping"
    end 


    opt_annuity = OptimalAnnuityAmount_beq(
        state_pension, wealth, value_func = value_func_at_year,
        gender = gender, age = age, year = year)
    
    return opt_annuity 
end 

ToAnnuitiseOrNot(2012, 65, "male", 30, 250)

