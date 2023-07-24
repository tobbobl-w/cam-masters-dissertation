

include("0__functions.jl")
include("1__lifecycle_with_income_function.jl")
include("4__bequest_functions.jl")

years = cat(repeat([2005], 26), [i for i in 2006:2015], dims=1)
ages = cat([i for i in 50:75], repeat([50], 10), dims=1)

year_ages_to_run_bequest_model = [years ages]

birth_years = year_ages_to_run_bequest_model[:, 1] - year_ages_to_run_bequest_model[:, 2]

println(year_ages_to_run_bequest_model)

# ------------- run bequest models ------------------

RunAllBequests = function ()

    for row in eachrow(year_ages_to_run_bequest_model)
        year = row[1]
        age = row[2]
        print(row)
        RetirementLifecycleWithIncomeBequests(
            bequests=true,
            age=age,
            gender="male",
            life_prob_types="objective",
            year=year,
            id_wave="",
            c0=20000)
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

end

# ------------------ run non-bequest models ------------------

RunAllNonBequest = function ()

    for row in eachrow(year_ages_to_run_bequest_model)
        year = row[1]
        age = row[2]
        print(row)
        RetirementLifecycleWithIncomeBequests(
            bequests=false,
            age=age,
            gender="male",
            life_prob_types="objective",
            year=year,
            id_wave="",
            c0=20000)
    end


    for row in eachrow(year_ages_to_run_bequest_model)
        year = row[1]
        age = row[2]
        RetirementLifecycleWithIncomeBequests(
            bequests=false,
            age=age,
            gender="female",
            life_prob_types="objective",
            year=year,
            id_wave="",
            c0=20000)
    end
end