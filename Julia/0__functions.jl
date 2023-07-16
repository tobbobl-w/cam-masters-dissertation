using CSV
using LinearAlgebra
using DataFrames
using Plots
using JSON3


function get_death_prob_function()
    # this function returns a function that gives death probs

    # First get some meta info
    first_two_lines = CSV.File("../data/ONS/males_transition_probs.csv", limit=2, header=false) |>
                      Tables.matrix

    header_info = first_two_lines[1, :]
    # first column is age 
    # next columns are year of birth

    # The second element of first column is the min age
    min_age = parse(Int, first_two_lines[2, 1])

    # Max year
    max_year = Int(header_info[2])


    # Objective transition probs from ons
    male_lftb = CSV.File("../data/ONS/males_transition_probs.csv") |>
                Tables.matrix

    female_lftb = CSV.File("../data/ONS/females_transition_probs.csv") |>
                  Tables.matrix
    # Each colum is a year of birth
    # each row is an age 
    return (male=male_lftb, female=female_lftb,
        top_year=max_year, min_age=min_age)
end

death_prob_matrices = get_death_prob_function()

# ONS life tables
function death_probabilities(; gender, year, age)
    if gender == "male"
        data = death_prob_matrices.male
    elseif gender == "female"
        data = death_prob_matrices.female
    end

    birth_year = year - age

    if birth_year < death_prob_matrices.top_year | age < death_prob_matrices.min_age
        println("Person is too young, we dont have death prob")
    end

    # plus 2 because first column is ages, and plus one because of indexing
    col_index = 2 + death_prob_matrices.top_year - birth_year # selects the correct column
    # second column of data is birth year 1975, so if if birth_year = 1975 we will select it

    start_row_index = 1 + age - death_prob_matrices.min_age # first row of interest

    death_probs = data[start_row_index:end, col_index]

    return death_probs
end

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



# asset accumulation
# budget constraint
bc_inc(a, a′, income) = a * (1 + r) - a′ + income
