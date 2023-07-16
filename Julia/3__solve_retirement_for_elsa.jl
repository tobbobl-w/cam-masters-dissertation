using LinearAlgebra
using DelimitedFiles
using CSV
using DataFrames
using Plots
using JSON

# so first get functions from 4
# then read in elsa data
# then run solve using real wealth amounts and

elsa_dt = CSV.File("../data/ELSA/elsa_to_use/for_julia.csv") |>
          DataFrame

names(elsa_dt)

# so for each individual get gender and financial wealth 
# then run for all combos and save as jsons?
# does dc pension count in that?

age_gender_year_pension_combos = CSV.File("../data/ELSA/elsa_to_use/gender_pension_age_unique.csv") |>
                                 DataFrame


# best to have elsa data completely ready for life cycle model solving

# solve and save lifecycle model for each row in the dataframe. 

# save as json, first save the call
# and then save the dataframes that come from it
