using LinearAlgebra
using DelimitedFiles
using CSV
using DataFrames
using Plots
using Pipe: @pipe


# so first get functions from 4
# then read in elsa data
# then run solve using real wealth amounts and

elsa_dt = CSV.File("../data/ELSA/elsa_to_use/for_julia.csv",) |>
          DataFrame

# that's slow
#  
Base.print_matrix(stdout, names(elsa_dt))

# so for each individual get gender and financial wealth 
# then run for all combos and save as jsons?
# does dc pension count in that?
small_elsa = @pipe filter(row -> row[:in_wave] == 1, elsa_dt) |>
    filter(row -> row[:retired] == 1, _)
 |> 
select(_, [:ragender, :age_at_interview,
    :public_pension, :fin_wealth, :retired_age]) 
    
elsa_dt.retired

# now round financial wealth to nearest 250
# pension to nearest 100?


small_elsa[small_elsa., :public_pension]

