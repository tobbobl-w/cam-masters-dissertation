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


# now round financial wealth to nearest 250
# pension to nearest 100?


small_elsa[small_elsa., :public_pension]


# best to have elsa data completely ready for life cycle model solving