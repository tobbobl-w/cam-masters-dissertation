module uk_rsp
using Revise, LinearAlgebra, DelimitedFiles
using CSV, DataFrames, Plots

include("death_probs.jl")
include("lifecycle_solve.jl")

export LifeCycleSolve, AssetPathFunction

end # module uk_rsp

