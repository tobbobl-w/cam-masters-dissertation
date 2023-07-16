# Simulate random income process

using CSV
using DataFrames
using Plots
using Dates
using Random
using StatsKit
using Distributions
# So I just want to simulate some arima paths


# Get shock distribution
perm = Normal(1, 1)
trans = Normal(0.1, 2)

# But isn't this just one normal with mean 1 and sd √(5)

# Set time periods
T = 100

function NormalRandomWalk(T=100000, θ=0.7)
    # intialise vector
    y = [1.0]
    ϵₜ₋₁ = 0
    for t in 1:T-1
        θ = t / 100
        yₜ = y[t]
        ϵₜ = rand(trans)
        yₜ₊₁ = yₜ + ϵₜ + θ * ϵₜ₋₁
        push!(y, yₜ₊₁)
        ϵₜ₋₁ = ϵₜ
    end
    return y
end

income = NormalRandomWalk()
income

Incomes = [NormalRandomWalk() for _ in 1:10]

plot(Incomes, legend=false)

# How can we estimate θ
# Now we have a different variance in each place

# even though I should be able to do this i cant


