# Is it better to use the quantecon package and their functions as a starting point or should I instead
# code by hand to get as better understanding of the mechanics of the problem even if it takes longer. 
# Also I should structure this as a julia project from the start otherwise it is going to get messy. 
# what files do I need to do that?


data = ones(10, 10, 10)

typeof(data)

data = [i, j]








using FastGaussQuadrature
using Distributions
using Plots

# Parameters
β = 0.9     # Discount factor
T = 10      # Number of periods
R_mean = 1.0  # Mean return on savings
R_std = 0.1   # Standard deviation of return on savings

# Quadrature nodes and weights
nodes, weights = gausslegendre(10)  # Adjust the number of quadrature nodes as needed

# Define the income distribution
function income_distribution(t)
    if t % 2 == 0
        return Normal(2.0, 0.5)  # Even periods have mean 2.0 and standard deviation 0.5
    else
        return Normal(1.0, 0.2)  # Odd periods have mean 1.0 and standard deviation 0.2
    end
end

# Value iteration function
function value_iteration(T, β, R_mean, R_std, nodes, weights)
    # Initialize value function and policy
    V = zeros(T + 1)
    c_policy = zeros(T)

    # Value iteration loop
    for t = T:-1:1
        max_value = -Inf
        max_c = -Inf

        # Iterate over all possible consumption choices
        for c in 0:0.1:V[t]
            value = 0.0

            # Iterate over income and interest rate quadrature nodes
            for (i, node) in enumerate(nodes)
                income_dist = income_distribution(t)
                income = node * sqrt(2) * std(income_dist) + mean(income_dist)

                # Calculate utility and future value
                u = log(c + income)
                future_value = weights[i] * (V[t+1] * β + c * R_mean)

                # Calculate total value
                value += weights[i] * (u + β * future_value)
            end

            # Update maximum value and consumption choice
            if value > max_value
                max_value = value
                max_c = c
            end
        end

        # Update value function and policy
        V[t] = max_value
        c_policy[t] = max_c
    end

    return V, c_policy
end

# Solve the stochastic savings problem
V, c_policy = value_iteration(T, β, R_mean, R_std, nodes, weights)

# Plot the consumption policy function
periods = 1:T
consumption = c_policy
plot(periods, consumption, xlabel="Period", ylabel="Consumption", legend=false)
title!("Optimal Consumption Policy")
