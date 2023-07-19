

RetirementLifecycleWithIncomeBequests = function (;
    bequests=true,
    age=65,
    gender="male",
    life_prob_types="objective",
    year=2015,
    id_wave="", 
    c0 = 20000)

    # two ways to run this function
    # either with objective probs given by age, gender and year
    # or subjective probs given by id_wave

    if life_prob_types == "objective"
        # set file name to save json to. 
        birth_year = year - age
        json_filename = string(
            "birth_year_", birth_year,"_", bequests, "_",
            gender, "_", life_prob_types, "_", c0,".json")

        dir_name = "../data/ELSA/lifecycle_outputs/beq_testing/"
        if !isdir(dir_name)
            mkdir(dir_name)
        end

        filename_with_dir = dir_name * json_filename

        death_probs = GetObjectiveDeathProbs(gender=gender, age=age, year=year)

    elseif life_prob_types == "subjective"
        dir_name = "../data/ELSA/lifecycle_outputs/id_wave"
        if !isdir(dir_name)
            mkdir(dir_name)
        end
        json_filename = string(age, "_beq", bequests, "_", gender, "_", life_prob_types, "_", year, "_", id_wave, ".json")
        filename_with_dir = dir_name * json_filename

        death_probs = GetSubjectiveDeathProbs(id_wave_to_get=id_wave)

    end


    # Check if file exists already
    # if isfile(filename_with_dir)
    #     return "Already done file"
    # end


    ## So now for each level of income and current assets we find best consumption next period
    terminal_age = 110
    start_age = age

    # Dimensions are the state variables
    # Income, current assets and age
    value_array = ones(Float64, inc_grid_points, asset_grid_points, 1+ terminal_age - start_age)
    policy_array = ones(Float64, inc_grid_points, asset_grid_points, terminal_age - start_age)

    bequest_value = [bequest_lock(assets, c0 = c0, m = 0.94) for assets in asset_grid]

    # for each income level find the optimal asset policy function
    for inc in eachindex(income_grid)

        utility_matrix = utility_array[:, :, inc]

        # Initialize a vector to store value functions in
        # it is age + 1 columns long becuase the terminal value is stored in age at 111
        temp_value = zeros(asset_grid_points, 1+ terminal_age - start_age)
        
        # Initialize a matrix to policy functions
        # each colum is an age
        # each row amount of assets next period. 
        opt_policy_function = zeros(asset_grid_points, terminal_age - start_age)
        # Initialize a matrix to store the optimal policy function

        # solve model backwards
        t = 1
        for t in 1:(terminal_age-start_age)
            
            # age moves one back
            cur_age = 1+terminal_age - t
            value_index = cur_age - start_age

            # prob of death is year by year
            prob_of_death = death_probs[value_index]
            death_probs[end] = 1
            
            ev_g =  temp_value[:, value_index+1] * (1 - prob_of_death)
 
            # Optimal assets are a function of age and cur assets
            # we are in age already so just need to return 
            # a vector as long as assets, then optimal assets next period (i.e. policy func), 
            # and max value as well. 
            

            outputs = zeros(asset_grid_points, 3)
            # col 1 is current asset index
            # col 2 is value of that choice
            # col 3 is assets next period index

            for x in eachindex(asset_grid)

                # Create objective function
                # utility from consumption, discounted value next period, expected value from bequest
                # Note in the last period (i.e. when t = 1 and cur_age == terminal_age) prob of death = 1 and ev_g is 0
                obj_g = utility_matrix[x, :] + (ev_g * β) + (prob_of_death .* bequest_value) * β 

                max_and_index = findmax(obj_g) 
                # First element is the max
                # second element is the index that achieves the max
                # therefore second element is the policy function.

                outputs[x, :] = [x, max_and_index[1], max_and_index[2]]
            end

            # save value function for this period. 
            temp_value[:, value_index] = outputs[:, 2]

            # save policy function from this period
            opt_policy_function[:, value_index] = outputs[:, 3]

        end

        value_array[inc, :, :] = temp_value
        # save value array for a given income

        policy_array[inc, :, :] = opt_policy_function
        # save policy function from this period

    end

    outputs_to_save = (
        pol_func=policy_array,
        value_func=value_array,
        beq=bequests,
        age=age,
        gender=gender,
        life_prob_types=life_prob_types,
        year=year,
        id_wave)

    open(filename_with_dir, "w") do io
        JSON3.pretty(io, outputs_to_save)
    end
    
end



