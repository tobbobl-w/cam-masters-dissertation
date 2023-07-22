

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

GetJson = function (
    birth_year=1955,
    gender="female" ,
    bequests=true,
    life_prob_types="subjective")

    if bequests == true
        folder_name = "../Data/ELSA/lifecycle_outputs/beq_testing/"
        if life_prob_types == "subjective"
            json_string = string("birth_year_", birth_year,"_true_", gender, "_objective_20000.json")
            full_file_name = folder_name * json_string
            println(full_file_name)
            if isfile(full_file_name)
                json_in = JSON3.read(full_file_name)
                return json_in
            else 
                return "cant find file"
            end 
        end 
    end 
end



OptimalAnnuityAmount_beq = function (
    income_start_point, asset_start_point, loading_factor=0.85;
    value_func, gender="male", age=65, year=2015)

    # comparing different starting values on the income-asset array
    # and the loading factor gives the cost of the tradeoff. I.e. how far we move on each one
    # value func should just be a matrix 
    value_of_start = value_func[income_start_point, asset_start_point]

    death_probs = GetObjectiveDeathProbs(age=age, year=year, gender=gender)

    # make a rough matrix of grid points to move on assets vs income
    annuity_income_vec = [Annuity_GeneralDeathProbs(assets, loading_factor, death_probs) for assets in asset_grid]

    # I am not going to allow very small annuity purchaes since basically no one does this anyway
    # get rid of assets below 10000
    # This is how many grid points of income each annuity is associated with
    annuity_income_grid_points = FindClosestGridPoint.(annuity_income_vec, inc_grid_gap, inc_grid_points)

    # number_asset_grid_points by 2 matrix. 1st row is asset point
    assets_annuity_movement_mat = [Int.(asset_grid / asset_grid_gap) annuity_income_grid_points]

    how_close_can_annuity_amount_be = 70
    # only allowed to pick a point that is within x of a grid point otherwise can get better than actruarily fair
    select_vec = abs.(FindClosestGridPoint.(annuity_income_vec, inc_grid_gap, inc_grid_points) * inc_grid_gap - annuity_income_vec) .< how_close_can_annuity_amount_be 

    # we want to evalute the value of these points and pick the max
    # the first column is starting point on the asset grid
    # second column is starting point on the income grid
    assets_annuity_movement_mat = assets_annuity_movement_mat[select_vec, :]
    
    value_of_points = []

    for row_index in 1:sum(select_vec)

        # Move up the income grid
        income_point = income_start_point + assets_annuity_movement_mat[row_index, 2]

        # Move down the asset grid
        asset_point = asset_start_point - assets_annuity_movement_mat[row_index, 1]

        # cannot have negative assets
        if asset_point < 1
            continue
        end

        if income_point > inc_grid_points
            # stop individuals going outside of the grid
            income_point = inc_grid_points
        end     
        if asset_point < 0
            # do not allow negative asset holdings. 
            append!(value_of_points, -Inf)    
        else
        append!(value_of_points, value_func[income_point, asset_point])
        end
    end

    if value_of_points == []
        return "too poor to annuitise"
    end

    max_value_and_index = findmax(value_of_points)

    if max_value_and_index[1] > value_of_start
        # return the optimal amount to spend on annuities and the
        to_return = (
            value = max_value_and_index[1], 
            move_down_assets = assets_annuity_movement_mat[max_value_and_index[2], 1], 
            move_up_income = assets_annuity_movement_mat[max_value_and_index[2], 2])

        return to_return
    elseif max_value_and_index[1] < value_of_start
        return ("do not annuitise", value_of_start)
    end

end



ToAnnuitiseOrNot = function(year, age, gender, state_pension, wealth, loading_factor, id_wave = "none")

    if id_wave == "none"
        birth_year = year - age
        json_in = GetJson(birth_year, gender)
        to_add = 1
    elseif id_wave != "none"
        json_in = GetJSON_IDWave(id_wave)
        to_add = 0
    end

    value_func = json_in["value_func"]

    years_of_life_left = Int(length(value_func)/(inc_grid_points*asset_grid_points))
    
    value_func = reshape(value_func, inc_grid_points, asset_grid_points, years_of_life_left)
    
    # need the to add because in the early subjective models I just had 
    # (terminal_age - years_of_life) dimensions, rather than (1 + terminal_age - years_of_life)  
    first_age = to_add + terminal_age - years_of_life_left

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
        state_pension, wealth, loading_factor, value_func = value_func_at_year,
        gender = gender, age = age, year = year)
    
    return opt_annuity 
end 

