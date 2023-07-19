## going to find bequest paramatmers that make the rate of annuitisation
# or could i fit the distribution of bequests

# cb 17.1 (1000)
# m 0.96

include("0__functions.jl")

## Set grid points
const asset_grid_points = 200
const asset_grid_gap = 3000
# define asset grid
const asset_grid = [i * asset_grid_gap for i in 1:asset_grid_points]

# define income grid
const inc_grid_points = 100
const inc_grid_gap = 500
const income_grid = [i * inc_grid_gap for i in 1:inc_grid_points]


consumption_array = CreateConsumptionArray()
utility_array = CreateUtilityArray()


# bequest function
# from lockwood RED 2012
bequest_lock(b; c0, m=0.9) = (m / (1 - m))^σ * (((m / (1 - m)) * c0) + b)^(1 - σ) / (1 - σ)
# but won't including bequests make utility lower

bequest_lock.([1000, 5000, 50000], c0 = 5000, m = 0.9)

# lets try running bequest function for different values of c0 and m 

# going to overwrite grid sizes so it is faster for now

RetirementLifecycleWithIncome_beq_edit = function (;
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
        json_filename = string(age, "_beq", bequests, "_", gender, "_", life_prob_types, "_", year, "_", c0,".json")

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

c0_vec = [i*1000 for i in 1:20]

# RetirementLifecycleWithIncome_beq_edit(c0 = 5000)
# RetirementLifecycleWithIncome_beq_edit(c0 = 10000)

for c0_value in c0_vec
    println(c0_value) 
    RetirementLifecycleWithIncome_beq_edit(c0 = c0_value)
end 
# oh this might actually take awhile
# less than a minute now. 

# I will plot consumption paths 
# and then see what we get


# need to change the asset path function to include something about bequests 
# hopefully this run will inform a bequest motive

call_values = (
bequests=true,
age=65,
gender="male",
life_prob_types="objective",
year=2015)

call_values.bequests


RetrievePolicyFunction_beq_edit = function (
    json_filename, 
    age)

    # check file exists
    if isfile(json_filename) == false
        return "file does not exist"
    end 

    # Read in json
    json_in = JSON3.read(json_filename)

    # Multi dimensional arrays were saved as long vectors so we reshape them  
    pol_func_readin = reshape(json_in["pol_func"], inc_grid_points, asset_grid_points, terminal_age - age)

    return pol_func_readin
end


RetrieveValueFunction_beq_edit = function (
    json_filename, 
    age)

    # Read in json
    json_in = JSON3.read(json_filename)

    # Multi dimensional arrays were saved as long vectors so we reshape them  
    pol_func_readin = reshape(json_in["value_func"], inc_grid_points, asset_grid_points, 1 + terminal_age - age)

    return pol_func_readin
end

ReturnDiffBeqMotiveAssets = function(beq_grid = 1, income_grid_point = 50, asset_grid_point_start = 100)

    json_filename = string( "../data/ELSA/lifecycle_outputs/beq_testing/", 
                call_values.age, "_beq", call_values.bequests,
                "_", call_values.gender, "_", call_values.life_prob_types,
                "_", call_values.year, "_", c0_vec[beq_grid ],".json")


    # function AssetPathFunction(opt_policy_function, asset_start_point, income_grid_point)
    # RetrievePolicyFunction_beq_edit(json_filename, 65)[80, :, :]
    # RetrieveValueFunction_beq_edit(json_filename, 65)[80, :, :]
    # esspecially weird because these people are high income individuals so 
    # can afford to not run down assets

    a_path = AssetPathFunction(
    RetrievePolicyFunction_beq_edit(json_filename, 65)[income_grid_point, :, :], 
    asset_grid_point_start, 
    income_grid_point)

    return a_path
end 

ReturnDiffBeqMotiveAssets(1)
     
plot([ReturnDiffBeqMotiveAssets(5, 50, 10).assets ReturnDiffBeqMotiveAssets(10, 50, 10).assets ReturnDiffBeqMotiveAssets(15, 50, 10).assets ReturnDiffBeqMotiveAssets(20, 50, 10).assets])

plot([ReturnDiffBeqMotiveAssets(20, 50, 40).assets ReturnDiffBeqMotiveAssets(20, 50, 80).assets])
# higher is slower build up at death
# This is quite good progress

# Later I can see what annuity demand is for these bequest people
# Why do we get the sudden drop to 0 is another question I have. 
# Now do some manasi work

# lets have a quick look at the paths. 






# -------------------- ANNUITY DEMAND --------------------
# FUNCTIONS TO USE
# Annuity_GeneralDeathProbs(annuity_cost, loading_factor, death_probs)


OptimalAnnuityAmount_beq = function (
    income_start_point, asset_start_point, loading_factor=0.85;
    value_func, gender="male", age=65, year=2015)

    # comparing different starting values on the income-asset array
    # and the loading factor gives the cost of the tradeoff. I.e. how far we move on each one
    # value func should just be a matrix 
    value_of_start = value_func[income_start_point, asset_start_point]

    death_probs = GetObjectiveDeathProbs(age=age, year=year, gender=gender)

    # make a rough matrix of grid points to move on assets vs income
    ann_inc_vec = [Annuity_GeneralDeathProbs(assets, loading_factor, death_probs) for assets in asset_grid]

    # I am not going to allow very small annuity purchaes since basically no one does this anyway
    # get rid of assets below 10000
    ann_inc_vec_grid_points = FindClosestGridPoint.(ann_inc_vec, inc_grid_gap, inc_grid_points)

    # Note this wont work for other
    ann_asset_grid_mat = [Int.(asset_grid / asset_grid_gap) ann_inc_vec_grid_points]

    # only allowed to pick a point that is within 100 of a grid point
    select_vec = abs.(FindClosestGridPoint.(ann_inc_vec, inc_grid_gap) * inc_grid_gap - ann_inc_vec) .< 100

    # we want to evalute the value of these points and pick the max
    # the first column is starting point on the asset grid
    # second column is starting point on the income grid
    points_to_evaluate = ann_asset_grid_mat[select_vec, :]

    
    value_of_points = []

    
    for row_index in 1:sum(select_vec)

        # Move up the income grid
        income_point = income_start_point + points_to_evaluate[row_index, 2]

        # Move down the asset grid
        asset_point = asset_start_point - points_to_evaluate[row_index, 1]

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

    max_value_and_index = findmax(value_of_points)

    if max_value_and_index[1] > value_of_start
        # return the optimal amount to spend on annuities and the
        return max_value_and_index
    elseif max_value_and_index[1] < value_of_start
        return ("do not annuitise" ,value_of_start)
    end

end


json_filename = string( "../data/ELSA/lifecycle_outputs/beq_testing/", 
        call_values.age, "_beq", call_values.bequests,
        "_", call_values.gender, "_", call_values.life_prob_types,
        "_", call_values.year, "_", c0_vec[20 ],".json")


val_func_test = RetrieveValueFunction_beq_edit(json_filename, 65)


OptimalAnnuityAmount_beq(
    50,200, gender = "male", age = 65, 
    year = 2015, value_func = val_func_test )
# even if high wealth and low income then do not annuitise much
json_filename


income_grid
    income_start_point = 50
asset_start_point = 50
loading_factor = 0.85
value_fun   


json_filename = string( "../data/ELSA/lifecycle_outputs/beq_testing/", 
            call_values.age, "_beq", call_values.bequests,
            "_", call_values.gender, "_", call_values.life_prob_types,
            "_", call_values.year, "_", c0_vec[1],".json")



test_val = RetrieveValueFunction_beq_edit(json_filename, 65)[80, :, :]

# these are values from picking optimal policy?
# each row is value of asset at that point?

test = [Int]

maxes = [findmax(col) for col in eachcol(test_val)]

mat = [maxe[k] for  maxe in maxes, k in 1:2]

plot(mat[:, 1])

# plot the value
# I think this is where something is going wrong




# ------------- Tests ---------------
# are choices different starting when 65 vs starting when 66 
# I dont think they should be but lets try and find out
# then we can run the model with a finer grid
# then we can compare consumption decision with forced annuitisation


RetirementLifecycleWithIncome_beq_edit(
    bequests=true,
    age=65,
    gender="male",
    life_prob_types="objective",
    year=2013,
    id_wave="", 
    c0 = 20000)

RetirementLifecycleWithIncome_beq_edit(
    bequests=true,
    age=66,
    gender="male",
    life_prob_types="objective",
    year=2014,
    id_wave="", 
    c0 = 20000)

    RetirementLifecycleWithIncome_beq_edit(
    bequests=true,
    age=70,
    gender="male",
    life_prob_types="objective",
    year=2018,
    id_wave="", 
    c0 = 20000)



    isfile.( json_filenames)
    dir_name = "../data/ELSA/lifecycle_outputs/beq_testing/"
 json_filenames = 
 [string(65, "_beq", bequests, "_", gender, "_", life_prob_types, "_", 2013, "_", c0,".json"), 
 string(66, "_beq", bequests, "_", gender, "_", life_prob_types, "_", 2014, "_", c0,".json"), 
 string(70, "_beq", bequests, "_", gender, "_", life_prob_types, "_",2018, "_", c0,".json")]
json_filenames = dir_name .*json_filenames


path_65 = AssetPathFunction(
    RetrievePolicyFunction_beq_edit(json_filenames[1], 65)[1, :, :], 
    100, 
    1)
    path_65.assets[5]

path_66 = AssetPathFunction(
    RetrievePolicyFunction_beq_edit(json_filenames[2], 66)[1, :, :], 
    98, 
    1)

    path_70 = AssetPathFunction(
    RetrievePolicyFunction_beq_edit(json_filenames[3], 70)[1, :, :], 
    90, 
    1)

    
    # I think these are identical
    # so we only need to run for age 60 all in one year () 
    plot([path_65.assets cat(missing, path_66.assets, dims =1) cat(repeat([missing], 5),  path_70.assets, dims =1 )])

    
sum(ismissing.(GetObjectiveDeathProbs(gender = "male", year = 2005, age = 50)))

# want to run this model from birth year 1930 through to 1965
# that is age 75 through 50 in 2005, 
# and then 50 in 2006-2015

years = cat(repeat([2005], 26) , [i for i in 2006:2015], dims =1 )
ages = cat([i for i in 50:75], repeat([50], 10), dims =1 )

year_ages_to_run_bequest_model = [years ages] 

