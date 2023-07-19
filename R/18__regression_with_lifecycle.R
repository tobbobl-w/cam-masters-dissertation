library(data.table)
library(dplyr)
library(ggplot2)
library(jsonlite)


# So I take the individuals who are untreated
# i.e. early

# estimate their bequest motive so that it fits the rate of 
# annuitisation with forced annuitisation of DC pensions
# then run regression on the difference as we do in real life

# first read objective no bequests
# need to price annuities
# one annuity price for everyone
# based on annuitant lifetables
#   i should check whether they are predictions or if they are what actually happened

# ------------ read in jsons ------------------

## Ok now lets read in and see the rate of annuitisation we would expect
dir("../../data/ELSA/lifecycle_outputs/", full.names = T)


test = read_json("../../data/ELSA/lifecycle_outputs/80_beqfalse_male_objective_2011.json")

# quite slow to read 
# it was much faster in julia
# perhaps i should do this kind of analysis in julia
# then output a df and analyse that in R. 
# first we need to compare asset paths with and without forced annuitisation 


class(test)

names(test)
class(test$pol_func)
names(test$pol_func) %>% head

# 100 income point
# 300 asset points
# 110 - age , age points
age_indexes = 110 - test$age


test$pol_func %>% length()


array(sapply(seq(5, 15, 5), function(i) vec[]), dim = c(5, 2, 3)) 

