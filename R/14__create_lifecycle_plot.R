library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(modelsummary)

source("./__ELSA_functions.R")

# want to read in all of those strings

vec_dt <- fread(
  "../../data/ELSA/lifecycle_outputs/model_ass_cons_paths/100006-6.csv",
  header = F
)

min_length <- vec_dt[1, V2] %>%
  strsplit(., ",", fixed = T) %>%
  unlist() %>%
  length()

lapply(vec_dt, strsplit, ", ") %>%
  unlist(recursive = F) %>%
  lapply(function(x) x[seq(1, min_length)]) %>%
  lapply(function(x) stringi::stri_replace(x, "", "[", regex = T, mode = "first"))



# start cut the first bits off the bequest and standard lifecycle models
# until they are same length as other ones

lapply(vec_dt, strsplit, ",") %>%
  unlist(recursive = F) %>%
  lapply(length) %>%
  unlist() %>%
  min()
# take last of this string






strsplit()
# three rows,
# sub
# stan
# beq


# should i start from age or retirement year?
