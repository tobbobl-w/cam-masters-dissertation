setwd(dirname(rstudioapi::getSourceEditorContext()$path))


library(data.table)
library(dplyr)
library(readxl)

# Item expenditure data set
item_expend <- fread("../../data/2011_UKDA-7272-tab/tab/2011_dv_set89_ukanon.tab")

# Household info
household_info <- fread("../../data/2011_UKDA-7272-tab/tab/2011_dvhh_ukanon.tab")

# Individual info
household_members_info <- fread("../../data/2011_UKDA-7272-tab/tab/2011_dvper_ukanon_v2.tab")

# Expenditure look up
expend_lookup <- read_excel("../../data/2011_UKDA-7272-tab/mrdoc/excel/7272volume_d_expenditure_codes_2011.xls", sheet = "Part 1")
library(httpgd)
install.packages("httpgd")
regions <- fread("Gor_name, Gor
North East, 1
North West,	2
Merseyside,	3
Yorkshire and the Humber,	4
East Midlands,	5
West Midlands,	6
Eastern,     	7
London,	8
South East (except Greater London),	9
South West,	10
Wales,	11
Scotland,	12
Northern Ireland,	13", header = T)


dim(household_info[, 1:2])

household_info[, 1:2]
household_info$Gor
household_info$OAC3D
household_info$incanon

household_info[, .(income = mean(incanon)), by = .(Gor)][regions, on = "Gor"] %>%
  arrange(income)

x <- c(1:1000 / 100)
y <- gamma(x)

ggplot2::ggplot(data = data.frame(x = x, y = y)) +
  geom_



head(expend_lookup)

# Ok so government region is included in 2011
# I can assume it is also included in every other year
# Let's try and match it though for individuals and then see their total bread expenditure



# Cool so we can get some basic summary stats

# How to calculate inflation indices from the data though
# Need to look at changes in cost of the same basket
# And then update the basket each year?

# I could check some of these against housing costs from census

dim(household_info)
household_info$

  dim(item_expend)
names(item_expend)

hist(item_expend$Person)

length(unique(item_expend$case))


# Ok, so we match case from item_expend with case from household

household_info$case


nrow(data)

head(data)

length(unique(item_expend$COI_PLUS))

unique(data$Perstyp2)


# Let's start with a simple task
# Find one category we are interested in and return the value
# See how this differs with income

household_info
household_members_info$A

# It would be nice to have a look up table

# Cool they are in the excel folder..


expend_lookup$DivisionalCode

# food_codes <-
