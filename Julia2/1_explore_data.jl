using CSV
using DataFrames
using XLSX
using ExcelFiles

using StatsKit

#df = CSV.read("")
#root = dirname(@__FILE__)
#joinpath(root, "hello.jl")

curdir = pwd()
data_path = abspath(joinpath(curdir, "../Data"))

# Item expenditure data set

item_expend = CSV.read(joinpath(data_path, "2011_UKDA-7272-tab/tab/2011_dv_set89_ukanon.tab"), DataFrame)

# Household info
household_info = CSV.read(joinpath(data_path, "2011_UKDA-7272-tab/tab/2011_dvhh_ukanon.tab"), DataFrame)

# Individual info
household_members_info = CSV.read(joinpath(data_path, "2011_UKDA-7272-tab/tab/2011_dvper_ukanon_v2.tab"), DataFrame)

# Expenditure look up
# Use ExcelFiles package
expend_lookup = DataFrame(load(joinpath(data_path, "2011_UKDA-7272-tab/mrdoc/excel/7272volume_d_expenditure_codes_2011.xls"), "Part 1"))


# Ok now we have all the data for 2011
# Can we get some aggregates, like consumption and income 

household_info
household_info.case

names(household_info)
names(expend_lookup)
names(household_members_info)

first(household_info, 4)

## Ok so we want to find the income columns and sum over those. 
names(household_info)
# Gross normal income of HRP by range

# p007p	Normal gross wage, salary- anonymised	 dvper
# gor	Government Office Region	 dvhh	gor	Yes

# a003	Household Reference Person	dvper	a003	Yes

"A003" in names(household_members_info)
"a003" in names(household_info)

first(names(household_info), 7)
first(household_info[:, :case], 10)
first(household_members_info[:, :A003], 10)

household_members_info[:, [:A003, :P007p]]

countmap(household_members_info[:, :A003])
# Mm this doesnt seem correct 


"P007p" in names(household_members_info)

names(household_members_info)

household_members_info[:, [:P007p]]

household_info[:, [:region]]

# Ok so need to join gor from hss to pvder
# what is the joining variable
# select colums first
# Working age adults only?

# I don't know if we actually can link households and individuals?
#   Surely there is a joining variable 





