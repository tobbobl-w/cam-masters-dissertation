library(data.table)
library(dplyr)
library(stringr)

source("__ELSA_functions.R")

elsa <- fread("../../data/ELSA/elsa_to_use/elsa_ifs_finance.csv")

# These are social care related expenditures.
elsa$capam[elsa$capam > 0]
elsa$cahsc[elsa$cahsc > 0]


elsa[foodint == 1, mean(foodinl, na.rm = T)]
elsa[foodint == 1, mean(foodinu, na.rm = T)]

elsa[foodoutt == 1, mean(foodoutu, na.rm = T)]
elsa[foodoutt == 1, mean(foodoutl, na.rm = T)]

elsa[, mean(leisuret == 1, na.rm = T)] # leisure and clothes are weird.
# what data does fes have.

# these are all the elsa sub categories
# I will only use data that have
# low
elsa[, .(mean(foodoutt == 1 & foodint == 1 & clothest == 1 & leisuret == 1, na.rm = T))]


foodinl
foodinu
foodint

foodoutl
foodoutu
foodoutt

clothesl
clothesu
clothest

leisurel
leisureu
leisuret

transfersl
transfersu
transferst


# these are energy use variables
usesgas
useselec
usescoal
usespara
usesoil
useswood
usesotherf
gaselect
gaselecl
gaselecu
gaselecmeth
gast
gasl
gasu
gasmeth
elect
elecl
elecu
elecmeth
coall
coalu
coalt
paral
parau
parat
oill
oilu
oilt
woodl
woodu
woodt
otherfl
otherfu
otherft



# also can see if more people think that they are going to pass on a larger bequest as a result
# of the change. Could also make predictions using the models.
