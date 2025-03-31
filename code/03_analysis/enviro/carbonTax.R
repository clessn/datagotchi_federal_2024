
# Load packages -----------------------------------------------------------
library(tidyverse)

# Load data ---------------------------------------------------------------

dataClean <- readRDS("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/datagotchi2025_canada_appPonderees_20250330.rds")

table(dataClean$tactical_proCarbonTax_numeric, dataClean$ses_genderFemale)
