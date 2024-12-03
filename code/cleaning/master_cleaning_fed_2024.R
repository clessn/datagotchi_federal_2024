# Packages ---------------------------------------------------------------------
library(dplyr)

# Data -------------------------------------------------------------------------

## load raw data here

data_raw <- haven::read_sav("_SharedFolder_datagotchi_federal_2024/data/datagotchi_federal_pilot_November+17,+2024_10.08.sav")

# Clean variables ---------------------------------------------------------

data_clean <- data.frame(id = 1:nrow(data_raw))

## ses -------------------------------------------------------------------------

source("code/cleaning/ses.R")

## lifestyle -------------------------------------------------------------------

source("code/cleaning/lifestyle.R")

## Values & Perceptions -------------------------------------------------------------------

source("code/cleaning/values.R")

## DV --------------------------------------------------------------

source("code/cleaning/dv.R")

## Attitudes -----------------------------------------------------------

source("code/cleaning/attitudes.R")

