# Packages ---------------------------------------------------------------------
library(dplyr)

# Data -------------------------------------------------------------------------

## load raw data here

<<<<<<< HEAD
data_raw <- haven::read_sav("_SharedFolder_datagotchi_federal_2024/data/datagotchi_federal_pilot_November+5,+2024_14.42.sav")
=======
data_raw <- haven::read_sav("_SharedFolder_datagotchi_federal_2024/data/datagotchi_federal_pilot_November+12,+2024_20.55.sav")
>>>>>>> ae1f9c1f4148d3ba4a6a380194fe2b34e15b4524

# Clean variables ---------------------------------------------------------

data_clean <- data.frame(id = 1:nrow(data_raw))

## ses -------------------------------------------------------------------------

source("code/cleaning/ses.R")


## lifestyle -------------------------------------------------------------------

source("code/cleaning/lifestyle.R")


## Values & Perceptions -------------------------------------------------------------------

source("code/cleaning/values_perceptions.R")


## DV --------------------------------------------------------------

source("code/cleaning/dv.R")

## Attitudes -----------------------------------------------------------

source("code/cleaning/attitudes.R")
