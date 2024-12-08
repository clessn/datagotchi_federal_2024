# Packages ---------------------------------------------------------------------
library(dplyr)

# Data -------------------------------------------------------------------------

## load raw data here


data_raw <- haven::read_sav("_SharedFolder_datagotchi_federal_2024/data/data_raw/datagotchi_federal_pilot_December+2,+2024_14.31.sav")




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

# Save -------------------------------------------------------------------------
saveRDS(data_clean, "_SharedFolder_datagotchi_federal_2024/data/data_clean.rds")

