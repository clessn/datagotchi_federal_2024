# Packages ---------------------------------------------------------------------
library(tidyverse)

# Data -------------------------------------------------------------------------

## load raw data here

data_raw <- haven::read_sav("_SharedFolder_datagotchi_federal_2024/data/pilote/data_raw/_previous/datagotchi_federal_pilot_simulated2.sav")

# Clean variables ---------------------------------------------------------

data_clean <- data.frame(id = 1:nrow(data_raw))

## ses -------------------------------------------------------------------------

source("code/01_cleaning/pilote/ses.R")


## lifestyle -------------------------------------------------------------------

source("code/01_cleaning/pilote/lifestyle.R")


## Values & Perceptions -------------------------------------------------------------------

source("code/01_cleaning/pilote/values.R")


## DV --------------------------------------------------------------

source("code/01_cleaning/pilote/dv.R")

## Attitudes -----------------------------------------------------------

source("code/01_cleaning/pilote/attitudes.R")

# Save -------------------------------------------------------------------------
saveRDS(data_clean, "_SharedFolder_datagotchi_federal_2024/data/pilote/data_clean_simulated_23-01-2025.rds")


