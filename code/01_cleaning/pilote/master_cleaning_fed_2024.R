# Packages ---------------------------------------------------------------------
library(tidyverse)

# Data -------------------------------------------------------------------------

## load raw data here

DataRaw <- haven::read_sav("_SharedFolder_datagotchi_federal_2024/data/pilote/data_raw/datagotchiCanadaPilote_30janvier2025.sav")

# Clean variables ---------------------------------------------------------

DataClean <- data.frame(id = 1:nrow(DataRaw))

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
saveRDS(Data_clean, "_SharedFolder_datagotchi_federal_2024/data/pilote/data_clean_simulated_23-01-2025.rds")


