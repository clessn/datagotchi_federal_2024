# Packages ---------------------------------------------------------------------
library(tidyverse)

# Data -------------------------------------------------------------------------

## load raw data here

DataRaw <- haven::read_sav("_SharedFolder_datagotchi_federal_2024/data/pilote/dataRaw/datagotchiCanadaPilote_30janvier2025.sav")

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

saveRDS(DataClean, "_SharedFolder_datagotchi_federal_2024/data/pilote/DataClean/datagotchi2025_canada_pilot_20250301.rds")
