# Packages ---------------------------------------------------------------------
library(tidyverse)

# Data -------------------------------------------------------------------------

## load raw data here

DataRaw <- read.csv("_SharedFolder_datagotchi_federal_2024/data/app/dataRaw/ECAN25_Data_20250305-20250318.csv")

# Clean variables ---------------------------------------------------------

DataClean <- data.frame(id = 1:nrow(DataRaw))

## ses -------------------------------------------------------------------------

source("code/01_cleaning/app/ses.R")

## lifestyle -------------------------------------------------------------------

source("code/01_cleaning/app/lifestyle.R")


## DV --------------------------------------------------------------

source("code/01_cleaning/app/dv.R")

## tactical ----------------------------------------------------------------

source("code/01_cleaning/app/tactical.R")

## Remove NA from quotas
# DataClean <- DataClean |> drop_na(starts_with("ses"))

# Save -------------------------------------------------------------------------

saveRDS(DataClean, "_SharedFolder_datagotchi_federal_2024/data/app/dataClean/datagotchi2025_canada_app_20250318.rds")


# Save for clustering usage ----------------------------------------------
# saveRDS(DataClean, "_SharedFolder_datagotchi_federal_2024/data/app/dataClean/datagotchi2025_canada_appClustering_20250318.rds")
