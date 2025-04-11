# Packages ---------------------------------------------------------------------

library(tidyverse)
library(cartessn)
library(clessnize)

# Assurez-vous que le package cartessn est inclus dans les dépendances
# Si ce n'est pas déjà fait, installez cartessn avec devtools::install_github("clessnverse/cartessn")

# Data -------------------------------------------------------------------------

## load raw data here

DataRaw <- read.csv("_SharedFolder_datagotchi_federal_2024/data/app/dataRaw/ECAN25_Data_20250305-20250410.csv")

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

## Riding attribution (optional, can be commented out if not needed) -------------
source("code/01_cleaning/app/transform_rta_to_ridings.R")

## Remove NA from quotas
#DataClean <- DataClean |> tidyr::drop_na(starts_with("ses"))

# Save -------------------------------------------------------------------------

saveRDS(DataClean, "_SharedFolder_datagotchi_federal_2024/data/app/dataClean/datagotchi2025_canada_app_20250410.rds")

