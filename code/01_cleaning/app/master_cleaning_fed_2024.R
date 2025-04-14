source("code/01_cleaning/app/package_checks.R")

# Data -------------------------------------------------------------------------
## load raw data here
DataRaw <- read.csv("_SharedFolder_datagotchi_federal_2024/data/app/dataRaw/ECAN25_Data_20250305-20250412.csv")

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

## music ----------------------------------------------------------------
source("code/01_cleaning/app/music_style.R")

## Riding attribution (optional, can be commented out if not needed) -------------
source("code/01_cleaning/app/transform_rta_to_ridings.R")

## pondération
source("code/01_cleaning/app/ponderation.R")

# Save -------------------------------------------------------------------------
current_date <- format(Sys.Date(), "%Y%m%d")
n_respondents <- nrow(DataClean)
file_name <- paste0(current_date, "_n", n_respondents, "datagotchi2025_canada_app.rds")
saveRDS(DataClean, paste0("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/", file_name))

