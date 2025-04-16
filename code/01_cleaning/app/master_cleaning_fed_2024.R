source("code/01_cleaning/app/package_checks.R")

# Data -------------------------------------------------------------------------
## Mettre le chemin vers le fichier de données ici
## Faire ainsi pour pouvoir nommer le fichier de sortie avec la date de fin

raw_file_path <- "_SharedFolder_datagotchi_federal_2024/data/app/dataRaw/ECAN25_Data_20250305-20250413.csv"
DataRaw <- read.csv(raw_file_path)

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
## Ne pas toucher le code ci-dessous, il sert à créer le nom du fichier de sortie
## et à le sauvegarder dans le bon dossier
## Le nom du fichier est de la forme YYYYMMDD_nXXXdatagotchi2025_canada_app.rds
## C'est fait automatiquement pour que le nom du fichier soit toujours le même
## Extract date from raw file name (using the end date)

file_name_parts <- unlist(strsplit(basename(raw_file_path), "_"))
date_range <- file_name_parts[length(file_name_parts)]  # Gets "20250305-20250410.csv"
end_date <- gsub("\\.csv$", "", unlist(strsplit(date_range, "-"))[2])  # Gets "20250410"
n_respondents <- nrow(DataClean)

file_name <- paste0(end_date, "_n", n_respondents, "datagotchi2025_canada_app.rds")
saveRDS(DataClean, paste0("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/", file_name))
saveRDS(DataClean, paste0("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/", "df_latest.rds"))
