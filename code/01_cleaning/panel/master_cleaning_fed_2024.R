library(dplyr)

# Data -------------------------------------------------------------------------
## Mettre le chemin vers le fichier de données ici
## Faire ainsi pour pouvoir nommer le fichier de sortie avec la date de fin

raw_file_path <- "_SharedFolder_datagotchi_federal_2024/data/panel/panel_questionnaire_April 22, 2025_12.50.sav"
DataRaw <- haven::read_sav(raw_file_path)

# Clean variables ---------------------------------------------------------
DataClean <- data.frame(id = 1:nrow(DataRaw))

## ses -------------------------------------------------------------------------
source("code/01_cleaning/panel/ses.R")

## lifestyle -------------------------------------------------------------------
source("code/01_cleaning/panel/lifestyle.R")

## pondération ------------------------------------------------
source("code/01_cleaning/panel/ponderation.R")

# Save -------------------------------------------------------------------------
## Ne pas toucher le code ci-dessous, il sert à créer le nom du fichier de sortie
## et à le sauvegarder dans le bon dossier
## Le nom du fichier est de la forme YYYYMMDD_nXXXdatagotchi2025_canada_app.rds
## C'est fait automatiquement pour que le nom du fichier soit toujours le même
## Extract date from raw file name (using the end date)

file_name <- paste0(Sys.Date(), "_n", nrow(DataRaw), "datagotchi2025_panel.rds")
saveRDS(DataClean, paste0("_SharedFolder_datagotchi_federal_2024/data/panel/", file_name))


