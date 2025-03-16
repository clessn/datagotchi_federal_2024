library(dplyr)
library(stringr)

DataModel <- readRDS("_SharedFolder_datagotchi_federal_2024/data/pilote/data_clean/DataCleanPilot_2025Janv30.rds")

# Supposons que votre dataframe s'appelle DataModel et qu'il contient la variable ses_Postalcode
DataModel <- DataModel %>%
  mutate(
    # 1. Mettre le code postal en majuscules et retirer l'espace éventuel
    postal = toupper(ses_postalCode),
    postal_no_space = gsub(" ", "", postal),
    
    # 2. Extraire le FSA (les 3 premiers caractères) et le premier caractère
    FSA = substr(postal_no_space, 1, 3),
    first_letter = substr(postal_no_space, 1, 1),
    
    # 3. Création de la variable ses_province
    ses_postalProv = case_when(
      first_letter == "A" ~ "Newfoundland and Labrador",
      first_letter == "B" ~ "Nova Scotia",
      first_letter == "C" ~ "Prince Edward Island",
      first_letter == "E" ~ "New Brunswick",
      first_letter == "G" ~ "Eastern Quebec",
      first_letter == "H" ~ "Metropolitan Montréal",
      first_letter == "J" ~ "Western Quebec",
      first_letter == "K" ~ "Eastern Ontario",
      first_letter == "L" ~ "Central Ontario",
      first_letter == "M" ~ "Metropolitan Toronto",
      first_letter == "N" ~ "South-western Ontario",
      first_letter == "P" ~ "Northern Ontario",
      first_letter == "R" ~ "Manitoba",
      first_letter == "S" ~ "Saskatchewan",
      first_letter == "T" ~ "Alberta",
      first_letter == "V" ~ "British Columbia",
      first_letter == "X" ~ case_when(
        # Pour les codes commençant par X, distinguer Nunavut et Northwest Territories
        FSA %in% c("X0A", "X0B", "X0C") ~ "Nunavut",
        FSA %in% c("X0E", "X0G", "X1A") ~ "Northwest Territories",
        TRUE ~ "Northwest Territories and Nunavut"
      ),
      first_letter == "Y" ~ "Yukon Territory",
      TRUE ~ NA_character_
    ),
    
    # 4. Création de la variable region (groupement régional plus large)
    ses_region = case_when(
      first_letter %in% c("A", "B", "C", "E") ~ "atlantic",
      first_letter %in% c("G", "H", "J") ~ "quebec",
      first_letter %in% c("K", "L", "M", "N", "P") ~ "ontario",
      first_letter %in% c("R", "S", "T") ~ "prairie",
      first_letter == "V" ~ "british_columbia",
      first_letter %in% c("X", "Y") ~ "territories",
      TRUE ~ NA_character_
    )
  )


