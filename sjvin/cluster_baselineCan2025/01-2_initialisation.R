#++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 01 Préparation initiales des données pour le clustering
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Script visant à initialiser les données pour la suite
# Les données préalablement nettoyées de façon générique
# sont préparées pour les fins de clustering.
#
# Dans le présent cas, il s'agit de sélectionner les variables
# communes au  pilot1 et à l'app

library(tidyverse)

# Charger les données
DataPilot <- readRDS("/Users/sarah-janevincent/Library/CloudStorage/Dropbox/clessn_capp/datagotchi/datagotchi_federal_2024/_SharedFolder_datagotchi_federal_2024/data/pilote/dataClean/datagotchi2025_canada_pilotClustering_20250310.rds")
DataApp <- readRDS("")

# Correction des noms de variables associées au Drink
# Les noms dans le pilot sont modifiés pour les noms dans l'app
DataPilot <- DataPilot %>%
  rename(
    cons_bubbleDrink = cons_sparklingDrink,
    cons_beerDrink = cons_regBeers,
    cons_microDrink = cons_microBeers,
    cons_cocktailDrink = cons_cocktailsDrink
  )

# Correction des noms de variables associées au dwelling
DataPilot <- DataPilot %>%
  rename(
    ses_dwelling_App = ses_dwelling_app,
    ses_dwelling_Loft = ses_dwelling_loft,
    ses_dwelling_Condo = ses_dwelling_condo,
    ses_dwelling_Tour = ses_dwelling_tour,
    ses_dwelling_Coop = ses_dwelling_coop,
    ses_dwelling_Mobile = ses_dwelling_mobile,
    ses_dwelling_Other = ses_dwelling_other
  ) %>%
  mutate(ses_dwelling_Other = if_else(
    ses_dwelling_semiDetached == 1, # Variables uniquement dans le pilot
    1,
    ses_dwelling_Other
  ))

DataApp <- DataApp %>%
  mutate(ses_dwelling_Other = ifelse(
    ses_dwelling_Duplex == 1, # Variable uniquement dans l'app
    1,
    ses_dwelling_Other
  ))



# Sélection des variables communes
DataPilot <- DataPilot %>%
  select(all_of(variables_communes))

DataApp <- DataApp %>%
  select(all_of(variables_communes))

# Sauvegarder les données
write_rds(DataPilot, file = "_PrivateFolder_datagotchi_federal_2025/data/clustering/can2025/01_pilot_2025.rds")
write_rds(DataApp, file = "_PrivateFolder_datagotchi_federal_2025/data/clustering/can2025/01_app_2025.rds")