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
DataPilot <- readRDS("_SharedFolder_datagotchi_federal_2024/data/pilote/dataClean/datagotchi2025_canada_pilotClustering_20250310.rds")
DataApp <- readRDS("")

# Sélection des variables communes
DataPilot <- DataPilot %>%
  select(all_of(variables_communes))

DataApp <- DataApp %>%
  select(all_of(variables_communes))

# Sauvegarder les données
write_rds(DataPilot, file = "_PrivateFolder_datagotchi_federal_2025/data/clustering/can2025/01_pilot_2025.rds")
write_rds(DataApp, file = "_PrivateFolder_datagotchi_federal_2025/data/clustering/can2025/01_app_2025.rds")