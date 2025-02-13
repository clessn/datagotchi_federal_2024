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
df_pilot1_2022 <- read.csv("_SharedFolder_datagotchi_federal_2024/data/clustering/qc_2022/pilote-1-quebec-prov-2022.csv")
df_app_2022 <- readRDS("_SharedFolder_datagotchi_federal_2024/data/clustering/qc_2022/data-hub-clean-2022-10-27_clean.rds")

# Correction des noms de variables associées au Drink
# Les noms dans le pilot sont modifiés pour les noms dans l'app
df_pilot1_2022 <- df_pilot1_2022 %>%
  rename(
    cons_bubbleDrink = cons_sparklingDrink,
    cons_beerDrink = cons_regBeers,
    cons_microDrink = cons_microBeers,
    cons_cocktailDrink = cons_cocktailsDrink
  )

# Correction des noms de variables associées au dwelling
df_pilot1_2022 <- df_pilot1_2022 %>%
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

df_app_2022 <- df_app_2022 %>%
  mutate(ses_dwelling_Other = ifelse(
    ses_dwelling_Duplex == 1, # Variable uniquement dans l'app
    1,
    ses_dwelling_Other
  ))

# Sélection des variables communes
df_pilot1_2022 <- df_pilot1_2022 %>%
  select(all_of(variables_communes))

df_app_2022 <- df_app_2022 %>%
  select(all_of(variables_communes))

# Sauvegarder les données
write_rds(df_pilot1_2022, file = "_PrivateFolder_datagotchi_federal_2025/data/clustering/qc2022/01_pilot1_2022.rds")
write_rds(df_app_2022, file = "_PrivateFolder_datagotchi_federal_2025/data/clustering/qc2022/01_app_2022.rds")
