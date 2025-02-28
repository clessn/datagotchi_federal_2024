# Load packages  ----------------------------------------------------------

library(dplyr)
library(ggplot2)

# Load datas --------------------------------------------------------------

# Il faudra mettre à jour le jeu de données à utiliser. 
# Jeu de données sortant de l'application avec les clusters.

DataRaw <- readRDS("_PrivateFolder_datagotchi_federal_2025/data/clustering/qc2022/03_pilot1_2022.rds")

# Data Wrangling, Need for Cluster, Pol Party, IRC, Day -------------------

DataRaw %>% 
  select(cluster_name, party_id, dv_voteChoice, dv_potgrowthLPC,
         dv_potgrowthCPC, dv_potgrowthNDP, dv_potgrowthBQ,
         dv_potgrowthGPC, day)

# Création d'un faux jeu de données en attendant pour faire graph  --------------------------


