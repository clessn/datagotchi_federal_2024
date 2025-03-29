# Packages ----------------------------------------------------------------
library(dplyr)

# Data -------------------------------------------------------------------

df_pilote_with_rci <- readRDS("_SharedFolder_datagotchi_federal_2024/data/potGrowth/01_pilote_with_rci.rds")    
df_pilote_with_clusters <- readRDS("_SharedFolder_datagotchi_federal_2024/data/clustering/can_2025/03_pilot_2025.rds")

# Assign clusters and RCI, new dataset --------------------------------------------------------
# Sélectionner les variables issues de dfIssues
df_pilote_with_rci <- df_pilote_with_rci %>%
  select(id, starts_with("rci_"))

# Sélectionner les variables clusters de dfUsedForClustering
df_pilote_with_clusters <- df_pilote_with_clusters %>%
  select(id, cluster_name)

# Fusionner les deux jeux de données sur id
df_with_rci_and_clusters <- df_pilote_with_rci %>%
  inner_join(df_pilote_with_clusters, by = "id")

# Save it ----------------------------------------------------------------

saveRDS(df_with_rci_and_clusters, "_SharedFolder_datagotchi_federal_2024/data/potGrowth/02_pilote_with_rci_and_clusters.rds")

