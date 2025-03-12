# Packages ----------------------------------------------------------------

library(dplyr)
library(tidyr)

# Load data ---------------------------------------------------------------

df_with_rci_and_clusters <- readRDS("_SharedFolder_datagotchi_federal_2024/data/potGrowth/02_pilote_with_rci_and_clusters.rds")

# Data Wrangling ----------------------------------------------------------
# Transformer les données en format long et calculer la moyenne par cluster et parti
df_aggregated_rci <- df_with_rci_and_clusters %>%
  pivot_longer(
    cols = starts_with("rci_"),  # Sélectionne toutes les colonnes commençant par "rci_"
    names_to = "party",          # Création d'une colonne "party" à partir des noms de colonnes
    names_prefix = "rci_",       # Retire le préfixe "rci_" des noms de colonnes
    values_to = "rci_score"      # Stocke les valeurs dans la colonne "rci_score"
  ) %>%
  group_by(cluster_name, party) %>%
  summarize(mean_rci = mean(rci_score, na.rm = TRUE), .groups = "drop")  # Moyenne du RCI par cluster et parti

# Save it ----------------------------------------------------------------

saveRDS(df_aggregated_rci, "_SharedFolder_datagotchi_federal_2024/data/potGrowth/03_aggregated_rci.rds")
