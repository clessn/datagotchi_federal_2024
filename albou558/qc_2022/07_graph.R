# Libraries ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

# Data ---------------------------------------------------------------------
results_by_day <- readRDS("_PrivateFolder_datagotchi_federal_2025/data/clustering/qc2022/06_cluster_voteIntent.rds")

# Définir une couleur pour chaque parti ------------------------------------
party_colors <- c(
  "CAQ"    = "#05d2e0",
  "PQ"     = "#0043FE",
  "PLQ"    = "#FF2806",
  "QS"     = "#f88808",
  "PCQ"    = "#311c68",
  "Autre"  = "#73F986",
  "NoVote" = "#707373"
)

# Ajouter les noms des clusters ---------------------------------------------
results_by_day <- results_by_day %>%
  mutate(cluster_name = case_when(
    cluster == 1 ~ "Julie",
    cluster == 2 ~ "Jean-Guy",
    cluster == 3 ~ "Mélanie",
    cluster == 4 ~ "Karim",
    cluster == 5 ~ "Charlie",
    cluster == 6 ~ "Jacques",
    TRUE ~ as.character(cluster)  # au cas où d'autres valeurs existent
  ))

# Graphique -----------------------------------------------------------------
p <- ggplot(results_by_day, aes(x = time, y = proportion, color = op_intent)) +
  geom_line() +
  facet_wrap(~ cluster_name) +  # utiliser les noms des clusters
  labs(title = "Évolution du vote intent par cluster",
       x = "Date",
       y = "Proportion de répondants",
       color = "Parti") +
  scale_color_manual(values = party_colors) +  # assigner les bonnes couleurs
  clessnize::theme_clean_light()

# Afficher le graphique
print(p)

# Sauvegarder le graphique
ggsave("_SharedFolder_datagotchi_federal_2024/graph/clustering/qc2022/07_cluster_vote_intent.png", plot = p, width = 12, height = 8)



# -----------------------------------------------------------------------------
# Graphique 2 : Facet par parti avec les courbes de clusters ------------------
#
# Dans ce graphique, chaque facette correspond à un parti (op_intent).
# À l’intérieur de chaque facette, les courbes représentent l’évolution des
# proportions pour les différents clusters (identifiés par leur nom).
#
# Ici, on utilise l'esthétique 'color' pour distinguer les clusters.
# Vous pouvez, si vous le souhaitez, définir une palette personnalisée pour les clusters.
# Par défaut, ggplot attribuera automatiquement des couleurs différentes.
# Vous pouvez aussi différencier les clusters par des linetypes, par exemple.

p2 <- ggplot(results_by_day, aes(x = time, y = proportion, color = cluster_name)) +
  geom_line() +
  facet_wrap(~ op_intent) +  # un panneau par parti
  labs(title = "Évolution du vote intent par parti et par cluster",
       x = "Date",
       y = "Proportion de répondants",
       color = "Cluster") +
  clessnize::theme_clean_light()

# Affichage et sauvegarde du graphique 2
print(p2)
ggsave("_SharedFolder_datagotchi_federal_2024/graph/clustering/qc2022/07_cluster_vote_intent_par_parti.png", plot = p2, width = 12, height = 8)
