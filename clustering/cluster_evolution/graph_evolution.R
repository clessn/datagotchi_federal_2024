# Packages ---------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(clessnize)  # Assurez-vous que le package clessnize est installé

# Data -------------------------------------------------------------------

pred_centroid <- readRDS("_SharedFolder_datagotchi_federal_2024/clustering/data/pred_centroid.rds")

# Transformation des données ---------------------------------------------

# Convertir les données au format long pour faciliter la visualisation
data_long <- pred_centroid %>%
  pivot_longer(
    cols = c(Bloc, Conservative, Green, NDP, Liberal),
    names_to = "party",
    values_to = "probability"
  )

# Chemin pour enregistrer les graphiques
output_dir <- "/home/alexab/Dropbox/Ulaval/CLESSN/datagotchi_federal_2024/_SharedFolder_datagotchi_federal_2024/clustering/graph"

# Créer le dossier s'il n'existe pas
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Couleurs spécifiques pour les partis
party_colors <- c(
  "Liberal" = "#d71920",
  "Bloc" = "#1bb5a4",
  "Conservative" = "#003f72",
  "Green" = "#74b946",
  "NDP" = "#ff993f"
)

# Déterminer le minimum et le maximum des jours
# Si vous voulez que les jours commencent à 0
min_day <- 0
max_day <- max(data_long$day)

# Créer une séquence de jours tous les 5 jours
breaks_days <- seq(from = min_day, to = max_day, by = 5)

# Graphiques par parti ----------------------------------------------------

# Liste des partis
parties <- unique(data_long$party)

# Boucle sur chaque parti
for (p in parties) {
  data_p <- data_long %>% filter(party == p)
  
  p_plot <- ggplot(data_p, aes(x = day, y = probability, color = factor(cluster))) +
    geom_line(size = 1) +
    scale_x_continuous(breaks = breaks_days, limits = c(min_day, max_day)) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(
      title = paste("Probabilité du parti", p, "par cluster"),
      x = "Jour",
      y = "Probabilité",
      color = "Cluster"
    ) +
    clessnize::theme_clean_light()
  
  # Enregistrer le graphique
  ggsave(
    filename = paste0(output_dir, "/graph_par_parti_", p, ".png"),
    plot = p_plot,
    width = 10,
    height = 6
  )
}

# Graphiques par cluster --------------------------------------------------

# Liste des clusters
clusters <- unique(data_long$cluster)

# Boucle sur chaque cluster
for (c in clusters) {
  data_c <- data_long %>% filter(cluster == c)
  
  c_plot <- ggplot(data_c, aes(x = day, y = probability, color = party)) +
    geom_line(size = 1) +
    scale_color_manual(values = party_colors) +
    scale_x_continuous(breaks = breaks_days, limits = c(min_day, max_day)) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(
      title = paste("Probabilité des partis pour le cluster", c),
      x = "Jour",
      y = "Probabilité",
      color = "Parti"
    ) +
    clessnize::theme_clean_light()
  
  # Enregistrer le graphique
  ggsave(
    filename = paste0(output_dir, "/graph_par_cluster_", c, ".png"),
    plot = c_plot,
    width = 10,
    height = 6
  )
}
