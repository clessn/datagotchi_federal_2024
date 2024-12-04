# Définir un vecteur de noms et un vecteur de couleurs
cluster_names <- c(
  "1. Sophia - Non-franco Urbaine",
  "2. Stéphane - Travailleur Discret",
  "3. Éloïse - Jeune Sportive",
  "4. Gabriel - L'Urbain Raffiné",
  "5. Zoé - Écolo Avant-gardiste",
  "6. Michel - Senior Traditionnaliste"
)

# Associer chaque cluster à une couleur distincte
cluster_colors <- c(
  "1. Sophia - Non-franco Urbaine" = "#f39741",
  "2. Stéphane - Travailleur Discret" = "#a50000",
  "3. Éloïse - Jeune Sportive" = "#45977a",
  "4. Gabriel - L'Urbain Raffiné" = "#f3f02e",
  "5. Zoé - Écolo Avant-gardiste" = "#e421ba",
  "6. Michel - Senior Traditionnaliste" = "#5f67b4")


# Filtrer les données uniquement pour le Bloc
data_vert <- data_long %>% 
  filter(party == "Green") %>%
  # Créer une variable factor pour les noms de cluster
  mutate(cluster_name = factor(cluster, levels = 1:6, labels = cluster_names))

# Déterminer le minimum et le maximum des jours
min_day <- 0
max_day <- max(data_vert$day)

# Créer une séquence de jours tous les 5 jours
breaks_days <- seq(from = min_day, to = max_day, by = 5)

# Graphique pour le Bloc avec noms et couleurs de cluster
plot_vert <- ggplot(data_vert, aes(x = day, y = probability, color = cluster_name)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = breaks_days, limits = c(min_day, max_day)) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(values = cluster_colors) +
  labs(
    title = "Évolution de la probabilité de vote pour les Verts",
    x = "Jour",
    y = "Probabilité",
    color = "Cluster"
  ) +
  clessnize::theme_clean_light()

# Chemin pour enregistrer le graphique
output_dir <- "/home/alexab/Dropbox/Ulaval/CLESSN/datagotchi_federal_2024/_SharedFolder_datagotchi_federal_2024/clustering/graph"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Enregistrer le graphique pour le Bloc
ggsave(
  filename = paste0(output_dir, "/2021_evolution_Vert.png"),
  plot = plot_vert,
  width = 10,
  height = 6
)
