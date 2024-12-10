# Filtrer les données pour le cluster 1
data_cluster6 <- data_long %>% 
  filter(cluster == 6)

# Déterminer le minimum et le maximum des jours
min_day <- 0
max_day <- max(data_cluster5$day)

# Créer une séquence de jours tous les 5 jours
breaks_days <- seq(from = min_day, to = max_day, by = 5)

# Graphique pour le cluster 1
plot_cluster6 <- ggplot(data_cluster6, aes(x = day, y = probability, color = party)) +
  geom_line(size = 1) +
    geom_vline(xintercept = 8, linetype = "dashed", color = "black") +
  scale_color_manual(values = party_colors) +
  scale_x_continuous(breaks = breaks_days, limits = c(min_day, max_day)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Probabilité des partis pour le cluster 6. Michel - Senior Traditionnaliste",
    x = "Jour",
    y = "Probabilité",
    color = "Parti"
  ) +
  clessnize::theme_clean_light() 

# Chemin pour enregistrer le graphique
output_dir <- "/home/alexab/Dropbox/Ulaval/CLESSN/datagotchi_federal_2024/_SharedFolder_datagotchi_federal_2024/clustering/graph"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Enregistrer le graphique pour le cluster 1
ggsave(
  filename = paste0(output_dir, "/2021_evolution_cluster6.png"),
  plot = plot_cluster6,
  width = 10,
  height = 6
)
