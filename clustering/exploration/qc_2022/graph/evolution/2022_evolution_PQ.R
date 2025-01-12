# Définir un vecteur de noms et un vecteur de couleurs
cluster_names <- c(
  "1. Jin - Universitaire Immigrant",
  "2. Steve - L'Homme de Plein Air",
  "3. James - Patriarche Anglophone",
  "4. Zoé - Écolo Avant-gardiste",
  "5. Isabelle - Classe moyenne Moderne",
  "6. Charlie - Urbain.e Queer",
  "7. Jayden - Sportif Altruiste",
  "8. Michel - Senior Traditionnaliste",
  "9. Geneviève et Philippe - Propriétaire Aisée",
  "10. Denise - Modeste Sans-voiture",
  "11. Gabriel - Urbain Raffiné"
)

# Associer chaque cluster à une couleur distincte
cluster_colors <- c(
  "1. Jin - Universitaire Immigrant" = "#8DD3C7",
  "2. Steve - L'Homme de Plein Air" = "#FFFFB3",
  "3. James - Patriarche Anglophone" = "#BEBADA",
  "4. Zoé - Écolo Avant-gardiste" = "#FB8072",
  "5. Isabelle - Classe moyenne Moderne" = "#80B1D3",
  "6. Charlie - Urbain.e Queer" = "#FDB462",
  "7. Jayden - Sportif Altruiste" = "#B3DE69",
  "8. Michel - Senior Traditionnaliste" = "#FCCDE5",
  "9. Geneviève et Philippe - Propriétaire Aisée" = "#D9D9D9",
  "10. Denise - Modeste Sans-voiture" = "#BC80BD",
  "11. Gabriel - Urbain Raffiné" = "#CCEBC5"
)


# Filtrer les données uniquement pour le CAQ
data_PQ <- data_long %>% 
  filter(party == "PQ") %>%
  # Créer une variable factor pour les noms de cluster
  mutate(cluster_name = factor(cluster, levels = 1:11, labels = cluster_names))

# Déterminer le minimum et le maximum des jours
min_day <- 0
max_day <- max(data_PQ$day)

# Créer une séquence de jours tous les 5 jours
breaks_days <- seq(from = min_day, to = max_day, by = 5)

# Graphique pour le Bloc avec noms et couleurs de cluster
plot_pq <- ggplot(data_PQ, aes(x = day, y = probability, color = cluster_name)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = breaks_days, limits = c(min_day, max_day)) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(values = cluster_colors) +
  labs(
    title = "Évolution de la probabilité de vote pour le PQ",
    x = "Jour",
    y = "Probabilité",
    color = "Cluster"
  ) +
  clessnize::theme_clean_light()

# Chemin pour enregistrer le graphique
output_dir <- "/home/alexab/Dropbox/Ulaval/CLESSN/datagotchi_federal_2024/_SharedFolder_datagotchi_federal_2024/clustering/graph/2022/réduit/pred"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Enregistrer le graphique pour le CAQ
ggsave(
  filename = paste0(output_dir, "/2022_evolution_PQ.png"),
  plot = plot_pq,
  width = 12,
  height = 8
)
