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
  facet_wrap(~ cluster_name) + 
  labs(title = "Évolution du vote intent par cluster",
       x = "Date",
       y = "Proportion de répondants",
       color = "Parti") +
  scale_color_manual(values = party_colors) + 
  clessnize::theme_clean_light()

# Afficher le graphique
print(p)

# Sauvegarder le graphique
ggsave("_SharedFolder_datagotchi_federal_2024/graph/clustering/qc2022/07_cluster_vote_intent.png", plot = p, width = 12, height = 8)



# -----------------------------------------------------------------------------
# Graphique 2 : Facet par parti avec les courbes de clusters ------------------

p2 <- ggplot(results_by_day, aes(x = time, y = proportion, color = cluster_name)) +
  geom_line() +
  facet_wrap(~ op_intent) + 
  labs(title = "Évolution du vote intent par parti et par cluster",
       x = "Date",
       y = "Proportion de répondants",
       color = "Cluster") +
  clessnize::theme_clean_light()

# Affichage et sauvegarde du graphique 2
print(p2)
ggsave("_SharedFolder_datagotchi_federal_2024/graph/clustering/qc2022/07_cluster_vote_intent_par_parti.png", plot = p2, width = 12, height = 8)


# Calculer les proportions globales par cluster à partir de results_by_day
global_proportions <- results_by_day %>%
  group_by(cluster, cluster_name) %>%       
  summarise(total_n = sum(n), .groups = "drop") %>%  
  mutate(global_proportion = total_n / sum(total_n))

# Calcul du nombre total de réponses (n total)
overall_n <- sum(global_proportions$total_n)

# Création du titre incluant le n total
title_text <- paste("Proportion des Cluster dans l'app Datagotchi 2022 (n total =", overall_n, ")")

# Créer le graphique en barres
p3 <- ggplot(global_proportions, aes(x = reorder(cluster_name, -global_proportion), 
                                    y = global_proportion, 
                                    fill = cluster_name)) +
  geom_bar(stat = "identity") +
  # Ajouter le texte sur chaque barre : "n = x"
  geom_text(aes(label = paste("n =", total_n)), 
            vjust = -0.5, 
            size = 4) +
  labs(title = title_text,
       x = "Cluster",
       y = "Proportion de répondants") +
  scale_y_continuous(labels = scales::percent,
                     # Augmenter la limite supérieure pour laisser la place aux étiquettes
                     limits = c(0, max(global_proportions$global_proportion) * 1.1)) +
  clessnize::theme_clean_light() +
  theme(legend.position = "none")

# Afficher le graphique
print(p3)

# Sauvegarder le graphique
ggsave("_SharedFolder_datagotchi_federal_2024/graph/clustering/qc2022/07_cluster_global_proportions.png", 
       plot = p3, width = 12, height = 8)
