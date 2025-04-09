library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(cartessn)

# 1. Chargement des données
data <- readRDS("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/datagotchi2025_canada_appPonderee_20250320.rds")

# 2. Chargement des données spatiales depuis cartessn
sf_ridings <- cartessn::spatial_canada_2022_electoral_ridings_aligned
sf_rta <- cartessn::spatial_canada_2021_rta

# 3. Extraction des 3 premiers caractères du code postal (RTA)
data$rta <- substr(data$ses_postalCode, 1, 3)

# 4. Utilisation de la fonction map_fsa_to_ridings de cartessn pour associer RTA à circonscriptions
# Cette fonction combine toutes les étapes intermédiaires de l'ancien script
mapping_results <- cartessn::map_fsa_to_ridings(
  sf_rta = sf_rta,
  sf_ridings = sf_ridings,
  tolerance = 50
)

# 5. Obtenir le mapping principal (RTA -> riding avec meilleure couverture)
rta_to_riding <- mapping_results$fsa_to_riding_mapping %>%
  select(rta, id_riding)

# 6. Joindre l'ID de circonscription à nos données
data <- data %>%
  left_join(rta_to_riding, by = "rta")

# 7. Vérifier le taux de jointure
matched_count <- sum(!is.na(data$id_riding))
total_count <- nrow(data)
match_rate <- matched_count / total_count * 100

print(paste("Nombre total de répondants:", total_count))
print(paste("Nombre de répondants avec une circonscription identifiée:", matched_count))
print(paste("Taux de correspondance:", round(match_rate, 2), "%"))

# 8. S'assurer que la variable weight existe, sinon créer une avec valeur 1
if(!"weight" %in% names(data)) {
  data$weight <- 1
}

# 9. Analyse des 4 chaînes de café par circonscription AVEC PONDÉRATION
lifestyle_eatMeatFreq_by_riding <- data %>%
  filter(!is.na(id_riding)) %>%
  group_by(id_riding) %>%
  summarize(
    # Somme des poids
    sum_weight = sum(weight, na.rm = TRUE),
    
    # Fans pondérés pour chaque chaîne
    jamais = sum((lifestyle_eatMeatFreq == 0) * weight, na.rm = TRUE),
    tres_rarement = sum((lifestyle_eatMeatFreq == 0.08) * weight, na.rm = TRUE),
    occasionnellement = sum((lifestyle_eatMeatFreq == 0.17) * weight, na.rm = TRUE),
    hebdomadaire = sum((lifestyle_eatMeatFreq == 0.33) * weight, na.rm = TRUE),
    plusieurs_fois_par_semaine = sum((lifestyle_eatMeatFreq == 0.67) * weight, na.rm = TRUE),
    quotidien = sum((lifestyle_eatMeatFreq == 0.83) * weight, na.rm = TRUE),
    plusieurs_fois_par_jour = sum((lifestyle_eatMeatFreq == 1) * weight, na.rm = TRUE),

    # Calcul des pourcentages pondérés
    jamais_pct = jamais / sum_weight * 100,
    tres_rarement_pct = tres_rarement / sum_weight * 100,
    occasionnellement_pct = occasionnellement / sum_weight * 100,
    hebdomadaire_pct = hebdomadaire / sum_weight * 100,
    plusieurs_fois_par_semaine_pct = plusieurs_fois_par_semaine / sum_weight * 100,
    quotidien_pct = quotidien / sum_weight * 100,
    plusieurs_fois_par_jour_pct = plusieurs_fois_par_jour / sum_weight * 100,

    # Nombre de répondants non-pondéré (pour référence)
    n_people = n()
  ) %>%
  ungroup()

# 10. Déterminer la chaîne dominante dans chaque circonscription
lifestyle_eatMeatFreq_by_riding <- lifestyle_eatMeatFreq_by_riding %>%
  mutate(
    # Créer une variable pour la fréquence dominante (celle avec le pourcentage pondéré le plus élevé)
    dominant_eatMeatFreq = case_when(
      jamais_pct >= tres_rarement_pct & jamais_pct >= occasionnellement_pct & jamais_pct >= hebdomadaire_pct & jamais_pct >= plusieurs_fois_par_semaine_pct & jamais_pct >= quotidien_pct & jamais_pct >= plusieurs_fois_par_jour_pct ~ "Jamais",
      tres_rarement_pct >= jamais_pct & tres_rarement_pct >= occasionnellement_pct & tres_rarement_pct >= hebdomadaire_pct & tres_rarement_pct >= plusieurs_fois_par_semaine_pct & tres_rarement_pct >= quotidien_pct & tres_rarement_pct >= plusieurs_fois_par_jour_pct ~ "Très rarement",
      occasionnellement_pct >= jamais_pct & occasionnellement_pct >= tres_rarement_pct & occasionnellement_pct >= hebdomadaire_pct & occasionnellement_pct >= plusieurs_fois_par_semaine_pct & occasionnellement_pct >= quotidien_pct & occasionnellement_pct >= plusieurs_fois_par_jour_pct ~ "Occasionnellement",
      hebdomadaire_pct >= jamais_pct & hebdomadaire_pct >= tres_rarement_pct & hebdomadaire_pct >= occasionnellement_pct & hebdomadaire_pct >= plusieurs_fois_par_semaine_pct & hebdomadaire_pct >= quotidien_pct & hebdomadaire_pct >= plusieurs_fois_par_jour_pct ~ "Hebdomadaire",
      plusieurs_fois_par_semaine_pct >= jamais_pct & plusieurs_fois_par_semaine_pct >= tres_rarement_pct & plusieurs_fois_par_semaine_pct >= occasionnellement_pct & plusieurs_fois_par_semaine_pct >= hebdomadaire_pct & plusieurs_fois_par_semaine_pct >= quotidien_pct & plusieurs_fois_par_semaine_pct >= plusieurs_fois_par_jour_pct ~ "Plusieurs fois par semaine",
      quotidien_pct >= jamais_pct & quotidien_pct >= tres_rarement_pct & quotidien_pct >= occasionnellement_pct & quotidien_pct >= hebdomadaire_pct & quotidien_pct >= plusieurs_fois_par_semaine_pct & quotidien_pct >= plusieurs_fois_par_jour_pct ~ "Quotidien",
      plusieurs_fois_par_jour_pct >= jamais_pct & plusieurs_fois_par_jour_pct >= tres_rarement_pct & plusieurs_fois_par_jour_pct >= occasionnellement_pct & plusieurs_fois_par_jour_pct >= hebdomadaire_pct & plusieurs_fois_par_jour_pct >= plusieurs_fois_par_semaine_pct & plusieurs_fois_par_jour_pct >= quotidien_pct ~ "Plusieurs fois par jour",
      TRUE ~ "Égalité"
    ),
    
    # Calcul du pourcentage associé à la fréquence dominante
    dominant_pct = case_when(
      dominant_eatMeatFreq == "Jamais" ~ jamais_pct,
      dominant_eatMeatFreq == "Très rarement" ~ tres_rarement_pct,
      dominant_eatMeatFreq == "Occasionnellement" ~ occasionnellement_pct,
      dominant_eatMeatFreq == "Hebdomadaire" ~ hebdomadaire_pct,
      dominant_eatMeatFreq == "Plusieurs fois par semaine" ~ plusieurs_fois_par_semaine_pct,
      dominant_eatMeatFreq == "Quotidien" ~ quotidien_pct,
      dominant_eatMeatFreq == "Plusieurs fois par jour" ~ plusieurs_fois_par_jour_pct,
      TRUE ~ NA_real_
    )
  )

# 11. Joindre les résultats aux données spatiales pour visualisation
sf_viande_map <- sf_ridings %>%
  left_join(lifestyle_eatMeatFreq_by_riding, by = "id_riding")

# 12. Sauvegarder les résultats intermédiaires
saveRDS(mapping_results, "_SharedFolder_datagotchi_federal_2024/reports/mapping_results_ridings_rta.rds")
saveRDS(lifestyle_eatMeatFreq_by_riding, "_SharedFolder_datagotchi_federal_2024/reports/coffee_battle_pondere.rds")
lifestyle_eatMeatFreq_by_riding <- readRDS("_SharedFolder_datagotchi_federal_2024/reports/coffee_battle_pondere.rds")
mapping_results <- readRDS("_SharedFolder_datagotchi_federal_2024/reports/mapping_results_ridings_rta.rds")

# 13. Prétraitement des données pour éliminer les NA avant la création de la carte
sf_viande_map_clean <- sf_viande_map %>%
  # Remplacer les NA par une valeur qui n'apparaîtra pas dans la légende
  mutate(dominant_eatMeatFreq = ifelse(is.na(dominant_eatMeatFreq), "Non disponible", dominant_eatMeatFreq))

# Création de la carte avec les données prétraitées
canada_viande_map <- cartessn::create_map(
  sf_viande_map_clean,
  value_column = "dominant_eatMeatFreq",
  title = "La fréquence de consommation de viande au Canada",
  subtitle = "Fréquence de consommation de viande par circonscription électorale",
  caption = "Source: Sondage Datagotchi 2025",
  legend_title = "Fréquence de consommation de viande",
  discrete_values = TRUE,
  fill_color = c(
      "Jamais" = "#ADD8E6",                   # Bleu clair
      "Très rarement" = "#6495ED",             # Bleu moyen
      "Occasionnellement" = "#40E0D0",          # Turquoise
      "Hebdomadaire" = "#90EE90",              # Vert clair
      "Plusieurs fois par semaine" = "#FFD700", # Doré
      "Quotidien" = "#FF8C00",                 # Orange foncé
      "Plusieurs fois par jour" = "#FF4500",    # Rouge orangé
      "Égalité" = "#D3D3D3",                   # Gris clair
      "Non disponible" = "#FFFFFF00"           # Transparent
  ),
  background = "light",
  border_size = 0.1
)


# Améliorer l'esthétique
canada_viande_map <- canada_viande_map +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, override.aes = list(alpha = 1))) +
  ggplot2::scale_fill_manual(
    values = c(
      "Jamais" = "#ADD8E6",                   # Bleu clair
      "Très rarement" = "#6495ED",             # Bleu moyen
      "Occasionnellement" = "#40E0D0",          # Turquoise
      "Hebdomadaire" = "#90EE90",              # Vert clair
      "Plusieurs fois par semaine" = "#FFD700", # Doré
      "Quotidien" = "#FF8C00",                 # Orange foncé
      "Plusieurs fois par jour" = "#FF4500",    # Rouge orangé
      "Égalité" = "#D3D3D3",                   # Gris clair
      "Non disponible" = "#FFFFFF00"           # Transparent
    ),
    breaks = c("Jamais", "Très rarement", "Occasionnellement", "Hebdomadaire", 
               "Plusieurs fois par semaine", "Quotidien", "Plusieurs fois par jour", "Égalité")
  ) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.margin = ggplot2::margin(t = 10, b = 10),
    plot.title = ggplot2::element_text(face = "bold", size = 18),
    plot.subtitle = ggplot2::element_text(size = 14, margin = ggplot2::margin(b = 20)),
    plot.caption = ggplot2::element_text(hjust = 1, margin = ggplot2::margin(t = 15)),
    legend.text = ggplot2::element_text(size = 10)
  )

# 14. Sauvegarder la carte
ggsave("canada_viande_map.png", canada_viande_map, width = 12, height = 8, dpi = 300)
canada_viande_map

# 15. Créer des cartes pour les principales régions urbaines
regions <- c("montreal", "toronto", "vancouver", "ottawa_gatineau", "quebec_city", "kitchener_waterloo", "london")

# 16. Créer des cartes individuelles pour chaque région
for (region in regions) {
  # Extraire la région avec crop_map
  region_map <- cartessn::crop_map(sf_viande_map, region)
  
  # Prétraitement des données régionales pour éliminer les NA
  region_map_clean <- region_map %>%
    mutate(dominant_eatMeatFreq = ifelse(is.na(dominant_eatMeatFreq), "Non disponible", dominant_eatMeatFreq))
  
  # Créer la carte thématique
  region_viande_map <- cartessn::create_map(
    region_map_clean,
    value_column = "dominant_eatMeatFreq",
    title = paste("La fréquence de consommation de viande à", gsub("_", " ", region)),
    subtitle = "Fréquence de consommation de viande par circonscription électorale",
    caption = "Source: Sondage Datagotchi 2025",
    legend_title = "Fréquence dominante",
    discrete_values = TRUE,
    fill_color = c(
      "Jamais" = "#ADD8E6",                   # Bleu clair
      "Très rarement" = "#6495ED",             # Bleu moyen
      "Occasionnellement" = "#40E0D0",          # Turquoise
      "Hebdomadaire" = "#90EE90",              # Vert clair
      "Plusieurs fois par semaine" = "#FFD700", # Doré
      "Quotidien" = "#FF8C00",                 # Orange foncé
      "Plusieurs fois par jour" = "#FF4500",    # Rouge orangé
      "Égalité" = "#D3D3D3",                   # Gris clair
      "Non disponible" = "#FFFFFF00"           # Transparent
    ),
    background = "light",
    border_size = 0.15
  )
  
  # Améliorer l'esthétique de la carte régionale
  region_viande_map <- region_viande_map +
    ggplot2::scale_fill_manual(
      values = c(
        "Jamais" = "#ADD8E6",                   # Bleu clair
        "Très rarement" = "#6495ED",             # Bleu moyen
        "Occasionnellement" = "#40E0D0",          # Turquoise
        "Hebdomadaire" = "#90EE90",              # Vert clair
        "Plusieurs fois par semaine" = "#FFD700", # Doré
        "Quotidien" = "#FF8C00",                 # Orange foncé
        "Plusieurs fois par jour" = "#FF4500",    # Rouge orangé
        "Égalité" = "#D3D3D3",                   # Gris clair
        "Non disponible" = "#FFFFFF00"           # Transparent
      ),
      breaks = c("Jamais", "Très rarement", "Occasionnellement", "Hebdomadaire", 
                 "Plusieurs fois par semaine", "Quotidien", "Plusieurs fois par jour", "Égalité")
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1)) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.margin = ggplot2::margin(t = 10, b = 10),
      plot.title = ggplot2::element_text(face = "bold", size = 16),
      plot.subtitle = ggplot2::element_text(size = 12, margin = ggplot2::margin(b = 15)),
      plot.caption = ggplot2::element_text(hjust = 1, margin = ggplot2::margin(t = 10)),
      legend.text = ggplot2::element_text(size = 9)
    )
  
  # Sauvegarder la carte régionale
  ggsave(paste0(region, "_viande_map.png"), region_viande_map, width = 10, height = 8, dpi = 300)
}

# 17. Créer une carte multi-panneaux pour comparer les grandes villes
# Prétraiter les données pour le multi-panel
sf_viande_map_clean <- sf_viande_map %>%
  mutate(dominant_eatMeatFreq = ifelse(is.na(dominant_eatMeatFreq), "Non disponible", dominant_eatMeatFreq))

# Créer une liste pour stocker les cartes de chaque région
region_maps <- list()
regions <- c("montreal", "toronto", "vancouver", "ottawa_gatineau")

# Créer une carte pour chaque région manuellement
for (i in seq_along(regions)) {
  region <- regions[i]
  
  # Extraire la région
  region_map <- cartessn::crop_map(sf_viande_map_clean, region)
  
  # Créer la carte
  region_maps[[i]] <- cartessn::create_map(
    region_map,
    value_column = "dominant_eatMeatFreq",
    title = gsub("_", " ", region),
    discrete_values = TRUE,
    fill_color = c(
      "Jamais" = "#ADD8E6",                   # Bleu clair
      "Très rarement" = "#6495ED",             # Bleu moyen
      "Occasionnellement" = "#40E0D0",          # Turquoise
      "Hebdomadaire" = "#90EE90",              # Vert clair
      "Plusieurs fois par semaine" = "#FFD700", # Doré
      "Quotidien" = "#FF8C00",                 # Orange foncé
      "Plusieurs fois par jour" = "#FF4500",    # Rouge orangé
      "Égalité" = "#D3D3D3",                   # Gris clair
      "Non disponible" = "#FFFFFF00"           # Transparent
    ),
    background = "light"
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "Jamais" = "#ADD8E6",                   # Bleu clair
      "Très rarement" = "#6495ED",             # Bleu moyen
      "Occasionnellement" = "#40E0D0",          # Turquoise
      "Hebdomadaire" = "#90EE90",              # Vert clair
      "Plusieurs fois par semaine" = "#FFD700", # Doré
      "Quotidien" = "#FF8C00",                 # Orange foncé
      "Plusieurs fois par jour" = "#FF4500",    # Rouge orangé
      "Égalité" = "#D3D3D3",                   # Gris clair
      "Non disponible" = "#FFFFFF00"           # Transparent
    ),
    breaks = c("Jamais", "Très rarement", "Occasionnellement", "Hebdomadaire", 
               "Plusieurs fois par semaine", "Quotidien", "Plusieurs fois par jour", "Égalité")
  )
}

# Combiner les cartes avec patchwork
multi_panel_map <- patchwork::wrap_plots(region_maps, ncol = 2) +
  patchwork::plot_annotation(
    title = "Fréquence de consommation de viande dans les grandes villes canadiennes",
    subtitle = "Fréquence dominante par circonscription électorale",
    caption = "Source: Sondage Datagotchi 2025",
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 14, hjust = 0.5),
      plot.caption = ggplot2::element_text(hjust = 1)
    )
  )
multi_panel_map

# 18. Sauvegarder la carte multi-panneaux
ggsave("multi_panel_viande_map.png", multi_panel_map, width = 16, height = 12, dpi = 300)

# 19. Analyse des proportions globales et création d'un graphique de synthèse
viande_summary <- lifestyle_eatMeatFreq_by_riding %>%
  summarize(
    total_ridings = n(),
    jamais_wins = sum(dominant_eatMeatFreq == "Jamais", na.rm = TRUE),
    tres_rarement_wins = sum(dominant_eatMeatFreq == "Très rarement", na.rm = TRUE),
    occasionnellement_wins = sum(dominant_eatMeatFreq == "Occasionnellement", na.rm = TRUE),
    hebdomadaire_wins = sum(dominant_eatMeatFreq == "Hebdomadaire", na.rm = TRUE),
    plusieurs_fois_par_semaine_wins = sum(dominant_eatMeatFreq == "Plusieurs fois par semaine", na.rm = TRUE),
    quotidien_wins = sum(dominant_eatMeatFreq == "Quotidien", na.rm = TRUE),
    plusieurs_fois_par_jour_wins = sum(dominant_eatMeatFreq == "Plusieurs fois par jour", na.rm = TRUE),
    ties = sum(dominant_eatMeatFreq == "Égalité", na.rm = TRUE)
  ) %>%
  mutate(
    jamais_pct = jamais_wins / total_ridings * 100,
    tres_rarement_pct = tres_rarement_wins / total_ridings * 100,
    occasionnellement_pct = occasionnellement_wins / total_ridings * 100,
    hebdomadaire_pct = hebdomadaire_wins / total_ridings * 100,
    plusieurs_fois_par_semaine_pct = plusieurs_fois_par_semaine_wins / total_ridings * 100,
    quotidien_pct = quotidien_wins / total_ridings * 100,
    plusieurs_fois_par_jour_pct = plusieurs_fois_par_jour_wins / total_ridings * 100,
    ties_pct = ties / total_ridings * 100
  )

print(viande_summary)

# Créer un graphique en barres pour visualiser les résultats globaux
summary_long <- data.frame(
  type = c("Jamais", "Très rarement", "Occasionnellement", "Hebdomadaire",
           "Plusieurs fois par semaine", "Quotidien", "Plusieurs fois par jour", "Égalité"),
  wins = c(
    viande_summary$jamais_wins,
    viande_summary$tres_rarement_wins,
    viande_summary$occasionnellement_wins,
    viande_summary$hebdomadaire_wins,
    viande_summary$plusieurs_fois_par_semaine_wins,
    viande_summary$quotidien_wins,
    viande_summary$plusieurs_fois_par_jour_wins,
    viande_summary$ties
  ),
  percentage = c(
    viande_summary$jamais_pct,
    viande_summary$tres_rarement_pct,
    viande_summary$occasionnellement_pct,
    viande_summary$hebdomadaire_pct,
    viande_summary$plusieurs_fois_par_semaine_pct,
    viande_summary$quotidien_pct,
    viande_summary$plusieurs_fois_par_jour_pct,
    viande_summary$ties_pct
  )
) %>%
  # Ordonner par nombre de victoires décroissant
  arrange(desc(wins))

# Créer le facteur ordonné pour l'axe des x
summary_long$type <- factor(summary_long$type, levels = summary_long$type)

viande_summary_plot <- ggplot2::ggplot(summary_long, aes(x = type, y = wins, fill = type)) +
  ggplot2::geom_bar(stat = "identity", width = 0.7) +
  ggplot2::geom_text(
    aes(label = paste0(round(percentage, 1), "%")),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 5
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "Jamais" = "#ADD8E6",                   # Bleu clair
      "Très rarement" = "#6495ED",             # Bleu moyen
      "Occasionnellement" = "#40E0D0",          # Turquoise
      "Hebdomadaire" = "#90EE90",              # Vert clair
      "Plusieurs fois par semaine" = "#FFD700", # Doré
      "Quotidien" = "#FF8C00",                 # Orange foncé
      "Plusieurs fois par jour" = "#FF4500",    # Rouge orangé
      "Égalité" = "#D3D3D3"                    # Gris clair
    )
  ) +
  ggplot2::labs(
    title = "Popularité des fréquences de consommation de viande au Canada",
    subtitle = "Nombre de circonscriptions où chaque fréquence est dominante",
    caption = "Source: Sondage Datagotchi 2025",
    x = NULL,
    y = "Nombre de circonscriptions"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "none",
    plot.title = ggplot2::element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
    axis.text.y = ggplot2::element_text(size = 10),
    panel.grid.major.x = ggplot2::element_blank()
  )
viande_summary_plot

# Sauvegarder le graphique
ggsave("viande_summary_plot.png", viande_summary_plot, width = 14, height = 8, dpi = 300)


# 20. Analyse par province
# D'abord, joindre les noms de provinces
province_names <- cartessn::names_canada_provinces
sf_coffee_map <- sf_coffee_map %>%
  left_join(province_names, by = "id_province")

# Résumer par province
coffee_by_province <- sf_coffee_map %>%
  st_drop_geometry() %>%
  group_by(name_province) %>%
  summarize(
    total_ridings = n(),
    tim_hortons_wins = sum(dominant_chain == "Tim Hortons 🍁", na.rm = TRUE),
    mcdonald_wins = sum(dominant_chain == "McDonald's 🇺🇸", na.rm = TRUE),
    starbucks_wins = sum(dominant_chain == "Starbucks ☕", na.rm = TRUE),
    secondcup_wins = sum(dominant_chain == "Second Cup 🇨🇦", na.rm = TRUE),
    ties = sum(dominant_chain == "Égalité", na.rm = TRUE)
  ) %>%
  mutate(
    tim_hortons_pct = tim_hortons_wins / total_ridings * 100,
    mcdonald_pct = mcdonald_wins / total_ridings * 100,
    starbucks_pct = starbucks_wins / total_ridings * 100,
    secondcup_pct = secondcup_wins / total_ridings * 100,
    ties_pct = ties / total_ridings * 100
  )

print(coffee_by_province)

# Créer un graphique de comparaison par province
# Préparer les données au format long
province_data_long <- coffee_by_province %>%
  select(name_province, tim_hortons_pct, mcdonald_pct, starbucks_pct, secondcup_pct, ties_pct) %>%
  pivot_longer(
    cols = c(tim_hortons_pct, mcdonald_pct, starbucks_pct, secondcup_pct, ties_pct),
    names_to = "chain",
    values_to = "percentage"
  ) %>%
  mutate(
    chain = case_when(
      chain == "tim_hortons_pct" ~ "Tim Hortons 🍁",
      chain == "mcdonald_pct" ~ "McDonald's 🇺🇸",
      chain == "starbucks_pct" ~ "Starbucks ☕",
      chain == "secondcup_pct" ~ "Second Cup 🇨🇦",
      chain == "ties_pct" ~ "Égalité"
    )
  )

# Créer le graphique par province
province_plot <- ggplot2::ggplot(
  province_data_long,
  aes(x = name_province, y = percentage, fill = chain)
) +
  ggplot2::geom_bar(stat = "identity", position = "dodge") +
  ggplot2::scale_fill_manual(
    values = c(
      "Tim Hortons 🍁" = "#dc143c",
      "McDonald's 🇺🇸" = "#ffc836",
      "Starbucks ☕" = "#036635",
      "Second Cup 🇨🇦" = "#003DA5",
      "Égalité" = "#D3D3D3"
    )
  ) +
  ggplot2::labs(
    title = "Préférences de café par province",
    subtitle = "Pourcentage de circonscriptions par chaîne dominante",
    caption = "Source: Sondage Datagotchi 2025",
    x = NULL,
    y = "% des circonscriptions",
    fill = "Chaîne"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  ggplot2::guides(fill = guide_legend(nrow = 1))

# Sauvegarder le graphique par province
ggsave("coffee_by_province_plot.png", province_plot, width = 12, height = 7, dpi = 300)

# 21. Sauvegarder les résultats finaux
saveRDS(coffee_summary, "R/shiny/data/coffee_summary.rds")
saveRDS(coffee_by_province, "R/shiny/data/coffee_by_province.rds")
saveRDS(sf_coffee_map, "R/shiny/data/sf_coffee_map.rds")
