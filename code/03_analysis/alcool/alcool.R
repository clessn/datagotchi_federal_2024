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
lifestyle_favAlcool_by_riding <- data %>%
  filter(!is.na(id_riding)) %>%
  group_by(id_riding) %>%
  summarize(
    # Somme des poids
    sum_weight = sum(weight, na.rm = TRUE),
    
    # Fans pondérés pour chaque chaîne
    beer_fans = sum((lifestyle_favAlcool == "beer") * weight, na.rm = TRUE),
    cocktail_fans = sum((lifestyle_favAlcool == "cocktail") * weight, na.rm = TRUE),
    dont_drink = sum((lifestyle_favAlcool == "dont_drink") * weight, na.rm = TRUE),
    spirits_fans = sum((lifestyle_favAlcool == "spirits") * weight, na.rm = TRUE),
    wine_fans = sum((lifestyle_favAlcool == "wine") * weight, na.rm = TRUE),

    # Calcul des pourcentages pondérés
    beer_fans_pct = beer_fans / sum_weight * 100,
    cocktail_fans_pct = cocktail_fans / sum_weight * 100,
    dont_drink_pct = dont_drink / sum_weight * 100,
    spirits_fans_pct = spirits_fans / sum_weight * 100,
    wine_fans_pct = wine_fans / sum_weight * 100,

    # Nombre de répondants non-pondéré (pour référence)
    n_people = n()
  ) %>%
  ungroup()

# 10. Déterminer la chaîne dominante dans chaque circonscription
lifestyle_favAlcool_by_riding <- lifestyle_favAlcool_by_riding %>%
  mutate(
    # Créer une variable pour le type d'alcool dominant (celui avec le plus de fans pondérés)
    dominant_alcool = case_when(
      beer_fans_pct >= cocktail_fans_pct & beer_fans_pct >= dont_drink_pct & beer_fans_pct >= spirits_fans_pct & beer_fans_pct >= wine_fans_pct ~ "Bière",
      cocktail_fans_pct >= beer_fans_pct & cocktail_fans_pct >= dont_drink_pct & cocktail_fans_pct >= spirits_fans_pct & cocktail_fans_pct >= wine_fans_pct ~ "Cocktail",
      dont_drink_pct >= beer_fans_pct & dont_drink_pct >= cocktail_fans_pct & dont_drink_pct >= spirits_fans_pct & dont_drink_pct >= wine_fans_pct ~ "Ne boivent pas",
      spirits_fans_pct >= beer_fans_pct & spirits_fans_pct >= cocktail_fans_pct & spirits_fans_pct >= dont_drink_pct & spirits_fans_pct >= wine_fans_pct ~ "Spiritueux",
      wine_fans_pct >= beer_fans_pct & wine_fans_pct >= cocktail_fans_pct & wine_fans_pct >= dont_drink_pct & wine_fans_pct >= spirits_fans_pct ~ "Vin",
      TRUE ~ "Égalité"
    ),
    
    # Pourcentage pour le type d'alcool dominant
    dominant_pct = case_when(
      dominant_alcool == "Bière" ~ beer_fans_pct,
      dominant_alcool == "Cocktail" ~ cocktail_fans_pct,
      dominant_alcool == "Ne boivent pas" ~ dont_drink_pct,
      dominant_alcool == "Spiritueux" ~ spirits_fans_pct,
      dominant_alcool == "Vin" ~ wine_fans_pct,
      TRUE ~ NA_real_
    )
  )

# 11. Joindre les résultats aux données spatiales pour visualisation
sf_alcool_map <- sf_ridings %>%
  left_join(lifestyle_favAlcool_by_riding, by = "id_riding")

# 12. Sauvegarder les résultats intermédiaires
saveRDS(mapping_results, "_SharedFolder_datagotchi_federal_2024/reports/mapping_results_ridings_rta.rds")
saveRDS(lifestyle_favAlcool_by_riding, "_SharedFolder_datagotchi_federal_2024/reports/coffee_battle_pondere.rds")
lifestyle_favAlcool_by_riding <- readRDS("_SharedFolder_datagotchi_federal_2024/reports/coffee_battle_pondere.rds")
mapping_results <- readRDS("_SharedFolder_datagotchi_federal_2024/reports/mapping_results_ridings_rta.rds")

# 13. Prétraitement des données pour éliminer les NA avant la création de la carte
sf_alcool_map_clean <- sf_alcool_map %>%
  # Remplacer les NA par une valeur qui n'apparaîtra pas dans la légende
  mutate(dominant_alcool = ifelse(is.na(dominant_alcool), "Non disponible", dominant_alcool))

# Création de la carte avec les données prétraitées
canada_alcool_map <- cartessn::create_map(
  sf_alcool_map_clean,
  value_column = "dominant_alcool",
  title = "La bataille des alcools au Canada",
  subtitle = "Alcool préféré par circonscription électorale",
  caption = "Source: Sondage Datagotchi 2025",
  legend_title = "Alcool dominant",
  discrete_values = TRUE,
  fill_color = c(
    "Bière" = "#f4a261",       # Orange doux
    "Cocktail" = "#e76f51",    # Saumon
    "Ne boivent pas" = "#2a9d8f",  # Teal
    "Spiritueux" = "#264653",  # Bleu profond
    "Vin" = "#9b2226",         # Bourgogne
    "Égalité" = "#D3D3D3",     
    "Non disponible" = "#FFFFFF00"  # Transparent
  ),
  background = "light",
  border_size = 0.1
)


# Améliorer l'esthétique
canada_alcool_map <- canada_alcool_map +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, override.aes = list(alpha = 1))) +
  ggplot2::scale_fill_manual(
    values = c(
      "Bière" = "#f4a261",
      "Cocktail" = "#e76f51",
      "Ne boivent pas" = "#2a9d8f",
      "Spiritueux" = "#264653",
      "Vin" = "#9b2226",
      "Égalité" = "#D3D3D3",
      "Non disponible" = "#FFFFFF00"  # Couleur transparente
    ),
    breaks = c("Bière", "Cocktail", "Ne boivent pas", "Spiritueux", "Vin", "Égalité")  # Exclure "Non disponible" de la légende
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
ggsave("canada_alcool_map.png", canada_alcool_map, width = 12, height = 8, dpi = 300)
canada_alcool_map

# 15. Créer des cartes pour les principales régions urbaines
regions <- c("montreal", "toronto", "vancouver", "ottawa_gatineau", "quebec_city", "kitchener_waterloo", "london")

# 16. Créer des cartes individuelles pour chaque région
for (region in regions) {
  # Extraire la région avec crop_map
  region_map <- cartessn::crop_map(sf_alcool_map, region)
  
  # Prétraitement des données régionales pour éliminer les NA
  region_map_clean <- region_map %>%
    mutate(dominant_alcool = ifelse(is.na(dominant_alcool), "Non disponible", dominant_alcool))
  
  # Créer la carte thématique
  region_alcool_map <- cartessn::create_map(
    region_map_clean,
    value_column = "dominant_alcool",
    title = paste("La bataille des alcools à", gsub("_", " ", region)),
    subtitle = "Type d'alcool dominant par circonscription électorale",
    caption = "Source: Sondage Datagotchi 2025",
    legend_title = "Type dominant",
    discrete_values = TRUE,
    fill_color = c(
      "Bière" = "#f4a261",
      "Cocktail" = "#e76f51",
      "Ne boivent pas" = "#2a9d8f",
      "Spiritueux" = "#264653",
      "Vin" = "#9b2226",
      "Égalité" = "#D3D3D3",
      "Non disponible" = "#FFFFFF00"  # Couleur transparente
    ),
    background = "light",
    border_size = 0.15
  )
  
  # Améliorer l'esthétique de la carte régionale
  region_alcool_map <- region_alcool_map +
    ggplot2::scale_fill_manual(
      values = c(
        "Bière" = "#f4a261",
        "Cocktail" = "#e76f51",
        "Ne boivent pas" = "#2a9d8f",
        "Spiritueux" = "#264653",
        "Vin" = "#9b2226",
        "Égalité" = "#D3D3D3",
        "Non disponible" = "#FFFFFF00"
      ),
      breaks = c("Bière", "Cocktail", "Ne boivent pas", "Spiritueux", "Vin", "Égalité")
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
  
  # Sauvegarder la carte
  ggsave(paste0(region, "_alcool_map.png"), region_alcool_map, width = 10, height = 8, dpi = 300)
}

# 17. Créer une carte multi-panneaux pour comparer les grandes villes
# Prétraiter les données pour le multi-panel
sf_alcool_map_clean <- sf_alcool_map %>%
  mutate(dominant_alcool = ifelse(is.na(dominant_alcool), "Non disponible", dominant_alcool))

# Créer une liste pour stocker les cartes de chaque région
region_maps <- list()
regions <- c("montreal", "toronto", "vancouver", "ottawa_gatineau")

# Créer une carte pour chaque région manuellement
for (i in seq_along(regions)) {
  region <- regions[i]
  
  # Extraire la région
  region_map <- cartessn::crop_map(sf_alcool_map_clean, region)
  
  # Créer la carte
  region_maps[[i]] <- cartessn::create_map(
    region_map,
    value_column = "dominant_alcool",
    title = gsub("_", " ", region),
    discrete_values = TRUE,
    fill_color = c(
      "Bière" = "#f4a261",
      "Cocktail" = "#e76f51",
      "Ne boivent pas" = "#2a9d8f",
      "Spiritueux" = "#264653",
      "Vin" = "#9b2226",
      "Égalité" = "#D3D3D3",
      "Non disponible" = "#FFFFFF00"
    ),
    background = "light"
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "Bière" = "#f4a261",
      "Cocktail" = "#e76f51",
      "Ne boivent pas" = "#2a9d8f",
      "Spiritueux" = "#264653",
      "Vin" = "#9b2226",
      "Égalité" = "#D3D3D3",
      "Non disponible" = "#FFFFFF00"
    ),
    breaks = c("Bière", "Cocktail", "Ne boivent pas", "Spiritueux", "Vin", "Égalité")
  )
}

# Combiner les cartes avec patchwork
multi_panel_map <- patchwork::wrap_plots(region_maps, ncol = 2) +
  patchwork::plot_annotation(
    title = "Bataille des alcools dans les grandes villes canadiennes",
    subtitle = "Type d'alcool dominant par circonscription électorale",
    caption = "Source: Sondage Datagotchi 2025",
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 14, hjust = 0.5),
      plot.caption = ggplot2::element_text(hjust = 1)
    )
  )
multi_panel_map

# 18. Sauvegarder la carte multi-panneaux
ggsave("multi_panel_alcool_map.png", multi_panel_map, width = 14, height = 12, dpi = 300)

# 19. Analyse des proportions globales et création d'un graphique de synthèse
alcool_summary <- lifestyle_favAlcool_by_riding %>%
  summarize(
    total_ridings = n(),
    beer_wins = sum(dominant_alcool == "Bière", na.rm = TRUE),
    cocktail_wins = sum(dominant_alcool == "Cocktail", na.rm = TRUE),
    nondrink_wins = sum(dominant_alcool == "Ne boivent pas", na.rm = TRUE),
    spirits_wins = sum(dominant_alcool == "Spiritueux", na.rm = TRUE),
    wine_wins = sum(dominant_alcool == "Vin", na.rm = TRUE),
    ties = sum(dominant_alcool == "Égalité", na.rm = TRUE)
  ) %>%
  mutate(
    beer_pct = beer_wins / total_ridings * 100,
    cocktail_pct = cocktail_wins / total_ridings * 100,
    nondrink_pct = nondrink_wins / total_ridings * 100,
    spirits_pct = spirits_wins / total_ridings * 100,
    wine_pct = wine_wins / total_ridings * 100,
    ties_pct = ties / total_ridings * 100
  )

print(alcool_summary)

# Créer un graphique en barres pour visualiser les résultats globaux
summary_long <- data.frame(
  type = c("Bière", "Cocktail", "Ne boivent pas", "Spiritueux", "Vin", "Égalité"),
  wins = c(
    alcool_summary$beer_wins,
    alcool_summary$cocktail_wins,
    alcool_summary$nondrink_wins,
    alcool_summary$spirits_wins,
    alcool_summary$wine_wins,
    alcool_summary$ties
  ),
  percentage = c(
    alcool_summary$beer_pct,
    alcool_summary$cocktail_pct,
    alcool_summary$nondrink_pct,
    alcool_summary$spirits_pct,
    alcool_summary$wine_pct,
    alcool_summary$ties_pct
  )
) %>%
  # Ordonner par nombre de victoires décroissant
  arrange(desc(wins))

# Créer le facteur ordonné pour l'axe des x
summary_long$type <- factor(summary_long$type, levels = summary_long$type)

# Créer le graphique
alcool_summary_plot <- ggplot2::ggplot(summary_long, aes(x = type, y = wins, fill = type)) +
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
      "Bière" = "#f4a261",
      "Cocktail" = "#e76f51",
      "Ne boivent pas" = "#2a9d8f",
      "Spiritueux" = "#264653",
      "Vin" = "#9b2226",
      "Égalité" = "#D3D3D3"
    )
  ) +
  ggplot2::labs(
    title = "Popularité des types d'alcool au Canada",
    subtitle = "Nombre de circonscriptions où chaque type d'alcool est dominant",
    caption = "Source: Sondage Datagotchi 2025",
    x = NULL,
    y = "Nombre de circonscriptions"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "none",
    plot.title = ggplot2::element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
    axis.text.x = ggplot2::element_text(size = 12, face = "bold"),
    axis.text.y = ggplot2::element_text(size = 10),
    panel.grid.major.x = ggplot2::element_blank()
  )
alcool_summary_plot

# Sauvegarder le graphique
ggsave("alcool_summary_plot.png", alcool_summary_plot, width = 10, height = 6, dpi = 300)

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
