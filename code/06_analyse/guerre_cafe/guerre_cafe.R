library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(cartessn)
library(patchwork)
library(cowplot)

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
saveRDS(mapping_results, "_SharedFolder_datagotchi_federal_2024/reports/mapping_results_ridings_rta.rds")


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
coffee_battle_by_riding <- data %>%
  filter(!is.na(id_riding)) %>%
  group_by(id_riding) %>%
  summarize(
    # Somme des poids
    sum_weight = sum(weight, na.rm = TRUE),
    
    # Fans pondérés pour chaque chaîne
    tim_hortons_fans = sum((lifestyle_consCoffeeTimHortons == 1) * weight, na.rm = TRUE),
    mcdo_fans = sum((lifestyle_consCoffeeMcDo == 1) * weight, na.rm = TRUE),
    starbucks_fans = sum((lifestyle_consCoffeeStarbucks == 1) * weight, na.rm = TRUE),
    secondcup_fans = sum((lifestyle_consCoffeeSecondCup == 1) * weight, na.rm = TRUE),
    
    # Calcul des pourcentages pondérés
    tim_fans_pct = tim_hortons_fans / sum_weight * 100,
    mcdo_fans_pct = mcdo_fans / sum_weight * 100,
    starbucks_fans_pct = starbucks_fans / sum_weight * 100,
    secondcup_fans_pct = secondcup_fans / sum_weight * 100,
    
    # Nombre de répondants non-pondéré (pour référence)
    n_people = n()
  ) %>%
  ungroup()

# 10. Déterminer la chaîne dominante dans chaque circonscription
coffee_battle_by_riding <- coffee_battle_by_riding %>%
  mutate(
    # Créer une variable pour la chaîne dominante (celle avec le plus de fans)
    dominant_chain = case_when(
      tim_fans_pct >= mcdo_fans_pct & tim_fans_pct >= starbucks_fans_pct & tim_fans_pct >= secondcup_fans_pct ~ "Tim Hortons 🇨🇦",
      mcdo_fans_pct >= tim_fans_pct & mcdo_fans_pct >= starbucks_fans_pct & mcdo_fans_pct >= secondcup_fans_pct ~ "McDonald's 🇺🇸",
      starbucks_fans_pct >= tim_fans_pct & starbucks_fans_pct >= mcdo_fans_pct & starbucks_fans_pct >= secondcup_fans_pct ~ "Starbucks 🇺🇸",
      secondcup_fans_pct >= tim_fans_pct & secondcup_fans_pct >= mcdo_fans_pct & secondcup_fans_pct >= starbucks_fans_pct ~ "Second Cup 🇨🇦",
      TRUE ~ "Égalité"
    ),
    
    # Pourcentage pour la chaîne dominante
    dominant_pct = case_when(
      dominant_chain == "Tim Hortons 🇨🇦" ~ tim_fans_pct,
      dominant_chain == "McDonald's 🇺🇸" ~ mcdo_fans_pct,
      dominant_chain == "Starbucks 🇺🇸" ~ starbucks_fans_pct,
      dominant_chain == "Second Cup 🇨🇦" ~ secondcup_fans_pct,
      TRUE ~ NA_real_
    )
  )

# 11. Joindre les résultats aux données spatiales pour visualisation
sf_coffee_map <- sf_ridings %>%
  left_join(coffee_battle_by_riding, by = "id_riding")

# 12. Sauvegarder les résultats intermédiaires

saveRDS(coffee_battle_by_riding, "_SharedFolder_datagotchi_federal_2024/reports/coffee_battle_pondere.rds")
coffee_battle_by_riding <- readRDS("_SharedFolder_datagotchi_federal_2024/reports/coffee_battle_pondere.rds")

#
# Correction 1: Modifier la fonction pour qu'elle fonctionne comme un thème ggplot2
# Au lieu de prendre un argument map, elle retourne directement un thème
remove_axes_improve_aesthetics <- function() {
  theme(
    # Suppression des axes et des étiquettes
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    # Amélioration de l'apparence globale
    panel.background = element_rect(fill = "#f5f5f5", color = NA),
    plot.background = element_rect(fill = "#ffffff", color = NA),
    legend.background = element_rect(fill = "#ffffff", color = NA),
    plot.margin = margin(10, 10, 10, 10),
    # Style du titre et sous-titre
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 15)),
    # Position et style de la légende
    legend.position = "bottom",
    legend.margin = margin(t = 5, b = 5),
    legend.box.margin = margin(t = 5),
    legend.text = element_text(size = 9)
  )
}

# Prétraitement des données et création de la carte principale du Canada
sf_coffee_map_clean <- sf_coffee_map %>%
  mutate(dominant_chain = ifelse(is.na(dominant_chain), "Non disponible", dominant_chain))

# Création de la carte principale avec les nouvelles couleurs et style
# Correction 2: Appliquer le thème directement après le scale_fill_manual
canada_coffee_map <- cartessn::create_map(
  sf_coffee_map_clean,
  value_column = "dominant_chain",
  title = "La bataille du café au Canada",
  subtitle = "Chaîne de café préférée par circonscription électorale",
  caption = "Source: Sondage Datagotchi 2025",
  legend_title = "Chaîne dominante",
  discrete_values = TRUE,
  fill_color = c(
    "Tim Hortons 🇨🇦" = "#C8102E", 
    "McDonald's 🇺🇸" = "#FFC72C", 
    "Starbucks 🇺🇸" = "#00704A",
    "Second Cup 🇨🇦" = "#003DA5",
    "Égalité" = "#D3D3D3",
    "Non disponible" = "#FFFFFF00"
  ),
  background = "light",
  border_size = 0.1
) +
scale_fill_manual(
  values = c(
    "Tim Hortons 🇨🇦" = "#C8102E", 
    "McDonald's 🇺🇸" = "#FFC72C", 
    "Starbucks 🇺🇸" = "#00704A",
    "Second Cup 🇨🇦" = "#003DA5",
    "Égalité" = "#D3D3D3",
    "Non disponible" = "#FFFFFF00"
  ),
  breaks = c("Tim Hortons 🇨🇦", "McDonald's 🇺🇸", "Starbucks 🇺🇸", "Second Cup 🇨🇦", "Égalité")
) +
remove_axes_improve_aesthetics()  # La fonction retourne maintenant un thème, pas besoin d'argument

# Création des cartes pour les principales villes avec un style cohérent
main_regions <- c("montreal", "toronto", "vancouver", "ottawa_gatineau")
region_maps <- list()

# Créer une carte pour chaque région principale
for (i in seq_along(main_regions)) {
  region <- main_regions[i]
  
  # Titre formaté proprement
  formatted_title <- gsub("_", " ", region)
  formatted_title <- paste0(toupper(substr(formatted_title, 1, 1)), 
                           substr(formatted_title, 2, nchar(formatted_title)))
  
  # Extraire la région
  region_map <- cartessn::crop_map(sf_coffee_map_clean, region)
  
  # Correction 3: Appliquer le thème correctement dans la boucle
  region_maps[[i]] <- cartessn::create_map(
    region_map,
    value_column = "dominant_chain",
    title = formatted_title,
    discrete_values = TRUE,
    fill_color = c(
      "Tim Hortons 🇨🇦" = "#C8102E",
      "McDonald's 🇺🇸" = "#FFC72C",
      "Starbucks 🇺🇸" = "#00704A",
      "Second Cup 🇨🇦" = "#003DA5",
      "Égalité" = "#D3D3D3",
      "Non disponible" = "#FFFFFF00"
    ),
    background = "light",
    border_size = 0.15
  ) +
  scale_fill_manual(
    values = c(
      "Tim Hortons 🇨🇦" = "#C8102E",
      "McDonald's 🇺🇸" = "#FFC72C",
      "Starbucks 🇺🇸" = "#00704A",
      "Second Cup 🇨🇦" = "#003DA5",
      "Égalité" = "#D3D3D3",
      "Non disponible" = "#FFFFFF00"
    ),
    breaks = c("Tim Hortons 🇨🇦", "McDonald's 🇺🇸", "Starbucks 🇺🇸", "Second Cup 🇨🇦", "Égalité")
  ) +
  remove_axes_improve_aesthetics() +  # Applique le thème correctement
  theme(legend.position = "none")  # Supprime la légende des cartes individuelles
}

# 4. Création du layout final avec patchwork
# Une légende commune pour toutes les cartes
common_legend <- cowplot::get_legend(
  canada_coffee_map + 
    guides(fill = guide_legend(nrow = 1, title.position = "top")) +
    theme(legend.position = "bottom",
          legend.box.margin = margin(10, 0, 0, 0))
)

# Supprimer la légende de la carte principale pour l'ajouter en bas du layout final
canada_coffee_map <- canada_coffee_map + theme(legend.position = "none")

# Combiner les cartes des villes en une grille 2x2
city_maps_grid <- patchwork::wrap_plots(region_maps, ncol = 2)

# Création du layout final : carte du Canada en haut, cartes des villes en bas
final_layout <- (canada_coffee_map / city_maps_grid) +
  plot_layout(heights = c(2, 3)) +
  plot_annotation(
    title = "La bataille du café au Canada",
    subtitle = "Préférences des chaînes de café par région",
    caption = "Source: Sondage Datagotchi 2025",
    theme = theme(
      plot.title = element_text(face = "bold", size = 24, hjust = 0.5, margin = margin(b = 5, t = 10)),
      plot.subtitle = element_text(size = 16, hjust = 0.5, margin = margin(b = 20)),
      plot.caption = element_text(size = 10, hjust = 1, margin = margin(t = 5, b = 5)),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  )

# Ajouter la légende commune au bas de la mise en page finale
final_combined_layout <- final_layout + 
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

final_combined_layout

# 5. Sauvegarder avec une haute résolution
ggsave("canada_coffee_analysis_combined.png", 
       final_combined_layout, 
       width = 14, 
       height = 16, 
       dpi = 300)

