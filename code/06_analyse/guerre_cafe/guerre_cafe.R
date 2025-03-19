# Carte améliorée montrant la bataille entre 4 chaînes de café par circonscription
# Tim Hortons vs McDonald's vs Starbucks vs Second Cup
# Avec symboles nationaux et vues des principales villes

# Chargement des bibliothèques nécessaires
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(cartessn)
library(gridExtra)
library(grid)
library(png)

# Chargement des données
data <- readRDS("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/datagotchi2025_canada_app_20250314.rds")

# Chargement des données spatiales
sf_ridings <- cartessn::spatial_canada_2022_electoral_ridings_aligned
sf_rta <- cartessn::spatial_canada_2021_rta

# Extraction des 3 premiers caractères du code postal (RTA)
data$rta <- substr(data$ses_postalCode, 1, 3)

# Calcul des intersections entre RTA et circonscriptions
intersections <- cartessn::intersect_spatial_objects(
  spatial_ref = sf_rta,
  id_ref = "rta",
  spatial_target = sf_ridings,
  id_target = "id_riding",
  dTolerance = 50
)

# Préparer la table d'appartenance RTA-circonscription
rta_circo_appartenance <- intersections %>%
  mutate(pourcentage_appartenance = prop_of_ref_area_covered_by_target * 100) %>%
  select(
    rta,
    id_riding,
    aire_intersection_m2 = area_covered_by_target_m2,
    pourcentage_appartenance
  ) %>%
  arrange(rta, desc(pourcentage_appartenance))

# Pour chaque RTA, sélectionner uniquement la circonscription avec la plus grande intersection
rta_to_riding <- rta_circo_appartenance %>%
  group_by(rta) %>%
  slice_max(order_by = pourcentage_appartenance, n = 1) %>%
  ungroup() %>%
  select(rta, id_riding)

# Joindre l'ID de circonscription à nos données
data <- data %>%
  left_join(rta_to_riding, by = "rta")

# Analyse des 4 chaînes de café par circonscription
coffee_battle_by_riding <- data %>%
  group_by(id_riding) %>%
  summarize(
    n_people = n(),
    tim_hortons_fans = sum(lifestyle_consCoffeeTimHortons == 1, na.rm = TRUE),
    mcdo_fans = sum(lifestyle_consCoffeeMcDo == 1, na.rm = TRUE),
    starbucks_fans = sum(lifestyle_consCoffeeStarbucks == 1, na.rm = TRUE),
    secondcup_fans = sum(lifestyle_consCoffeeSecondCup == 1, na.rm = TRUE),
    
    tim_fans_pct = tim_hortons_fans / n_people * 100,
    mcdo_fans_pct = mcdo_fans / n_people * 100,
    starbucks_fans_pct = starbucks_fans / n_people * 100,
    secondcup_fans_pct = secondcup_fans / n_people * 100
  ) %>%
  filter(!is.na(id_riding)) %>%
  ungroup()

# Déterminer la chaîne dominante dans chaque circonscription
coffee_battle_by_riding <- coffee_battle_by_riding %>%
  mutate(
    # Créer une variable pour la chaîne dominante (celle avec le plus de fans)
    dominant_chain = case_when(
      tim_fans_pct >= mcdo_fans_pct & tim_fans_pct >= starbucks_fans_pct & tim_fans_pct >= secondcup_fans_pct ~ "Tim Hortons 🍁",
      mcdo_fans_pct >= tim_fans_pct & mcdo_fans_pct >= starbucks_fans_pct & mcdo_fans_pct >= secondcup_fans_pct ~ "McDonald's 🇺🇸",
      starbucks_fans_pct >= tim_fans_pct & starbucks_fans_pct >= mcdo_fans_pct & starbucks_fans_pct >= secondcup_fans_pct ~ "Starbucks",
      secondcup_fans_pct >= tim_fans_pct & secondcup_fans_pct >= mcdo_fans_pct & secondcup_fans_pct >= starbucks_fans_pct ~ "Second Cup",
      TRUE ~ "Égalité"
    )
  )

# Joindre ces données à notre shapefile des circonscriptions
sf_ridings_with_coffee <- sf_ridings %>%
  left_join(coffee_battle_by_riding, by = "id_riding")

# Palette de couleurs pour la carte des 4 chaînes
coffee_colors <- c(
  "Tim Hortons" = "#C8102E",    # Rouge Tim Hortons
  "McDonald's" = "#FFC72C",     # Jaune McDonald's
  "Starbucks" = "#006241",      # Vert Starbucks
  "Second Cup" = "#0066B2",     # Bleu Second Cup
  "Égalité" = "#8B4513"         # Brun café pour les zones contestées
)

# Création de la carte nationale P1
p1 <- ggplot() +
  geom_sf(data = sf_ridings_with_coffee, aes(fill = dominant_chain)) +
  scale_fill_manual(
    values = coffee_colors,
    name = "Chaîne dominante"
  ) +
  labs(
    title = "La grande bataille du café canadien",
    subtitle = "Quelle chaîne règne dans votre circonscription électorale?",
    caption = "Source: Données Datagotchi 2025"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

# Afficher la carte nationale
print(p1)

# Fonction pour créer des cartes dérivées pour les villes
create_city_coffee_map <- function(city_name, title) {
  # Extraire la carte de la ville
  city_map <- cartessn::crop_map(sf_ridings_with_coffee, city_name)
  
  # Créer une carte pour la ville
  ggplot() +
    geom_sf(data = city_map, aes(fill = dominant_chain)) +
    scale_fill_manual(
      values = coffee_colors,
      name = "Chaîne dominante"
    ) +
    labs(
      title = paste("Bataille du café :", title),
      subtitle = "Quelle chaîne règne dans votre circonscription?",
      caption = "Source: Données Datagotchi 2025"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10),
      legend.position = "right",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )
}

# Créer des cartes pour différentes villes canadiennes
p_montreal <- create_city_coffee_map("montreal", "Montréal")
p_toronto <- create_city_coffee_map("toronto", "Toronto")
p_vancouver <- create_city_coffee_map("vancouver", "Vancouver")
p_quebec <- create_city_coffee_map("quebec_city", "Québec")
p_ottawa <- create_city_coffee_map("ottawa_gatineau", "Ottawa")

# Afficher les cartes des villes individuelles
print(p_montreal)
print(p_toronto)
print(p_vancouver)
print(p_quebec)
print(p_ottawa)

# Option: Créer une carte multi-panneaux comparant plusieurs villes
p_multi <- gridExtra::grid.arrange(
  p_montreal, p_toronto, p_vancouver, p_quebec, p_ottawa,
  ncol = 2,
  top = textGrob("La bataille du café dans les grandes villes canadiennes", 
                 gp = gpar(fontsize = 16, fontface = "bold"))
)

print(p_multi)