library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(cartessn)
library(patchwork)
library(cowplot)
library(showtext)
library(magick)

# Load data
data <- readRDS("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/datagotchi2025_canada_appPonderee_20250323.rds")

library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(cartessn)
library(patchwork)
library(cowplot)
library(showtext)
library(magick)

# 1. Chargement des données
data <- readRDS("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/datagotchi2025_canada_appPonderee_20250323.rds")

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
# Si le mapping existe déjà, on peut le charger directement
mapping_results <- readRDS("_SharedFolder_datagotchi_federal_2024/reports/mapping_results_ridings_rta.rds")

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

# 9. Analyse des types de transport par circonscription AVEC PONDÉRATION
transport_battle_by_riding <- data %>%
  filter(!is.na(id_riding)) %>%
  group_by(id_riding) %>%
  summarize(
    # Somme des poids
    sum_weight = sum(weight, na.rm = TRUE),
    
    # Utilisateurs pondérés pour chaque type de transport
    car_users = sum(lifestyle_Transport == "car" * weight, na.rm = TRUE),
    suv_users = sum(lifestyle_Transport == "suv" * weight, na.rm = TRUE),
    public_transit_users = sum(lifestyle_Transport == "public_transit" * weight, na.rm = TRUE),
    walk_users = sum(lifestyle_Transport == "walk" * weight, na.rm = TRUE),
    bicycle_users = sum(lifestyle_Transport == "bicycle" * weight, na.rm = TRUE),
    motorcycle_users = sum(lifestyle_Transport == "motorcycle" * weight, na.rm = TRUE),
    
    # Calcul des pourcentages pondérés
    car_pct = car_users / sum_weight * 100,
    suv_pct = suv_users / sum_weight * 100,
    public_transit_pct = public_transit_users / sum_weight * 100,
    walk_pct = walk_users / sum_weight * 100,
    bicycle_pct = bicycle_users / sum_weight * 100,
    motorcycle_pct = motorcycle_users / sum_weight * 100,
    
    # Nombre de répondants non-pondéré (pour référence)
    n_people = n()
  ) %>%
  ungroup()

# 10. Déterminer le type de transport dominant dans chaque circonscription
transport_battle_by_riding <- transport_battle_by_riding %>%
  mutate(
    # Créer une variable pour le type de transport dominant (celui avec le plus d'utilisateurs)
    dominant_mode = case_when(
      car_pct >= suv_pct & car_pct >= public_transit_pct & car_pct >= walk_pct & car_pct >= bicycle_pct & car_pct >= motorcycle_pct ~ "Voiture 🚗",
      suv_pct >= car_pct & suv_pct >= public_transit_pct & suv_pct >= walk_pct & suv_pct >= bicycle_pct & suv_pct >= motorcycle_pct ~ "VUS 🚙",
      public_transit_pct >= car_pct & public_transit_pct >= suv_pct & public_transit_pct >= walk_pct & public_transit_pct >= bicycle_pct & public_transit_pct >= motorcycle_pct ~ "Transport en commun 🚇",
      walk_pct >= car_pct & walk_pct >= suv_pct & walk_pct >= public_transit_pct & walk_pct >= bicycle_pct & walk_pct >= motorcycle_pct ~ "Marche 🚶",
      bicycle_pct >= car_pct & bicycle_pct >= suv_pct & bicycle_pct >= public_transit_pct & bicycle_pct >= walk_pct & bicycle_pct >= motorcycle_pct ~ "Vélo 🚲",
      motorcycle_pct >= car_pct & motorcycle_pct >= suv_pct & motorcycle_pct >= public_transit_pct & motorcycle_pct >= walk_pct & motorcycle_pct >= bicycle_pct ~ "Moto 🏍️",
      TRUE ~ "Égalité"
    ),
    
    # Pourcentage pour le type de transport dominant
    dominant_pct = case_when(
      dominant_mode == "Voiture 🚗" ~ car_pct,
      dominant_mode == "VUS 🚙" ~ suv_pct,
      dominant_mode == "Transport en commun 🚇" ~ public_transit_pct,
      dominant_mode == "Marche 🚶" ~ walk_pct,
      dominant_mode == "Vélo 🚲" ~ bicycle_pct,
      dominant_mode == "Moto 🏍️" ~ motorcycle_pct,
      TRUE ~ NA_real_
    )
  )

# 11. Joindre les résultats aux données spatiales pour visualisation
sf_transport_map <- sf_ridings %>%
  left_join(transport_battle_by_riding, by = "id_riding")

# 12. Sauvegarder les résultats intermédiaires
saveRDS(transport_battle_by_riding, "_SharedFolder_datagotchi_federal_2024/reports/transport_battle_pondere.rds")

# 13. Paramètres pour éviter les problèmes de mémoire
options(future.globals.maxSize = 1000 * 1024^2)  # Augmenter la limite à 1 Go
sf_use_s2(FALSE)  # Désactiver les fonctionnalités S2 de sf pour réduire l'utilisation de la mémoire

# 14. Thème simplifié pour les cartes
theme_map_dark <- function() {
  theme_minimal() +
    theme(
      # Fond noir
      plot.background = element_rect(fill = "#121212", color = NA),
      panel.background = element_rect(fill = "#121212", color = NA),
      
      # Suppression des axes et grilles
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      
      # Textes en blanc
      plot.title = element_text(face = "bold", size = 14, color = "white", hjust = 0.5),
      plot.subtitle = element_text(size = 11, color = "#CCCCCC", hjust = 0.5),
      plot.caption = element_text(size = 12, color = "#BBBBBB", hjust = 1),
      
      # Légende
      legend.position = "bottom",
      legend.background = element_rect(fill = "#121212", color = NA),
      legend.title = element_text(size = 10, color = "white"),
      legend.text = element_text(size = 9, color = "#CCCCCC")
    )
}

# 15. Définition des couleurs pour les types de transport
transport_colors <- c(
  "Voiture 🚗" = "#3498DB",        # Bleu
  "VUS 🚙" = "#E74C3C",            # Rouge
  "Transport en commun 🚇" = "#2ECC71",  # Vert
  "Marche 🚶" = "#F1C40F",         # Jaune
  "Vélo 🚲" = "#9B59B6",           # Violet
  "Moto 🏍️" = "#E67E22",          # Orange
  "Non disponible" = "#33333300"   # Gris foncé transparent
)

# 16. Prétraitement des données
sf_transport_map_clean <- sf_transport_map %>%
  mutate(dominant_mode = ifelse(is.na(dominant_mode), "Non disponible", dominant_mode))

# 17. Information sur le nombre d'observations
n_observations <- nrow(data)  # Utilisez le nombre réel de répondants

# 18. ===== CARTE DU CANADA =====
canada_transport_map <- ggplot(sf_transport_map_clean) +
  geom_sf(aes(fill = dominant_mode), color = "#121212", size = 0.2) +
  scale_fill_manual(
    name = "Mode de transport dominant",
    values = transport_colors,
    breaks = c("Voiture 🚗", "VUS 🚙", "Transport en commun 🚇", "Marche 🚶", "Vélo 🚲", "Moto 🏍️")
  ) +
  theme_map_dark() +
  theme(legend.position = "none")

ggsave("canada_transport_map.png", 
       canada_transport_map, 
       width = 16, 
       height = 12, 
       dpi = 200,
       bg = "#121212")

# 19. ===== CARTES URBAINES =====
main_regions <- c("montreal", "toronto", "vancouver", "ottawa_gatineau")

# 20. Créer et sauvegarder chaque carte urbaine individuellement
for (region in main_regions) {
  # Extraire la région
  region_map <- cartessn::crop_map(sf_transport_map_clean, region)
  
  # Créer la carte manuellement
  city_map <- ggplot(region_map) +
    geom_sf(aes(fill = dominant_mode), color = "#121212", size = 0.15) +
    scale_fill_manual(
      values = transport_colors,
      breaks = c("Voiture 🚗", "VUS 🚙", "Transport en commun 🚇", "Marche 🚶", "Vélo 🚲", "Moto 🏍️")
    ) +
    theme_map_dark() +
    theme(legend.position = "none")
  
  # Sauvegarder chaque carte urbaine séparément avec aspect ratio carré
  ggsave(paste0(tolower(gsub("-", "_", region)), "_transport_map.png"), 
         city_map, 
         width = 6, 
         height = 6, 
         dpi = 150,
         bg = "#121212")
}

# 21-44. Génération d'une image composite similaire au code original pour café
# Cette section gère la mise en page et l'assemblage des cartes et de la légende

# 45. Version simplifiée du graphique transport-politique avec élimination des doublons

# 46. Reprendre les calculs essentiels pour l'indice transport-politique
national_averages <- data %>%
  summarize(
    sum_weight = sum(weight, na.rm = TRUE),
    car_avg = sum(lifestyle_Transport == "car" * weight, na.rm = TRUE) / sum_weight * 100,
    suv_avg = sum(lifestyle_Transport == "suv" * weight, na.rm = TRUE) / sum_weight * 100,
    public_transit_avg = sum(lifestyle_Transport == "public_transit" * weight, na.rm = TRUE) / sum_weight * 100,
    walk_avg = sum(lifestyle_Transport == "walk" * weight, na.rm = TRUE) / sum_weight * 100,
    bicycle_avg = sum(lifestyle_Transport == "bicycle" * weight, na.rm = TRUE) / sum_weight * 100,
    motorcycle_avg = sum(lifestyle_Transport == "motorcycle" * weight, na.rm = TRUE) / sum_weight * 100
  )

# Arrondir les valeurs pour l'affichage
car_national <- round(national_averages$car_avg, 1)
suv_national <- round(national_averages$suv_avg, 1)
public_transit_national <- round(national_averages$public_transit_avg, 1)
walk_national <- round(national_averages$walk_avg, 1)
bicycle_national <- round(national_averages$bicycle_avg, 1)
motorcycle_national <- round(national_averages$motorcycle_avg, 1)

# Calcul des écarts par parti
transport_by_party <- data %>%
  # Filtrer les NA et limiter aux partis politiques que nous voulons analyser
  filter(!is.na(dv_voteChoice)) %>%
  filter(dv_voteChoice %in% c("lpc", "cpc", "ndp", "bq", "gpc")) %>%
  group_by(dv_voteChoice) %>%
  summarize(
    sum_weight = sum(weight, na.rm = TRUE),
    car_users_pct = sum(lifestyle_Transport == "car" * weight, na.rm = TRUE) / sum_weight * 100,
    suv_users_pct = sum(lifestyle_Transport == "suv" * weight, na.rm = TRUE) / sum_weight * 100,
    public_transit_users_pct = sum(lifestyle_Transport == "public_transit" * weight, na.rm = TRUE) / sum_weight * 100,
    walk_users_pct = sum(lifestyle_Transport == "walk" * weight, na.rm = TRUE) / sum_weight * 100,
    bicycle_users_pct = sum(lifestyle_Transport == "bicycle" * weight, na.rm = TRUE) / sum_weight * 100,
    motorcycle_users_pct = sum(lifestyle_Transport == "motorcycle" * weight, na.rm = TRUE) / sum_weight * 100,
    n_people = n()
  ) %>%
  ungroup() %>%
  mutate(
    party_name = case_when(
      dv_voteChoice == "lpc" ~ "Parti libéral",
      dv_voteChoice == "cpc" ~ "Parti conservateur",
      dv_voteChoice == "ndp" ~ "NPD",
      dv_voteChoice == "bq" ~ "Bloc Québécois",
      dv_voteChoice == "gpc" ~ "Parti vert",
      TRUE ~ NA_character_  # Convertir tout autre parti en NA
    ),
    car_deviation = car_users_pct - national_averages$car_avg,
    suv_deviation = suv_users_pct - national_averages$suv_avg,
    public_transit_deviation = public_transit_users_pct - national_averages$public_transit_avg,
    walk_deviation = walk_users_pct - national_averages$walk_avg,
    bicycle_deviation = bicycle_users_pct - national_averages$bicycle_avg,
    motorcycle_deviation = motorcycle_users_pct - national_averages$motorcycle_avg
  ) %>%
  # Filtrer à nouveau pour éliminer tout parti dont le nom est NA
  filter(!is.na(party_name))

# Préparation des données pour le graphique
# Pour limiter la complexité, incluons seulement les 4 principaux modes:
# voiture, VUS, transport en commun, marche (exclus vélo et moto)
transport_by_party_long <- transport_by_party %>%
  select(party_name, car_deviation, suv_deviation, public_transit_deviation, walk_deviation) %>%
  pivot_longer(
    cols = c(car_deviation, suv_deviation, public_transit_deviation, walk_deviation),
    names_to = "transport_mode",
    values_to = "deviation"
  ) %>%
  mutate(
    transport_mode = case_when(
      transport_mode == "car_deviation" ~ "Voiture 🚗",
      transport_mode == "suv_deviation" ~ "VUS 🚙",
      transport_mode == "public_transit_deviation" ~ "Transport en commun 🚇",
      transport_mode == "walk_deviation" ~ "Marche 🚶"
    )
  )

# Ordonner les partis politiques du plus à droite au plus à gauche
party_order <- c("Parti conservateur", "Parti libéral", "Bloc Québécois", "NPD", "Parti vert")
transport_by_party_long$party_name <- factor(transport_by_party_long$party_name, levels = party_order)

# Sous-titre avec les moyennes nationales des 4 principaux modes
ref_subtitle <- paste0("Moyennes nationales: Voiture = ", car_national, 
                       "%, VUS = ", suv_national, 
                       "%, Transport en commun = ", public_transit_national, 
                       "%, Marche = ", walk_national, "%")

# 47. Créer un graphique simplifié avec un seul titre et des annotations claires
simplified_plot <- ggplot(transport_by_party_long, aes(x = party_name, y = deviation, fill = transport_mode)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_hline(yintercept = 0, color = "#555555", linetype = "dashed", size = 0.8) +
  scale_fill_manual(
    name = "Mode de transport",
    values = transport_colors[1:4]  # Utiliser seulement les 4 premiers modes
  ) +
  labs(
    title = "L'INDICE TRANSPORT-POLITIQUE",
    subtitle = "Écart de préférence de transport par rapport à la moyenne nationale (points de %)",
    caption = paste0("Moyennes nationales: Voiture = ", car_national, 
                     "%, VUS = ", suv_national, 
                     "%, Transport en commun = ", public_transit_national, 
                     "%, Marche = ", walk_national, "%"),
    x = "",
    y = "",
    size = 12
  ) +
  # Annotations explicatives
  annotate("text", x = 2.1, y = 5, 
           label = "Valeurs positives = préférence\nsupérieure à la moyenne nationale", 
           color = "white", size = 5, hjust = 0.5, vjust = -0.5) +
  annotate("text", x = 4.4, y = -6, 
           label = "Valeurs négatives = préférence\ninférieure à la moyenne nationale", 
           color = "white", size = 5, hjust = 0.5, vjust = 1.5) +
  theme_map_dark() +
  theme(
    plot.title = element_text(face = "bold", size = 24, color = "white", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 16, color = "#CCCCCC", hjust = 0.5, margin = margin(b = 20)),
    # Suppression de la légende standard
    legend.position = "none",
    axis.text.x = element_text(color = "white", size = 14, angle = 0, hjust = 0.5),
    axis.text.y = element_text(color = "white", size = 16),
    panel.grid.major.y = element_line(color = "#333333", size = 0.2),
    plot.caption = element_text(color = "#BBBBBB", size = 17, hjust = 0.5, margin = margin(t = 20, b = 10)),
    # Augmenter DRASTIQUEMENT la marge en bas pour donner énormément d'espace
    plot.margin = margin(t = 20, r = 20, b = 60, l = 20),
    plot.background = element_rect(fill = "#121212", color = NA),
    panel.background = element_rect(fill = "#121212", color = NA)
  )

# Sauvegarder le graphique sans légende avec une hauteur augmentée
ggsave("_SharedFolder_datagotchi_federal_2024/graph/analyses/transport/indice_transport_sans_legende.png", 
       simplified_plot, 
       width = 14, 
       height = 12,
       dpi = 200,
       bg = "#121212")

# Le reste du code pour ajouter la légende et finaliser l'image peut être adapté
# de la même manière que dans le code original pour le café

# Au lieu d'images d'icônes de café, on pourrait utiliser des emojis ou symboles de transport
# ou créer/utiliser des icônes spécifiques pour les modes de transport