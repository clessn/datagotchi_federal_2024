library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(cartessn)
library(patchwork)
library(cowplot)
library(showtext)
library(magick)

transport_icons <- list(
  car = "_SharedFolder_datagotchi_federal_2024/graph/analyses/transport/icons/car.png",
  suv = "_SharedFolder_datagotchi_federal_2024/graph/analyses/transport/icons/suv.png",
  transit = "_SharedFolder_datagotchi_federal_2024/graph/analyses/transport/icons/bus.png",
  walk = "_SharedFolder_datagotchi_federal_2024/graph/analyses/transport/icons/walk.png",
  bicycle = "_SharedFolder_datagotchi_federal_2024/graph/analyses/transport/icons/bike.png",
  moto = "_SharedFolder_datagotchi_federal_2024/graph/analyses/transport/icons/moto.png"
)

# Load and resize icons
icon_size <- 80
transport_imgs <- list(
  car_icon = image_read(transport_icons$car) %>% image_scale(paste0(icon_size, "x", icon_size)),
  suv_icon = image_read(transport_icons$suv) %>% image_scale(paste0(icon_size, "x", icon_size)),
  transit_icon = image_read(transport_icons$transit) %>% image_scale(paste0(icon_size, "x", icon_size)),
  walk_icon = image_read(transport_icons$walk) %>% image_scale(paste0(icon_size, "x", icon_size)),
  bicycle_icon = image_read(transport_icons$bicycle) %>% image_scale(paste0(icon_size, "x", icon_size)),
  moto_icon = image_read(transport_icons$moto) %>% image_scale(paste0(icon_size, "x", icon_size))
)


# 1. Chargement des données
data <- readRDS("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/datagotchi2025_canada_appPonderee_20250323.rds")

# 2. Chargement des données spatiales depuis cartessn
sf_ridings <- cartessn::spatial_canada_2022_electoral_ridings_aligned
sf_rta <- cartessn::spatial_canada_2021_rta

# 3. Extraction des 3 premiers caractères du code postal (RTA)
data$rta <- substr(data$ses_postalCode, 1, 3)

# 4. Utilisation de la fonction map_fsa_to_ridings de cartessn pour associer RTA à circonscriptions
# Cette fonction combine toutes les étapes intermédiaires de l'ancien script
#mapping_results <- cartessn::map_fsa_to_ridings(
#  sf_rta = sf_rta,
#  sf_ridings = sf_ridings,
#  tolerance = 50
#)
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
    car_users = sum((lifestyle_Transport == "car") * weight, na.rm = TRUE),
    suv_users = sum((lifestyle_Transport == "suv") * weight, na.rm = TRUE),
    public_transit_users = sum((lifestyle_Transport == "public_transit") * weight, na.rm = TRUE),
    walk_users = sum((lifestyle_Transport == "walk") * weight, na.rm = TRUE),
    bicycle_users = sum((lifestyle_Transport == "bicycle") * weight, na.rm = TRUE),
    motorcycle_users = sum((lifestyle_Transport == "motorcycle") * weight, na.rm = TRUE),
    
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
    name = "Mode de transport",
    values = transport_colors,
    breaks = c("Voiture 🚗", "VUS 🚙", "Transport en commun 🚇", "Marche 🚶", "Vélo 🚲", "Moto 🏍️")
  ) +
  theme_map_dark() +
  theme(legend.position = "none")  # Change this to "none" instead of "bottom"

ggsave("canada_transport_map.png", 
       canada_transport_map, 
       width = 16, 
       height = 12, 
       dpi = 200,
       bg = "#121212")

# 19. ===== CARTES URBAINES =====
main_regions <- c("montreal", "toronto", "vancouver", "quebec_city")

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

# Cette section gère la mise en page et l'assemblage des cartes et de la légende

# Définition des paramètres de dimensions
canvas_width <- 1800      # Largeur totale du canvas
canada_height <- 1000     # Hauteur pour la carte du Canada
city_height <- 400        # Hauteur pour les cartes de villes
city_spacing <- 20        # Espacement entre les cartes de villes
section_spacing <- 20     # Espacement entre les sections

# 22. Fonction pour créer une carte de ville avec de meilleures proportions
create_city_map <- function(region_name, display_title = NULL) {
  # Utiliser le titre personnalisé si fourni, sinon utiliser region_name
  display_name <- ifelse(is.null(display_title), toupper(region_name), toupper(display_title))
  
  # Lire l'image existante
  img_path <- paste0(tolower(gsub("-", "_", region_name)), "_transport_map.png")
  img <- image_read(img_path)
  
  # Redimensionner l'image en préservant le ratio carré
  img_resized <- image_scale(img, paste0(toString(city_height), "x", toString(city_height)))
  
  # Créer un canvas noir avec une largeur fixe pour toutes les villes
  city_width <- city_height  # Maintenir un aspect carré
  canvas <- image_blank(width = city_width, 
                        height = city_height + 60,  # Plus d'espace pour le titre
                        color = "#121212")
  
  # Placer l'image sur le canvas (centrée)
  canvas_with_map <- image_composite(canvas, img_resized, 
                                     gravity = "center")
  
  # Ajouter le titre en bas
  canvas_with_title <- image_annotate(canvas_with_map, 
                                      display_name,
                                      color = "white", 
                                      size = 28,  # Taille de police augmentée
                                      font = "Arial-Bold",
                                      gravity = "south",
                                      location = "+0+20")  # Plus d'espace au bas
  
  return(canvas_with_title)
}

# 23. Lire et redimensionner la carte du Canada avec légende
canada_img <- image_read("canada_transport_map.png")
canada_resized <- image_scale(canada_img, paste0(toString(canvas_width - 40), "x", toString(canada_height)))

# 24. Créer un canvas pour la carte du Canada
canada_canvas <- image_blank(width = canvas_width, 
                             height = canada_height + 60,  # Plus d'espace pour éviter le rognage
                             color = "#121212")

# 25. Centrer la carte du Canada
canada_centered <- image_composite(canada_canvas, canada_resized, 
                                   gravity = "center")

# 26. Créer les cartes de villes avec de meilleures proportions
montreal_map <- create_city_map("montreal")
toronto_map <- create_city_map("toronto")
vancouver_map <- create_city_map("vancouver")
quebec_map <- create_city_map("quebec_city", "QUÉBEC")

# 27. Calculer l'espacement latéral pour centrer les cartes de villes
city_width = image_info(montreal_map)$width
city_total_width <- 4 * city_width + (3 * city_spacing)
city_padding <- max(0, (canvas_width - city_total_width) / 2)

# 28. Créer des séparateurs plus visibles entre les villes
city_separator <- image_blank(width = city_spacing, 
                              height = image_info(montreal_map)$height, 
                              color = "#121212")

# 29. Assemblage des villes avec espacement
city_row <- image_append(c(montreal_map, 
                           city_separator,
                           toronto_map, 
                           city_separator,
                           vancouver_map,
                           city_separator,
                           quebec_map), 
                         stack = FALSE)

# 30. Appliquer le padding latéral
if (city_padding > 0) {
  left_padding <- image_blank(width = city_padding, height = image_info(city_row)$height, color = "#121212")
  right_padding <- image_blank(width = city_padding, height = image_info(city_row)$height, color = "#121212")
  city_row_padded <- image_append(c(left_padding, city_row, right_padding), stack = FALSE)
} else {
  city_row_padded <- city_row
}

# 31. Titre principal avec dimensions augmentées
title_height <- 100  # Hauteur augmentée
title_bg <- image_blank(width = canvas_width,
                        height = title_height,
                        color = "#121212")

title <- image_annotate(title_bg,
                        "LA BATAILLE DU TRANSPORT AU CANADA",
                        color = "white",
                        size = 48,  # Taille augmentée
                        gravity = "center",
                        font = "Arial-Bold")

# 32. Sous-titre avec dimensions augmentées
subtitle_height <- 60  # Hauteur augmentée
subtitle_bg <- image_blank(width = canvas_width,
                           height = subtitle_height,
                           color = "#121212")

subtitle <- image_annotate(subtitle_bg,
                           "Mode de transport préféré par circonscription électorale",
                           color = "#CCCCCC",
                           size = 32,  # Taille augmentée
                           gravity = "center",
                           font = "Arial-Bold")


# Correction pour la légende des transports
# Remplacer les lignes concernant la légende (lignes 33-109 dans la partie assemblage)

# 33. Légende améliorée avec une hauteur augmentée pour éviter les superpositions
# Create a taller legend background to accommodate all transport modes
map_legend_height <- 300  # Renamed to avoid conflicts with later code
map_legend_bg <- image_blank(width = canvas_width,
                             height = map_legend_height,
                             color = "#121212")

# Parameters for icon positioning
x_start <- 150
x_spacing <- 350
y_row1 <- 50
y_row2 <- 180

# First Row
map_legend_bg <- map_legend_bg %>%
  image_composite(transport_imgs$car_icon, offset = paste0("+", x_start, "+", y_row1)) %>%
  image_annotate("Voiture 🚗", color = "white", size = 32,
                 location = paste0("+", x_start + icon_size + 30, "+", y_row1 + 10),
                 font = "Arial-Bold") %>%
  image_composite(transport_imgs$suv_icon, offset = paste0("+", x_start + x_spacing, "+", y_row1)) %>%
  image_annotate("VUS 🚙", color = "white", size = 32,
                 location = paste0("+", x_start + x_spacing + icon_size + 30, "+", y_row1 + 10),
                 font = "Arial-Bold") %>%
  image_composite(transport_imgs$transit_icon, offset = paste0("+", x_start + 2*x_spacing, "+", y_row1)) %>%
  image_annotate("Transport en commun 🚇", color = "white", size = 32,
                 location = paste0("+", x_start + 2*x_spacing + icon_size + 30, "+", y_row1 + 10),
                 font = "Arial-Bold")

# Second Row
map_legend_bg <- map_legend_bg %>%
  image_composite(transport_imgs$walk_icon, offset = paste0("+", x_start, "+", y_row2)) %>%
  image_annotate("Marche 🚶", color = "white", size = 32,
                 location = paste0("+", x_start + icon_size + 30, "+", y_row2 + 10),
                 font = "Arial-Bold") %>%
  image_composite(transport_imgs$bicycle_icon, offset = paste0("+", x_start + x_spacing, "+", y_row2)) %>%
  image_annotate("Vélo 🚲", color = "white", size = 32,
                 location = paste0("+", x_start + x_spacing + icon_size + 30, "+", y_row2 + 10),
                 font = "Arial-Bold") %>%
  image_composite(transport_imgs$moto_icon, offset = paste0("+", x_start + 2*x_spacing, "+", y_row2)) %>%
  image_annotate("Moto 🏍️", color = "white", size = 32,
                 location = paste0("+", x_start + 2*x_spacing + icon_size + 30, "+", y_row2 + 10),
                 font = "Arial-Bold")




# 36. Note méthodologique avec dimensions augmentées
caption_height <- 80  # Hauteur augmentée
caption_bg <- image_blank(width = canvas_width,
                          height = caption_height,
                          color = "#121212")

# Utilise le nombre réel d'observations
n_observations <- nrow(data)  # Utilisez le nombre réel de répondants
caption <- image_annotate(caption_bg,
                          paste0("Source: Léger-Datagotchi 2025 | n=", format(n_observations, big.mark = " ")),
                          color = "#BBBBBB",
                          size = 24,  # Taille augmentée
                          location = "+40+25",  # Position ajustée
                          font = "Arial-Bold")

caption <- image_annotate(caption,
                          "Données pondérées selon: le genre, l'âge, la province, la langue, le niveau d'éducation, le revenu, l'immigration, le type d'habitation",
                          color = "#BBBBBB",
                          size = 22,  # Taille augmentée
                          location = "+40+55",  # Position ajustée
                          font = "Arial-Bold")

# 37. Ligne séparatrice plus visible
separator_height <- 3  # Épaisseur augmentée
separator <- image_blank(width = canvas_width,
                         height = separator_height,
                         color = "#555555")  # Couleur légèrement plus claire

# 38. Espacement entre sections
spacer <- image_blank(width = canvas_width,
                      height = section_spacing,
                      color = "#121212")

# 39. Assembler l'image finale avec le nouvel ordre et meilleurs espacements
final_image <- c(
  title,                           # Titre principal
  subtitle,                        # Sous-titre
  spacer,                          # Espacement
  separator,                       # Ligne de séparation
  spacer,                          # Espacement
  map_legend_bg,                   # Légende personnalisée en HAUT (avec icônes)
  spacer,                          # Espacement
  separator,                       # Ligne de séparation
  spacer,                          # Espacement
  city_row_padded,                 # Cartes des villes
  spacer,                          # Espacement
  separator,                       # Ligne de séparation
  spacer,                          # Espacement
  canada_centered,                 # Carte du Canada (sans légende intégrée)
  spacer,                          # Espacement
  caption                          # Notes méthodologiques
)

final_combined <- image_append(final_image, stack = TRUE)

# 40. Ajouter une bordure noire
final_with_border <- image_border(final_combined, "#121212", "30x30")  # Bordure plus grande

# 41. Charger le logo (si disponible)
logo_path <- "_SharedFolder_datagotchi_federal_2024/logos/FR/logo_fr.png"
if (file.exists(logo_path)) {
  logo <- image_read(logo_path)
  
  # 42. Redimensionner le logo à une taille appropriée
  logo_width <- round(image_info(final_with_border)$width * 0.15)
  logo_resized <- image_scale(logo, paste0(logo_width, "x"))
  
  # 43. Calculer la position pour le coin inférieur droit
  margin <- 30
  x_position <- image_info(final_with_border)$width - image_info(logo_resized)$width - margin
  y_position <- image_info(final_with_border)$height - image_info(logo_resized)$height - margin
  
  # 44. Ajouter le logo à l'image finale
  final_with_logo <- image_composite(
    final_with_border, 
    logo_resized, 
    offset = paste0("+", x_position, "+", y_position)
  )
  
  # 45. Sauvegarder l'image finale avec logo
  image_write(final_with_logo, "_SharedFolder_datagotchi_federal_2024/graph/analyses/transport/bataille_transport_canada_final_avec_logo.png")
  
  cat("Image finale avec logo créée avec succès : bataille_transport_canada_final_avec_logo.png\n")
} else {
  # Si le logo n'est pas disponible, sauvegarder sans logo
  image_write(final_with_border, "_SharedFolder_datagotchi_federal_2024/graph/analyses/transport/bataille_transport_canada_final.png")
  
  cat("Image finale sans logo créée avec succès : bataille_transport_canada_final.png\n")
}







## Essaie de code 

# Calculate national averages for transport modes
national_averages <- data %>%
  summarize(
    sum_weight = sum(weight, na.rm = TRUE),
    car_avg = sum((lifestyle_Transport == "car") * weight, na.rm = TRUE) / sum_weight * 100,
    suv_avg = sum((lifestyle_Transport == "suv") * weight, na.rm = TRUE) / sum_weight * 100,
    public_transit_avg = sum((lifestyle_Transport == "public_transit") * weight, na.rm = TRUE) / sum_weight * 100,
    walk_avg = sum((lifestyle_Transport == "walk") * weight, na.rm = TRUE) / sum_weight * 100,
    bicycle_avg = sum((lifestyle_Transport == "bicycle") * weight, na.rm = TRUE) / sum_weight * 100,
    motorcycle_avg = sum((lifestyle_Transport == "motorcycle") * weight, na.rm = TRUE) / sum_weight * 100
  )

# Round values for display
car_national <- round(national_averages$car_avg, 1)
suv_national <- round(national_averages$suv_avg, 1)
transit_national <- round(national_averages$public_transit_avg, 1)
walk_national <- round(national_averages$walk_avg, 1)
bicycle_national <- round(national_averages$bicycle_avg, 1)
motorcycle_national <- round(national_averages$motorcycle_avg, 1)

# Calculate deviations by party
transport_by_party <- data %>%
  # Filter for main political parties
  filter(!is.na(dv_voteChoice)) %>%
  filter(dv_voteChoice %in% c("lpc", "cpc", "ndp", "bq", "gpc")) %>%
  group_by(dv_voteChoice) %>%
  summarize(
    sum_weight = sum(weight, na.rm = TRUE),
    car_pct = sum((lifestyle_Transport == "car") * weight, na.rm = TRUE) / sum_weight * 100,
    suv_pct = sum((lifestyle_Transport == "suv") * weight, na.rm = TRUE) / sum_weight * 100,
    public_transit_pct = sum((lifestyle_Transport == "public_transit") * weight, na.rm = TRUE) / sum_weight * 100,
    walk_pct = sum((lifestyle_Transport == "walk") * weight, na.rm = TRUE) / sum_weight * 100,
    bicycle_pct = sum((lifestyle_Transport == "bicycle") * weight, na.rm = TRUE) / sum_weight * 100,
    motorcycle_pct = sum((lifestyle_Transport == "motorcycle") * weight, na.rm = TRUE) / sum_weight * 100,
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
      TRUE ~ NA_character_
    ),
    car_deviation = car_pct - national_averages$car_avg,
    suv_deviation = suv_pct - national_averages$suv_avg,
    public_transit_deviation = public_transit_pct - national_averages$public_transit_avg,
    walk_deviation = walk_pct - national_averages$walk_avg,
    bicycle_deviation = bicycle_pct - national_averages$bicycle_avg,
    motorcycle_deviation = motorcycle_pct - national_averages$motorcycle_avg
  ) %>%
  filter(!is.na(party_name))

# Convert to long format for plotting
transport_by_party_long <- transport_by_party %>%
  select(party_name, car_deviation, suv_deviation, public_transit_deviation, 
         walk_deviation, bicycle_deviation, motorcycle_deviation) %>%
  pivot_longer(
    cols = c(car_deviation, suv_deviation, public_transit_deviation, 
             walk_deviation, bicycle_deviation, motorcycle_deviation),
    names_to = "transport_mode",
    values_to = "deviation"
  ) %>%
  mutate(
    transport_mode = case_when(
      transport_mode == "car_deviation" ~ "Voiture 🚗",
      transport_mode == "suv_deviation" ~ "VUS 🚙",
      transport_mode == "public_transit_deviation" ~ "Transport en commun 🚇",
      transport_mode == "walk_deviation" ~ "Marche 🚶",
      transport_mode == "bicycle_deviation" ~ "Vélo 🚲",
      transport_mode == "motorcycle_deviation" ~ "Moto 🏍️"
    )
  )

# Order parties from right to left politically
party_order <- c("Parti conservateur", "Parti libéral", "Bloc Québécois", "NPD", "Parti vert")
transport_by_party_long$party_name <- factor(transport_by_party_long$party_name, levels = party_order)

# First, for the ggplot annotations, move them even further from the chart content
# Modify the transport_plot ggplot code
transport_plot <- ggplot(transport_by_party_long, aes(x = party_name, y = deviation, fill = transport_mode)) +
  # Thicker baseline with proper positioning
  geom_hline(yintercept = 0, color = "#999999", linetype = "solid", size = 2) +
  
  # Add +/- symbols aligned with discrete axis
  annotate("text", x = 0.5, y = 5, 
           label = "+", color = "white", size = 12, fontface = "bold") +
  annotate("text", x = 0.5, y = -10, 
           label = "-", color = "white", size = 12, fontface = "bold") +
  
  # Keep the bar plot
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  
  # Add coord_cartesian to prevent clipping
  coord_cartesian(clip = "off") +
  
  # Keep existing scale and labels
  scale_fill_manual(
    name = "Mode de transport",
    values = transport_colors
  ) +
  labs(
    title = "L'INDICE TRANSPORT-POLITIQUE",
    subtitle = "Écart de préférence de transport par rapport à la moyenne nationale (points de %)",
    caption = paste0("Moyennes nationales: Voiture = ", car_national, 
                     "%, VUS = ", suv_national, 
                     "%, Transport en commun = ", transit_national,
                     "%, Marche = ", walk_national,
                     "%, Vélo = ", bicycle_national,
                     "%, Moto = ", motorcycle_national, "%"),
    x = "",
    y = ""
  ) +
  # Modified theme settings with reduced text sizes
  theme_map_dark() +
  theme(
    text = element_text(family = "Arial-Bold"),
    plot.title = element_text(face = "bold", size = 24, color = "white", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 16, color = "#CCCCCC", hjust = 0.5, margin = margin(b = 20)),
    legend.position = "none",
    axis.text.x = element_text(color = "white", size = 14, angle = 0, hjust = 0.5),
    axis.text.y = element_blank(),  # Remove y-axis labels
    panel.grid.major.y = element_line(color = "#333333", size = 0.2),
    plot.caption = element_text(color = "#BBBBBB", size = 17, hjust = 0.5, margin = margin(t = 20, b = 10)),
    plot.margin = margin(t = 20, r = 20, b = 30, l = 30),
    plot.background = element_rect(fill = "#121212", color = NA),
    panel.background = element_rect(fill = "#121212", color = NA)
  )

# Save the graph without legend, with increased height
ggsave("_SharedFolder_datagotchi_federal_2024/graph/analyses/transport/indice_transport_sans_legende.png", 
       transport_plot, 
       width = 14, 
       height = 12,
       dpi = 200,
       bg = "#121212")

# Read the graph with magick
graph_img <- image_read("_SharedFolder_datagotchi_federal_2024/graph/analyses/transport/indice_transport_sans_legende.png")

# Modified legend creation with proper labels and spacing
legend_height <- 300  # Increased height for better spacing
legend_bg <- image_blank(width = image_info(graph_img)$width,
                         height = legend_height,
                         color = "#121212")

# Parameters for icon and text positioning
x_start <- 150   # Starting X position
x_spacing <- 350 # Horizontal space between items
y_row1 <- 50     # First row Y position
y_row2 <- 180    # Second row Y position

# First Row: Voiture, VUS, Transport en commun
# Voiture 🚗
legend_bg <- image_composite(legend_bg, transport_imgs$car_icon, 
                             offset = paste0("+", x_start, "+", y_row1))
legend_bg <- image_annotate(legend_bg, "Voiture 🚗",
                            color = "white", size = 32,
                            location = paste0("+", x_start + icon_size + 30, "+", y_row1 + 10),
                            font = "Arial-Bold")

# VUS 🚙
legend_bg <- image_composite(legend_bg, transport_imgs$suv_icon, 
                             offset = paste0("+", x_start + x_spacing, "+", y_row1))
legend_bg <- image_annotate(legend_bg, "VUS 🚙",
                            color = "white", size = 32,
                            location = paste0("+", x_start + x_spacing + icon_size + 30, "+", y_row1 + 10),
                            font = "Arial-Bold")

# Transport en commun 🚇
legend_bg <- image_composite(legend_bg, transport_imgs$transit_icon, 
                             offset = paste0("+", x_start + 2*x_spacing, "+", y_row1))
legend_bg <- image_annotate(legend_bg, "Transport en commun 🚇",
                            color = "white", size = 32,
                            location = paste0("+", x_start + 2*x_spacing + icon_size + 30, "+", y_row1 + 10),
                            font = "Arial-Bold")

# Second Row: Marche, Vélo, Moto
# Marche 🚶
legend_bg <- image_composite(legend_bg, transport_imgs$walk_icon, 
                             offset = paste0("+", x_start, "+", y_row2))
legend_bg <- image_annotate(legend_bg, "Marche 🚶",
                            color = "white", size = 32,
                            location = paste0("+", x_start + icon_size + 30, "+", y_row2 + 10),
                            font = "Arial-Bold")

# Vélo 🚲
legend_bg <- image_composite(legend_bg, transport_imgs$bicycle_icon, 
                             offset = paste0("+", x_start + x_spacing, "+", y_row2))
legend_bg <- image_annotate(legend_bg, "Vélo 🚲",
                            color = "white", size = 32,
                            location = paste0("+", x_start + x_spacing + icon_size + 30, "+", y_row2 + 10),
                            font = "Arial-Bold")

# Moto 🏍️
legend_bg <- image_composite(legend_bg, transport_imgs$moto_icon, 
                             offset = paste0("+", x_start + 2*x_spacing, "+", y_row2))
legend_bg <- image_annotate(legend_bg, "Moto 🏍️",
                            color = "white", size = 32,
                            location = paste0("+", x_start + 2*x_spacing + icon_size + 30, "+", y_row2 + 10),
                            font = "Arial-Bold")

# Create caption with source info
# Create caption with source info - cafe style positioning
caption_height <- 150  # Reduced from 180
caption_bg <- image_blank(width = image_info(graph_img)$width,
                          height = caption_height,
                          color = "#121212")

# Add source information with cafe style positioning and reduced text size
caption <- image_annotate(caption_bg,
                          paste0("Source: Léger-Datagotchi 2025 | n=", format(n_observations, big.mark = " ")),
                          color = "#BBBBBB",
                          size = 28,  # Reduced from 40
                          location = "+40+30",
                          font = "Arial-Bold")

caption <- image_annotate(caption,
                          "Données pondérées selon: le genre, l'âge, la province, la langue, le niveau d'éducation, le revenu, l'immigration, le type d'habitation",
                          color = "#BBBBBB",
                          size = 26,  # Reduced from 40
                          location = "+40+70",  # Adjusted for smaller text
                          font = "Arial-Bold")

# Logo positioning more like the cafe style
logo_x_pos <- image_info(caption)$width - image_info(logo_resized)$width - 50
logo_y_pos <- 40  # Like in cafe code

# Add logo
logo_path <- "_SharedFolder_datagotchi_federal_2024/logos/FR/logo_fr.png"
logo <- image_read(logo_path)
logo_width <- round(image_info(graph_img)$width * 0.15)
logo_resized <- image_scale(logo, paste0(logo_width, "x"))

logo_x_pos <- image_info(caption)$width - image_info(logo_resized)$width - 40
logo_y_pos <- 30
caption_with_logo <- image_composite(
  caption, 
  logo_resized, 
  offset = paste0("+", logo_x_pos, "+", logo_y_pos)
)

# Assemble final image
final_image <- image_append(c(graph_img, legend_bg, caption_with_logo), stack = TRUE)

# Add border
final_with_border <- image_border(final_image, "#121212", "40x40")

# Save final image
image_write(final_with_border, "_SharedFolder_datagotchi_federal_2024/graph/analyses/transport/indice_transport_final.png")

cat("Transport-Politique index graph created successfully!\n")



