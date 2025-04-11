library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(cartessn)
library(patchwork)
library(cowplot)
library(showtext)
library(magick)
library(clessnize)

# 1. Chargement des données
data <- readRDS("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/datagotchi2025_canada_appPonderees_20250410.rds")

# 2. Chargement des données spatiales depuis cartessn
sf_ridings <- cartessn::spatial_canada_2022_electoral_ridings_aligned
sf_rta <- cartessn::spatial_canada_2021_rta

# 3. Renommage pour la cohérence avec le script original
data <- data %>%
  rename(id_riding = ses_riding_id)

# 4. Vérifier le taux de disponibilité des IDs de circonscription
matched_count <- sum(!is.na(data$id_riding))
total_count <- nrow(data)
match_rate <- matched_count / total_count * 100
print(paste("Nombre total de répondants:", total_count))
print(paste("Nombre de répondants avec une circonscription identifiée:", matched_count))
print(paste("Taux de disponibilité:", round(match_rate, 2), "%"))

# 5. S'assurer que la variable weight existe, sinon créer une avec valeur 1
if(!"weight" %in% names(data)) {
  data$weight <- 1
}

# 6. Analyse des 4 chaînes de café par circonscription AVEC PONDÉRATION
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
  mutate(id_riding = as.character(id_riding)) %>%
  left_join(coffee_battle_by_riding %>% 
              mutate(id_riding = as.character(id_riding)), 
            by = "id_riding")


# 12. Sauvegarder les résultats intermédiaires
saveRDS(coffee_battle_by_riding, "_SharedFolder_datagotchi_federal_2024/reports/coffee_battle_pondere_light.rds")

# 13. Paramètres pour éviter les problèmes de mémoire
options(future.globals.maxSize = 1000 * 1024^2)  # Augmenter la limite à 1 Go
sf_use_s2(FALSE)  # Désactiver les fonctionnalités S2 de sf pour réduire l'utilisation de la mémoire

# 14. Thème simplifié pour les cartes en mode light
theme_map_light <- function() {
  theme_minimal() +
    theme(
      # Fond blanc
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      
      # Suppression des axes et grilles
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      
      # Textes en noir
      plot.title = element_text(face = "bold", size = 14, color = "black", hjust = 0.5),
      plot.subtitle = element_text(size = 11, color = "#555555", hjust = 0.5),
      plot.caption = element_text(size = 12, color = "#666666", hjust = 1),
      
      # Légende
      legend.position = "bottom",
      legend.background = element_rect(fill = "white", color = NA),
      legend.title = element_text(size = 10, color = "black"),
      legend.text = element_text(size = 9, color = "#555555")
    )
}

# 15. Définition des couleurs pour les chaînes de café (couleurs plus vives pour mode light)
coffee_colors <- c(
  "Tim Hortons 🇨🇦" = "#C8102E",   # Rouge Tim Hortons
  "McDonald's 🇺🇸" = "#FFC72C",    # Jaune McDonald's
  "Starbucks 🇺🇸" = "#00704A",     # Vert Starbucks
  "Second Cup 🇨🇦" = "#4f4f4f",    # Gris Second Cup
  "Non disponible" = "#EEEEEE"      # Gris clair
)

# 16. Prétraitement des données
sf_coffee_map_clean <- sf_coffee_map %>%
  mutate(dominant_chain = ifelse(is.na(dominant_chain), "Non disponible", dominant_chain))

# 17. Information sur le nombre d'observations
n_observations <- nrow(data)  # Utilisez le nombre réel de répondants

# 18. ===== CARTE DU CANADA =====
canada_coffee_map <- ggplot(sf_coffee_map_clean) +
  geom_sf(aes(fill = dominant_chain), color = "#DDDDDD", size = 0.2) +
  scale_fill_manual(
    name = "Chaîne dominante",
    values = coffee_colors,
    breaks = c("Tim Hortons 🇨🇦", "McDonald's 🇺🇸", "Starbucks 🇺🇸", "Second Cup 🇨🇦")
  ) +
  theme_map_light() +
  theme(legend.position = "none")

ggsave("canada_coffee_map_light_2.0.png", 
       canada_coffee_map, 
       width = 16, 
       height = 12, 
       dpi = 200,
       bg = "white")

# 19. ===== CARTES URBAINES =====
main_regions <- c("montreal", "toronto", "vancouver", "quebec_city")

# 20. Créer et sauvegarder chaque carte urbaine individuellement
for (region in main_regions) {
  # Extraire la région
  region_map <- cartessn::crop_map(sf_coffee_map_clean, region)
  
  # Créer la carte manuellement
  city_map <- ggplot(region_map) +
    geom_sf(aes(fill = dominant_chain), color = "#DDDDDD", size = 0.15) +
    scale_fill_manual(
      values = coffee_colors,
      breaks = c("Tim Hortons 🇨🇦", "McDonald's 🇺🇸", "Starbucks 🇺🇸", "Second Cup 🇨🇦")
    ) +
    theme_map_light() +
    theme(legend.position = "none")
  
  # Sauvegarder chaque carte urbaine séparément avec aspect ratio carré
  ggsave(paste0(tolower(gsub("-", "_", region)), "_coffee_map_light.png"), 
         city_map, 
         width = 6, 
         height = 6, 
         dpi = 150,
         bg = "white")
}

# 21. Paramètres dimensionnels pour l'image finale
canvas_width <- 1800      # Largeur totale du canvas
canada_height <- 1000     # Hauteur pour la carte du Canada
city_height <- 400        # Hauteur pour les cartes de villes
city_spacing <- 20        # Espacement entre les cartes de villes
section_spacing <- 40     # Espacement entre les sections

# 22. Fonction pour créer une carte de ville avec de meilleures proportions
create_city_map <- function(region_name, display_title = NULL) {
  # Utiliser le titre personnalisé si fourni, sinon utiliser region_name
  display_name <- ifelse(is.null(display_title), toupper(region_name), toupper(display_title))
  
  # Lire l'image existante
  img_path <- paste0(tolower(gsub("-", "_", region_name)), "_coffee_map_light.png")
  img <- image_read(img_path)
  
  # Redimensionner l'image en préservant le ratio carré
  img_resized <- image_scale(img, paste0(toString(city_height), "x", toString(city_height)))
  
  # Créer un canvas blanc avec une largeur fixe pour toutes les villes
  city_width <- city_height  # Maintenir un aspect carré
  canvas <- image_blank(width = city_width, 
                        height = city_height + 60,  # Plus d'espace pour le titre
                        color = "white")
  
  # Placer l'image sur le canvas (centrée)
  canvas_with_map <- image_composite(canvas, img_resized, 
                                     gravity = "center")
  
  # Ajouter le titre en bas
  canvas_with_title <- image_annotate(canvas_with_map, 
                                      display_name,
                                      color = "black", 
                                      size = 28,
                                      font = "Arial-Bold",
                                      gravity = "south",
                                      location = "+0+20")
  
  return(canvas_with_title)
}

# 23. Lire et redimensionner la carte du Canada
canada_img <- image_read("canada_coffee_map_light_2.0.png")
canada_resized <- image_scale(canada_img, paste0(toString(canvas_width - 40), "x", toString(canada_height)))

# 24. Créer un canvas pour la carte du Canada
canada_canvas <- image_blank(width = canvas_width, 
                             height = canada_height + 60,  # Plus d'espace pour éviter le rognage
                             color = "white")

# 25. Centrer la carte du Canada
canada_centered <- image_composite(canada_canvas, canada_resized, 
                                   gravity = "center")

# 26. Créer les cartes de villes avec de meilleures proportions
montreal_map <- create_city_map("montreal")
toronto_map <- create_city_map("toronto")
vancouver_map <- create_city_map("vancouver")
quebec_map <- create_city_map("quebec_city", "Québec")

# 27. Calculer l'espacement latéral pour centrer les cartes de villes
city_width = image_info(montreal_map)$width
city_total_width <- 4 * city_width + (3 * city_spacing)
city_padding <- max(0, (canvas_width - city_total_width) / 2)

# 28. Créer des séparateurs plus visibles entre les villes
city_separator <- image_blank(width = city_spacing, 
                              height = image_info(montreal_map)$height, 
                              color = "white")

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
  left_padding <- image_blank(width = city_padding, height = image_info(city_row)$height, color = "white")
  right_padding <- image_blank(width = city_padding, height = image_info(city_row)$height, color = "white")
  city_row_padded <- image_append(c(left_padding, city_row, right_padding), stack = FALSE)
} else {
  city_row_padded <- city_row
}

# 31. Titre principal avec dimensions augmentées
title_height <- 100  # Hauteur augmentée
title_bg <- image_blank(width = canvas_width,
                        height = title_height,
                        color = "white")

title <- image_annotate(title_bg,
                        "LA BATAILLE DU CAFÉ AU CANADA",
                        color = "black",
                        size = 48,  # Taille augmentée
                        gravity = "center",
                        font = "Arial-Bold")

# 32. Sous-titre avec dimensions augmentées
subtitle_height <- 60  # Hauteur augmentée
subtitle_bg <- image_blank(width = canvas_width,
                           height = subtitle_height,
                           color = "white")

subtitle <- image_annotate(subtitle_bg,
                           "Chaîne de café préférée par circonscription électorale",
                           color = "#555555",
                           size = 32,  # Taille augmentée
                           gravity = "center",
                           font = "Arial")

# 33. Légende améliorée avec plus d'espace
legend_height <- 100  # Hauteur augmentée
legend_bg <- image_blank(width = canvas_width,
                         height = legend_height,
                         color = "white")

# 34-35. Ajouter les étiquettes de la légende et les images au lieu des carrés colorés
legend_text <- image_annotate(legend_bg,
                              "Chaîne dominante",
                              color = "black",
                              size = 32,
                              location = "+40+30",
                              font = "Arial")

# Remplacer les carrés colorés par des icônes de café
# Chemins des images
starbucks_icon_path <- "_SharedFolder_datagotchi_federal_2024/graph/analyses/café/CoffeePack/CoffeePack__0001_cafe-2-starbuck.png"
mcdo_icon_path <- "_SharedFolder_datagotchi_federal_2024/graph/analyses/café/CoffeePack/CoffeePack__0002_cafe-3-mcdo.png"
tim_icon_path <- "_SharedFolder_datagotchi_federal_2024/graph/analyses/café/CoffeePack/CoffeePack__0007_tim.png"
secondcup_icon_path <- "_SharedFolder_datagotchi_federal_2024/graph/analyses/café/CoffeePack/CoffeePack__0003_cafe-4-secondCut.png"

# Charger les images
starbucks_icon <- image_read(starbucks_icon_path)
mcdo_icon <- image_read(mcdo_icon_path)
tim_icon <- image_read(tim_icon_path)

# Modifier ces sections :
# Redimensionnement des icônes (augmenter la taille)
icon_size <- 95  # Augmenté de 65 à 130
starbucks_icon_resized <- image_scale(starbucks_icon, paste0(icon_size, "x", icon_size))
mcdo_icon_resized <- image_scale(mcdo_icon, paste0(icon_size, "x", icon_size))
tim_icon_resized <- image_scale(tim_icon, paste0(icon_size, "x", icon_size))

# Ajustement du positionnement
x_start <- 500  # Début plus à gauche pour compenser la taille accrue
x_spacing <- 450  # Espacement augmenté entre les éléments

# Tim Hortons
legend_text <- image_composite(legend_text, tim_icon_resized, 
                               offset = paste0("+", x_start, "+20"))  # Ajustement vertical
legend_text <- image_annotate(legend_text, 
                              "Tim Hortons 🇨🇦",
                              color = "black",
                              size = 28,
                              location = paste0("+", x_start + icon_size + 30, "+36"),  # Décalage augmenté
                              font = "Arial")

# McDonald's
legend_text <- image_composite(legend_text, mcdo_icon_resized, 
                               offset = paste0("+", x_start + x_spacing, "+20"))  # Ajustement vertical
legend_text <- image_annotate(legend_text, 
                              "McDonald's 🇺🇸",
                              color = "black",
                              size = 28,
                              location = paste0("+", x_start + x_spacing + icon_size + 30, "+36"),  # Décalage augmenté
                              font = "Arial")

# Starbucks
legend_text <- image_composite(legend_text, starbucks_icon_resized, 
                               offset = paste0("+", x_start + 2*x_spacing, "+20"))  # Ajustement vertical
legend_text <- image_annotate(legend_text, 
                              "Starbucks 🇺🇸",
                              color = "black",
                              size = 28,
                              location = paste0("+", x_start + 2*x_spacing + icon_size + 30, "+36"),  # Décalage augmenté
                              font = "Arial")

# 36. Note méthodologique avec dimensions augmentées
caption_height <- 80  # Hauteur augmentée
caption_bg <- image_blank(width = canvas_width,
                          height = caption_height,
                          color = "white")

# Utilise le nombre réel d'observations
caption <- image_annotate(caption_bg,
                          paste0("Source: Léger-Datagotchi 2025 | n=", format(n_observations, big.mark = " ")),
                          color = "#555555",
                          size = 12,  # Taille augmentée
                          location = "+40+25",  # Position ajustée
                          font = "Arial")

caption <- image_annotate(caption,
                          "Données pondérées selon: le genre, l'âge, la province, la langue, le niveau d'éducation, le revenu, l'immigration, le type d'habitation",
                          color = "#555555",
                          size = 22,  # Taille augmentée
                          location = "+40+55",  # Position ajustée
                          font = "Arial")

# 37. Ligne séparatrice plus visible
separator_height <- 3  # Épaisseur augmentée
separator <- image_blank(width = canvas_width,
                         height = separator_height,
                         color = "#AAAAAA")  # Couleur grise claire

# 38. Espacement entre sections
spacer <- image_blank(width = canvas_width,
                      height = section_spacing,
                      color = "white")

# 39. Assembler l'image finale avec le nouvel ordre et meilleurs espacements
final_image <- c(
  title,                           # Titre principal
  subtitle,                        # Sous-titre
  spacer,                          # Espacement
  separator,                       # Ligne de séparation
  spacer,                          # Espacement
  city_row_padded,                 # Cartes des villes avec espacement amélioré
  spacer,                          # Espacement
  separator,                       # Ligne de séparation
  spacer,                          # Espacement
  legend_text,                     # Légende 
  spacer,                          # Espacement
  separator,                       # Ligne de séparation
  spacer,                          # Espacement
  canada_centered,                 # Carte du Canada
  spacer,                          # Espacement
  caption                          # Notes méthodologiques
)

final_combined <- image_append(final_image, stack = TRUE)

# 40. Ajouter une bordure blanche
final_with_border <- image_border(final_combined, "white", "30x30")  # Bordure plus grande

# 41. Charger le logo
logo_path <- "_SharedFolder_datagotchi_federal_2024/logos/FR/logo_fr.png"
logo <- image_read(logo_path)

# 42. Redimensionner le logo à une taille appropriée
# Définir la largeur du logo à 15% de la largeur de l'image
logo_width <- round(image_info(final_with_border)$width * 0.15)
logo_resized <- image_scale(logo, paste0(logo_width, "x"))

# 43. Calculer la position pour le coin inférieur droit
# Laisser une marge de 30 pixels par rapport aux bords
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
image_write(final_with_logo, "_SharedFolder_datagotchi_federal_2024/graph/analyses/café/bataille_cafe_canada_light_avec_logo.png")

cat("Image finale avec logo créée avec succès : bataille_cafe_canada_light_avec_logo.png\n")

# Version simplifiée du graphique café-politique avec élimination des doublons

# 46. Reprendre les calculs essentiels pour l'indice café-politique
national_averages <- data %>%
  summarize(
    sum_weight = sum(weight, na.rm = TRUE),
    tim_hortons_avg = sum((lifestyle_consCoffeeTimHortons == 1) * weight, na.rm = TRUE) / sum_weight * 100,
    mcdo_avg = sum((lifestyle_consCoffeeMcDo == 1) * weight, na.rm = TRUE) / sum_weight * 100,
    starbucks_avg = sum((lifestyle_consCoffeeStarbucks == 1) * weight, na.rm = TRUE) / sum_weight * 100
  )

# Arrondir les valeurs pour l'affichage
tim_national <- round(national_averages$tim_hortons_avg, 1)
mcdo_national <- round(national_averages$mcdo_avg, 1)
starbucks_national <- round(national_averages$starbucks_avg, 1)

# Calcul des écarts par parti
coffee_by_party <- data %>%
  # Filtrer les NA et limiter aux partis politiques que nous voulons analyser
  filter(!is.na(dv_voteChoice)) %>%
  filter(dv_voteChoice %in% c("lpc", "cpc", "ndp", "bq", "gpc")) %>%
  group_by(dv_voteChoice) %>%
  summarize(
    sum_weight = sum(weight, na.rm = TRUE),
    tim_fans_pct = sum((lifestyle_consCoffeeTimHortons == 1) * weight, na.rm = TRUE) / sum_weight * 100,
    mcdo_fans_pct = sum((lifestyle_consCoffeeMcDo == 1) * weight, na.rm = TRUE) / sum_weight * 100,
    starbucks_fans_pct = sum((lifestyle_consCoffeeStarbucks == 1) * weight, na.rm = TRUE) / sum_weight * 100,
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
    tim_deviation = tim_fans_pct - national_averages$tim_hortons_avg,
    mcdo_deviation = mcdo_fans_pct - national_averages$mcdo_avg,
    starbucks_deviation = starbucks_fans_pct - national_averages$starbucks_avg
  ) %>%
  # Filtrer à nouveau pour éliminer tout parti dont le nom est NA
  filter(!is.na(party_name))

# Préparation des données pour le graphique
coffee_by_party_long <- coffee_by_party %>%
  select(party_name, tim_deviation, mcdo_deviation, starbucks_deviation) %>%
  pivot_longer(
    cols = c(tim_deviation, mcdo_deviation, starbucks_deviation),
    names_to = "coffee_chain",
    values_to = "deviation"
  ) %>%
  mutate(
    coffee_chain = case_when(
      coffee_chain == "tim_deviation" ~ "Tim Hortons 🇨🇦",
      coffee_chain == "mcdo_deviation" ~ "McDonald's 🇺🇸",
      coffee_chain == "starbucks_deviation" ~ "Starbucks 🇺🇸"
    )
  )

# Ordonner les partis politiques du plus à droite au plus à gauche
party_order <- c("Parti conservateur", "Parti libéral", "Bloc Québécois", "NPD", "Parti vert")
coffee_by_party_long$party_name <- factor(coffee_by_party_long$party_name, levels = party_order)

# Couleurs pour les chaînes de café (mêmes couleurs que dans la version sombre)
coffee_colors <- c(
  "Tim Hortons 🇨🇦" = "#C8102E",   # Rouge Tim Hortons
  "McDonald's 🇺🇸" = "#FFC72C",    # Jaune McDonald's
  "Starbucks 🇺🇸" = "#00704A"      # Vert Starbucks
)

# Sous-titre avec les moyennes nationales
ref_subtitle <- paste0("Moyennes nationales: Tim Hortons = ", tim_national, 
                       "%, McDonald's = ", mcdo_national, 
                       "%, Starbucks = ", starbucks_national, "%")

# Créer un graphique simplifié en mode light
simplified_plot <- ggplot(coffee_by_party_long, aes(x = party_name, y = deviation, fill = coffee_chain)) +
  # Ligne médiane plus épaisse
  # Replace geom_hline with geom_segment
  geom_segment(
    x = 0.58,                 # Starting x position (0.5 position on the x-axis)
    xend = length(party_order) + 0.5, # End at the last party (adjustable as needed)
    y = 0,                   # y position (zero line)
    yend = 0,                # keep y position the same to create a horizontal line
    color = "#999999", 
    linetype = "solid", 
    size = 2
  ) +
  
  # Barres du graphique
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  
  # Add +/- symbols aligned with discrete axis
  annotate("text", x = 0.5, y = 0, 
           label = "moyenne canadienne", color = "black", size = 12, fontface = "bold", angle = 90) +
  annotate("text", x = 0.5, y = 10, 
           label = "+", color = "black", size = 10, fontface = "bold") +
  annotate("text", x = 0.5, y = -10, 
           label = "-", color = "black", size = 10, fontface = "bold") +
  
  scale_fill_manual(
    name = "Chaîne de café",
    values = coffee_colors
  ) +
  
  labs(
    title = "L'INDICE CAFÉ-POLITIQUE",
    subtitle = "Écart de consommation par rapport à la moyenne canadienne (points de %)",
    caption = paste0("Moyennes canadiennes: Tim Hortons = ", tim_national, "%, McDonald's = ", mcdo_national, "%, Starbucks = ", starbucks_national, "%"),
    x = "",
    y = ""
  ) +
  theme_datagotchi_light() +
  theme(
    plot.title = element_text(face = "bold", size = 60, color = "black", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(face = "bold", size = 40, color = "#555555", hjust = 0.5, margin = margin(b = 20)),
    # Suppression de la légende standard
    legend.position = "none",
    axis.text.x = element_text(color = "black", size = 34, angle = 0, hjust = 0.5),
    # Suppression des étiquettes sur l'axe Y 
    axis.text.y = element_blank(),
    panel.grid.major.y = element_line(color = "#DDDDDD", size = 0.2),
    plot.margin = margin(t = 20, r = 20, b = 30, l = 30),
    plot.caption = element_text(color = "#666666", size = 17, hjust = 0.5, margin = margin(t = 20, b = 10)),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Sauvegarder le graphique sans légende
ggsave("_SharedFolder_datagotchi_federal_2024/graph/analyses/café/indice_cafe_light_sans_legende.png", 
       simplified_plot, 
       width = 14, 
       height = 10,
       dpi = 200,
       bg = "white")

# Ajout des éléments graphiques avec magick
library(magick)

# Lire le graphique généré
plot_img <- image_read("_SharedFolder_datagotchi_federal_2024/graph/analyses/café/indice_cafe_light_sans_legende.png")

# Dimensions
img_info <- image_info(plot_img)
width <- img_info$width
height <- img_info$height

# Créer une légende centrée
legend_height <- 100
legend_bg <- image_blank(width, legend_height, color = "white")

# Charger les icônes
tim_icon <- image_read("_SharedFolder_datagotchi_federal_2024/graph/analyses/café/CoffeePack/CoffeePack__0007_tim.png") %>% 
  image_scale("80x80")
mcdo_icon <- image_read("_SharedFolder_datagotchi_federal_2024/graph/analyses/café/CoffeePack/CoffeePack__0002_cafe-3-mcdo.png") %>% 
  image_scale("80x80")
starbucks_icon <- image_read("_SharedFolder_datagotchi_federal_2024/graph/analyses/café/CoffeePack/CoffeePack__0001_cafe-2-starbuck.png") %>% 
  image_scale("80x80")

# Positionnement des icônes
icon_spacing <- 400
start_x <- (width - (3 * 50 + 2 * icon_spacing)) / 2

# Composite la légende
legend_bg <- legend_bg %>%
  image_composite(tim_icon, offset = paste0("+", start_x, "+25")) %>%
  image_annotate("Tim Hortons 🇨🇦", color = "black", size = 40, 
                 location = paste0("+", start_x + 100, "+35"),
                 font = "Arial-Bold") %>%
  image_composite(mcdo_icon, offset = paste0("+", start_x + icon_spacing + 50, "+30")) %>%
  image_annotate("McDonald's 🇺🇸", color = "black", size = 40,
                 location = paste0("+", start_x + icon_spacing + 150, "+35"),
                 font = "Arial-Bold") %>%
  image_composite(starbucks_icon, offset = paste0("+", start_x + 2*icon_spacing + 100, "+30")) %>%
  image_annotate("Starbucks 🇺🇸", color = "black", size = 40,
                 location = paste0("+", start_x + 2*icon_spacing + 200, "+35"),
                 font = "Arial-Bold")

# Créer le pied de page
footer_height <- 200
footer <- image_blank(width, footer_height, color = "white") %>%
  image_annotate(paste0("Source: Léger-Datagotchi 2025 | n=", format(n_observations, big.mark = " ")), 
                 color = "#555555", 
                 size = 40,
                 location = "+40+30", 
                 font = "Arial", 
                 gravity = "west") %>%
  image_annotate("Données pondérées selon: le genre, l'âge, la province, la langue, le niveau d'éducation, le revenu, l'immigration, le type d'habitation",
                 color = "#555555",
                 size = 30,
                 location = "+40+55",
                 font = "Arial",
                 gravity = "west")

# Ajouter le logo Datagotchi
logo <- image_read("_SharedFolder_datagotchi_federal_2024/logos/FR/logo_fr.png") %>% 
  image_scale("300x") %>%
  image_background("white")

footer <- footer %>%
  image_composite(logo, gravity = "east", offset = "+40+0")

# Assemblage final
final_img <- image_blank(width, height + legend_height + footer_height, color = "white") %>%
  image_composite(plot_img, offset = "+0+0") %>%
  image_composite(legend_bg, offset = paste0("+0+", height)) %>%
  image_composite(footer, offset = paste0("+0+", height + legend_height))

# Sauvegarder
image_write(final_img, "_SharedFolder_datagotchi_federal_2024/graph/analyses/café/indice_cafe_politique_light_final.png")
