tatou



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
data <- readRDS("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/datagotchi2025_canada_appPonderees_20250330.rds")
names(data)
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
#mapping_results <- readRDS("_SharedFolder_datagotchi_federal_2024/reports/mapping_results_ridings_rta.rds")
#saveRDS(mapping_results, "_SharedFolder_datagotchi_federal_2024/reports/mapping_results_ridings_rta.rds")


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
# 6. Agréger les données pour la question sur les tatouages
# Ici, la variable 'lifestyle_hasTattoos' est numérique :
# 0 pour "non", 1 pour "oui". On calcule donc la somme pondérée pour 1.
tattoos_by_riding <- data %>%
  dplyr::filter(!is.na(id_riding)) %>%
  dplyr::group_by(id_riding) %>%
  dplyr::summarize(
    sum_weight    = sum(weight, na.rm = TRUE),
    tattoo_weight = sum(lifestyle_hasTattoos * weight, na.rm = TRUE),
    pct_tattoo    = tattoo_weight / sum_weight * 100,
    n_people      = n()
  ) %>%
  dplyr::ungroup()

# 7. Joindre ces résultats agrégés à la couche spatiale des circonscriptions
sf_ridings_with_tattoos <- sf_ridings %>%
  dplyr::left_join(tattoos_by_riding, by = "id_riding")

# 8. Sauvegarder les résultats intermédiaires
saveRDS(tattoos_by_riding, "_SharedFolder_datagotchi_federal_2024/reports/tatou_pondere.rds")

# 9. Paramètres pour éviter les problèmes de mémoire
options(future.globals.maxSize = 1000 * 1024^2)  # Augmenter la limite à 1 Go
sf_use_s2(FALSE)  # Désactiver les fonctionnalités S2 de sf pour réduire l'utilisation de la mémoire


# 10. Thème simplifié pour les cartes
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

# 14. Création de la carte du Canada pour les tatouages
for (region in main_regions) {
  # Extraire la région avec la fonction crop_map de cartessn
  region_map <- cartessn::crop_map(sf_ridings_with_tattoos, region)
  
  # Créer la carte pour la région
  city_map <- ggplot(region_map) +
    geom_sf(aes(fill = pct_tattoo), color = "#121212", size = 0.15) +
    scale_fill_viridis_c(option = "plasma", name = "Pourcentage\nde tatouages (%)") +
    labs(title = toupper(region)) +
    theme_map_dark() +
    theme(legend.position = "none")
  
  # Sauvegarder la carte urbaine (format carré) dans le répertoire spécifié
  ggsave(paste0("_SharedFolder_datagotchi_federal_2024/graph/analyses/tatou/", 
                tolower(gsub("-", "_", region)), "_tattoo_map.png"), 
         city_map, 
         width = 6, 
         height = 6, 
         dpi = 150,
         bg = "#121212")
}

# 17. Assemblage final avec magick pour créer une vue d’ensemble

# Paramètres pour le canvas final
canvas_width <- 1800      # Largeur totale du canvas
canada_height <- 1000     # Hauteur de la carte du Canada
city_height <- 400        # Hauteur des cartes urbaines
city_spacing <- 20        # Espacement entre les cartes urbaines
section_spacing <- 20     # Espacement entre les sections

# 18. Fonction pour créer une image de carte urbaine avec titre (via magick)
create_city_map <- function(region_name, display_title = NULL) {
  display_name <- ifelse(is.null(display_title), toupper(region_name), toupper(display_title))
  
  # Chemin de l'image sauvegardée pour la région
  img_path <- paste0(tolower(gsub("-", "_", region_name)), "_tattoo_map.png")
  img <- image_read(img_path)
  
  # Redimensionner l'image en conservant un format carré
  img_resized <- image_scale(img, paste0(as.character(city_height), "x", as.character(city_height)))
  
  # Créer un canvas noir pour la carte
  city_width <- city_height
  canvas <- image_blank(width = city_width, height = city_height + 60, color = "#121212")
  
  # Centrer l'image sur le canvas
  canvas_with_map <- image_composite(canvas, img_resized, gravity = "center")
  
  # Ajouter un titre en bas du canvas
  canvas_with_title <- image_annotate(canvas_with_map, 
                                      display_name,
                                      color = "white", 
                                      size = 28,
                                      font = "Arial-Bold",
                                      gravity = "south",
                                      location = "+0+20")
  
  return(canvas_with_title)
}

# 19. Lire et redimensionner la carte du Canada
canada_img <- image_read("_SharedFolder_datagotchi_federal_2024/graph/analyses/tatou/canada_tattoo_map.png")
canada_resized <- image_scale(canada_img, paste0(as.character(canvas_width - 40), "x", as.character(canada_height)))

# 20. Créer un canvas pour la carte du Canada et centrer la carte
canada_canvas <- image_blank(width = canvas_width, height = canada_height + 60, color = "#121212")
canada_centered <- image_composite(canada_canvas, canada_resized, gravity = "center")

# 21. Création des images pour chacune des cartes urbaines
montreal_map <- create_city_map("_SharedFolder_datagotchi_federal_2024/graph/analyses/tatou/montreal_tattoo_map.png")
toronto_map <- create_city_map("_SharedFolder_datagotchi_federal_2024/graph/analyses/tatou/toronto_tattoo_map.png")
vancouver_map <- create_city_map("_SharedFolder_datagotchi_federal_2024/graph/analyses/tatou/vancouver_tattoo_map.png")
quebec_map <- create_city_map("_SharedFolder_datagotchi_federal_2024/graph/analyses/tatou/quebec_city_tattoo_map.png", "QUÉBEC")

# 22. Calculer l'espacement latéral pour centrer les cartes urbaines
city_width <- image_info(montreal_map)$width
city_total_width <- 4 * city_width + (3 * city_spacing)
city_padding <- max(0, (canvas_width - city_total_width) / 2)

# 23. Créer un séparateur pour les cartes urbaines
city_separator <- image_blank(width = city_spacing, height = image_info(montreal_map)$height, color = "#121212")

# 24. Assembler les cartes urbaines en une rangée
city_row <- image_append(c(montreal_map, city_separator, toronto_map, city_separator, vancouver_map, city_separator, quebec_map), stack = FALSE)

# 25. Appliquer un padding latéral si nécessaire
if (city_padding > 0) {
  left_padding <- image_blank(width = city_padding, height = image_info(city_row)$height, color = "#121212")
  right_padding <- image_blank(width = city_padding, height = image_info(city_row)$height, color = "#121212")
  city_row_padded <- image_append(c(left_padding, city_row, right_padding), stack = FALSE)
} else {
  city_row_padded <- city_row
}

# 26. Créer le titre principal avec magick
title_height <- 100
title_bg <- image_blank(width = canvas_width, height = title_height, color = "#121212")
title <- image_annotate(title_bg,
                        "LA DIFFUSION DES TATOUAGES AU CANADA",
                        color = "white",
                        size = 48,
                        gravity = "center",
                        font = "Arial-Bold")

# 27. Créer le sous-titre
subtitle_height <- 60
subtitle_bg <- image_blank(width = canvas_width, height = subtitle_height, color = "#121212")
subtitle <- image_annotate(subtitle_bg,
                           "Pourcentage de répondants avec tatouages par circonscription",
                           color = "#CCCCCC",
                           size = 32,
                           gravity = "center",
                           font = "Arial-Bold")

# 28. Créer une ligne de séparation
separator_height <- 3
separator <- image_blank(width = canvas_width, height = separator_height, color = "#555555")

# 29. Espacement entre sections
spacer <- image_blank(width = canvas_width, height = section_spacing, color = "#121212")

# 30. Créer une légende simplifiée (texte indiquant l'échelle de couleur)
legend_text <- image_blank(width = canvas_width, height = 100, color = "#121212")
legend_text <- image_annotate(legend_text,
                              "Échelle de couleur : Pourcentage de tatouages (%)",
                              color = "white",
                              size = 32,
                              gravity = "center",
                              font = "Arial-Bold")

# 31. Créer la note méthodologique
caption_height <- 80
caption_bg <- image_blank(width = canvas_width, height = caption_height, color = "#121212")
n_observations <- nrow(data)
caption <- image_annotate(caption_bg,
                          paste0("Source: Léger-Datagotchi 2025 | n = ", format(n_observations, big.mark = " ")),
                          color = "#BBBBBB",
                          size = 24,
                          location = "+40+25",
                          font = "Arial-Bold")
caption <- image_annotate(caption,
                          "Données pondérées selon le genre, l'âge, la province, la langue, le niveau d'éducation, le revenu, l'immigration et le type d'habitation",
                          color = "#BBBBBB",
                          size = 22,
                          location = "+40+55",
                          font = "Arial-Bold")

# 32. Assembler l'image finale avec tous les éléments
final_image <- c(
  title,                           # Titre principal
  subtitle,                        # Sous-titre
  spacer,                          # Espacement
  separator,                       # Séparateur
  spacer,                          # Espacement
  legend_text,                     # Légende simplifiée
  spacer,                          # Espacement
  separator,                       # Séparateur
  spacer,                          # Espacement
  city_row_padded,                 # Rangée des cartes urbaines
  spacer,                          # Espacement
  separator,                       # Séparateur
  spacer,                          # Espacement
  canada_centered,                 # Carte du Canada
  spacer,                          # Espacement
  caption                          # Note méthodologique
)
final_combined <- image_append(final_image, stack = TRUE)

# 33. Ajouter une bordure noire à l'image finale
final_with_border <- image_border(final_combined, "#121212", "30x30")

# 34. Ajouter le logo si disponible
logo_path <- "_SharedFolder_datagotchi_federal_2024/logos/FR/logo_fr.png"
if (file.exists(logo_path)) {
  logo <- image_read(logo_path)
  logo_width <- round(image_info(final_with_border)$width * 0.15)
  logo_resized <- image_scale(logo, paste0(logo_width, "x"))
  margin <- 30
  x_position <- image_info(final_with_border)$width - image_info(logo_resized)$width - margin
  y_position <- image_info(final_with_border)$height - image_info(logo_resized)$height - margin
  final_with_logo <- image_composite(final_with_border, logo_resized, offset = paste0("+", x_position, "+", y_position))
  
  image_write(final_with_logo, "_SharedFolder_datagotchi_federal_2024/graph/analyses/tatouages/diffusion_tatouages_final_avec_logo.png")
  cat("Image finale avec logo créée avec succès : diffusion_tatouages_final_avec_logo.png\n")
} else {
  image_write(final_with_border, "_SharedFolder_datagotchi_federal_2024/graph/analyses/tatouages/diffusion_tatouages_final.png")
  cat("Image finale sans logo créée avec succès : diffusion_tatouages_final.png\n")
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