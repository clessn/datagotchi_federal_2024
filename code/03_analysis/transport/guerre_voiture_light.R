library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(magick)
library(patchwork)

# 1. Load datasets (assuming data, sf_ridings, sf_rta are already available)
# This follows similar steps to the original script, but we'll modify to show deviations

# 2. Chargement des données spatiales depuis cartessn
sf_ridings <- cartessn::spatial_canada_2022_electoral_ridings_aligned
sf_rta <- cartessn::spatial_canada_2021_rta

# 3. Extraction des 3 premiers caractères du code postal (RTA)
data$rta <- substr(data$ses_postalCode, 1, 3)

# 4. Load mapping results
mapping_results <- readRDS("_SharedFolder_datagotchi_federal_2024/_previous/mapping_results_ridings_rta.rds")

# 5. Get main mapping (RTA -> riding with best coverage)
rta_to_riding <- mapping_results$fsa_to_riding_mapping %>%
  select(rta, id_riding)

# 6. Join riding ID to data
data <- data %>%
  left_join(rta_to_riding, by = "rta")

# 7. Make sure weight exists, otherwise create with value 1
if(!"weight" %in% names(data)) {
  data$weight <- 1
}

# 8. Calculate national averages for each transport mode
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

# 9. Calculate transport preferences by riding WITH DEVIATION FROM NATIONAL AVERAGE
transport_by_riding <- data %>%
  filter(!is.na(id_riding)) %>%
  group_by(id_riding) %>%
  summarize(
    # Sum of weights
    sum_weight = sum(weight, na.rm = TRUE),
    
    # Weighted users for each transport type
    car_pct = sum((lifestyle_Transport == "car") * weight, na.rm = TRUE) / sum_weight * 100,
    suv_pct = sum((lifestyle_Transport == "suv") * weight, na.rm = TRUE) / sum_weight * 100,
    public_transit_pct = sum((lifestyle_Transport == "public_transit") * weight, na.rm = TRUE) / sum_weight * 100,
    walk_pct = sum((lifestyle_Transport == "walk") * weight, na.rm = TRUE) / sum_weight * 100,
    bicycle_pct = sum((lifestyle_Transport == "bicycle") * weight, na.rm = TRUE) / sum_weight * 100,
    motorcycle_pct = sum((lifestyle_Transport == "motorcycle") * weight, na.rm = TRUE) / sum_weight * 100,
    
    # Calculate deviations from national average
    car_deviation = car_pct - national_averages$car_avg,
    suv_deviation = suv_pct - national_averages$suv_avg,
    public_transit_deviation = public_transit_pct - national_averages$public_transit_avg,
    walk_deviation = walk_pct - national_averages$walk_avg,
    bicycle_deviation = bicycle_pct - national_averages$bicycle_avg,
    motorcycle_deviation = motorcycle_pct - national_averages$motorcycle_avg,
    
    # Non-weighted respondent count (for reference)
    n_people = n()
  ) %>%
  ungroup()

# 10. Determine which transport mode has the largest POSITIVE deviation in each riding
transport_by_riding <- transport_by_riding %>%
  mutate(
    # Create variables for the transport mode with the largest positive deviation
    max_deviation = pmax(
      car_deviation, 
      suv_deviation, 
      public_transit_deviation, 
      walk_deviation, 
      bicycle_deviation, 
      motorcycle_deviation
    ),
    
    # Identify which mode has the largest positive deviation
    highest_deviation_mode = case_when(
      max_deviation <= 0 ~ "Aucun mode au-dessus de la moyenne",
      car_deviation == max_deviation ~ "Voiture 🚗",
      suv_deviation == max_deviation ~ "VUS 🚙",
      public_transit_deviation == max_deviation ~ "Transport en commun 🚇",
      walk_deviation == max_deviation ~ "Marche 🚶",
      bicycle_deviation == max_deviation ~ "Vélo 🚲",
      motorcycle_deviation == max_deviation ~ "Moto 🏍️"
    ),
    
    # Get deviation percentage for the highest deviation mode
    highest_deviation_pct = max_deviation
  )

# 11. Join results to spatial data for visualization
sf_transport_map <- sf_ridings %>%
  mutate(id_riding = as.character(id_riding)) %>%
  left_join(transport_by_riding, by = "id_riding")

# 12. Define color scheme for transport modes
transport_colors <- c(
  "Voiture 🚗" = "#3498DB",        # Blue
  "VUS 🚙" = "#E74C3C",            # Red
  "Transport en commun 🚇" = "#2ECC71",  # Green
  "Marche 🚶" = "#F1C40F",         # Yellow
  "Vélo 🚲" = "#9B59B6",           # Purple
  "Moto 🏍️" = "#E67E22",          # Orange
  "Aucun mode au-dessus de la moyenne" = "#CCCCCC",  # Grey
  "Non disponible" = "#EEEEEE"     # Light grey
)

# 13. Clean data for mapping
sf_transport_map_clean <- sf_transport_map %>%
  mutate(
    highest_deviation_mode = ifelse(is.na(highest_deviation_mode), "Non disponible", highest_deviation_mode),
    # Create deviation category for intensity-based shading
    deviation_category = case_when(
      is.na(highest_deviation_pct) ~ "Non disponible",
      highest_deviation_pct <= 0 ~ "0% ou moins",
      highest_deviation_pct > 0 & highest_deviation_pct <= 5 ~ "0-5%",
      highest_deviation_pct > 5 & highest_deviation_pct <= 10 ~ "5-10%",
      highest_deviation_pct > 10 & highest_deviation_pct <= 15 ~ "10-15%",
      highest_deviation_pct > 15 ~ "15%+",
    )
  )

# 14. Parameters to avoid memory issues
options(future.globals.maxSize = 1000 * 1024^2)  # Increase limit to 1 GB
sf_use_s2(FALSE)  # Disable sf S2 features to reduce memory usage

# 15. Simplified theme for light mode maps
theme_map_light <- function() {
  theme_minimal() +
    theme(
      # White background
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      
      # Remove axes and grids
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      
      # Black text
      plot.title = element_text(face = "bold", size = 14, color = "black", hjust = 0.5),
      plot.subtitle = element_text(size = 11, color = "#555555", hjust = 0.5),
      plot.caption = element_text(size = 12, color = "#666666", hjust = 1),
      
      # Legend
      legend.position = "bottom",
      legend.background = element_rect(fill = "white", color = NA),
      legend.title = element_text(size = 10, color = "black"),
      legend.text = element_text(size = 9, color = "#555555")
    )
}

# 16. Create custom alpha values for intensity
get_alpha_values <- function(deviation_category) {
  alpha_values <- c(
    "0% ou moins" = 0.2,
    "0-5%" = 0.4,
    "5-10%" = 0.6,
    "10-15%" = 0.8,
    "15%+" = 1.0,
    "Non disponible" = 0.1
  )
  return(alpha_values[deviation_category])
}

# 17. Create a map showing which transport mode has the largest positive deviation
canada_transport_diff_map <- ggplot(sf_transport_map_clean) +
  geom_sf(aes(fill = highest_deviation_mode, alpha = deviation_category), 
          color = "#DDDDDD", size = 0.2) +
  scale_fill_manual(
    name = "Mode de transport avec l'écart positif\nle plus important par rapport à la moyenne",
    values = transport_colors,
    breaks = c("Voiture 🚗", "VUS 🚙", "Transport en commun 🚇", 
               "Marche 🚶", "Vélo 🚲", "Moto 🏍️", 
               "Aucun mode au-dessus de la moyenne")
  ) +
  scale_alpha_manual(
    name = "Importance de l'écart",
    values = c(
      "0% ou moins" = 0.2,
      "0-5%" = 0.4, 
      "5-10%" = 0.7,
      "10-15%" = 0.85,
      "15%+" = 1.0,
      "Non disponible" = 0.1
    ),
    breaks = c("0-5%", "5-10%", "10-15%", "15%+", "0% ou moins", "Non disponible"),
    labels = c("0-5%", "5-10%", "10-15%", "15% et plus", "Pas d'écart positif", "Non disponible")
  ) +
  theme_map_light() +
  labs(
    title = "ÉCART DES PRÉFÉRENCES DE TRANSPORT PAR RAPPORT À LA MOYENNE CANADIENNE",
    subtitle = "Mode de transport avec la plus grande différence positive par circonscription électorale",
    caption = paste0(
      "Moyennes nationales: ",
      "Voiture = ", round(national_averages$car_avg, 1), "%, ",
      "VUS = ", round(national_averages$suv_avg, 1), "%, ",
      "Transport en commun = ", round(national_averages$public_transit_avg, 1), "%, ",
      "Marche = ", round(national_averages$walk_avg, 1), "%, ",
      "Vélo = ", round(national_averages$bicycle_avg, 1), "%, ",
      "Moto = ", round(national_averages$motorcycle_avg, 1), "%"
    )
  ) +
  guides(
    fill = guide_legend(override.aes = list(alpha = 0.8), order = 1),
    alpha = guide_legend(override.aes = list(fill = "#666666"), order = 2)
  )

# 18. Save the Canada map
ggsave("canada_transport_diff_map.png", 
       canada_transport_diff_map, 
       width = 16, 
       height = 12, 
       dpi = 200,
       bg = "white")

# 19. Create urban area maps
main_regions <- c("montreal", "toronto", "vancouver", "quebec_city")

# 20. Create and save each urban map individually
for (region in main_regions) {
  # Extract region
  region_map <- cartessn::crop_map(sf_transport_map_clean, region)
  
  # Create map manually
  city_map <- ggplot(region_map) +
    geom_sf(aes(fill = highest_deviation_mode, alpha = deviation_category), 
            color = "#DDDDDD", size = 0.15) +
    scale_fill_manual(
      values = transport_colors,
      breaks = c("Voiture 🚗", "VUS 🚙", "Transport en commun 🚇", 
                 "Marche 🚶", "Vélo 🚲", "Moto 🏍️", 
                 "Aucun mode au-dessus de la moyenne")
    ) +
    scale_alpha_manual(
      values = c(
        "0% ou moins" = 0.2,
        "0-5%" = 0.4, 
        "5-10%" = 0.7,
        "10-15%" = 0.85,
        "15%+" = 1.0,
        "Non disponible" = 0.1
      )
    ) +
    theme_map_light() +
    theme(legend.position = "none")
  
  # Save each urban map separately with square aspect ratio
  ggsave(paste0(tolower(gsub("-", "_", region)), "_transport_diff_map.png"), 
         city_map, 
         width = 6, 
         height = 6, 
         dpi = 150,
         bg = "white")
}

# 21. Create alternative visualization focusing on specific modes
# This will create one map per transport mode showing deviation from average

# Define transport modes to create maps for
transport_modes <- c("car", "suv", "public_transit", "walk", "bicycle")
mode_names <- c(
  "car" = "Voiture 🚗",
  "suv" = "VUS 🚙",
  "public_transit" = "Transport en commun 🚇",
  "walk" = "Marche 🚶",
  "bicycle" = "Vélo 🚲"
)

# Function to create mode-specific map
create_mode_map <- function(mode, mode_name) {
  # Get deviation column name
  deviation_col <- paste0(mode, "_deviation")
  
  # Create custom breaks and color scheme for deviation
  deviation_breaks <- c(-Inf, -10, -5, -2, 2, 5, 10, Inf)
  deviation_labels <- c("< -10%", "-10% à -5%", "-5% à -2%", "-2% à +2%", "+2% à +5%", "+5% à +10%", "> +10%")
  deviation_colors <- c("#053061", "#2166AC", "#92C5DE", "#F7F7F7", "#FDAE61", "#F46D43", "#A50026")
  
  # Create map
  ggplot(sf_transport_map) +
    geom_sf(aes_string(fill = deviation_col), color = "#DDDDDD", size = 0.2) +
    scale_fill_gradientn(
      name = paste0("Écart par rapport à la moyenne (", mode_name, ")"),
      colors = deviation_colors,
      breaks = c(-10, -5, 0, 5, 10),
      labels = c("-10%", "-5%", "Moyenne", "+5%", "+10%"),
      limits = c(-15, 15),
      na.value = "#EEEEEE"
    ) +
    theme_map_light() +
    labs(
      title = paste0("ÉCART DE PRÉFÉRENCE: ", mode_name),
      subtitle = "Différence en points de % par rapport à la moyenne nationale",
      caption = paste0("Moyenne nationale pour ", mode_name, ": ", 
                       round(national_averages[[paste0(mode, "_avg")]], 1), "%")
    )
}

# Create and save mode-specific maps
for (mode in transport_modes) {
  mode_map <- create_mode_map(mode, mode_names[mode])
  ggsave(paste0("canada_", mode, "_deviation_map.png"), 
         mode_map, 
         width = 12, 
         height = 9, 
         dpi = 200,
         bg = "white")
}

# 22. Create a combined dashboard with main map and details for major cities
# This includes the Canada map with city detail boxes

# Definition of dimension parameters
canvas_width <- 1800      # Total canvas width
canada_height <- 1000     # Height for Canada map
city_height <- 400        # Height for city maps
city_spacing <- 20        # Spacing between city maps
section_spacing <- 40     # Spacing between sections

# 23. Create city map with better proportions
create_city_map <- function(region_name, display_title = NULL) {
  # Use custom title if provided, otherwise use region_name
  display_name <- ifelse(is.null(display_title), toupper(region_name), toupper(display_title))
  
  # Read existing image
  img_path <- paste0(tolower(gsub("-", "_", region_name)), "_transport_diff_map.png")
  img <- image_read(img_path)
  
  # Resize image preserving square ratio
  img_resized <- image_scale(img, paste0(toString(city_height), "x", toString(city_height)))
  
  # Create white canvas with fixed width for all cities
  city_width <- city_height  # Maintain square aspect
  canvas <- image_blank(width = city_width, 
                        height = city_height + 60,  # More space for title
                        color = "white")
  
  # Place image on canvas (centered)
  canvas_with_map <- image_composite(canvas, img_resized, 
                                     gravity = "center")
  
  # Add title at bottom
  canvas_with_title <- image_annotate(canvas_with_map, 
                                      display_name,
                                      color = "black", 
                                      size = 28,  # Increased font size
                                      font = "Arial-Bold",
                                      gravity = "south",
                                      location = "+0+20")  # More space at bottom
  
  return(canvas_with_title)
}

# 24. Read and resize Canada map with legend
canada_img <- image_read("canada_transport_diff_map.png")
canada_resized <- image_scale(canada_img, paste0(toString(canvas_width - 40), "x", toString(canada_height)))

# 25. Create canvas for Canada map
canada_canvas <- image_blank(width = canvas_width, 
                             height = canada_height + 60,  # More space to avoid cropping
                             color = "white")

# 26. Center Canada map
canada_centered <- image_composite(canada_canvas, canada_resized, 
                                   gravity = "center")

# 27. Create city maps with better proportions
montreal_map <- create_city_map("montreal")
toronto_map <- create_city_map("toronto")
vancouver_map <- create_city_map("vancouver")
quebec_map <- create_city_map("quebec_city", "QUÉBEC")

# 28. Calculate lateral spacing to center city maps
city_width = image_info(montreal_map)$width
city_total_width <- 4 * city_width + (3 * city_spacing)
city_padding <- max(0, (canvas_width - city_total_width) / 2)

# 29. Create more visible separators between cities
city_separator <- image_blank(width = city_spacing, 
                              height = image_info(montreal_map)$height, 
                              color = "white")

# 30. Assemble cities with spacing
city_row <- image_append(c(montreal_map, 
                           city_separator,
                           toronto_map, 
                           city_separator,
                           vancouver_map,
                           city_separator,
                           quebec_map), 
                         stack = FALSE)

# 31. Apply lateral padding
if (city_padding > 0) {
  left_padding <- image_blank(width = city_padding, height = image_info(city_row)$height, color = "white")
  right_padding <- image_blank(width = city_padding, height = image_info(city_row)$height, color = "white")
  city_row_padded <- image_append(c(left_padding, city_row, right_padding), stack = FALSE)
} else {
  city_row_padded <- city_row
}

# 32. Main title with increased dimensions
title_height <- 100  # Increased height
title_bg <- image_blank(width = canvas_width,
                        height = title_height,
                        color = "white")

title <- image_annotate(title_bg,
                        "ÉCART DES PRÉFÉRENCES DE TRANSPORT PAR RAPPORT À LA MOYENNE",
                        color = "black",
                        size = 48,  # Increased size
                        gravity = "center",
                        font = "Arial-Bold")

# 33. Subtitle with increased dimensions
subtitle_height <- 60  # Increased height
subtitle_bg <- image_blank(width = canvas_width,
                           height = subtitle_height,
                           color = "white")

subtitle <- image_annotate(subtitle_bg,
                           "Mode de transport avec la plus grande différence positive par circonscription électorale",
                           color = "#555555",
                           size = 32,  # Increased size
                           gravity = "center",
                           font = "Arial")

# 34. Methodological note with increased dimensions
caption_height <- 80  # Increased height
caption_bg <- image_blank(width = canvas_width,
                          height = caption_height,
                          color = "white")

# Use actual number of observations
n_observations <- nrow(data)  # Use actual number of respondents
caption <- image_annotate(caption_bg,
                          paste0("Source: Léger-Datagotchi 2025 | n=", format(n_observations, big.mark = " ")),
                          color = "#555555",
                          size = 24,  # Increased size
                          location = "+40+25",  # Adjusted position
                          font = "Arial-Bold")

caption <- image_annotate(caption,
                          "Données pondérées selon: le genre, l'âge, la province, la langue, le niveau d'éducation, le revenu, l'immigration, le type d'habitation",
                          color = "#555555",
                          size = 22,  # Increased size
                          location = "+40+55",  # Adjusted position
                          font = "Arial-Bold")

# 35. More visible separator line
separator_height <- 3  # Increased thickness
separator <- image_blank(width = canvas_width,
                         height = separator_height,
                         color = "#AAAAAA")  # Light grey color

# 36. Section spacing
spacer <- image_blank(width = canvas_width,
                      height = section_spacing,
                      color = "white")

# 37. Assemble final image with new order and better spacing
final_image <- c(
  title,                           # Main title
  subtitle,                        # Subtitle
  spacer,                          # Spacing
  separator,                       # Separator line
  spacer,                          # Spacing
  city_row_padded,                 # City maps
  spacer,                          # Spacing
  separator,                       # Separator line
  spacer,                          # Spacing
  canada_centered,                 # Canada map (without integrated legend)
  spacer,                          # Spacing
  caption                          # Methodological notes
)

final_combined <- image_append(final_image, stack = TRUE)

# 38. Add white border
final_with_border <- image_border(final_combined, "white", "30x30")  # Larger border

# 39. Load logo (if available)
logo_path <- "_SharedFolder_datagotchi_federal_2024/logos/FR/logo_black.png"
if (file.exists(logo_path)) {
  logo <- image_read(logo_path)
  
  # 40. Resize logo to appropriate size
  logo_width <- round(image_info(final_with_border)$width * 0.15)
  logo_resized <- image_scale(logo, paste0(logo_width, "x"))
  
  # 41. Calculate position for bottom right corner
  margin <- 30
  x_position <- image_info(final_with_border)$width - image_info(logo_resized)$width - margin
  y_position <- image_info(final_with_border)$height - image_info(logo_resized)$height - margin
  
  # 42. Add logo to final image
  final_with_logo <- image_composite(
    final_with_border, 
    logo_resized, 
    offset = paste0("+", x_position, "+", y_position)
  )
  
  # 43. Save final image with logo
  image_write(final_with_logo, "_SharedFolder_datagotchi_federal_2024/graph/analyses/transport/ecart_transport_canada_avec_logo.png")
  
  cat("Final image with logo created successfully: ecart_transport_canada_avec_logo.png\n")
} else {
  # If logo not available, save without logo
  image_write(final_with_border, "_SharedFolder_datagotchi_federal_2024/graph/analyses/transport/ecart_transport_canada.png")
  
  cat("Final image without logo created successfully: ecart_transport_canada.png\n")
}

