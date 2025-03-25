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


# 1. Load data
data <- readRDS("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/datagotchi2025_canada_appPonderee_20250323.rds")

# 2. Load spatial data from cartessn
sf_ridings <- cartessn::spatial_canada_2022_electoral_ridings_aligned
sf_rta <- cartessn::spatial_canada_2021_rta

# 3. Extract FSA (first 3 characters of postal code)
data$rta <- substr(data$ses_postalCode, 1, 3)

# 4. Load existing mapping results
mapping_results <- readRDS("_SharedFolder_datagotchi_federal_2024/_previous/mapping_results_ridings_rta.rds")

# 5. Get the main mapping (FSA -> riding with best coverage)
rta_to_riding <- mapping_results$fsa_to_riding_mapping %>%
  select(rta, id_riding)

# 6. Join riding ID to our data
data <- data %>%
  left_join(rta_to_riding, by = "rta")

# 7. Check join rate
matched_count <- sum(!is.na(data$id_riding))
total_count <- nrow(data)
match_rate <- matched_count / total_count * 100

print(paste("Total number of respondents:", total_count))
print(paste("Number of respondents with an identified riding:", matched_count))
print(paste("Match rate:", round(match_rate, 2), "%"))

# 8. Ensure weight variable exists
if(!"weight" %in% names(data)) {
  data$weight <- 1
}

# 9. Analyze transportation types by riding WITH WEIGHTING
transport_battle_by_riding <- data %>%
  filter(!is.na(id_riding)) %>%
  group_by(id_riding) %>%
  summarize(
    # Sum of weights
    sum_weight = sum(weight, na.rm = TRUE),
    
    # Weighted users for each transport type
    car_users = sum((lifestyle_Transport == "car") * weight, na.rm = TRUE),
    suv_users = sum((lifestyle_Transport == "suv") * weight, na.rm = TRUE),
    public_transit_users = sum((lifestyle_Transport == "public_transit") * weight, na.rm = TRUE),
    walk_users = sum((lifestyle_Transport == "walk") * weight, na.rm = TRUE),
    bicycle_users = sum((lifestyle_Transport == "bicycle") * weight, na.rm = TRUE),
    motorcycle_users = sum((lifestyle_Transport == "motorcycle") * weight, na.rm = TRUE),
    
    # Calculate weighted percentages
    car_pct = car_users / sum_weight * 100,
    suv_pct = suv_users / sum_weight * 100,
    public_transit_pct = public_transit_users / sum_weight * 100,
    walk_pct = walk_users / sum_weight * 100,
    bicycle_pct = bicycle_users / sum_weight * 100,
    motorcycle_pct = motorcycle_users / sum_weight * 100,
    
    # Number of unweighted respondents (for reference)
    n_people = n()
  ) %>%
  ungroup()

# 10. Determine dominant transport type in each riding
transport_battle_by_riding <- transport_battle_by_riding %>%
  mutate(
    # Create variable for dominant transport mode (the one with most users)
    dominant_mode = case_when(
      car_pct >= suv_pct & car_pct >= public_transit_pct & car_pct >= walk_pct & car_pct >= bicycle_pct & car_pct >= motorcycle_pct ~ "Car 🚗",
      suv_pct >= car_pct & suv_pct >= public_transit_pct & suv_pct >= walk_pct & suv_pct >= bicycle_pct & suv_pct >= motorcycle_pct ~ "SUV 🚙",
      public_transit_pct >= car_pct & public_transit_pct >= suv_pct & public_transit_pct >= walk_pct & public_transit_pct >= bicycle_pct & public_transit_pct >= motorcycle_pct ~ "Public Transit 🚇",
      walk_pct >= car_pct & walk_pct >= suv_pct & walk_pct >= public_transit_pct & walk_pct >= bicycle_pct & walk_pct >= motorcycle_pct ~ "Walk 🚶",
      bicycle_pct >= car_pct & bicycle_pct >= suv_pct & bicycle_pct >= public_transit_pct & bicycle_pct >= walk_pct & bicycle_pct >= motorcycle_pct ~ "Bicycle 🚲",
      motorcycle_pct >= car_pct & motorcycle_pct >= suv_pct & motorcycle_pct >= public_transit_pct & motorcycle_pct >= walk_pct & motorcycle_pct >= bicycle_pct ~ "Motorcycle 🏍️",
      TRUE ~ "Tie"
    ),
    
    # Percentage for dominant transport type
    dominant_pct = case_when(
      dominant_mode == "Car 🚗" ~ car_pct,
      dominant_mode == "SUV 🚙" ~ suv_pct,
      dominant_mode == "Public Transit 🚇" ~ public_transit_pct,
      dominant_mode == "Walk 🚶" ~ walk_pct,
      dominant_mode == "Bicycle 🚲" ~ bicycle_pct,
      dominant_mode == "Motorcycle 🏍️" ~ motorcycle_pct,
      TRUE ~ NA_real_
    )
  )

# 11. Join results to spatial data for visualization
sf_transport_map <- sf_ridings %>%
  left_join(transport_battle_by_riding, by = "id_riding")

# 12. Save intermediate results
saveRDS(transport_battle_by_riding, "_SharedFolder_datagotchi_federal_2024/reports/transport_battle_pondere_light_en.rds")

# 13. Parameters to avoid memory issues
options(future.globals.maxSize = 1000 * 1024^2)  # Increase limit to 1GB
sf_use_s2(FALSE)  # Disable sf S2 features to reduce memory usage

# 14. Simplified theme for light mode maps
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

# 15. Define colors for transport types
transport_colors <- c(
  "Car 🚗" = "#3498DB",        # Blue
  "SUV 🚙" = "#E74C3C",            # Red
  "Public Transit 🚇" = "#2ECC71",  # Green
  "Walk 🚶" = "#F1C40F",         # Yellow
  "Bicycle 🚲" = "#9B59B6",           # Purple
  "Motorcycle 🏍️" = "#E67E22",          # Orange
  "Not available" = "#EEEEEE"   # Light grey
)

# 16. Preprocess data
sf_transport_map_clean <- sf_transport_map %>%
  mutate(dominant_mode = ifelse(is.na(dominant_mode), "Not available", dominant_mode))

# 17. Information on number of observations
n_observations <- nrow(data)  # Use actual number of respondents

# 18. ===== CANADA MAP =====
canada_transport_map <- ggplot(sf_transport_map_clean) +
  geom_sf(aes(fill = dominant_mode), color = "#DDDDDD", size = 0.2) +
  scale_fill_manual(
    name = "Transport Mode",
    values = transport_colors,
    breaks = c("Car 🚗", "SUV 🚙", "Public Transit 🚇", "Walk 🚶", "Bicycle 🚲", "Motorcycle 🏍️")
  ) +
  theme_map_light() +
  theme(legend.position = "none")  # Change this to "none" instead of "bottom"

ggsave("canada_transport_map_light_en.png", 
       canada_transport_map, 
       width = 16, 
       height = 12, 
       dpi = 200,
       bg = "white")

# 19. ===== URBAN MAPS =====
main_regions <- c("montreal", "toronto", "vancouver", "quebec_city")

# 20. Create and save each urban map individually
for (region in main_regions) {
  # Extract region
  region_map <- cartessn::crop_map(sf_transport_map_clean, region)
  
  # Create map manually
  city_map <- ggplot(region_map) +
    geom_sf(aes(fill = dominant_mode), color = "#DDDDDD", size = 0.15) +
    scale_fill_manual(
      values = transport_colors,
      breaks = c("Car 🚗", "SUV 🚙", "Public Transit 🚇", "Walk 🚶", "Bicycle 🚲", "Motorcycle 🏍️")
    ) +
    theme_map_light() +
    theme(legend.position = "none")
  
  # Save each urban map separately with square aspect ratio
  ggsave(paste0(tolower(gsub("-", "_", region)), "_transport_map_light_en.png"), 
         city_map, 
         width = 6, 
         height = 6, 
         dpi = 150,
         bg = "white")
}

# This section handles layout and assembly of maps and legend

# Definition of dimension parameters
canvas_width <- 1800      # Total canvas width
canada_height <- 1000     # Height for Canada map
city_height <- 400        # Height for city maps
city_spacing <- 20        # Spacing between city maps
section_spacing <- 40     # Spacing between sections increased

# 22. Function to create a city map with better proportions
create_city_map <- function(region_name, display_title = NULL) {
  # Use custom title if provided, otherwise use region_name
  display_name <- ifelse(is.null(display_title), toupper(region_name), toupper(display_title))
  
  # Read existing image
  img_path <- paste0(tolower(gsub("-", "_", region_name)), "_transport_map_light_en.png")
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

# 23. Read and resize Canada map with legend
canada_img <- image_read("canada_transport_map_light_en.png")
canada_resized <- image_scale(canada_img, paste0(toString(canvas_width - 40), "x", toString(canada_height)))

# 24. Create canvas for Canada map
canada_canvas <- image_blank(width = canvas_width, 
                             height = canada_height + 60,  # More space to avoid cropping
                             color = "white")

# 25. Center Canada map
canada_centered <- image_composite(canada_canvas, canada_resized, 
                                   gravity = "center")

# 26. Create city maps with better proportions
montreal_map <- create_city_map("montreal")
toronto_map <- create_city_map("toronto")
vancouver_map <- create_city_map("vancouver")
quebec_map <- create_city_map("quebec_city", "QUÉBEC")

# 27. Calculate lateral spacing to center city maps
city_width = image_info(montreal_map)$width
city_total_width <- 4 * city_width + (3 * city_spacing)
city_padding <- max(0, (canvas_width - city_total_width) / 2)

# 28. Create more visible separators between cities
city_separator <- image_blank(width = city_spacing, 
                              height = image_info(montreal_map)$height, 
                              color = "white")

# 29. Assemble cities with spacing
city_row <- image_append(c(montreal_map, 
                           city_separator,
                           toronto_map, 
                           city_separator,
                           vancouver_map,
                           city_separator,
                           quebec_map), 
                         stack = FALSE)

# 30. Apply lateral padding
if (city_padding > 0) {
  left_padding <- image_blank(width = city_padding, height = image_info(city_row)$height, color = "white")
  right_padding <- image_blank(width = city_padding, height = image_info(city_row)$height, color = "white")
  city_row_padded <- image_append(c(left_padding, city_row, right_padding), stack = FALSE)
} else {
  city_row_padded <- city_row
}

# 31. Main title with increased dimensions
title_height <- 100  # Increased height
title_bg <- image_blank(width = canvas_width,
                        height = title_height,
                        color = "white")

title <- image_annotate(title_bg,
                        "TRANSPORTATION BATTLE IN CANADA",
                        color = "black",
                        size = 48,  # Increased size
                        gravity = "center",
                        font = "Arial-Bold")

# 32. Subtitle with increased dimensions
subtitle_height <- 60  # Increased height
subtitle_bg <- image_blank(width = canvas_width,
                           height = subtitle_height,
                           color = "white")

subtitle <- image_annotate(subtitle_bg,
                           "Preferred transportation mode by electoral district",
                           color = "#555555",
                           size = 32,  # Increased size
                           gravity = "center",
                           font = "Arial")


# Correction for transport legend

# 33. Improved legend with increased height to avoid overlaps
# Create a taller legend background to accommodate all transport modes
map_legend_height <- 300  # Renamed to avoid conflicts with later code
map_legend_bg <- image_blank(width = canvas_width,
                             height = map_legend_height,
                             color = "white")

# Parameters for icon positioning
x_start <- 150
x_spacing <- 350
y_row1 <- 50
y_row2 <- 180

# First Row
map_legend_bg <- map_legend_bg %>%
  image_composite(transport_imgs$car_icon, offset = paste0("+", x_start, "+", y_row1)) %>%
  image_annotate("Car 🚗", color = "black", size = 32,
                 location = paste0("+", x_start + icon_size + 30, "+", y_row1 + 10),
                 font = "Arial-Bold") %>%
  image_composite(transport_imgs$suv_icon, offset = paste0("+", x_start + x_spacing, "+", y_row1)) %>%
  image_annotate("SUV 🚙", color = "black", size = 32,
                 location = paste0("+", x_start + x_spacing + icon_size + 30, "+", y_row1 + 10),
                 font = "Arial-Bold") %>%
  image_composite(transport_imgs$transit_icon, offset = paste0("+", x_start + 2*x_spacing, "+", y_row1)) %>%
  image_annotate("Public Transit 🚇", color = "black", size = 32,
                 location = paste0("+", x_start + 2*x_spacing + icon_size + 30, "+", y_row1 + 10),
                 font = "Arial-Bold")

# Second Row
map_legend_bg <- map_legend_bg %>%
  image_composite(transport_imgs$walk_icon, offset = paste0("+", x_start, "+", y_row2)) %>%
  image_annotate("Walk 🚶", color = "black", size = 32,
                 location = paste0("+", x_start + icon_size + 30, "+", y_row2 + 10),
                 font = "Arial-Bold") %>%
  image_composite(transport_imgs$bicycle_icon, offset = paste0("+", x_start + x_spacing, "+", y_row2)) %>%
  image_annotate("Bicycle 🚲", color = "black", size = 32,
                 location = paste0("+", x_start + x_spacing + icon_size + 30, "+", y_row2 + 10),
                 font = "Arial-Bold") %>%
  image_composite(transport_imgs$moto_icon, offset = paste0("+", x_start + 2*x_spacing, "+", y_row2)) %>%
  image_annotate("Motorcycle 🏍️", color = "black", size = 32,
                 location = paste0("+", x_start + 2*x_spacing + icon_size + 30, "+", y_row2 + 10),
                 font = "Arial-Bold")


# 36. Methodological note with increased dimensions
caption_height <- 80  # Increased height
caption_bg <- image_blank(width = canvas_width,
                          height = caption_height,
                          color = "white")

# Use actual number of observations
n_observations <- nrow(data)  # Use actual number of respondents
caption <- image_annotate(caption_bg,
                          paste0("Source: Léger-Datagotchi 2025 | n=", format(n_observations, big.mark = ",")),
                          color = "#555555",
                          size = 24,  # Increased size
                          location = "+40+25",  # Adjusted position
                          font = "Arial-Bold")

caption <- image_annotate(caption,
                          "Data weighted by: gender, age, province, language, education level, income, immigration status, housing type",
                          color = "#555555",
                          size = 22,  # Increased size
                          location = "+40+55",  # Adjusted position
                          font = "Arial-Bold")

# 37. More visible separator line
separator_height <- 3  # Increased thickness
separator <- image_blank(width = canvas_width,
                         height = separator_height,
                         color = "#AAAAAA")  # Light grey color

# 38. Spacing between sections
spacer <- image_blank(width = canvas_width,
                      height = section_spacing,
                      color = "white")

# 39. Assemble final image with new order and better spacing
final_image <- c(
  title,                           # Main title
  subtitle,                        # Subtitle
  spacer,                          # Spacing
  separator,                       # Separator line
  spacer,                          # Spacing
  map_legend_bg,                   # Custom legend at TOP (with icons)
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

# 40. Add white border
final_with_border <- image_border(final_combined, "white", "30x30")  # Larger border

# 41. Load logo (if available)
logo_path <- "_SharedFolder_datagotchi_federal_2024/logos/FR/logo_black.png"
if (file.exists(logo_path)) {
  logo <- image_read(logo_path)
  
  # 42. Resize logo to appropriate size
  logo_width <- round(image_info(final_with_border)$width * 0.15)
  logo_resized <- image_scale(logo, paste0(logo_width, "x"))
  
  # 43. Calculate position for bottom right corner
  margin <- 30
  x_position <- image_info(final_with_border)$width - image_info(logo_resized)$width - margin
  y_position <- image_info(final_with_border)$height - image_info(logo_resized)$height - margin
  
  # 44. Add logo to final image
  final_with_logo <- image_composite(
    final_with_border, 
    logo_resized, 
    offset = paste0("+", x_position, "+", y_position)
  )
  
  # 45. Save final image with logo
  image_write(final_with_logo, "_SharedFolder_datagotchi_federal_2024/graph/analyses/transport/battle_transport_canada_light_with_logo_en.png")
  
  cat("Final image with logo created successfully: battle_transport_canada_light_with_logo_en.png\n")
} else {
  # If logo not available, save without logo
  image_write(final_with_border, "_SharedFolder_datagotchi_federal_2024/graph/analyses/transport/battle_transport_canada_light_en.png")
  
  cat("Final image without logo created successfully: battle_transport_canada_light_en.png\n")
}



## Transport-Politics Index in light mode

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
      dv_voteChoice == "lpc" ~ "Liberal Party",
      dv_voteChoice == "cpc" ~ "Conservative Party",
      dv_voteChoice == "ndp" ~ "NDP",
      dv_voteChoice == "bq" ~ "Bloc Québécois",
      dv_voteChoice == "gpc" ~ "Green Party",
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
      transport_mode == "car_deviation" ~ "Car 🚗",
      transport_mode == "suv_deviation" ~ "SUV 🚙",
      transport_mode == "public_transit_deviation" ~ "Public Transit 🚇",
      transport_mode == "walk_deviation" ~ "Walk 🚶",
      transport_mode == "bicycle_deviation" ~ "Bicycle 🚲",
      transport_mode == "motorcycle_deviation" ~ "Motorcycle 🏍️"
    )
  )

# Order parties from right to left politically
party_order <- c("Conservative Party", "Liberal Party", "Bloc Québécois", "NDP", "Green Party")
transport_by_party_long$party_name <- factor(transport_by_party_long$party_name, levels = party_order)

# Modify the transport_plot ggplot code for light theme
transport_plot <- ggplot(transport_by_party_long, aes(x = party_name, y = deviation, fill = transport_mode)) +

  # Replace geom_hline with geom_segment
  geom_segment(
    x = 0.5,                      # Starting x position
    xend = length(party_order) + 0.5, # End at the last party
    y = 0,                        # y position (zero line)
    yend = 0,                     # keep y position the same for horizontal line
    color = "#999999", 
    linetype = "solid", 
    size = 2
  ) +
  
  # Add +/- symbols and labels aligned with discrete axis
  annotate("text", x = 0.3, y = 0, 
           label = "Canadian average", color = "black", size = 7, fontface = "bold", angle = 90) +
  annotate("text", x = 0.3, y = 10, 
           label = "+", color = "black", size = 10, fontface = "bold") +
  annotate("text", x = 0.3, y = -10, 
           label = "-", color = "black", size = 10, fontface = "bold") +
  
  # Keep the bar plot
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
   
  # Add coord_cartesian to prevent clipping
  coord_cartesian(clip = "off") +
  
  # Ensure X is discrete
  scale_x_discrete() +
  
  # Keep existing scale and labels
  scale_fill_manual(
    name = "Transport Mode",
    values = transport_colors
  ) +
  labs(
    title = "TRANSPORT-POLITICAL INDEX",
    subtitle = "Deviation from national average transportation preference (% points)",
    caption = paste0("National averages: Car = ", car_national, 
                     "%, SUV = ", suv_national, 
                     "%, Public Transit = ", transit_national,
                     "%, Walking = ", walk_national,
                     "%, Bicycle = ", bicycle_national,
                     "%, Motorcycle = ", motorcycle_national, "%"),
    x = "",
    y = ""
  ) +
  # Modified theme settings with reduced text sizes
  theme_minimal() +
  theme(
    text = element_text(family = "Arial-Bold"),
    plot.title = element_text(face = "bold", size = 24, color = "black", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 16, color = "#555555", hjust = 0.5, margin = margin(b = 20)),
    legend.position = "none",
    axis.text.x = element_text(color = "black", size = 14, angle = 0, hjust = 0.5),
    axis.text.y = element_blank(),  # Remove y-axis labels
    panel.grid.major.y = element_line(color = "#DDDDDD", size = 0.2),
    plot.caption = element_text(color = "#666666", size = 14, hjust = 0.5, margin = margin(t = 20, b = 10)),
    plot.margin = margin(t = 20, r = 20, b = 30, l = 30),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Create directory if it doesn't exist
dir.create("_SharedFolder_datagotchi_federal_2024/graph/analyses/transport", recursive = TRUE, showWarnings = FALSE)

# Save the graph without legend, with increased height
ggsave("_SharedFolder_datagotchi_federal_2024/graph/analyses/transport/index_transport_light_no_legend_en.png", 
       transport_plot, 
       width = 14, 
       height = 10,
       dpi = 200,
       bg = "white")

# Read the graph with magick
graph_img <- image_read("_SharedFolder_datagotchi_federal_2024/graph/analyses/transport/index_transport_light_no_legend_en.png")

# Modified legend creation with proper labels and spacing
legend_height <- 100  # Legend height
legend_bg <- image_blank(width = image_info(graph_img)$width,
                         height = legend_height,
                         color = "white")

# Parameters for icon and text positioning
x_start <- 150   # Starting X position
x_spacing <- 350 # Horizontal space between items
y_row1 <- 50     # First row Y position
y_row2 <- 180    # Second row Y position

# First Row: Car, SUV, Public Transit
# Car 🚗
legend_bg <- image_composite(legend_bg, transport_imgs$car_icon, 
                             offset = paste0("+", x_start, "+", y_row1/2))
legend_bg <- image_annotate(legend_bg, "Car 🚗",
                            color = "black", size = 32,
                            location = paste0("+", x_start + icon_size + 30, "+", y_row1/2 + 10),
                            font = "Arial-Bold")

# SUV 🚙
legend_bg <- image_composite(legend_bg, transport_imgs$suv_icon, 
                             offset = paste0("+", x_start + x_spacing, "+", y_row1/2))
legend_bg <- image_annotate(legend_bg, "SUV 🚙",
                            color = "black", size = 32,
                            location = paste0("+", x_start + x_spacing + icon_size + 30, "+", y_row1/2 + 10),
                            font = "Arial-Bold")

# Public Transit 🚇
legend_bg <- image_composite(legend_bg, transport_imgs$transit_icon, 
                             offset = paste0("+", x_start + 2*x_spacing, "+", y_row1/2))
legend_bg <- image_annotate(legend_bg, "Public Transit 🚇",
                            color = "black", size = 32,
                            location = paste0("+", x_start + 2*x_spacing + icon_size + 30, "+", y_row1/2 + 10),
                            font = "Arial-Bold")

# Create caption with source info - improved positioning
caption_height <- 150  # Caption height
caption_bg <- image_blank(width = image_info(graph_img)$width,
                          height = caption_height,
                          color = "white")

# Add source information with improved positioning and text size
caption <- image_annotate(caption_bg,
                          paste0("Source: Léger-Datagotchi 2025 | n=", format(n_observations, big.mark = ",")),
                          color = "#555555",
                          size = 24,  # Adjusted size
                          location = "+40+30",
                          font = "Arial-Bold")

caption <- image_annotate(caption,
                          "Data weighted by: gender, age, province, language, education level, income, immigration status, housing type",
                          color = "#555555",
                          size = 22,  # Adjusted size
                          location = "+40+70",  # Adjusted for smaller text
                          font = "Arial-Bold")

# Logo positioning
logo_path <- "_SharedFolder_datagotchi_federal_2024/logos/FR/logo_black.png"
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

# Create a second row for the remaining transport modes
legend_row2_height <- 100
legend_row2 <- image_blank(width = image_info(graph_img)$width,
                           height = legend_row2_height,
                           color = "white")

# Second Row: Walk, Bicycle, Motorcycle
legend_row2 <- legend_row2 %>%
  image_composite(transport_imgs$walk_icon, offset = paste0("+", x_start, "+", y_row1/2)) %>%
  image_annotate("Walk 🚶", color = "black", size = 32,
                 location = paste0("+", x_start + icon_size + 30, "+", y_row1/2 + 10),
                 font = "Arial-Bold") %>%
  image_composite(transport_imgs$bicycle_icon, offset = paste0("+", x_start + x_spacing, "+", y_row1/2)) %>%
  image_annotate("Bicycle 🚲", color = "black", size = 32,
                 location = paste0("+", x_start + x_spacing + icon_size + 30, "+", y_row1/2 + 10),
                 font = "Arial-Bold") %>%
  image_composite(transport_imgs$moto_icon, offset = paste0("+", x_start + 2*x_spacing, "+", y_row1/2)) %>%
  image_annotate("Motorcycle 🏍️", color = "black", size = 32,
                 location = paste0("+", x_start + 2*x_spacing + icon_size + 30, "+", y_row1/2 + 10),
                 font = "Arial-Bold")

# Assemble final image
final_image <- image_append(c(graph_img, legend_bg, legend_row2, caption_with_logo), stack = TRUE)

# Add border
final_with_border <- image_border(final_image, "white", "40x40")

# Save final image
image_write(final_with_border, "_SharedFolder_datagotchi_federal_2024/graph/analyses/transport/index_transport_light_final_en.png")

cat("Transport-Politics index light graph created successfully!\n")
