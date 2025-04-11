library(tidyr)
library(sf)
library(ggplot2)
library(cartessn)
library(patchwork)
library(cowplot)
library(showtext)
library(magick)
library(dplyr)

# Define transport icons
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

# Load data
df <- readRDS("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/datagotchi2025_canada_appPonderee_20250323.rds")

# Group and calculate percentages
df_grouped <- df %>%
  group_by(dv_voteChoice, lifestyle_Transport) %>%
  # Calculate the count and sum of weights per group
  summarise(n = n(), weight = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  # Calculate the percentage within each vote choice
  group_by(dv_voteChoice) %>%
  mutate(percentage = weight / sum(weight, na.rm = TRUE) * 100) %>%
  ungroup() %>%
  # Remove NA values if needed
  filter(!is.na(dv_voteChoice) & !is.na(lifestyle_Transport))

# Create a more readable order for the plot
party_levels <- c("lpc", "cpc", "bq", "ndp", "gpc", "ppc", "Other")
transport_levels <- c("car", "suv", "public_transit", "walk", "bicycle", "motorcycle")

# Adjust the factor levels if they exist in your data
if(all(party_levels %in% unique(df_grouped$dv_voteChoice))) {
  df_grouped$dv_voteChoice <- factor(df_grouped$dv_voteChoice, levels = party_levels)
}

if(all(transport_levels %in% unique(df_grouped$lifestyle_Transport))) {
  df_grouped$lifestyle_Transport <- factor(df_grouped$lifestyle_Transport, levels = transport_levels)
}

# Map party codes to readable names
df_grouped <- df_grouped %>%
  mutate(party_name = case_when(
    dv_voteChoice == "lpc" ~ "Parti libéral",
    dv_voteChoice == "cpc" ~ "Parti conservateur",
    dv_voteChoice == "ndp" ~ "NPD",
    dv_voteChoice == "bq" ~ "Bloc Québécois",
    dv_voteChoice == "gpc" ~ "Parti vert",
    dv_voteChoice == "ppc" ~ "PPC",
    TRUE ~ as.character(dv_voteChoice)
  ))

# Map transport codes to readable names with emojis
df_grouped <- df_grouped %>%
  mutate(transport_mode = case_when(
    lifestyle_Transport == "car" ~ "Voiture 🚗",
    lifestyle_Transport == "suv" ~ "VUS 🚙",
    lifestyle_Transport == "public_transit" ~ "Transport en commun 🚇",
    lifestyle_Transport == "walk" ~ "Marche 🚶",
    lifestyle_Transport == "bicycle" ~ "Vélo 🚲",
    lifestyle_Transport == "motorcycle" ~ "Moto 🏍️",
    TRUE ~ as.character(lifestyle_Transport)
  ))

# Order parties from right to left politically
party_order <- c("Parti conservateur", "PPC", "Parti libéral", "Bloc Québécois", "NPD", "Parti vert")
df_grouped$party_name <- factor(df_grouped$party_name, levels = party_order)

# Define transport colors to match
transport_colors <- c(
  "Voiture 🚗" = "#3498DB",       # Bleu
  "VUS 🚙" = "#E74C3C",           # Rouge
  "Transport en commun 🚇" = "#2ECC71", # Vert
  "Marche 🚶" = "#F1C40F",        # Jaune
  "Vélo 🚲" = "#9B59B6",          # Violet
  "Moto 🏍️" = "#E67E22",         # Orange
  "Other" = "#EEEEEE"             # Gris clair
)

# Create the dodged bar plot with improved aesthetics
transport_plot_dodge <- ggplot(df_grouped, aes(x = party_name, y = percentage, fill = transport_mode)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(
    name = "Mode de transport",
    values = transport_colors
  ) +
  labs(
    title = "MODES DE TRANSPORT PAR INTENTION DE VOTE",
    subtitle = "Pourcentage d'électeurs par mode de transport préféré",
    x = "",
    y = "Pourcentage (%)",
    caption = paste0("Source: Léger-Datagotchi 2025 | n=", format(nrow(df), big.mark = " "))
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial-Bold"),
    plot.title = element_text(face = "bold", size = 24, color = "black", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 16, color = "#555555", hjust = 0.5, margin = margin(b = 20)),
    legend.position = "none",
    axis.text.x = element_text(color = "black", size = 14, angle = 0, hjust = 0.5),
    axis.text.y = element_text(color = "#555555", size = 12),
    panel.grid.major.y = element_line(color = "#DDDDDD", size = 0.2),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(color = "#666666", size = 14, hjust = 0, margin = margin(t = 20, b = 10)),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Create directory if it doesn't exist
dir.create("_SharedFolder_datagotchi_federal_2024/graph/analyses/transport", recursive = TRUE, showWarnings = FALSE)

# Save the graph without legend
ggsave("_SharedFolder_datagotchi_federal_2024/graph/analyses/transport/transport_vote_sans_legende.png", 
       transport_plot_dodge, 
       width = 14, 
       height = 10,
       dpi = 200,
       bg = "white")

# Read the graph with magick
graph_img <- image_read("_SharedFolder_datagotchi_federal_2024/graph/analyses/transport/transport_vote_sans_legende.png")

# Create legend with icons
legend_height <- 200
legend_bg <- image_blank(width = image_info(graph_img)$width,
                         height = legend_height,
                         color = "white")

# Parameters for icon positioning
x_start <- 150
x_spacing <- 350
y_row1 <- 50
y_row2 <- 130

# First Row
legend_bg <- legend_bg %>%
  image_composite(transport_imgs$car_icon, offset = paste0("+", x_start, "+", y_row1)) %>%
  image_annotate("Voiture 🚗", color = "black", size = 32,
                 location = paste0("+", x_start + icon_size + 30, "+", y_row1 + 10),
                 font = "Arial-Bold") %>%
  image_composite(transport_imgs$suv_icon, offset = paste0("+", x_start + x_spacing, "+", y_row1)) %>%
  image_annotate("VUS 🚙", color = "black", size = 32,
                 location = paste0("+", x_start + x_spacing + icon_size + 30, "+", y_row1 + 10),
                 font = "Arial-Bold") %>%
  image_composite(transport_imgs$transit_icon, offset = paste0("+", x_start + 2*x_spacing, "+", y_row1)) %>%
  image_annotate("Transport en commun 🚇", color = "black", size = 32,
                 location = paste0("+", x_start + 2*x_spacing + icon_size + 30, "+", y_row1 + 10),
                 font = "Arial-Bold")

# Second Row
legend_bg <- legend_bg %>%
  image_composite(transport_imgs$walk_icon, offset = paste0("+", x_start, "+", y_row2)) %>%
  image_annotate("Marche 🚶", color = "black", size = 32,
                 location = paste0("+", x_start + icon_size + 30, "+", y_row2 + 10),
                 font = "Arial-Bold") %>%
  image_composite(transport_imgs$bicycle_icon, offset = paste0("+", x_start + x_spacing, "+", y_row2)) %>%
  image_annotate("Vélo 🚲", color = "black", size = 32,
                 location = paste0("+", x_start + x_spacing + icon_size + 30, "+", y_row2 + 10),
                 font = "Arial-Bold") %>%
  image_composite(transport_imgs$moto_icon, offset = paste0("+", x_start + 2*x_spacing, "+", y_row2)) %>%
  image_annotate("Moto 🏍️", color = "black", size = 32,
                 location = paste0("+", x_start + 2*x_spacing + icon_size + 30, "+", y_row2 + 10),
                 font = "Arial-Bold")

# Create caption with methodological notes
caption_height <- 80
caption_bg <- image_blank(width = image_info(graph_img)$width,
                          height = caption_height,
                          color = "white")

n_observations <- nrow(df)
caption <- image_annotate(caption_bg,
                          paste0("Source: Léger-Datagotchi 2025 | n=", format(n_observations, big.mark = " ")),
                          color = "#555555",
                          size = 24,
                          location = "+40+25",
                          font = "Arial-Bold")

caption <- image_annotate(caption,
                          "Données pondérées selon: le genre, l'âge, la province, la langue, le niveau d'éducation, le revenu, l'immigration, le type d'habitation",
                          color = "#555555",
                          size = 22,
                          location = "+40+55",
                          font = "Arial-Bold")

# Create separator
separator_height <- 3
separator <- image_blank(width = image_info(graph_img)$width,
                         height = separator_height,
                         color = "#AAAAAA")

# Create spacer
spacer_height <- 40
spacer <- image_blank(width = image_info(graph_img)$width,
                      height = spacer_height,
                      color = "white")

# Assemble final image
final_image <- c(
  graph_img,
  spacer,
  separator,
  spacer,
  legend_bg,
  spacer,
  separator,
  spacer,
  caption
)

final_combined <- image_append(final_image, stack = TRUE)

# Add border
final_with_border <- image_border(final_combined, "white", "30x30")

# Add logo if available
logo_path <- "_SharedFolder_datagotchi_federal_2024/logos/FR/logo_fr.png"
if (file.exists(logo_path)) {
  logo <- image_read(logo_path)
  
  # Resize logo to appropriate size
  logo_width <- round(image_info(final_with_border)$width * 0.15)
  logo_resized <- image_scale(logo, paste0(logo_width, "x"))
  
  # Position in bottom right corner
  margin <- 30
  x_position <- image_info(final_with_border)$width - image_info(logo_resized)$width - margin
  y_position <- image_info(final_with_border)$height - image_info(logo_resized)$height - margin
  
  # Add logo to final image
  final_with_logo <- image_composite(
    final_with_border, 
    logo_resized, 
    offset = paste0("+", x_position, "+", y_position)
  )
  
  # Save final image with logo
  image_write(final_with_logo, "_SharedFolder_datagotchi_federal_2024/graph/analyses/transport/transport_vote_final.png")
  
  cat("Image finale avec logo créée avec succès : transport_vote_final.png\n")
} else {
  # Save without logo if not available
  image_write(final_with_border, "_SharedFolder_datagotchi_federal_2024/graph/analyses/transport/transport_vote_final.png")
  
  cat("Image finale sans logo créée avec succès : transport_vote_final.png\n")
}
