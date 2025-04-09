library(tidyr)
library(sf)
library(ggplot2)
library(cartessn)
library(patchwork)
library(cowplot)
library(showtext)
library(magick)
library(dplyr)

# Load data
df <- readRDS("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/datagotchi2025_canada_appPonderee_20250323.rds") %>%
  filter(dv_voteChoice != "other")

# Group and calculate percentages - INVERTED RELATIONSHIP
# Each party's transport modes will sum to 100%
df_grouped <- df %>%
  group_by(dv_voteChoice, lifestyle_Transport) %>%
  # Calculate the count and sum of weights per group
  summarise(n = n(), weight = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  # Calculate the percentage within each party (so transport modes sum to 100% for each party)
  group_by(dv_voteChoice) %>%
  mutate(percentage = weight / sum(weight, na.rm = TRUE) * 100) %>%
  ungroup() %>%
  # Remove NA values if needed
  filter(!is.na(dv_voteChoice) & !is.na(lifestyle_Transport))

# Define Canadian party colors based on direct party codes
party_colors <- c(
  "lpc" = "#D71920",                # Red
  "cpc" = "#1A4782",                # Blue
  "ndp" = "#F58220",                # Orange
  "bq" = "#33B2CC",                 # Light Blue
  "gpc" = "#3D9B35",                # Green
  "ppc" = "#49176D",                # Purple
  "Other" = "#AAAAAA"               # Grey
)

# Map party codes to abbreviated names for labels
party_labels <- c(
  "lpc" = "PLC",
  "cpc" = "PCC",
  "ndp" = "NPD", 
  "bq" = "BQ",
  "gpc" = "PVC",
  "ppc" = "PPC",
  "other" = "Other"
)

# Create text labels for X-axis (empty placeholder)
df_grouped <- df_grouped %>%
  mutate(transport_label = case_when(
    lifestyle_Transport == "car" ~ "Voiture",
    lifestyle_Transport == "suv" ~ "VUS",
    lifestyle_Transport == "public_transit" ~ "Transport en commun",
    lifestyle_Transport == "walk" ~ "Marche",
    lifestyle_Transport == "bicycle" ~ "Vélo",
    lifestyle_Transport == "motorcycle" ~ "Moto",
    TRUE ~ as.character(lifestyle_Transport)
  ))

# Define order for transport modes
transport_order <- c("Voiture", "VUS", "Transport en commun", "Marche", "Vélo", "Moto")
df_grouped$transport_label <- factor(df_grouped$transport_label, levels = transport_order)

# Create emoji + text mapping for labels above bars
transport_labels_with_emoji <- c(
  "Voiture" = "🚗\nVoiture",
  "VUS" = "🚙\nVUS", 
  "Transport en commun" = "🚇\nTransport\nen commun",
  "Marche" = "🚶\nMarche",
  "Vélo" = "🚲\nVélo",
  "Moto" = "🏍️\nMoto"
)

# Create the modified plot with party abbreviations below bars and transport emojis+text above
transport_plot_modified <- ggplot(df_grouped, aes(x = transport_label, y = percentage, fill = dv_voteChoice)) +
  # Use position_dodge with wider spacing
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  scale_fill_manual(values = party_colors) +
  # Add party labels below bars
  geom_text(
    aes(label = party_labels[dv_voteChoice], y = -3, group = dv_voteChoice),
    position = position_dodge(width = 0.9),
    vjust = 1,
    size = 4
  ) +
  # Add transport emojis and text above bars (without boxes)
  geom_text(
    aes(label = transport_labels_with_emoji[as.character(transport_label)], y = max(percentage) * 1.05),
    size = 4,
    position = "identity",
    vjust = 0
  ) +
  labs(
    title = "MODES DE TRANSPORT UTILISÉS PAR PARTI POLITIQUE",
    subtitle = "Pourcentage d'électeurs de chaque parti utilisant différents modes de transport",
    x = "",
    y = "Pourcentage (%)",
    caption = paste0("Source: Léger-Datagotchi 2025 | n=", format(nrow(df), big.mark = " "), 
                   "\nDonnées pondérées selon: le genre, l'âge, la province, la langue, le niveau d'éducation, le revenu, l'immigration, le type d'habitation")
  ) +
  # Expand y-axis limits to make room for party labels below zero and a small amount above
  scale_y_continuous(expand = expansion(mult = c(0.15, 0.1))) +
  theme_minimal() +
  theme(
    text = element_text(family = "sans"),
    plot.title = element_text(face = "bold", size = 24, color = "black", hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 16, color = "#555555", hjust = 0.5, margin = margin(b = 10)),
    # Remove the legend
    legend.position = "none",
    # Other styling elements
    axis.text.x = element_blank(), # Remove x-axis text since we have labels above
    axis.text.y = element_text(color = "#555555", size = 12),
    panel.grid.major.y = element_line(color = "#DDDDDD", size = 0.2),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(color = "#666666", size = 12, hjust = 0, margin = margin(t = 15, b = 10)),
    plot.margin = margin(t = 15, r = 20, b = 15, l = 20),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Create directory if it doesn't exist
dir.create("_SharedFolder_datagotchi_federal_2024/graph/analyses/transport", recursive = TRUE, showWarnings = FALSE)

# Save the modified graph
# Read the logo
logo_path <- "_SharedFolder_datagotchi_federal_2024/logos/FR/logo_black.png"
if (file.exists(logo_path)) {
  logo <- image_read(logo_path)
  
  # Save the plot to a temporary file
  temp_file <- tempfile(fileext = ".png")
  ggsave(temp_file, transport_plot_modified, width = 14, height = 12, dpi = 200, bg = "white")
  
  # Read the plot with magick
  plot_img <- image_read(temp_file)
  
  # Resize logo to appropriate size (15% of plot width)
  logo_width <- round(image_info(plot_img)$width * 0.15)
  logo_resized <- image_scale(logo, paste0(logo_width, "x"))
  
  # Calculate position for bottom right corner
  margin <- 40
  x_position <- image_info(plot_img)$width - image_info(logo_resized)$width - margin
  y_position <- image_info(plot_img)$height - image_info(logo_resized)$height - margin
  
  # Add logo to the plot
  final_img <- image_composite(
    plot_img,
    logo_resized,
    offset = paste0("+", x_position, "+", y_position)
  )
  
  # Save the final image
  image_write(final_img, "_SharedFolder_datagotchi_federal_2024/graph/analyses/transport/transport_vote_modified.png")
  
  # Remove temporary file
  file.remove(temp_file)
  
  cat("Graphique créé avec succès avec logo : transport_vote_modified.png\n")
} else {
  # If logo not found, save without it
  ggsave("_SharedFolder_datagotchi_federal_2024/graph/analyses/transport/transport_vote_modified.png", 
         transport_plot_modified, 
         width = 14, 
         height = 12,
         dpi = 200,
         bg = "white")
  
  cat("Graphique créé avec succès sans logo : transport_vote_modified.png\n")
  cat("Logo non trouvé à : ", logo_path, "\n")
}
