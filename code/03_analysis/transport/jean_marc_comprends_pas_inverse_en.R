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

# Define Canadian party colors
party_colors <- c(
  "Liberal Party" = "#D71920",       # Red
  "Conservative Party" = "#1A4782",  # Blue
  "New Democratic Party" = "#F58220", # Orange
  "Bloc Québécois" = "#33B2CC",      # Light Blue
  "Green Party" = "#3D9B35",         # Green
  "PPC" = "#49176D",                 # Purple
  "Other" = "#AAAAAA"                # Grey
)

# Map party codes to readable names in English
df_grouped <- df_grouped %>%
  mutate(party_name = case_when(
    dv_voteChoice == "lpc" ~ "Liberal Party",
    dv_voteChoice == "cpc" ~ "Conservative Party",
    dv_voteChoice == "ndp" ~ "New Democratic Party",
    dv_voteChoice == "bq" ~ "Bloc Québécois",
    dv_voteChoice == "gpc" ~ "Green Party",
    dv_voteChoice == "ppc" ~ "PPC",
    TRUE ~ "Other"
  ))

# Map transport codes to readable names with emojis in English
df_grouped <- df_grouped %>%
  mutate(transport_mode = case_when(
    lifestyle_Transport == "car" ~ "Car 🚗",
    lifestyle_Transport == "suv" ~ "SUV 🚙",
    lifestyle_Transport == "public_transit" ~ "Public Transit 🚇",
    lifestyle_Transport == "walk" ~ "Walking 🚶",
    lifestyle_Transport == "bicycle" ~ "Bicycle 🚲",
    lifestyle_Transport == "motorcycle" ~ "Motorcycle 🏍️",
    TRUE ~ as.character(lifestyle_Transport)
  ))

# Order parties for the fill colors
party_order <- c("Liberal Party", "Conservative Party", "New Democratic Party", "Bloc Québécois", "Green Party", "PPC", "Other")
df_grouped$party_name <- factor(df_grouped$party_name, levels = party_order)

# Order transport modes for X-axis
transport_order <- c("Car 🚗", "SUV 🚙", "Public Transit 🚇", "Walking 🚶", "Bicycle 🚲", "Motorcycle 🏍️")
df_grouped$transport_mode <- factor(df_grouped$transport_mode, levels = transport_order)

# Create the inverted plot with legend
transport_plot_dodge <- ggplot(df_grouped, aes(x = transport_mode, y = percentage, fill = party_name)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(
    name = "Voting Intentions",
    values = party_colors
  ) +
  labs(
    title = "TRANSPORTATION MODES BY POLITICAL PARTY",
    subtitle = "Percentage of each party's voters using different modes of transportation",
    x = "",
    y = "Percentage (%)",
    caption = paste0("Source: Léger-Datagotchi 2025 | n=", format(nrow(df), big.mark = ","), 
                    "\nWeighted data by: gender, age, province, language, education level, income, immigration status, housing type")
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "sans"),
    plot.title = element_text(face = "bold", size = 24, color = "black", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 16, color = "#555555", hjust = 0.5, margin = margin(b = 20)),
    # Position the legend at the bottom with better styling
    legend.position = "bottom",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.box = "horizontal",
    legend.margin = margin(t = 20, b = 10),
    # Adjust the legend key size and spacing
    legend.key.size = unit(1.5, "cm"),
    legend.spacing.x = unit(0.4, "cm"),
    # Other styling elements
    axis.text.x = element_text(color = "black", size = 14, angle = 0, hjust = 0.5),
    axis.text.y = element_text(color = "#555555", size = 12),
    panel.grid.major.y = element_line(color = "#DDDDDD", size = 0.2),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(color = "#666666", size = 12, hjust = 0, margin = margin(t = 20, b = 10)),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Create directory if it doesn't exist
dir.create("_SharedFolder_datagotchi_federal_2024/graph/analyses/transport", recursive = TRUE, showWarnings = FALSE)

# Save the graph with legend
# Read the logo
logo_path <- "_SharedFolder_datagotchi_federal_2024/logos/FR/logo_black.png"
if (file.exists(logo_path)) {
  logo <- image_read(logo_path)
  
  # Save the plot to a temporary file
  temp_file <- tempfile(fileext = ".png")
  ggsave(temp_file, transport_plot_dodge, width = 14, height = 12, dpi = 200, bg = "white")
  
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
  image_write(final_img, "_SharedFolder_datagotchi_federal_2024/graph/analyses/transport/transport_vote_inverse_final_en.png")
  
  # Remove temporary file
  file.remove(temp_file)
  
  cat("Graph created successfully with logo: transport_vote_inverse_final_en.png\n")
} else {
  # If logo not found, save without it
  ggsave("_SharedFolder_datagotchi_federal_2024/graph/analyses/transport/transport_vote_inverse_final_en.png", 
         transport_plot_dodge, 
         width = 14, 
         height = 12,  # Increased height to accommodate the legend
         dpi = 200,
         bg = "white")
  
  cat("Graph created successfully without logo: transport_vote_inverse_final_en.png\n")
  cat("Logo not found at: ", logo_path, "\n")
}