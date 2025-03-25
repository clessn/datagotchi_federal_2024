library(dplyr)
library(ggplot2)
library(patchwork)
library(scales)

# 1. Charger les données et calculs existants
data <- readRDS("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/datagotchi2025_canada_appPonderee_20250323.rds")

# 2. Définir les couleurs officielles des partis
party_colors <- c(
  "CPC" = "#1A4782",  # Conservative - Blue
  "LPC" = "#D71920",   # Liberal - Red
  "BQ" = "#33B2CC",    # Bloc Québécois - Light blue
  "NDP" = "#F58220",   # NDP - Orange
  "GP" = "#3D9B35"     # Green Party - Green
)

# 3. Calculer la consommation par parti (version simplifiée)
coffee_by_party <- data %>%
  filter(!is.na(dv_voteChoice)) %>%
  filter(dv_voteChoice %in% c("lpc", "cpc", "ndp", "bq", "gpc")) %>%
  group_by(dv_voteChoice) %>%
  summarize(
    sum_weight = sum(weight, na.rm = TRUE),
    tim_pct = sum((lifestyle_consCoffeeTimHortons == 1) * weight) / sum_weight * 100,
    mcdo_pct = sum((lifestyle_consCoffeeMcDo == 1) * weight) / sum_weight * 100,
    starbucks_pct = sum((lifestyle_consCoffeeStarbucks == 1) * weight) / sum_weight * 100
  ) %>%
  mutate(
    party = case_when(
      dv_voteChoice == "cpc" ~ "CPC",
      dv_voteChoice == "lpc" ~ "LPC", 
      dv_voteChoice == "ndp" ~ "NDP",
      dv_voteChoice == "bq" ~ "BQ",
      dv_voteChoice == "gpc" ~ "GP"
    )
  ) %>%
  mutate(party = factor(party, levels = c("CPC", "LPC", "BQ", "NDP", "GP")))

# 4. Fonction pour créer les graphiques
create_coffee_plot <- function(data, var, title, y_limits) {
  ggplot(data, aes(x = party, y = .data[[var]], fill = party)) +
    geom_col(width = 0.7) +
    scale_fill_manual(values = party_colors) +
    scale_y_continuous(
      labels = percent_format(scale = 1), 
      limits = y_limits,
      breaks = seq(y_limits[1], y_limits[2], by = 10)  # Grille explicite
    ) +
    labs(
      title = toupper(title),
      x = "",
      y = ifelse(var == "tim_pct", "Pourcentage de consommateurs", "")  # Label seulement à gauche
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14, color = "black"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 12, color = "black"),
      axis.text.y = element_text(size = 10, color = "black"),
      legend.position = "none",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

# Garder TOUTES les étiquettes Y visibles
plot_tim <- create_coffee_plot(coffee_by_party, "tim_pct", "Tim Hortons", c(0, 50)) +
  theme(plot.margin = margin(r = 15))

plot_mcdo <- create_coffee_plot(coffee_by_party, "mcdo_pct", "McDonald's", c(0, 20))

plot_starbucks <- create_coffee_plot(coffee_by_party, "starbucks_pct", "Starbucks", c(0, 15))



# 6. Combiner les graphiques
combined_plot <- plot_tim + plot_mcdo + plot_starbucks +
  plot_layout(nrow = 1, widths = c(1, 1, 1)) 

# 7. Ajouter titre global et annotations
final_plot <- combined_plot +
  plot_annotation(
    title = "CONSOMMATION DE CAFÉ PAR AFFILIATION POLITIQUE",
    subtitle = "Pourcentage de partisans qui consomment régulièrement de chaque chaîne\nRemarque: Les échelles de l'axe Y diffèrent entre les graphiques pour une meilleure lisibilité",
    caption = paste0(
      "Source: Léger-Datagotchi 2025 | n = ", format(nrow(data), big.mark = " "), 
      "\nDonnées pondérées selon: sexe, âge, province, langue, niveau d'éducation, revenu, immigration, type de logement"
    ),
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), # Ajouté pour centrer
      plot.subtitle = element_text(hjust = 0.5, size = 12, color = "#555555", margin = margin(t = 5, b = 15)),
      plot.caption = element_text(hjust = 0.5, size = 10, color = "#666666", lineheight = 1.2, margin = margin(t = 10)),
      plot.background = element_rect(fill = "white", color = NA)
    )
  )
final_plot
  ggsave("_SharedFolder_datagotchi_federal_2024/graph/analyses/café/coffee_politics_triple-FR.png", 
  final_plot, 
  width = 16, 
  height = 8,
  dpi = 200,
  bg = "white")

# Après la sauvegarde avec ggsave, ajouter :
library(magick)

# Lire l'image sauvegardée
plot_img <- image_read("_SharedFolder_datagotchi_federal_2024/graph/analyses/café/coffee_politics_triple-FR.png")

# Charger le logo
logo_path <- "_SharedFolder_datagotchi_federal_2024/logos/FR/logo_black.png"
logo <- image_read(logo_path)

# Redimensionner le logo
logo_width <- round(image_info(plot_img)$width * 0.15)
logo_resized <- image_scale(logo, paste0(logo_width, "x"))

# Positionnement dans le coin inférieur droit
x_position <- image_info(plot_img)$width - image_info(logo_resized)$width - 30
y_position <- image_info(plot_img)$height - image_info(logo_resized)$height - 30

# Ajouter le logo
final_with_logo <- image_composite(plot_img, logo_resized, offset = paste0("+", x_position, "+", y_position))

# Sauvegarder l'image finale
image_write(final_with_logo, "_SharedFolder_datagotchi_federal_2024/graph/analyses/café/coffee_politics_triple-FR.png")
final_plot
# 8. Sauvegarder