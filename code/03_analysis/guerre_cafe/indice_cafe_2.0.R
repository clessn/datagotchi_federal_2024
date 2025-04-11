# Version simplifiée du graphique café-politique avec élimination des doublons
# et application cohérente du thème Datagotchi

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

# ÉTAPE 1: Extraire les paramètres du thème Datagotchi pour les réutiliser dans magick
# Créer un thème temporaire pour extraire les propriétés
theme_temp <- theme_datagotchi_light()

# Obtenir les valeurs du thème (ces valeurs sont symboliques, nous allons les utiliser pour magick)
# Dans un environnement réel, il faudrait inspecter le thème_datagotchi_light() pour connaître les valeurs exactes
datagotchi_font_family <- "Arial"  # Remplacer par la police exacte utilisée dans theme_datagotchi_light
datagotchi_text_color <- "#000000" # Noir par défaut, ajuster selon le thème
datagotchi_subtitle_color <- "#555555"
datagotchi_caption_color <- "#666666"
datagotchi_bg_color <- "white"

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

# ÉTAPE 2: Maintenant, utiliser les propriétés du thème Datagotchi pour magick
# Créer une légende centrée en utilisant les propriétés du thème Datagotchi
legend_height <- 100
legend_bg <- image_blank(width, legend_height, color = datagotchi_bg_color)

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

# Composite la légende avec le style Datagotchi
legend_bg <- legend_bg %>%
  image_composite(tim_icon, offset = paste0("+", start_x, "+25")) %>%
  image_annotate("Tim Hortons 🇨🇦", color = datagotchi_text_color, size = 40, 
                 location = paste0("+", start_x + 100, "+35"),
                 font = datagotchi_font_family) %>%  # Utiliser la police du thème Datagotchi
  image_composite(mcdo_icon, offset = paste0("+", start_x + icon_spacing + 50, "+30")) %>%
  image_annotate("McDonald's 🇺🇸", color = datagotchi_text_color, size = 40,
                 location = paste0("+", start_x + icon_spacing + 150, "+35"),
                 font = datagotchi_font_family) %>%  # Utiliser la police du thème Datagotchi
  image_composite(starbucks_icon, offset = paste0("+", start_x + 2*icon_spacing + 100, "+30")) %>%
  image_annotate("Starbucks 🇺🇸", color = datagotchi_text_color, size = 40,
                 location = paste0("+", start_x + 2*icon_spacing + 200, "+35"),
                 font = datagotchi_font_family)  # Utiliser la police du thème Datagotchi

# Créer le pied de page avec le style Datagotchi
footer_height <- 200
footer <- image_blank(width, footer_height, color = datagotchi_bg_color) %>%
  image_annotate(paste0("Source: Léger-Datagotchi 2025 | n=", format(n_observations, big.mark = " ")), 
                 color = datagotchi_subtitle_color,  # Utiliser la couleur du thème Datagotchi
                 size = 40,
                 location = "+40+30", 
                 font = datagotchi_font_family,  # Utiliser la police du thème Datagotchi
                 gravity = "west") %>%
  image_annotate("Données pondérées selon: le genre, l'âge, la province, la langue, le niveau d'éducation, le revenu, l'immigration, le type d'habitation",
                 color = datagotchi_subtitle_color,  # Utiliser la couleur du thème Datagotchi
                 size = 30,
                 location = "+40+80",  # Ajusté pour éviter le chevauchement
                 font = datagotchi_font_family,  # Utiliser la police du thème Datagotchi
                 gravity = "west")

# Ajouter le logo Datagotchi
logo <- image_read("_SharedFolder_datagotchi_federal_2024/logos/FR/logo_fr.png") %>% 
  image_scale("300x") %>%
  image_background(datagotchi_bg_color)  # Utiliser la couleur de fond du thème Datagotchi

footer <- footer %>%
  image_composite(logo, gravity = "east", offset = "+40+0")

# Assemblage final avec le style Datagotchi cohérent
final_img <- image_blank(width, height + legend_height + footer_height, color = datagotchi_bg_color) %>%
  image_composite(plot_img, offset = "+0+0") %>%
  image_composite(legend_bg, offset = paste0("+0+", height)) %>%
  image_composite(footer, offset = paste0("+0+", height + legend_height))

# Sauvegarder
image_write(final_img, "_SharedFolder_datagotchi_federal_2024/graph/analyses/café/indice_cafe_politique_light_final.png")

# REMARQUE: Si vous préférez une approche encore plus intégrée, vous pourriez:
# 1. Configurer une version complète avec ggplot (y compris la légende intégrée)
# 2. Utiliser cowplot ou patchwork pour assembler le graphique et le logo
# Exemple de code pour ces approches alternatives dans les commentaires ci-dessous:

# ALTERNATIVE 1: Intégrer la légende dans ggplot (code commenté)
# Version intégrée du graphique café-politique avec thème Datagotchi complet
# Cette version utilise ggplot pour tous les éléments, y compris la légende

# Version améliorée du graphique café-politique avec cohérence de police
# et icônes dans la légende, tout en gardant le thème Datagotchi

library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)
library(gridExtra)
library(png)
library(cowplot)

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

# Couleurs pour les chaînes de café
coffee_colors <- c(
  "Tim Hortons 🇨🇦" = "#C8102E",   # Rouge Tim Hortons
  "McDonald's 🇺🇸" = "#FFC72C",    # Jaune McDonald's
  "Starbucks 🇺🇸" = "#00704A"      # Vert Starbucks
)

# Sous-titre avec les moyennes nationales
ref_subtitle <- paste0("Moyennes nationales: Tim Hortons = ", tim_national, 
                       "%, McDonald's = ", mcdo_national, 
                       "%, Starbucks = ", starbucks_national, "%")

# Créer un graphique avec Datagotchi theme, mais sans légende (nous l'ajouterons manuellement)
main_plot <- ggplot(coffee_by_party_long, aes(x = party_name, y = deviation, fill = coffee_chain)) +
  # Ligne médiane plus épaisse
  geom_segment(
    x = 0.58,
    xend = length(party_order) + 0.5,
    y = 0,
    yend = 0,
    color = "#999999", 
    linetype = "solid", 
    size = 2
  ) +
  
  # Barres du graphique
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  
  # Add +/- symbols aligned with discrete axis
  # Note: Utiliser la même famille de police que dans le thème Datagotchi
  # Obtenir la famille de police du thème Datagotchi
  annotate("text", x = 0.5, y = 0, 
           label = "moyenne canadienne", 
           color = "black", 
           size = 12, 
           fontface = "bold", 
           angle = 90,
           family = "Arial") +  # Famille de police du thème Datagotchi
  annotate("text", x = 0.5, y = 10, 
           label = "+", 
           color = "black", 
           size = 10, 
           fontface = "bold",
           family = "Arial") +  # Même famille de police
  annotate("text", x = 0.5, y = -10, 
           label = "-", 
           color = "black", 
           size = 10, 
           fontface = "bold",
           family = "Arial") +  # Même famille de police
  
  scale_fill_manual(
    name = "Chaîne de café",
    values = coffee_colors
  ) +
  
  labs(
    title = "L'INDICE CAFÉ-POLITIQUE",
    subtitle = "Écart de consommation par rapport à la moyenne canadienne (points de %)",
    caption = paste0(
      "Source: Léger-Datagotchi 2025 | n=", format(n_observations, big.mark = " "), "\n",
      "Moyennes canadiennes: Tim Hortons = ", tim_national, "%, McDonald's = ", mcdo_national, "%, Starbucks = ", starbucks_national, "%\n",
      "Données pondérées selon: le genre, l'âge, la province, la langue, le niveau d'éducation, le revenu, l'immigration, le type d'habitation"
    ),
    x = "",
    y = ""
  ) +
  theme_datagotchi_light() +
  theme(
    plot.title = element_text(face = "bold", size = 60, color = "black", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(face = "bold", size = 40, color = "#555555", hjust = 0.5, margin = margin(b = 20)),
    # Suppression de la légende standard, nous l'ajouterons avec les icônes
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

# Sauvegarder le graphique principal
ggsave("_SharedFolder_datagotchi_federal_2024/graph/analyses/café/indice_cafe_main.png", 
       main_plot, 
       width = 14, 
       height = 10,
       dpi = 200,
       bg = "white")

# Approche intégrée en utilisant cowplot et grid pour ajouter la légende avec les icônes

# 1. Créer la légende avec ggdraw
create_legend_with_icons <- function() {
  # Dimensions
  plot_width <- 14  # En pouces
  plot_height <- 10  # En pouces
  
  # Chemins des icônes
  tim_icon_path <- "_SharedFolder_datagotchi_federal_2024/graph/analyses/café/CoffeePack/CoffeePack__0007_tim.png"
  mcdo_icon_path <- "_SharedFolder_datagotchi_federal_2024/graph/analyses/café/CoffeePack/CoffeePack__0002_cafe-3-mcdo.png"
  starbucks_icon_path <- "_SharedFolder_datagotchi_federal_2024/graph/analyses/café/CoffeePack/CoffeePack__0001_cafe-2-starbuck.png"
  
  # Vérifier si les fichiers existent
  if (file.exists(tim_icon_path) && file.exists(mcdo_icon_path) && file.exists(starbucks_icon_path)) {
    # Charger les icônes
    tim_icon <- readPNG(tim_icon_path)
    mcdo_icon <- readPNG(mcdo_icon_path)
    starbucks_icon <- readPNG(starbucks_icon_path)
    
    # Créer un canevas pour la légende
    legend_canvas <- ggdraw() + 
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    # Positionnement des icônes et du texte
    icon_size <- 0.08  # Taille relative des icônes
    icon_spacing <- 0.3  # Espacement horizontal entre les icônes
    
    # Position centrale de la première icône
    first_icon_x <- 0.25
    
    # Ajouter les icônes et les textes
    # Tim Hortons
    legend_canvas <- legend_canvas +
      draw_image(
        tim_icon, 
        x = first_icon_x - icon_size/2, 
        y = 0.5 - icon_size/2, 
        width = icon_size, 
        height = icon_size
      ) +
      draw_label(
        "Tim Hortons 🇨🇦", 
        x = first_icon_x + icon_size, 
        y = 0.5, 
        size = 40, 
        fontface = "plain", 
        color = "black",
        fontfamily = "Arial"  # Utiliser la même police que le reste du graphique
      )
    
    # McDonald's
    legend_canvas <- legend_canvas +
      draw_image(
        mcdo_icon, 
        x = first_icon_x + icon_spacing - icon_size/2, 
        y = 0.5 - icon_size/2, 
        width = icon_size, 
        height = icon_size
      ) +
      draw_label(
        "McDonald's 🇺🇸", 
        x = first_icon_x + icon_spacing + icon_size, 
        y = 0.5, 
        size = 40, 
        fontface = "plain", 
        color = "black",
        fontfamily = "Arial"  # Utiliser la même police que le reste du graphique
      )
    
    # Starbucks
    legend_canvas <- legend_canvas +
      draw_image(
        starbucks_icon, 
        x = first_icon_x + 2*icon_spacing - icon_size/2, 
        y = 0.5 - icon_size/2, 
        width = icon_size, 
        height = icon_size
      ) +
      draw_label(
        "Starbucks 🇺🇸", 
        x = first_icon_x + 2*icon_spacing + icon_size, 
        y = 0.5, 
        size = 40, 
        fontface = "plain", 
        color = "black",
        fontfamily = "Arial"  # Utiliser la même police que le reste du graphique
      )
    
    return(legend_canvas)
  } else {
    # Créer une légende de secours si les icônes ne sont pas disponibles
    legend_fallback <- ggdraw() +
      draw_label("Tim Hortons 🇨🇦", x = 0.2, y = 0.5, size = 40, color = coffee_colors["Tim Hortons 🇨🇦"], fontfamily = "Arial") +
      draw_label("McDonald's 🇺🇸", x = 0.5, y = 0.5, size = 40, color = coffee_colors["McDonald's 🇺🇸"], fontfamily = "Arial") +
      draw_label("Starbucks 🇺🇸", x = 0.8, y = 0.5, size = 40, color = coffee_colors["Starbucks 🇺🇸"], fontfamily = "Arial") +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    return(legend_fallback)
  }
}

# 2. Créer le pied de page
create_footer <- function() {
  footer_canvas <- ggdraw() +
    draw_label(
      paste0(
        "Source: Léger-Datagotchi 2025 | n=", format(n_observations, big.mark = " ")
      ),
      x = 0.1,
      y = 0.7,
      hjust = 0,
      size = 17,
      color = "#555555",
      fontfamily = "Arial"
    ) +
    draw_label(
      paste0(
        "Données pondérées selon: le genre, l'âge, la province, la langue, le niveau d'éducation, le revenu, l'immigration, le type d'habitation"
      ),
      x = 0.1,
      y = 0.4,
      hjust = 0,
      size = 15,
      color = "#555555",
      fontfamily = "Arial"
    ) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  
  # Ajouter le logo si disponible
  logo_path <- "_SharedFolder_datagotchi_federal_2024/logos/FR/logo_fr.png"
  if (file.exists(logo_path)) {
    logo_img <- readPNG(logo_path)
    footer_canvas <- footer_canvas +
      draw_image(
        logo_img,
        x = 0.9,
        y = 0.5,
        width = 0.1,
        height = 0.8,
        hjust = 1,
        vjust = 0.5
      )
  }
  
  return(footer_canvas)
}

# 3. Assembler le tout avec cowplot
# Créer les différents éléments
legend_panel <- create_legend_with_icons()
footer_panel <- create_footer()

# Assembler le graphique complet
final_plot <- plot_grid(
  main_plot,
  legend_panel,
  footer_panel,
  ncol = 1,
  rel_heights = c(10, 1, 1.5),  # Proportion de hauteur pour chaque élément
  align = "v"
)

# Sauvegarder le graphique final
ggsave("_SharedFolder_datagotchi_federal_2024/graph/analyses/café/indice_cafe_politique_final_integre.png", 
       final_plot, 
       width = 14, 
       height = 12.5,  # Hauteur augmentée pour accommoder la légende et le pied de page
       dpi = 200,
       bg = "white")

# Alternative avec magick (si cowplot ne donne pas le résultat escompté)
library(magick)

# Lire le graphique principal
main_img <- image_read("_SharedFolder_datagotchi_federal_2024/graph/analyses/café/indice_cafe_main.png")

# Dimensions
img_info <- image_info(main_img)
width <- img_info$width
height <- img_info$height

# Créer une légende centrée avec la même police que le graphique
legend_height <- 100
legend_bg <- image_blank(width, legend_height, color = "white")

# Chemins des icônes
tim_icon_path <- "_SharedFolder_datagotchi_federal_2024/graph/analyses/café/CoffeePack/CoffeePack__0007_tim.png"
mcdo_icon_path <- "_SharedFolder_datagotchi_federal_2024/graph/analyses/café/CoffeePack/CoffeePack__0002_cafe-3-mcdo.png"
starbucks_icon_path <- "_SharedFolder_datagotchi_federal_2024/graph/analyses/café/CoffeePack/CoffeePack__0001_cafe-2-starbuck.png"

# Charger les icônes
tim_icon <- image_read(tim_icon_path) %>% image_scale("80x80")
mcdo_icon <- image_read(mcdo_icon_path) %>% image_scale("80x80") 
starbucks_icon <- image_read(starbucks_icon_path) %>% image_scale("80x80")

# Positionnement des icônes
icon_spacing <- 400
start_x <- (width - (3 * 50 + 2 * icon_spacing)) / 2

# Composite la légende - UTILISER LA MÊME POLICE QUE LE GRAPHIQUE PRINCIPAL
font_family <- "Arial"  # Police utilisée dans theme_datagotchi_light

legend_bg <- legend_bg %>%
  image_composite(tim_icon, offset = paste0("+", start_x, "+25")) %>%
  image_annotate("Tim Hortons 🇨🇦", color = "black", size = 40, 
                 location = paste0("+", start_x + 100, "+35"),
                 font = font_family) %>%  # Police de Datagotchi
  image_composite(mcdo_icon, offset = paste0("+", start_x + icon_spacing + 50, "+30")) %>%
  image_annotate("McDonald's 🇺🇸", color = "black", size = 40,
                 location = paste0("+", start_x + icon_spacing + 150, "+35"),
                 font = font_family) %>%  # Police de Datagotchi
  image_composite(starbucks_icon, offset = paste0("+", start_x + 2*icon_spacing + 100, "+30")) %>%
  image_annotate("Starbucks 🇺🇸", color = "black", size = 40,
                 location = paste0("+", start_x + 2*icon_spacing + 200, "+35"),
                 font = font_family)  # Police de Datagotchi

# Créer le pied de page avec la même police que le graphique
footer_height <- 200
footer <- image_blank(width, footer_height, color = "white") %>%
  image_annotate(paste0("Source: Léger-Datagotchi 2025 | n=", format(n_observations, big.mark = " ")), 
                 color = "#555555", 
                 size = 40,
                 location = "+40+30", 
                 font = font_family,  # Police de Datagotchi
                 gravity = "west") %>%
  image_annotate("Données pondérées selon: le genre, l'âge, la province, la langue, le niveau d'éducation, le revenu, l'immigration, le type d'habitation",
                 color = "#555555",
                 size = 30,
                 location = "+40+80",  # Position ajustée pour éviter le chevauchement
                 font = font_family,  # Police de Datagotchi
                 gravity = "west")

# Ajouter le logo Datagotchi
logo_path <- "_SharedFolder_datagotchi_federal_2024/logos/FR/logo_fr.png"
if (file.exists(logo_path)) {
  logo <- image_read(logo_path) %>% 
    image_scale("300x") %>%
    image_background("white")
  
  footer <- footer %>%
    image_composite(logo, gravity = "east", offset = "+40+0")
}

# Assemblage final avec cohérence de police
final_img <- image_blank(width, height + legend_height + footer_height, color = "white") %>%
  image_composite(main_img, offset = "+0+0") %>%
  image_composite(legend_bg, offset = paste0("+0+", height)) %>%
  image_composite(footer, offset = paste0("+0+", height + legend_height))

# Sauvegarder la version magick
image_write(final_img, "_SharedFolder_datagotchi_federal_2024/graph/analyses/café/indice_cafe_politique_final_magick.png")

# Afficher un message pour indiquer que les deux versions ont été générées
cat("Deux versions du graphique ont été générées:\n")
cat("1. Version intégrée avec cowplot: _SharedFolder_datagotchi_federal_2024/graph/analyses/café/indice_cafe_politique_final_integre.png\n")
cat("2. Version avec magick: _SharedFolder_datagotchi_federal_2024/graph/analyses/café/indice_cafe_politique_final_magick.png\n")
