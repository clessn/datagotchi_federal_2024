# ------------------------- Préparation des données -------------------------
library(dplyr)
library(ggplot2)
library(png)
library(grid)
library(cowplot)
library(shadowtext)
library(clessnize)
library(magick)
library(ggimage)
library(patchwork)
library(jpeg)
library(gifski)

# Chargement des données RCI
df_aggregated_rci <- readRDS("_SharedFolder_datagotchi_federal_2024/data/potGrowth/03_aggregated_rci.rds")

# Créer le data frame des informations sur les clusters
cluster_info <- data.frame(
  cluster_name   = 1:10,
  cluster_label  = c("Jennifer", "Arjun", "Michel", "David", "John",
                     "Maxime", "Zoe", "Julie", "Robert", "Emily"),
  image_tete     = rep("_SharedFolder_datagotchi_federal_2024/images/landingPage_potgrowth/Cluster_Datagotchi__0000_Jennifer2000.png", 10)
)

# Fusionner les infos de clusters avec les données RCI
df_aggregated_rci <- df_aggregated_rci %>%
  mutate(cluster_name = as.numeric(cluster_name))
df_plot <- df_aggregated_rci %>%
  left_join(cluster_info, by = "cluster_name")

# Définir les couleurs spécifiques par parti
party_colors <- c(
  "LPC" = "#D71B1E",
  "CPC" = "#142E52",
  "NDP" = "#F58220",
  "BQ"  = "#080236",
  "GPC" = "#3D9B35",
  "PPC" = "#442D7B"
)

# Préparation du logo (identique pour tous les clusters)
logo_image <- readPNG("_SharedFolder_datagotchi_federal_2024/logos/FR/logo_black.png")
logo_grob <- rasterGrob(logo_image, interpolate = TRUE)

# ------------------------- Boucle sur les clusters sans logo  -------------------------
for(cluster in unique(df_plot$cluster_name)) {
  
  # Filtrer les données pour le cluster courant
  df_filtered <- df_plot %>% filter(cluster_name == cluster)
  
  # Boucle sur les 8 images iceberg
  for(i in 1:8) {
  
  # Charger l'image iceberg correspondant à la itération i
  img_filename <- paste0("_SharedFolder_datagotchi_federal_2024/images/landingPage_potgrowth/iceberg_", i, ".jpg")
  img_iceberg <- readJPEG(img_filename)
  iceberg_grob <- rasterGrob(
    img_iceberg,
    width = unit(1, "npc"),
    height = unit(1, "npc"),
    interpolate = TRUE
  )
  
  # -------------------- Graphique RCI en français --------------------
  plot_rciFr <- ggplot(df_filtered, aes(x = party, y = rci)) +
    annotation_custom(iceberg_grob, xmin = -1.05, xmax = 7.02, ymin = -Inf, ymax = Inf) +
    geom_bar(aes(fill = party), stat = "identity", width = 0.35) + 
    geom_text(aes(label = round(rci, 0),
                  y = ifelse(rci >= 0, rci + 15, rci - 15)),
              size = 22, color = "black", family = "PixelOperatorSC") +
    geom_image(aes(image = image_tete), size = 0.08, by = "width") +
    scale_fill_manual(values = party_colors) +
    scale_color_manual(values = party_colors) +
    scale_x_discrete(labels = c(
      "LPC" = "PLC",
      "CPC" = "PCC",
      "NDP" = "NPD",
      "BQ"  = "BQ",
      "GPC" = "PVC",
      "PPC" = "PPC"
    )) +
    labs(
      title = paste0("Potentiel de croissance par\nparti pour ", df_filtered$cluster_label[1]),
      x = NULL,
      y = NULL
    ) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -100, ymax = 0,
             fill = "#1bb8d3", alpha = 0.3) +
    annotate("text", x = 0, y = 0, label = "Seuil\nde vote",
             hjust = 0.5, vjust = -1, angle = 90, size = 20, lineheight = 0.2, family = "PixelOperatorSC") +
    annotate("text", x = 0, y = 50, label = "Solidité du vote",
             angle = 90, hjust = 0.3, vjust = -3.5, size = 20, family = "PixelOperatorSC") +
    annotate("text", x = 0, y = 50, label = "Vote potentiel",
             angle = 90, hjust = 3, vjust = -3.5, size = 20, family = "PixelOperatorSC") +
    coord_cartesian(clip = "off") +
    clessnize::theme_datagotchi_light(base_size = 60) +
    scale_y_continuous(
      limits = c(-100, 100)
    ) +
    theme(text = element_text(size = 70),
          legend.position = "none",
          plot.title = element_text(lineheight = 0.2),
          plot.caption = element_text(lineheight = 0.2, size = 40),
          plot.caption.position = "plot",
          plot.margin = margin(t = 10, r = 10, b = 10, l = 20) 
    )
  
  # -------------------- Graphique RCI en anglais --------------------
  plot_rciEn <- ggplot(df_filtered, aes(x = party, y = rci)) +
    annotation_custom(iceberg_grob, xmin = -1.05, xmax = 7.02, ymin = -Inf, ymax = Inf) +
    geom_bar(aes(fill = party), stat = "identity", width = 0.35) + 
    geom_text(aes(label = round(rci, 0),
                  y = ifelse(rci >= 0, rci + 15, rci - 15)),
              size = 22, color = "black", family = "PixelOperatorSC") +
    geom_image(aes(image = image_tete), size = 0.08, by = "width") +
    scale_fill_manual(values = party_colors) +
    scale_color_manual(values = party_colors) +
    labs(
      title = paste0("Potential for Growth per\npolitical party for ", df_filtered$cluster_label[1]),
      x = NULL, y = NULL
    ) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -100, ymax = 0,
             fill = "#1bb8d3", alpha = 0.3) +
    annotate("text", x = 0, y = 0, label = "Voting\nThreshold",
             hjust = 0.5, vjust = -1, angle = 90, size = 20, family = "PixelOperatorSC",
             lineheight = 0.2) +
    annotate("text", x = 0, y = 50, label = "Vote certainty",
             angle = 90, hjust = 0.3, vjust = -3.5, size = 20, family = "PixelOperatorSC") +
    annotate("text", x = 0, y = 50, label = "Potential vote",
             angle = 90, hjust = 3, vjust = -3.5, size = 20, family = "PixelOperatorSC") +
    coord_cartesian(clip = "off") +
    clessnize::theme_datagotchi_light(base_size = 60) +
    scale_y_continuous(limits = c(-100, 100)) +
    theme(text = element_text(size = 70),
          legend.position = "none",
          plot.title = element_text(lineheight = 0.2),
          plot.caption = element_text(lineheight = 0.2, size = 40),
          plot.caption.position = "plot",
          plot.margin = margin(t = 10, r = 10, b = 10, l = 30)
    ) 
  
  plot_finalFr <- ggdraw() +
    draw_plot(plot_rciFr, 
              x = -0.10,        # Décale légèrement vers la gauche pour réduire l'espace inutile
              y = 0.08,         # Monte un peu vers le haut pour libérer de l'espace en bas
              width = 1.1,      # Augmente la largeur légèrement pour remplir mieux l'espace
              height = 0.82) +  # Réduit la hauteur pour faire place à la légende
    draw_label(
      "Source : Léger-Datagotchi 2025 | Méthodologie: Pour calculer le RCI, les répondants évaluent sur 10 leur probabilité de voter pour\nchaque parti.Un score relatif est ensuite calculé : le parti préféré reçoit un score positif (écart avec le 2e), les autres un score\nnégatif (écart avec le 1er). Ce score permet de mesurer la solidité du vote et le potentiel de croissance des partis dans chaque\nsegment électoral.Pour élaborer les clusters, nous avons utilisé la méthode k-means sur les données de notre sondage pilote (n = 1021).",
      x = 0.10, y = 0.05, 
      hjust = 0, vjust = 0,
      fontfamily = "PixelOperatorSC",
      size = 36,                # Ajuste légèrement la taille pour garantir la lisibilité
      lineheight = 0.3,         # Interligne un peu plus aéré pour lisibilité
      color = "black"
    )
  
  plot_finalEn <- ggdraw() +
    draw_plot(plot_rciEn, 
              x = -0.10,        # Décale légèrement vers la gauche pour réduire l'espace inutile
              y = 0.08,         # Monte un peu vers le haut pour libérer de l'espace en bas
              width = 1.1,      # Augmente la largeur légèrement pour remplir mieux l'espace
              height = 0.82) +  # Réduit la hauteur pour faire place à la légende
    draw_label(
      "Source : Léger-Datagotchi 2025 | Methodology: To calculate the RCI, respondents rate their likelihood of voting for each party on a\nscale of 10. A relative score is then calculated: the preferred party receives a positive score (the difference with the second),\nwhile the others receive a negative score (the difference with the first). This score measures the solidity of the vote and the\ngrowth potential of parties in each electoral segment. To develop the clusters, we used the k-means method on the data from our\npilot survey (n = 1021).",
      x = 0.10, y = 0.03, 
      hjust = 0, vjust = 0,
      fontfamily = "PixelOperatorSC",
      size = 36,                # Ajuste légèrement la taille pour garantir la lisibilité
      lineheight = 0.3,         # Interligne un peu plus aéré pour lisibilité
      color = "black"
    )
  
  # -------------------- Sauvegardes (sans logo) --------------------
  ggsave(
    filename = paste0("_SharedFolder_datagotchi_federal_2024/graph/analyses/landingPage_clusterPotGrowth/graphWithoutLogo_fr/cluster_rci_plotFr_withoutLogo_", 
                      cluster, "_iceberg", i, ".png"),
    plot = plot_finalFr,
    width = 10, height = 10, dpi = 300, bg = "white", device = "png"
  )
  ggsave(
    filename = paste0("_SharedFolder_datagotchi_federal_2024/graph/analyses/landingPage_clusterPotGrowth/graphWithoutLogo_en/cluster_rci_plotEn_withoutLogo_", 
                      cluster, "_iceberg", i, ".png"),
    plot = plot_finalEn,
    width = 10, height = 10, dpi = 300, bg = "white", device = "png"
  )
  cat("Cluster", cluster, "traité et sauvegardé.\n")
  } 
  
  cat("Cluster", cluster, ": 8 images FR et EN générées.\n")
  
  # 4) Créer le GIF français pour ce cluster
  frames_fr <- paste0(
    "_SharedFolder_datagotchi_federal_2024/graph/analyses/landingPage_clusterPotGrowth/graphWithoutLogo_fr/cluster_rci_plotFr_withoutLogo_",
    cluster, "_iceberg", 1:8, ".png"
  )
  gifski(
    png_files = frames_fr,
    gif_file  = paste0("_SharedFolder_datagotchi_federal_2024/graph/analyses/landingPage_clusterPotGrowth/cluster_rci_plotFr_withoutLogo_", cluster, ".gif"),
    delay = 0.5,
    loop = TRUE
  )
  
  # 5) Créer le GIF anglais pour ce cluster
  frames_en <- paste0(
    "_SharedFolder_datagotchi_federal_2024/graph/analyses/landingPage_clusterPotGrowth/graphWithoutLogo_en/cluster_rci_plotEn_withoutLogo_",
    cluster, "_iceberg", 1:8, ".png"
  )
  gifski(
    png_files = frames_en,
    gif_file  = paste0("_SharedFolder_datagotchi_federal_2024/graph/analyses/landingPage_clusterPotGrowth/cluster_rci_plotEn_withoutLogo_", cluster, ".gif"),
    delay = 0.5,
    loop = TRUE
  )
  
  cat("Cluster", cluster, ": GIF FR et GIF EN créés.\n\n")
  
} # Fin boucle sur les clusters
  }
