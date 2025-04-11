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

# Chargement des données RCI
df_aggregated_rci <- readRDS("_SharedFolder_datagotchi_federal_2024/data/potGrowth/03_aggregated_rci.rds")

# Créer le data frame des informations sur les clusters
cluster_info <- data.frame(
  cluster_name = c("Maxime", "Michel", "David", "Robert", "Emily",
                   "Jennifer", "John", "Julie", "Arjun", "Zoe"),
  cluster_nameFR = c("Maxime, le millénial naturel",
                     "Michel, le patriote québécois",
                     "David, le Red Tory",
                     "Robert, le Blue Grit",
                     "Emily, l'hipster engagée",
                     "Jennifer, la professionnelle urbaine",
                     "John, le réformiste",
                     "Julie, la terre-à-terre rangée",
                     "Arjun, le pragmatique cosmopolite",
                     "Zoe, l'urbaine branchée"),
  cluster_nameEN = c("Maxime, the natural millennial",
                     "Michel, the Québécois patriot",
                     "David, the Red Tory",
                     "Robert, the Blue Grit",
                     "Emily, the committed hipster",
                     "Jennifer, the urban professional",
                     "John, the reformist",
                     "Julie, the grounded traditionalist",
                     "Arjun, the pragmatic cosmopolitan",
                     "Zoe, the trendy urbanite"),
  image_tete = c("_SharedFolder_datagotchi_federal_2024/images/landingPage_potgrowth/All_Clusters_Persona/x2000/Clusters-x2000-_0000s_0018_Maxime.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/landingPage_potgrowth/All_Clusters_Persona/x2000/Clusters-x2000-_0000s_0007_Michel.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/landingPage_potgrowth/All_Clusters_Persona/x2000/Clusters-x2000-_0000s_0010_David2.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/landingPage_potgrowth/All_Clusters_Persona/x2000/Clusters-x2000-_0000s_0030_Robert.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/landingPage_potgrowth/All_Clusters_Persona/x2000/Clusters-x2000-_0000s_0034_10--Emilie.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/landingPage_potgrowth/All_Clusters_Persona/x2000/Clusters-x2000-_0000s_0001_Jen-Layers.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/landingPage_potgrowth/All_Clusters_Persona/x2000/Clusters-x2000-_0000s_0014_John-Layers.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/landingPage_potgrowth/All_Clusters_Persona/x2000/Clusters-x2000-_0000s_0026_Julie.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/landingPage_potgrowth/All_Clusters_Persona/x2000/Clusters-x2000-_0000s_0004_Arjun.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/landingPage_potgrowth/All_Clusters_Persona/x2000/Clusters-x2000-_0000s_0022_Zoe.png")
)

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
  
  # ----- Graphique en français -----
  plot_rciFr <- ggplot(df_filtered, aes(x = party, y = rci)) +
    annotation_custom(iceberg_grob, xmin = -1.05, xmax = 7.02, ymin = -Inf, ymax = Inf) +
    geom_bar(aes(fill = party), stat = "identity", width = 0.35) + 
    geom_hline(yintercept = -1, color = "#040280", size = 2) +
    geom_text(aes(label = round(rci, 0),
                  y = ifelse(rci >= 0, rci + 20, rci - 20)),
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
      title = paste0(df_filtered$cluster_nameFR[1]),
      x = NULL,
      y = NULL
    ) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -100, ymax = 0,
             fill = "#1bb8d3", alpha = 0.3) +
    annotate("text", x = 0, y = 0, label = "\nPotentiel de croissance par parti",
             hjust = -0.35, vjust = -8, angle = 0, size = 30, lineheight = 0.2, family = "PixelOperatorSC") +
    annotate("text", x = 0, y = 0, label = "Seuil\nde vote",
             hjust = 0.5, vjust = -1, angle = 90, size = 20, lineheight = 0.2, family = "PixelOperatorSC") +
    annotate("text", x = 0, y = 50, label = "Solidité du vote",
             angle = 90, hjust = 0.3, vjust = -3.5, size = 20, family = "PixelOperatorSC") +
    annotate("text", x = 0, y = 50, label = "Vote potentiel",
             angle = 90, hjust = 3, vjust = -3.5, size = 20, family = "PixelOperatorSC") +
    coord_cartesian(clip = "off") +
    clessnize::theme_datagotchi_light(base_size = 60) +
    scale_y_continuous(limits = c(-100, 100)) +
    theme(text = element_text(size = 90),
          legend.position = "none",
          plot.title = element_text(size = 130, face = "bold", lineheight = 0.2),
          plot.caption = element_text(lineheight = 0.2, size = 40),
          plot.caption.position = "plot",
          plot.margin = margin(t = 30, r = 30, b = 30, l = 30)
    )
  
  # ----- Graphique en anglais -----
  plot_rciEn <- ggplot(df_filtered, aes(x = party, y = rci)) +
    annotation_custom(iceberg_grob, xmin = -1.05, xmax = 7.02, ymin = -Inf, ymax = Inf) +
    geom_bar(aes(fill = party), stat = "identity", width = 0.35) + 
    geom_hline(yintercept = -1, color = "#040280", size = 2) +
    geom_text(aes(label = round(rci, 0),
                  y = ifelse(rci >= 0, rci + 20, rci - 20)),
              size = 22, color = "black", family = "PixelOperatorSC") +
    geom_image(aes(image = image_tete), size = 0.08, by = "width") +
    scale_fill_manual(values = party_colors) +
    scale_color_manual(values = party_colors) +
    labs(
      title = paste0(df_filtered$cluster_nameEN[1]),
      x = NULL, y = NULL
    ) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -100, ymax = 0,
             fill = "#1bb8d3", alpha = 0.3) +
    annotate("text", x = 0, y = 0, label = "\nPotential for Growth per political party",
             hjust = -0.2, vjust = -8, angle = 0, size = 30, lineheight = 0.2, family = "PixelOperatorSC") +
    annotate("text", x = 0, y = 0, label = "Voting\nThreshold",
             hjust = 0.5, vjust = -1, angle = 90, size = 20, family = "PixelOperatorSC", lineheight = 0.2) +
    annotate("text", x = 0, y = 50, label = "Vote certainty",
             angle = 90, hjust = 0.3, vjust = -3.5, size = 20, family = "PixelOperatorSC") +
    annotate("text", x = 0, y = 50, label = "Potential vote",
             angle = 90, hjust = 3, vjust = -3.5, size = 20, family = "PixelOperatorSC") +
    coord_cartesian(clip = "off") +
    clessnize::theme_datagotchi_light(base_size = 60) +
    scale_y_continuous(limits = c(-100, 100)) +
    theme(text = element_text(size = 90),
          legend.position = "none",
          plot.title = element_text(size = 130, face = "bold", lineheight = 0.2),
          plot.caption = element_text(lineheight = 0.2, size = 40),
          plot.caption.position = "plot",
          plot.margin = margin(t = 10, r = 10, b = 10, l = 30)
    ) 
  
  # -------------------- Sauvegardes (sans logo) --------------------
  ggsave(
    filename = paste0("_SharedFolder_datagotchi_federal_2024/graph/analyses/landingPage_clusterPotGrowth/graphWithoutLogo_fr/cluster_rci_plotFr_withoutLogo_", 
                      cluster, "_iceberg", i, ".png"),
    plot = plot_rciFr,
    width = 10, height = 10, dpi = 300, bg = "white", device = "png"
  )
  ggsave(
    filename = paste0("_SharedFolder_datagotchi_federal_2024/graph/analyses/landingPage_clusterPotGrowth/graphWithoutLogo_en/cluster_rci_plotEn_withoutLogo_", 
                      cluster, "_iceberg", i, ".png"),
    plot = plot_rciEn,
    width = 10, height = 10, dpi = 300, bg = "white", device = "png"
  )
  cat("Cluster", cluster, "traité et sauvegardé.\n")
  } 
  
  cat("Cluster", cluster, ": 8 images FR et EN générées.\n")

# ---- Créer le GIF français ----
frames_fr <- paste0(
  "_SharedFolder_datagotchi_federal_2024/graph/analyses/landingPage_clusterPotGrowth/graphWithoutLogo_fr/cluster_rci_plotFr_withoutLogo_",
  cluster, "_iceberg", 1:8, ".png"
)
img_fr <- image_read(frames_fr)
gif_fr <- image_animate(img_fr, delay = 50, loop = 0)  # delay = 50 pour 0.5 sec
image_write(gif_fr,
            path = paste0("_SharedFolder_datagotchi_federal_2024/graph/analyses/landingPage_clusterPotGrowth/cluster_rci_plotFr_withoutLogo_", cluster, ".gif"))

# ---- Créer le GIF anglais ----
frames_en <- paste0(
  "_SharedFolder_datagotchi_federal_2024/graph/analyses/landingPage_clusterPotGrowth/graphWithoutLogo_en/cluster_rci_plotEn_withoutLogo_",
  cluster, "_iceberg", 1:8, ".png"
)
img_en <- image_read(frames_en)
gif_en <- image_animate(img_en, delay = 50, loop = 0)
image_write(gif_en,
            path = paste0("_SharedFolder_datagotchi_federal_2024/graph/analyses/landingPage_clusterPotGrowth/cluster_rci_plotEn_withoutLogo_", cluster, ".gif"))

cat("Cluster", cluster, ": GIF FR et GIF EN créés.\n\n")
}

