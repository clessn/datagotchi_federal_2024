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

# ------------------------- Préparation des données -------------------------
df_aggregated_rci <- readRDS("_SharedFolder_datagotchi_federal_2024/data/potGrowth/03_aggregated_rci.rds")

cluster_info <- data.frame(
  cluster_name = c("Maxime", "Michel", "David", "Robert", "Emily",
                   "Jennifer", "John", "Julie", "Arjun", "Zoe"),
  cluster_nameFR = c("Maxime, le milénial naturel",
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
  image_tete = c("_SharedFolder_datagotchi_federal_2024/images/landingPage_potgrowth/All_Clusters_Persona/x1/Clusters-x1-_0000s_0018_Maxime.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/landingPage_potgrowth/All_Clusters_Persona/x1/Clusters-x1-_0000s_0007_Michel.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/landingPage_potgrowth/All_Clusters_Persona/x1/Clusters-x1-_0000s_0010_David2.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/landingPage_potgrowth/All_Clusters_Persona/x1/Clusters-x1-_0000s_0030_Robert.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/landingPage_potgrowth/All_Clusters_Persona/x1/Clusters-x1-_0000s_0034_10--Emilie.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/landingPage_potgrowth/All_Clusters_Persona/x1/Clusters-x1-_0000s_0001_Jen-Layers.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/landingPage_potgrowth/All_Clusters_Persona/x1/Clusters-x1-_0000s_0014_John-Layers.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/landingPage_potgrowth/All_Clusters_Persona/x1/Clusters-x1-_0000s_0026_Julie.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/landingPage_potgrowth/All_Clusters_Persona/x1/Clusters-x1-_0000s_0004_Arjun.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/landingPage_potgrowth/All_Clusters_Persona/x1/Clusters-x1-_0000s_0022_Zoe.png")
)

df_plot <- df_aggregated_rci %>%
  left_join(cluster_info, by = "cluster_name")

party_colors <- c(
  "LPC" = "#D71B1E",
  "CPC" = "#142E52",
  "NDP" = "#F58220",
  "BQ"  = "#080236",
  "GPC" = "#3D9B35",
  "PPC" = "#442D7B"
)

logo_image <- readPNG("_SharedFolder_datagotchi_federal_2024/logos/FR/logo_black.png")
logo_grob <- rasterGrob(logo_image, interpolate = TRUE)

# ------------------------- Boucle principale sur les clusters -------------------------
for(cluster in unique(df_plot$cluster_name)) {
  
  # 1) Filtrer les données pour le cluster courant
  df_filtered <- df_plot %>% filter(cluster_name == cluster)
  
  # 2) Générer les 8 images (fr et en) pour ce cluster
  for(i in 1:8) {
    
    # Charger l'image iceberg
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
        title = paste0("Potentiel de croissance par\nparti pour ", df_filtered$cluster_nameFR[1]),
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
      scale_y_continuous(limits = c(-100, 100)) +
      theme(text = element_text(size = 70),
            legend.position = "none",
            plot.title = element_text(lineheight = 0.2),
            plot.caption = element_text(lineheight = 0.2, size = 40),
            plot.caption.position = "plot",
            plot.margin = margin(t = 10, r = 10, b = 10, l = 20)
      )
    
    plot_finalFr <- ggdraw() +
      draw_plot(plot_rciFr, 
                x = -0.10,        
                y = 0.08,         
                width = 1.1,      
                height = 0.82) +  
      draw_label(
        "Source : Léger-Datagotchi 2025 | Méthodologie: Pour calculer le RCI, les répondants évaluent sur 10 leur\nprobabilité de voter pour chaque parti. Un score relatif est ensuite calculé : le parti préféré reçoit un\nscore positif (écart avec le 2e), les autres un score négatif (écart avec le 1er). Ce score permet de mesurer\nla solidité du vote et le potentiel de croissance des partis dans chaque segment électoral. Pour élaborer\nles clusters, nous avons utilisé la méthode k-means sur les données de notre sondage pilote (n = 1021).",
        x = 0.10, y = 0.03, 
        hjust = 0, vjust = 0,
        fontfamily = "PixelOperatorSC",
        size = 36,
        lineheight = 0.3,
        color = "black"
      ) +
      draw_grob(logo_grob, x = 0.97, y = -0.46, hjust = 1, vjust = 0, width = 0.12)
    
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
        title = paste0("Potential for Growth per\npolitical party for ", df_filtered$cluster_nameEN[1]),
        x = NULL, y = NULL
      ) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = -100, ymax = 0,
               fill = "#1bb8d3", alpha = 0.3) +
      annotate("text", x = 0, y = 0, label = "Voting\nThreshold",
               hjust = 0.5, vjust = -1, angle = 90, size = 20, family = "PixelOperatorSC", lineheight = 0.2) +
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
    
    plot_finalEn <- ggdraw() +
      draw_plot(plot_rciEn, 
                x = -0.10,        
                y = 0.08,         
                width = 1.1,      
                height = 0.82) +  
      draw_label(
        "Source : Léger-Datagotchi 2025 | Methodology: To calculate the RCI, respondents rate their likelihood of voting for\neach party on a scale of 10.A relative score is then calculated: the preferred party receives a positive score (the\ndifference with the second), while the others receive a negative score (the difference with the first). This score\nmeasures the solidity of the vote and the growth potential of parties in each electoral segment. To develop\nthe clusters, we used the k-means method on the data from our pilot survey (n = 1021).",
        x = 0.10, y = 0.03, 
        hjust = 0, vjust = 0,
        fontfamily = "PixelOperatorSC",
        size = 36,
        lineheight = 0.3,
        color = "black"
      ) +
      draw_grob(logo_grob, x = 0.97, y = -0.46, hjust = 1, vjust = 0, width = 0.12)
    
    # 3) Sauvegarder chaque image
    ggsave(
      filename = paste0("_SharedFolder_datagotchi_federal_2024/graph/analyses/landingPage_clusterPotGrowth/graphWithLogo_fr/cluster_rci_plotFr_withLogo_",
                        cluster, "_iceberg", i, ".png"),
      plot = plot_finalFr,
      width = 10, height = 10, dpi = 300, bg = "white", device = "png"
    )
    
    ggsave(
      filename = paste0("_SharedFolder_datagotchi_federal_2024/graph/analyses/landingPage_clusterPotGrowth/graphWithLogo_en/cluster_rci_plotEn_withLogo_",
                        cluster, "_iceberg", i, ".png"),
      plot = plot_finalEn,
      width = 10, height = 10, dpi = 300, bg = "white", device = "png"
    )
    
  } # Fin de la boucle i=1..8 pour générer les images
  
  cat("Cluster", cluster, ": 8 images FR et EN générées.\n")
  
  # 4) Créer le GIF français pour ce cluster
  frames_fr <- paste0(
    "_SharedFolder_datagotchi_federal_2024/graph/analyses/landingPage_clusterPotGrowth/graphWithLogo_fr/cluster_rci_plotFr_withLogo_",
    cluster, "_iceberg", 1:8, ".png"
  )
  gifski(
    png_files = frames_fr,
    gif_file  = paste0("_SharedFolder_datagotchi_federal_2024/graph/analyses/landingPage_clusterPotGrowth/cluster_rci_plotFr_withLogo_", cluster, ".gif"),
    delay = 0.5,
    loop = TRUE
  )
  
  # 5) Créer le GIF anglais pour ce cluster
  frames_en <- paste0(
    "_SharedFolder_datagotchi_federal_2024/graph/analyses/landingPage_clusterPotGrowth/graphWithLogo_en/cluster_rci_plotEn_withLogo_",
    cluster, "_iceberg", 1:8, ".png"
  )
  gifski(
    png_files = frames_en,
    gif_file  = paste0("_SharedFolder_datagotchi_federal_2024/graph/analyses/landingPage_clusterPotGrowth/cluster_rci_plotEn_withLogo_", cluster, ".gif"),
    delay = 0.5,
    loop = TRUE
  )
  
  cat("Cluster", cluster, ": GIF FR et GIF EN créés.\n\n")
} # Fin de la boucle sur les clusters

