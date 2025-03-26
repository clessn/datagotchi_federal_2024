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

# Charger l'image iceberg et créer un grob
img_iceberg <- readPNG("_SharedFolder_datagotchi_federal_2024/images/landingPage_potgrowth/icebergPixel.png")
iceberg_grob <- rasterGrob(
  img_iceberg,
  width = unit(1, "npc"),
  height = unit(1, "npc"),
  interpolate = TRUE
)

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

# Définir le texte de méthodologie
methodologyTextFr <- "Méthodologie: Pour calculer le RCI, les répondants évaluent sur 10 leur probabilité de voter pour chaque parti.\n Un score relatif est ensuite calculé : le parti préféré reçoit un score positif (écart avec le 2e),\n les autres un score négatif (écart avec le 1er).\n Ce score permet de mesurer la solidité du vote et le potentiel de croissance des partis dans chaque segment électoral.\n Pour élaborer les clusters, nous avons utilisé la méthode k-means sur les données de notre sondage pilote (n = 1021)."
methodologyTextEn <- "Methodology: To calculate the RCI, respondents rate their likelihood of voting for each party on a scale of 10.\n A relative score is then calculated: the preferred party receives a positive score (the difference with the second),\n while the others receive a negative score (the difference with the first).\n This score measures the solidity of the vote and the growth potential of parties in each electoral segment.\n To develop the clusters, we used the k-means method on the data from our pilot survey (n = 1021)."

# ------------------------- Boucle sur les clusters -------------------------
for(cluster in unique(df_plot$cluster_name)) {
  
  # Filtrer les données pour le cluster courant
  df_filtered <- df_plot %>% filter(cluster_name == cluster)
  
  # Créer le plot de méthodologie (texte)
  plot_methodologyFr <- ggplot() +
    annotate("text", x = 0.5, y = 0.5,
             label = methodologyTextFr,
             size = 18, family = "PixelOperatorSC",
             lineheight = 0.3,
             hjust = 0.5, vjust = 0.5) +
    theme_void()
  
  plot_methodologyEn <- ggplot() +
    annotate("text", x = 0.5, y = 0.5,
             label = methodologyTextEn,
             size = 18, family = "PixelOperatorSC",
             lineheight = 0.3,
             hjust = 0.5, vjust = 0.5) +
    theme_void()
  
  # -------------------- Graphique RCI en français --------------------
  plot_rciFr <- ggplot(df_filtered, aes(x = party, y = rci)) +
    annotation_custom(iceberg_grob, xmin = -Inf, xmax = Inf, ymin = -100, ymax = 55) +
    geom_bar(aes(fill = party), stat = "identity", width = 0.35) + 
    geom_text(aes(label = round(rci, 0),
                  y = ifelse(rci >= 0, rci + 15, rci - 15)),
              size = 22, color = "black", family = "PixelOperatorSC") +
    geom_image(aes(image = image_tete), size = 0.08, by = "width") +
    geom_hline(yintercept = 0, color = "#040280", size = 2) +
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
      x = NULL, y = NULL
    ) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -100, ymax = 0,
             fill = "lightblue", alpha = 0.3) +
    annotate("text", x = 0, y = 0, label = "Seuil",
             hjust = 1.6, vjust = 0.5, angle = 0, size = 20, family = "PixelOperatorSC") +
    annotate("text", x = 0, y = 0, label = "de vote",
             hjust = 1.3, vjust = 3.2, angle = 0, size = 20, family = "PixelOperatorSC") +
    annotate("text", x = 0, y = 50, label = "Solidité du vote",
             angle = 90, hjust = 0.3, vjust = -3.5, size = 20, family = "PixelOperatorSC") +
    annotate("text", x = 0, y = 50, label = "Vote potentiel",
             angle = 90, hjust = 3, vjust = -3.5, size = 20, family = "PixelOperatorSC") +
    clessnize::theme_datagotchi_light(base_size = 60) +
    scale_y_continuous(limits = c(-100, 100)) +
    theme(text = element_text(size = 70),
          legend.position = "none",
          plot.title = element_text(lineheight = 0.2)) +
    coord_cartesian(clip = "off")
  
  # -------------------- Graphique RCI en anglais --------------------
  plot_rciEn <- ggplot(df_filtered, aes(x = party, y = rci)) +
    annotation_custom(iceberg_grob, xmin = -Inf, xmax = Inf, ymin = -100, ymax = 55) +
    geom_bar(aes(fill = party), stat = "identity", width = 0.35) + 
    geom_text(aes(label = round(rci, 0),
                  y = ifelse(rci >= 0, rci + 15, rci - 15)),
              size = 22, color = "black", family = "PixelOperatorSC") +
    geom_image(aes(image = image_tete), size = 0.08, by = "width") +
    geom_hline(yintercept = 0, color = "#040280", size = 2) +
    scale_fill_manual(values = party_colors) +
    scale_color_manual(values = party_colors) +
    labs(
      title = paste0("Potential for Growth per\npolitical party for ", df_filtered$cluster_label[1]),
      x = NULL, y = NULL
    ) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -100, ymax = 0,
             fill = "lightblue", alpha = 0.3) +
    annotate("text", x = 0, y = 0, label = "Voting",
             hjust = 1.6, vjust = 0.5, angle = 0, size = 20, family = "PixelOperatorSC",
             lineheight = 0.3) +
    annotate("text", x = 0, y = 0, label = "Threshold",
             hjust = 1.3, vjust = 3.5, angle = 0, size = 20, family = "PixelOperatorSC",
             lineheight = 0.3) +
    annotate("text", x = 0, y = 50, label = "Vote certainty",
             angle = 90, hjust = 0.3, vjust = -3.5, size = 20, family = "PixelOperatorSC") +
    annotate("text", x = 0, y = 50, label = "Potential vote",
             angle = 90, hjust = 3, vjust = -3.5, size = 20, family = "PixelOperatorSC") +
    clessnize::theme_datagotchi_light(base_size = 60) +
    scale_y_continuous(limits = c(-100, 100)) +
    theme(text = element_text(size = 70),
          legend.position = "none",
          plot.title = element_text(lineheight = 0.2)) +
    coord_cartesian(clip = "off")
  
  # 2) Superposer la méthodologie en bas à gauche
  final_plotFr <- ggdraw(plot_rciFr) +
    draw_plot(
      plot_methodologyFr,
      x = 0.005,  # décalage horizontal (plus c'est petit, plus c'est à gauche)
      y = -0.12,  # décalage vertical (plus c'est petit, plus c'est bas)
      width = 0.6, height = 0.40  # ajustez la taille de la boîte de méthodologie
    )
  
  final_plotEn <- ggdraw(plot_rciEn) +
    draw_plot(
      plot_methodologyEn,
      x = 0.005, y = -0.12,
      width = 0.6, height = 0.40
    )
  
  # -------------------- Sauvegardes (sans logo) --------------------
  ggsave(
    filename = paste0("_SharedFolder_datagotchi_federal_2024/graph/analyses/landingPage_clusterPotGrowth/cluster_rci_plotFr_withoutLogo_", cluster, ".png"),
    plot = final_plotFr,
    width = 10, height = 10, dpi = 300, bg = "white", device = "png"
  )
  ggsave(
    filename = paste0("_SharedFolder_datagotchi_federal_2024/graph/analyses/landingPage_clusterPotGrowth/cluster_rci_plotEn_withoutLogo_", cluster, ".png"),
    plot = final_plotEn,
    width = 10, height = 10, dpi = 300, bg = "white", device = "png"
  )
  
  # -------------------- Ajout du logo (FR) --------------------
  final_plot_with_logo_Fr <- ggdraw(clip = "off") +
    draw_plot(final_plotFr, x = 0, y = 0.05, width = 1, height = 0.95) +
    draw_grob(logo_grob, x = 0.98, y = -0.45, hjust = 1, vjust = 0, width = 0.10)
  
  ggsave(
    filename = paste0("_SharedFolder_datagotchi_federal_2024/graph/analyses/landingPage_clusterPotGrowth/cluster_rci_plotFr_withLogo_", cluster, ".png"),
    plot = final_plot_with_logo_Fr,
    width = 10, height = 10, dpi = 300, bg = "white", device = "png"
  )
  
  # -------------------- Ajout du logo (EN) --------------------
  final_plot_with_logo_En <- ggdraw(clip = "off") +
    draw_plot(final_plotEn, x = 0, y = 0.05, width = 1, height = 0.95) +
    draw_grob(logo_grob, x = 0.98, y = -0.45, hjust = 1, vjust = 0, width = 0.10)
  
  ggsave(
    filename = paste0("_SharedFolder_datagotchi_federal_2024/graph/analyses/landingPage_clusterPotGrowth/cluster_rci_plotEn_withLogo_", cluster, ".png"),
    plot = final_plot_with_logo_En,
    width = 10, height = 10, dpi = 300, bg = "white", device = "png"
  )
  
  cat("Cluster", cluster, "traité et sauvegardé.\n")
}

