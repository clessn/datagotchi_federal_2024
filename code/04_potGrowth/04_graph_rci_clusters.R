# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(png)
library(grid)
library(cowplot)
library(shadowtext)
library(clessnize)
library(ggimage)

# Load data ---------------------------------------------------------------

df_aggregated_rci <- readRDS("_SharedFolder_datagotchi_federal_2024/data/potGrowth/03_aggregated_rci.rds")

# Plot Preparation  -------------------------------------------------------------------

# Ajouter des images et descriptions factices pour chaque cluster
cluster_info <- data.frame(
  cluster_name = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  image_path = c("_SharedFolder_datagotchi_federal_2024/images/cluster_1_Zoé.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/mario1.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/mario1.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/mario1.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/mario1.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/mario1.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/mario1.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/mario1.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/mario1.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/mario1.png"), 
  descriptionFr = c("Description de ce cluster:\n Jeune et Urbaine\n Aime le yoga",
                    "Électeurs conservateurs",
                    "Pro-environnement",
                    "Modérés",
                    "Indécis",
                    "Jeunes urbains",
                    "Jeunes urbains",
                    "Jeunes urbains",
                    "Jeunes urbains",
                    "Traditionnalistes"),
descriptionEn = c("Cluster's description:\n Young, Urban\n Loves yoga",
                  "Électeurs conservateurs",
                  "Pro-environnement", 
                  "Modérés",
                  "Indécis",
                  "Jeunes urbains",
                  "Jeunes urbains",
                  "Jeunes urbains",
                  "Jeunes urbains",
                  "Traditionnalistes"))

# Charger l'image iceberg
img_iceberg <- readPNG("_SharedFolder_datagotchi_federal_2024/images/icebergPixel.png")

# Créer un rasterGrob
iceberg_grob <- rasterGrob(
  img_iceberg,
  width = unit(1, "npc"),   # occupe toute la largeur du panel
  height = unit(1, "npc"),  # occupe toute la hauteur du panel
  interpolate = TRUE
)

# Fusionner les informations avec les RCI
df_aggregated_rci <- df_aggregated_rci %>%
  mutate(cluster_name = as.numeric(cluster_name))

df_plot <- df_aggregated_rci %>%
  left_join(cluster_info, by = "cluster_name")

# Sélectionner un cluster à afficher
selected_cluster <- 1  # Change ce nombre pour afficher un autre cluster

df_filtered <- df_plot %>%
  filter(cluster_name == selected_cluster)

# Ajouter une colonne pour l'image tête (pour toutes les observations)
df_filtered$image_tete <- "_SharedFolder_datagotchi_federal_2024/images/Cluster_1_Zoé_Head.png"

# Vérifier que l'image iceberg existe bien
 image_path <- df_filtered$image_path[1]
 if (!file.exists(image_path)) stop("L'image n'existe pas à ce chemin : ", image_path)

# Charger l'image correspondante
img <- readPNG(image_path)  
img_grob <- rasterGrob(img, interpolate = TRUE)  

image_plot <- ggdraw() +
  draw_grob(
    img_grob, 
    x = 0.15,        # marge à gauche (0 = bord gauche, 1 = bord droit)
    y = 0.05,        # marge en bas (0 = bord bas, 1 = bord haut)
    width = 0.9,
    height = 0.9 
  )


# Créer la description fr en ggplot ------------------------------------------

plot_textFr <- ggplot() + 
  annotate("text", 
           x = 1, 
           y = 1, 
           label = df_filtered$descriptionFr[1], 
           size = 22, 
           fontface = "bold", 
           family = "PixelOperatorSC",
           lineheight = 0.4
           ) +
  theme_void() 

# Créer la description fr en ggplot ------------------------------------------

plot_textEn <- ggplot() + 
  annotate("text", 
           x = 1, 
           y = 1, 
           label = df_filtered$descriptionEn[1], 
           size = 22, 
           fontface = "bold", 
           family = "PixelOperatorSC",
           lineheight = 0.4
  ) +
  theme_void() 

# Créer le graph potGrowth ------------------------------------------------

# Définir les couleurs spécifiques par parti
party_colors <- c(
  "LPC" = "#D71B1E",
  "CPC" = "#142E52",
  "NDP" = "#F58220",
  "BQ"  = "#080236",
  "GPC" = "#3D9B35",
  "PPC" = "#442D7B"
)

# Graphique Fr

plot_rciFr <- ggplot(df_filtered, aes(x = party, y = rci)) +
  annotation_custom(
    iceberg_grob,
    xmin = -Inf,
    xmax = Inf,
    ymin = -100,
    ymax = 55
  ) +
  geom_bar(aes(fill = party),
           stat = "identity",
           width = 0.35) + 
  geom_text(aes(label = round(rci, 0),
                y = ifelse(rci >= 0, rci + 15, rci - 15)),
            size = 22,
            color = "black",
            family = "PixelOperatorSC") +
  geom_image(aes(image = image_tete),
             size = 0.08,
             by = "width") +
  geom_hline(yintercept = 0, color = "#040280", size = 2) +
  scale_fill_manual(values = party_colors) +
  scale_color_manual(values = party_colors) +
  labs(
    title = paste("Potentiel de croissance par\nparti pour Zoé, la jeune éduquée"),
    x = NULL, 
    y = NULL
  ) +
  annotate("rect", 
           xmin = -Inf, xmax = Inf, 
           ymin = -100, ymax = 0,
           fill = "lightblue", alpha = 0.3) +
  annotate("text",
           x = 0,
           y = 0, 
           label = "Seuil de vote",
           hjust = 1.3,       
           vjust = 0.5,
           angle = 0,
           size = 20,
           family = "PixelOperatorSC") +
  annotate("text",
           x = 0,   
           y = 50,     
           label = "Solidité du vote",
           angle = 90, 
           hjust = 0.3,
           vjust = -3.5,
           size = 20,
           family = "PixelOperatorSC") +
  annotate("text",
           x = 0,   
           y = 50,     
           label = "Vote potentiel",
           angle = 90, 
           hjust = 3,
           vjust = -3.5,
           size = 20,
           family = "PixelOperatorSC") +
  clessnize::theme_datagotchi_light(base_size = 60) +
  scale_y_continuous(limits = c(-100, 100)) +
  theme(
    text = element_text(size = 70),
    legend.position = "none",
    plot.title = element_text(lineheight = 0.4)
  ) +
  coord_cartesian(clip = "off")

print(plot_rciFr)

# Graphique En

plot_rciEn <- ggplot(df_filtered, aes(x = party, y = rci)) +
  annotation_custom(
    iceberg_grob,
    xmin = -Inf,
    xmax = Inf,
    ymin = -100,
    ymax = 55
  ) +
  geom_bar(aes(fill = party),
           stat = "identity",
           width = 0.35) + 
  geom_text(aes(label = round(rci, 0),
                y = ifelse(rci >= 0, rci + 15, rci - 15)),
            size = 22,
            color = "black",
            family = "PixelOperatorSC") +
  geom_image(aes(image = image_tete),
             size = 0.08,
             by = "width") +
  geom_hline(yintercept = 0, color = "#040280", size = 2) +
  scale_fill_manual(values = party_colors) +
  scale_color_manual(values = party_colors) +
  labs(
    title = paste("Potential for Growth per\npolitical party for Zoé"),
    x = NULL, 
    y = NULL
  ) +
  annotate("rect", 
           xmin = -Inf, xmax = Inf, 
           ymin = -100, ymax = 0,
           fill = "lightblue", alpha = 0.3) +
  annotate("text",
           x = 0,
           y = 0, 
           label = "Seuil de vote",
           hjust = 1.3,       
           vjust = 0.5,
           angle = 0,
           size = 20,
           family = "PixelOperatorSC") +
  annotate("text",
           x = 0,   
           y = 50,     
           label = "Solidité du vote",
           angle = 90, 
           hjust = 0.3,
           vjust = -3.5,
           size = 20,
           family = "PixelOperatorSC") +
  annotate("text",
           x = 0,   
           y = 50,     
           label = "Vote potentiel",
           angle = 90, 
           hjust = 3,
           vjust = -3.5,
           size = 20,
           family = "PixelOperatorSC") +
  clessnize::theme_datagotchi_light(base_size = 60) +
  scale_y_continuous(limits = c(-100, 100)) +
  theme(
    text = element_text(size = 70),
    legend.position = "none",
    plot.title = element_text(lineheight = 0.4)
  ) +
  coord_cartesian(clip = "off")

print(plot_rciEn)

# Assembler les 3 parties avec cowplot fr
final_plotFr <- plot_grid(
  image_plot,  
  plot_textFr,  
  plot_rciFr,  
  ncol = 3,  
  rel_widths = c(0.8, 2, 3)  
)

# Afficher le plot final
print(final_plotFr)

# Save it Fr ----------------------------------------------------------------

ggsave(
  filename = "_SharedFolder_datagotchi_federal_2024/graph/analyses/landingPage_clusterPotGrowth/cluster_rci_plotFr.png",
  width = 20, 
  height = 10, 
  dpi = 300, 
  bg = "white",
  device = "png"
)

# Assembler les 3 parties avec cowplot En
final_plotEn <- plot_grid(
  image_plot,  
  plot_textEn,  
  plot_rciEn,  
  ncol = 3,  
  rel_widths = c(0.8, 2, 3)  
)

# Afficher le plot final
print(final_plotEn)

# Save it En ----------------------------------------------------------------

ggsave(
  filename = "_SharedFolder_datagotchi_federal_2024/graph/analyses/landingPage_clusterPotGrowth/cluster_rci_plotEn.png",
  width = 20, 
  height = 10, 
  dpi = 300, 
  bg = "white",
  device = "png"
)

