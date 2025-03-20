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
  cluster_name = c(1, 2, 3, 4, 5, 6, 7),
  image_path = c("_SharedFolder_datagotchi_federal_2024/images/mario1.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/mario1.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/mario1.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/mario1.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/mario1.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/mario1.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/mario1.png"), 
  description = c("Description de ce cluster:\n Jeune et Urbaine\n Aime le yoga", "Électeurs conservateurs", "Pro-environnement", 
                  "Modérés", "Indécis", "Jeunes urbains", "Traditionnalistes")
)
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


# Créer la description en ggplot ------------------------------------------

plot_text <- ggplot() + 
  annotate("text", x = 1, y = 1, label = df_filtered$description[1], size = 22, fontface = "bold", family = "PixelOperatorSC") +
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

# Graphique

plot_rci <- ggplot(df_filtered, aes(x = party, y = rci)) +
  annotation_custom(
    iceberg_grob,
    xmin = -Inf,
    xmax = Inf,
    ymin = -100,
    ymax = 55
  ) +
  geom_bar(aes(fill = party),
           stat = "identity",
           width = 0.5) + 
  geom_text(aes(label = round(rci, 1),
                y = ifelse(rci >= 0, rci + 15, rci - 15)),
            size = 8,
            color = "black",
            family = "PixelOperatorSC") +
  geom_image(aes(image = image_tete),
             size = 0.08,
             by = "width") +
  geom_hline(yintercept = 0, color = "blue", size = 2) +
  scale_fill_manual(values = party_colors) +
  scale_color_manual(values = party_colors) +
  labs(
    title = paste("Potentiel de croissance par \n parti pour Zoé, la jeune éduquée"),
    x = "Parti politique", 
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
           size = 8,
           family = "PixelOperatorSC") +
  annotate("text",
           x = 0,   
           y = 50,     
           label = "% de solidité du vote",
           angle = 90, 
           hjust = 0.5,
           vjust = -3.5,
           size = 8,
           family = "PixelOperatorSC") +
  annotate("text",
           x = 0,   
           y = 50,     
           label = "% du vote potentiel",
           angle = 90, 
           hjust = 2,
           vjust = -3.5,
           size = 8,
           family = "PixelOperatorSC") +
  clessnize::theme_datagotchi_light(base_size = 30) +
  scale_y_continuous(limits = c(-100, 100)) +
  theme(
    text = element_text(size = 25),
    legend.position = "none"
  ) +
  coord_cartesian(clip = "off")

print(plot_rci)

# Assembler les 3 parties avec cowplot
final_plot <- plot_grid(
  img_grob,  
  plot_text,  
  plot_rci,  
  ncol = 3,  
  rel_widths = c(1, 2, 3)  
)

# Afficher le plot final
print(final_plot)

# Save it ----------------------------------------------------------------

ggsave(
  filename = "_SharedFolder_datagotchi_federal_2024/images/cluster_rci_plot.png",
  width = 25, 
  height = 10, 
  dpi = 300, 
  bg = "white",
  device = "png"
)

