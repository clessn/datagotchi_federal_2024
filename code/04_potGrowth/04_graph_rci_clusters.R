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

# Plot -------------------------------------------------------------------

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
  description = c("Voici la brève \n description du cluster 1", "Électeurs conservateurs", "Pro-environnement", 
                  "Modérés", "Indécis", "Jeunes urbains", "Traditionnalistes")
)
# Charger l'image iceberg et visage
img_iceberg <- readPNG("_SharedFolder_datagotchi_federal_2024/images/icebergPixel.png")
img_tete <- readPNG("_SharedFolder_datagotchi_federal_2024/images/tete.png")

# 2. Créer un rasterGrob
iceberg_grob <- rasterGrob(
  img_iceberg,
  width = unit(1, "npc"),   # occupe toute la largeur du panel
  height = unit(1, "npc"),  # occupe toute la hauteur du panel
  interpolate = TRUE
)

tete_grob <- rasterGrob(
  img_tete,
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

# Vérifier que l'image existe bien
image_path <- df_filtered$image_path[1]
if (!file.exists(image_path)) stop("L'image n'existe pas à ce chemin : ", image_path)

# Charger l'image correspondante
img <- readPNG(image_path)  
img_iceberg_grob <- rasterGrob(img, interpolate = TRUE)  
img_tete_grob <- rasterGrob(img, interpolate = TRUE)

# Créer la description en ggplot
plot_text <- ggplot() + 
  annotate("text", x = 1, y = 1, label = df_filtered$description[1], size = 8, fontface = "bold") +
  theme_void()

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
    xmin = -Inf,   # toute la largeur
    xmax = Inf,
    ymin = -100,   # bas de l'iceberg
    ymax = 55     # haut de l'iceberg
  ) +
  annotate("rect", 
           xmin = -Inf, xmax = Inf, 
           ymin = -100, ymax = 0,
           fill = "lightblue", alpha = 0.3) +
  geom_hline(yintercept = 0, color = "blue", size = 1.2) +
  geom_bar(
    aes(fill = party),
    stat = "identity",
    width = 0.05) + 
  geom_point(size = 3) +
  geom_text(
    aes(
      label = round(rci, 1),
      color = party,
      y = ifelse(rci >= 0, rci + 8, rci - 8)
    ),
    size = 5
  ) +
  scale_fill_manual(values = party_colors) +
  scale_color_manual(values = party_colors) +
  labs(
    title = paste("Potentiel de croissance des partis pour le cluster", selected_cluster),
    x = "Parti politique", 
    y = "RCI"
  ) +
  clessnize::theme_datagotchi_light(base_size = 20)+
  scale_y_continuous(limits = c(-100, 100)) +
  theme(
    text = element_text(size = 14),
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
  filename = "_SharedFolder_datagotchi_federal_2024/images/cluster_rci_plot.pdf",
  width = 20, 
  height = 10, 
  dpi = 300, 
  device = "pdf"  # Force l'enregistrement en PNG
)

