# Packages ----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(png)
library(grid)
library(cowplot)

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
img_grob <- rasterGrob(img, interpolate = TRUE)  

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

# Créer le graphique pour ce cluster
plot_rci <- ggplot(df_filtered, aes(x = party, y = rci, label = round(rci, 1), color = party)) +
  geom_hline(yintercept = 0, color = "black", size = 1.2) +  # Ligne de référence
  geom_point(size = 6) +  # Points colorés pour le RCI
  geom_text(vjust = -1, size = 5) +  # Valeurs du RCI sur les points
  scale_color_manual(values = party_colors) +  # Appliquer les couleurs spécifiques
  labs(title = paste("Potentiel de croissance des partis pour le cluster", selected_cluster),
       x = "Parti politique", y = "RCI") +
  theme_minimal() +
  scale_y_continuous(limits = c(-100, 100)) +
  theme(
    text = element_text(size = 14),
    legend.position = "none"
  )

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
  plot = final_plot,  # Spécifie bien le plot
  width = 20, 
  height = 10, 
  dpi = 300, 
  device = "pdf"  # Force l'enregistrement en PNG
)

