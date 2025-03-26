# Packages ----------------------------------------------------------------
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

# Load data ---------------------------------------------------------------

df_aggregated_rci <- readRDS("_SharedFolder_datagotchi_federal_2024/data/potGrowth/03_aggregated_rci.rds")

# Plot Preparation  -------------------------------------------------------------------

# Ajouter des images et descriptions factices pour chaque cluster
cluster_info <- data.frame(
  cluster_name = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  image_path = c("_SharedFolder_datagotchi_federal_2024/images/Cluster_Datagotchi__0001_Jennifer2000.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/Cluster_Datagotchi__0001_Jennifer2000.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/Cluster_Datagotchi__0001_Jennifer2000.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/Cluster_Datagotchi__0001_Jennifer2000.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/Cluster_Datagotchi__0001_Jennifer2000.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/Cluster_Datagotchi__0001_Jennifer2000.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/Cluster_Datagotchi__0001_Jennifer2000.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/Cluster_Datagotchi__0001_Jennifer2000.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/Cluster_Datagotchi__0001_Jennifer2000.png", 
                 "_SharedFolder_datagotchi_federal_2024/images/Cluster_Datagotchi__0001_Jennifer2000.png"), 
  descriptionFr = c("Description de ce cluster:\n Jennifer est une Ontarienne\n urbaine de 50 ans\n faisant partie de la classe moyenne.\n Elle habite un condo,\n va au gym et prend\n son café au Starbucks.\n Elle se déplace en transport en commun.\n Elle aime le vin rouge\n et fréquente les musées.",
                    "Description de ce cluster:\n Arjun est un immigrant Ontarien ou de l'Ouest de 45 ans. Il accompli souvent des tâches manuelles et aime les sports motorisés.",
                    "Description de ce cluster:\n Michel est une Québécois francophone\n de 60 ans qui habite en région.\n Il est retraité.\n Il habite son duplex et\n aime faire des voyages à saveur historique.\n Il ne consulte pas beaucoup\n les réseaux sociaux. Il aime le vin\n et ne boit pas beaucoup de café.\n Il principalement une personne de matin.",
                    "Description de ce cluster:\n David est un Ontarien éduqué et aisé\n de 50 ans qui habite dans une\n maison de ville. Il occupe un emploi professionnel.\n Il a un chien, va au gym,\n est prend son café au Tim Hortons. Il aime pêcher.",
                    "Description de ce cluster:\n John est un albertain de 40 ans,\n qui occupe un emploi manuel et\n qui fait partie de la classe moyenne.\n Il fait des sports motorisés, chasse et pêche.\n Il mange de la viande à tous\n les repas ou presque. Il prend\n son café dans les chaines (surtout au Tim,\n mais McDonald's ça lui plait aussi).\n Il aime les boissons spiritueuses.",
                    "Description de ce cluster:\n Maxime est un québécois de 35 ans.\n Il vit dans son appartement, fait du plein air\n et des sports motorisés.\n Il chasse et pêche à l'occasion.\n Il boit de la bière.",
                    "Description de ce cluster:\n Zoe est une femme de 35 ans\n vivant dans un milieu urbain.\n Elle habite son appartement\n et occupe un emploi de professionnel ou de management.\n Elle se déplace à vélo et est tatouée.\n Elle consulte beaucoup instagram.\n Elle a un chat et un chien.\n Elle prend son café au Starbucks\n et fait de la course à pieds.",
                    "Description de ce cluster:\n Julie est une québécoise de 50 ans\n peu éduquée avec peu de moyens.\n Elle habite son bloc appartement avec son chat\n et prend des marches.\n Elle ne boit pas de café et aime le vin.\n Elle est tatouée.",
                    "Description de ce cluster:\n Robert est un homme des provinces atlantiques de 55 ans.\n Il habite sa maison et occupe un emploi manuel.\n Il pêche beaucoup, chasse à l'occasion.\n Il aime la bière et les boissons spiritueuses.\n Il prend son café au Tim Hortons.\n Il accompli souvent des tâches manuelles.",
                    "Description de ce cluster:\n Emily est éduquée et vit dans un appartement ou un condo avec son chat. Elle est végétarienne. Elle fait du yoga et fréquente les cafés indépendants. Elle s'habille dans les friperies. Elle aime les cocktails et fume du pot à l'occasion."),
descriptionEn = c("Cluster's description:\n Jennifer is a 50-year-old\n urban Ontarian who is part\n of the middle class.\n She lives in a condo,\n goes to the gym,\n gets her coffee at Starbucks.\n She uses public transit.\n She enjoys red wine\n and visits museums.",
                  "Cluster's description:\n Électeurs conservateurs",
                  "Cluster's description:\n Pro-environnement", 
                  "Cluster's description:\n Modérés",
                  "Cluster's description:\n Indécis",
                  "Cluster's description:\n Jeunes urbains",
                  "Cluster's description:\n Jeunes urbains",
                  "Cluster's description:\n Jeunes urbains",
                  "Cluster's description:\n Jeunes urbains",
                  "Cluster's description:\n Traditionnalistes"))

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
df_filtered$image_tete <- "_SharedFolder_datagotchi_federal_2024/images/Cluster_Datagotchi__0000_Jennifer2000.png"

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

# Graphique Fr sans logo --------------------------------------------------

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

# Assembler les 3 parties avec cowplot fr
final_plotFr <- plot_grid(
  image_plot,  
  plot_textFr,  
  plot_rciFr,  
  ncol = 3,  
  rel_widths = c(0.8, 2, 3)  
)

# Save graph without logo Fr ----------------------------------------------------------------

ggsave(
  filename = "_SharedFolder_datagotchi_federal_2024/graph/analyses/landingPage_clusterPotGrowth/cluster_rci_plotFr_withoutLogo.png",
  width = 20, 
  height = 10, 
  dpi = 300, 
  bg = "white",
  device = "png"
)

# Graphique En sans logo --------------------------------------------------

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
           label = "Voting threshold",
           hjust = 1.3,       
           vjust = 0.5,
           angle = 0,
           size = 20,
           family = "PixelOperatorSC") +
  annotate("text",
           x = 0,   
           y = 50,     
           label = "Vote certainty",
           angle = 90, 
           hjust = 0.3,
           vjust = -3.5,
           size = 20,
           family = "PixelOperatorSC") +
  annotate("text",
           x = 0,   
           y = 50,     
           label = "Potential vote",
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

# Save graph without logo En ----------------------------------------------------------------

ggsave(
  filename = "_SharedFolder_datagotchi_federal_2024/graph/analyses/landingPage_clusterPotGrowth/cluster_rci_plotEn_withoutLogo.png",
  width = 20, 
  height = 10, 
  dpi = 300, 
  bg = "white",
  device = "png"
)

# Add logo Fr graph ----------------------------------------------------------------

# Ajouter logo
logo_image <- readPNG("_SharedFolder_datagotchi_federal_2024/logos/FR/logo_black.png")
logo_grob <- rasterGrob(logo_image, interpolate = TRUE)

# 2. Use a blank ggdraw(), then draw_plot() + draw_grob()
final_plot_with_logo <- ggdraw() +
  draw_plot(final_plotFr, x = 0, y = 0.05, width = 1, height = 0.95) +
  # Then draw the logo on top, anchored to bottom-right
  draw_grob(
    logo_grob,
    x = 0.98,               
    y = -0.45,               
    hjust = 1,               
    vjust = 0,               
    width = 0.10            
  )

print(final_plot_with_logo)

ggsave(
  filename = "_SharedFolder_datagotchi_federal_2024/graph/analyses/landingPage_clusterPotGrowth/cluster_rci_plotFr_withLogo.png",
  width = 20, 
  height = 10, 
  dpi = 300, 
  bg = "white",
  device = "png"
)

# Add logo En graph ----------------------------------------------------------------

# Use a blank ggdraw(), then draw_plot() + draw_grob()
final_plot_withLogo_En <- ggdraw() +
  draw_plot(final_plotEn, x = 0, y = 0.05, width = 1, height = 0.95) +
  # Then draw the logo on top, anchored to bottom-right
  draw_grob(
    logo_grob,
    x = 0.98,               
    y = -0.45,               
    hjust = 1,               
    vjust = 0,               
    width = 0.10            
  )

print(final_plot_withLogo_En)

ggsave(
  filename = "_SharedFolder_datagotchi_federal_2024/graph/analyses/landingPage_clusterPotGrowth/cluster_rci_plotEn_withLogo.png",
  width = 20, 
  height = 10, 
  dpi = 300, 
  bg = "white",
  device = "png"
)

