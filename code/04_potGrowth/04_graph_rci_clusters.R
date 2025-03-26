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
  descriptionFr = c("Description de ce cluster:\nJennifer est une Ontarienne\nurbaine de 50 ans\nfaisant partie de la classe moyenne.\nElle habite un condo,\nva au gym et prend\nson café au Starbucks.\nElle se déplace en transport en commun.\nElle aime le vin rouge\net fréquente les musées.",
    "Description de ce cluster:\nArjun est un immigrant Ontarien\nou de l'Ouest de 45 ans.\nIl accomplit souvent des tâches manuelles\net aime les sports motorisés.",
    "Description de ce cluster:\nMichel est un Québécois francophone\nde 60 ans qui habite en région.\nIl est retraité, habite un duplex\net aime les voyages historiques.\nIl consulte peu les réseaux sociaux.",
    "Description de ce cluster:\nDavid est un Ontarien éduqué et aisé\nde 50 ans qui habite dans une maison de ville.\nIl occupe un emploi professionnel,\na un chien, va au gym\net prend son café au Tim Hortons.\nIl aime pêcher.",
    "Description de ce cluster:\nJohn est un Albertain de 40 ans,\nqui occupe un emploi manuel\net fait partie de la classe moyenne.\nIl pratique des sports motorisés,\nla chasse et la pêche.\nIl mange de la viande presque à chaque repas\net prend son café dans des chaînes.",
    "Description de ce cluster:\nMaxime est un Québécois de 35 ans.\nIl vit dans un appartement,\nfait du plein air et des sports motorisés.\nIl chasse et pêche occasionnellement,\net boit de la bière.",
    "Description de ce cluster:\nZoe est une femme de 35 ans\nvivant en milieu urbain.\nElle habite en appartement et occupe un emploi professionnel ou de management.\nElle se déplace à vélo, est tatouée,\nconsulte beaucoup Instagram,\na un chat et un chien,\nprend son café au Starbucks\net fait de la course à pied.",
    "Description de ce cluster:\nJulie est une Québécoise de 50 ans\npeu éduquée et avec peu de moyens.\nElle habite dans un bloc avec son chat,\nprend des marches,\nne boit pas de café et aime le vin.\nElle est tatouée.",
    "Description de ce cluster:\nRobert est un homme des provinces atlantiques de 55 ans.\nIl habite sa maison et occupe un emploi manuel.\nIl pêche beaucoup, chasse à l'occasion,\naime la bière et les boissons spiritueuses,\net prend son café au Tim Hortons.",
    "Description de ce cluster:\nEmily est éduquée et vit dans un appartement ou condo avec son chat.\nElle est végétarienne, fait du yoga,\nfréquente les cafés indépendants,\ns'habille en friperie,\naime les cocktails et fume occasionnellement."),
  descriptionEn  = c(
    "Cluster's description:\nJennifer is a 50-year-old urban Ontarian\nwho is part of the middle class.\nShe lives in a condo,\ngoes to the gym,\ngets her coffee at Starbucks,\nuses public transit,\nenjoys red wine\nand visits museums.",
    "Cluster's description:\nArjun is an Ontarian immigrant\nor from the West, aged 45.\nHe often performs manual tasks\nand enjoys motor sports.",
    "Cluster's description:\nMichel is a French-speaking Quebecer\naged 60 living in a rural area.\nHe is retired, lives in a duplex,\nand enjoys historically flavored trips.\nHe rarely uses social media.",
    "Cluster's description:\nDavid is a well-educated, affluent Ontarian\naged 50 living in a townhouse.\nHe works in a professional field,\nhas a dog, goes to the gym,\ngets his coffee at Tim Hortons,\nand enjoys fishing.",
    "Cluster's description:\nJohn is a 40-year-old Albertan\nwith a manual job and a middle-class lifestyle.\nHe enjoys motor sports, hunting and fishing,\neats meat almost every meal,\nand drinks coffee from chains.",
    "Cluster's description:\nMaxime is a 35-year-old Quebecer\nliving in an apartment,\nwho enjoys outdoor and motor sports,\noccasionally hunts and fishes,\nand drinks beer.",
    "Cluster's description:\nZoe is a 35-year-old urban woman\nliving in an apartment with a professional or management job.\nShe cycles, has tattoos,\nfrequently uses Instagram,\nlives with a cat and a dog,\ngets her coffee at Starbucks,\nand runs.",
    "Cluster's description:\nJulie is a 50-year-old Quebecer\nwith limited education and means.\nShe lives in a housing block with her cat,\ntakes walks,\ndoes not drink coffee, and enjoys wine.\nShe is tattooed.",
    "Cluster's description:\nRobert is a 55-year-old man from the Atlantic provinces.\nHe lives in his house and works manually.\nHe fishes a lot, hunts occasionally,\nenjoys beer and spirits,\nand gets his coffee at Tim Hortons.",
    "Cluster's description:\nEmily is educated and lives in an apartment or condo with her cat.\nShe is vegetarian, practices yoga,\nvisits independent cafes,\nshops at thrift stores,\nloves cocktails and occasionally smokes cannabis."
  ))

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

