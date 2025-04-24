# Script pour générer des images des cartes électorales régionales pour les réseaux sociaux
# Ce script fonctionne indépendamment de l'application Shiny

# Chargement des bibliothèques nécessaires
library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Configuration du dossier de sortie pour les images
output_dir <- "code/03_analysis/battlefields/images"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Définir les options pour éviter les messages d'avertissement
options(warn = 1)  # Afficher les avertissements immédiatement
options(dplyr.summarise.inform = FALSE)  # Éviter les messages de dplyr

# Chargement des données nécessaires
map_data <- cartessn::spatial_canada_2022_electoral_ridings_aligned

# Assurez-vous que map_data est correctement formaté
map_data$id_riding <- as.character(map_data$id_riding)
map_data$name_riding_en <- as.character(map_data$name_riding_en)
map_data$name_riding_fr <- as.character(map_data$name_riding_fr)

# Chargement des données des sondages/prédictions
df <- read.csv("https://raw.githubusercontent.com/clessn/agregateur_data/main/data/df.csv")
df$riding_id <- as.character(df$riding_id)

# Définir les couleurs des partis politiques
party_colors <- c(
  "LPC" = "#D91920",  # Liberal - Red
  "CPC" = "#0E4C92",  # Conservative - Blue
  "NDP" = "#f58220",  # NDP - Orange
  "BQ" = "#29b2e6",   # Bloc Québécois - Light blue
  "GPC" = "#39D353"   # Green Party - Green
)

# Fonction pour obtenir toutes les circonscriptions d'une province donnée
get_province_ridings <- function(province_codes) {
  # Convertir en vecteur si un seul code est fourni
  if(!is.vector(province_codes)) province_codes <- c(province_codes)
  
  # Trouver toutes les circonscriptions correspondant aux codes de province
  ridings <- c()
  for(code in province_codes) {
    # Le code de province est les deux premiers caractères de l'ID de circonscription
    matching_ridings <- unique(as.character(
      map_data$id_riding[substr(map_data$id_riding, 1, 2) == code]
    ))
    ridings <- c(ridings, matching_ridings)
  }
  
  return(ridings)
}

# Définition du region_mapping avec toutes les circonscriptions des provinces concernées
region_mapping <- list(
  "quebec_region" = list(
    "ridings" = get_province_ridings("24"),  # Québec
    "coordinates" = c("xmin" = -72.0, "xmax" = -70.6, "ymin" = 46.5, "ymax" = 47.3)
  ),
  
  "montreal_region" = list(
    "ridings" = get_province_ridings("24"),  # Québec
    "coordinates" = c("xmin" = -74.3, "xmax" = -73.2, "ymin" = 45.2, "ymax" = 46.0)
  ),
  
  "toronto_region" = list(
    "ridings" = get_province_ridings("35"),  # Ontario
    "coordinates" = c("xmin" = -80.0, "xmax" = -78.8, "ymin" = 43.4, "ymax" = 44.2)
  ),
  
  "ottawa_gatineau_region" = list(
    "ridings" = get_province_ridings(c("35", "24")),  # Ontario et Québec
    "coordinates" = c("xmin" = -76.4, "xmax" = -75.2, "ymin" = 45.0, "ymax" = 45.7)
  ),
  
  "vancouver_region" = list(
    "ridings" = get_province_ridings("59"),  # Colombie-Britannique
    "coordinates" = c("xmin" = -123.8, "xmax" = -121.8, "ymin" = 48.6, "ymax" = 49.8)
  ),
  
  "winnipeg_region" = list(
    "ridings" = get_province_ridings("46"),  # Manitoba
    "coordinates" = c("xmin" = -97.8, "xmax" = -96.2, "ymin" = 49.4, "ymax" = 50.4)
  ),
  
  "kitchener_waterloo_region" = list(
    "ridings" = get_province_ridings("35"),  # Ontario
    "coordinates" = c("xmin" = -81.2, "xmax" = -79.8, "ymin" = 43.0, "ymax" = 44.0)
  ),
  
  "london_region" = list(
    "ridings" = get_province_ridings("35"),  # Ontario
    "coordinates" = c("xmin" = -81.8, "xmax" = -80.6, "ymin" = 42.6, "ymax" = 43.3)
  ),
  
# Ajouter cette entrée à la liste region_mapping 
"maritimes_region" = list(
  "ridings" = get_province_ridings(c("10", "11", "12", "13", "24")),  # NB, PE, NS, NL, QC
  "coordinates" = c("xmin" = -70.0, "xmax" = -52.0, "ymin" = 41.0, "ymax" = 52.0)
),
  "edmonton_region" = list(
    "ridings" = get_province_ridings(c("48", "47")),  # Alberta et Saskatchewan
    "coordinates" = c("xmin" = -114.0, "xmax" = -113.0, "ymin" = 53.2, "ymax" = 54.0)
  ),
  
  "calgary_region" = list(
    "ridings" = get_province_ridings("48"),  # Alberta
    "coordinates" = c("xmin" = -114.4, "xmax" = -113.4, "ymin" = 50.8, "ymax" = 51.4)
  )
)

# Noms des régions en français pour les titres (maintenu pour référence mais ne sera pas affiché)
region_names_fr <- c(
  "montreal_region" = "Région de Montréal",
  "toronto_region" = "Région du Grand Toronto",
  "vancouver_region" = "Région du Grand Vancouver",
  "quebec_region" = "Région de Québec",
  "ottawa_gatineau_region" = "Région d'Ottawa-Gatineau",
  "winnipeg_region" = "Région de Winnipeg",
  "kitchener_waterloo_region" = "Région de Kitchener-Waterloo-Cambridge",
  "london_region" = "Région de London",
  "maritimes_region" = "Région des Maritimes",
  "edmonton_region" = "Région d'Edmonton",
  "calgary_region" = "Région de Calgary"
)

# Function pour traiter et préparer les données
prepare_data <- function(map_data, df) {
  # Vérifier que les colonnes nécessaires existent
  if(!all(c("riding_id", "prediction", "probability") %in% colnames(df))) {
    stop("Le jeu de données doit contenir les colonnes: riding_id, prediction, probability")
  }
  
  # Joindre les données des sondages avec les données géographiques
  map_data_copy <- map_data %>%
    mutate(
      id_riding = as.character(id_riding),
      name_riding_fr = as.character(name_riding_fr)
    )
  
  df_processed <- df %>%
    rename(
      riding = riding_id,
      party = prediction,
      first_party_percentage = probability
    ) %>%
    mutate(
      # Convertir la probabilité si nécessaire
      first_party_percentage = if(max(first_party_percentage, na.rm = TRUE) <= 1) 
        first_party_percentage * 100 
      else 
        first_party_percentage,
      riding = as.character(riding)
    )
  
  # Joindre les données
  map_filtered <- map_data_copy %>%
    left_join(df_processed, by = c("id_riding" = "riding"))
  
  # Gérer les NA
  map_filtered <- map_filtered %>%
    mutate(
      party = ifelse(is.na(party), "Unknown", party),
      first_party_percentage = ifelse(is.na(first_party_percentage), 50, first_party_percentage),
      # Catégoriser par niveaux de compétitivité pour les battlefields
      battlefield_intensity = pmin(100, pmax(0, 100 - first_party_percentage)),
      # Alpha pour la transparence selon la certitude
      alpha_category = case_when(
        first_party_percentage <= 52 ~ 0.35,
        first_party_percentage > 52 & first_party_percentage <= 58 ~ 0.5,
        first_party_percentage > 58 & first_party_percentage <= 70 ~ 0.7,
        first_party_percentage > 70 & first_party_percentage <= 85 ~ 0.85,
        first_party_percentage > 85 ~ 1.0,
        TRUE ~ 0.6
      ),
      # Catégories textuelles de compétitivité
      closeness_category = case_when(
        first_party_percentage <= 52 ~ "Trop serré pour prédire",
        first_party_percentage > 52 & first_party_percentage <= 58 ~ "Course serrée",
        first_party_percentage > 58 & first_party_percentage <= 70 ~ "Avance modérée",
        first_party_percentage > 70 & first_party_percentage <= 85 ~ "Avance confortable",
        first_party_percentage > 85 ~ "Victoire quasi-certaine",
        TRUE ~ "Inconnu"
      )
    )
  
  return(map_filtered)
}

# Fonction pour extraire les circonscriptions les plus compétitives
get_competitive_ridings <- function(data, threshold = 58) {
  if (!"first_party_percentage" %in% colnames(data)) {
    warning("Colonne 'first_party_percentage' non trouvée dans les données")
    return(data[0,])
  }
  
  competitive_ridings <- data %>%
    filter(first_party_percentage <= threshold) %>%
    arrange(first_party_percentage)
  
  return(competitive_ridings)
}

# Fonction pour générer la carte complète du Canada (tous les partis)
# Fonction pour générer des cartes sans AUCUNE marge
generate_region_map <- function(data, region_code, coordinates, output_path, battlefield_mode = FALSE) {
  # Vérifier si le code de région existe
  if (!(region_code %in% names(region_mapping))) {
    warning(paste("Code de région inconnu:", region_code))
    return(NULL)
  }
  
  # Filtrer les données pour cette région
  region_data <- data %>%
    filter(id_riding %in% region_mapping[[region_code]]$ridings)
  
  # Transformer en WGS84 pour la cohérence
  if (nrow(region_data) > 0) {
    region_data <- st_transform(region_data, 4326)
  }
  
  # Vérifier si des circonscriptions ont été trouvées
  if (nrow(region_data) == 0) {
    warning(paste("Aucune circonscription trouvée pour la région:", region_code))
    return(NULL)
  }
  
  # Définir si c'est une carte du Canada complet (avec marges) ou une carte régionale (sans marge)
  is_canada_complete_map <- region_code %in% c("canada_complete", "canada_tout")
  
  if (battlefield_mode) {
    # Mode champs de bataille
    p <- ggplot() +
      geom_sf(data = region_data, 
              aes(fill = battlefield_intensity),
              color = "#333333", size = 0.3) +
      scale_fill_gradientn(
        colors = c("black", "white", "#FFCC00", "#FFA500"),
        values = c(0, 0.4, 0.7, 1),
        limits = c(0, 100),
        name = NULL
      )
  } else {
    # Mode couleurs des partis
    p <- ggplot() +
      geom_sf(data = region_data, 
              aes(fill = party, alpha = alpha_category),
              color = "#333333", size = 0.3) +
      scale_fill_manual(
        values = party_colors,
        name = NULL
      ) +
      scale_alpha_identity()
  }
  
  # Ajouter les éléments communs
  p <- p +
    coord_sf(
      xlim = c(coordinates["xmin"], coordinates["xmax"]),
      ylim = c(coordinates["ymin"], coordinates["ymax"]),
      expand = FALSE,  # Crucial pour éviter l'expansion automatique
      clip = "off"     # Empêcher le rognage des éléments
    ) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.position = "none",
      plot.margin = margin(0, 0, 0, 0, "cm"),  # Marges NULLES
      panel.border = element_blank(),
      panel.grid = element_blank(),
      panel.spacing = unit(0, "cm"),
      # Supprimer absolument tous les éléments possibles
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank()
    )
  
  # Utiliser une méthode alternative de sauvegarde qui force l'absence de marge
  # NE PAS utiliser ggsave qui a un comportement inconsistant avec les cartes géographiques
  
  # 1. Créer un dispositif graphique avec des dimensions exactes
  png(output_path, width = 1000, height = 800, units = "px", res = 120, bg = "white")
  
  # 2. Imprimer le graphique
  print(p)
  
  # 3. Fermer le dispositif
  dev.off()
  
  cat("Carte de", region_names_fr[region_code], "sauvegardée à:", output_path, "\n")
  return(invisible(p))
}

# Fonction pour générer la carte du Canada complet (tous les partis)
generate_canada_map <- function(data, output_path) {
  # Utiliser le même moteur mais avec des coordonnées Canada-wide
  # Créer les coordonnées du Canada
  canada_coords <- c("xmin" = -141, "xmax" = -52, "ymin" = 41, "ymax" = 83)
  
  # Créer un mapping spécifique pour le Canada complet
  region_mapping[["canada_complete"]] <- list(
    "ridings" = unique(data$id_riding),
    "coordinates" = canada_coords
  )
  region_names_fr[["canada_complete"]] <- "Canada complet"
  
  # Utiliser notre fonction améliorée pour générer la carte
  p <- generate_region_map(data, "canada_complete", canada_coords, output_path, battlefield_mode = FALSE)
  
  return(p)
}

# Fonction pour générer la carte des battlefields du Canada
generate_battlefield_map <- function(data, output_path) {
  # Utiliser le même moteur mais avec des coordonnées Canada-wide et en mode battlefield
  # Créer les coordonnées du Canada
  canada_coords <- c("xmin" = -141, "xmax" = -52, "ymin" = 41, "ymax" = 83)
  
  # Créer un mapping spécifique pour le Canada complet
  region_mapping[["canada_battlefield"]] <- list(
    "ridings" = unique(data$id_riding),
    "coordinates" = canada_coords
  )
  region_names_fr[["canada_battlefield"]] <- "Canada - Champs de bataille"
  
  # Utiliser notre fonction améliorée pour générer la carte
  p <- generate_region_map(data, "canada_battlefield", canada_coords, output_path, battlefield_mode = TRUE)
  
  return(p)
}

# Fonction principale modifiée pour utiliser les nouvelles fonctions
main <- function() {
  cat("Début du traitement des cartes électorales régionales pour les réseaux sociaux\n")
  
  # Vérifier si les données nécessaires sont disponibles
  if (!exists("map_data") || !exists("df")) {
    stop("Les données 'map_data' ou 'df' ne sont pas disponibles. Assurez-vous qu'elles sont correctement chargées.")
  }
  
  tryCatch({
    # Préparer les données
    processed_data <- prepare_data(map_data, df)
    
    # 1. Générer la carte du Canada (tous les partis)
    cat("Génération de la carte électorale du Canada...\n")
    canada_map <- generate_canada_map(processed_data, 
                                    file.path(output_dir, "canada_tous_partis.png"))
    
    # 2. Générer la carte des champs de bataille du Canada
    cat("Génération de la carte des champs de bataille du Canada...\n")
    battlefield_map <- generate_battlefield_map(processed_data,
                                              file.path(output_dir, "canada_battlefields.png"))
    
    # 3. Générer les cartes des régions en mode battlefield et mode partis
    cat("Génération des cartes régionales...\n")
    maps_generated <- 2  # Déjà compté les deux cartes du Canada
  }, 
  error = function(e) {
    cat("ERREUR lors de la préparation des données ou la génération des cartes nationales:\n")
    cat(paste("  ", e$message, "\n"))
    stop("Traitement interrompu en raison d'erreurs.")
  })
  
  for (region in names(region_mapping)) {
    # Ignorer les deux nouvelles entrées canada_complete et canada_battlefield
    if (region %in% c("canada_complete", "canada_battlefield")) {
      next
    }
    
    tryCatch({
      cat(paste("Traitement de la région:", region_names_fr[region], "\n"))
      
      # Vérifier si le nom de la région est correctement défini
      if (!(region %in% names(region_names_fr))) {
        cat(paste("  ATTENTION: Nom français manquant pour la région", region, "- utilisation du code\n"))
        region_names_fr[region] <- region  # Utiliser le code comme fallback
      }
      
      # Vérification des circonscriptions existantes dans les données
      ridings_in_data <- sum(region_mapping[[region]]$ridings %in% processed_data$id_riding)
      cat(paste("  Circonscriptions trouvées:", ridings_in_data, "/", 
                length(region_mapping[[region]]$ridings), "\n"))
      
      if (ridings_in_data == 0) {
        cat(paste("  ATTENTION: Aucune circonscription trouvée pour", region_names_fr[region], "- carte ignorée\n"))
        next
      }
      
      # Mode battlefield (champs de bataille)
      region_battlefield_path <- file.path(output_dir, paste0(region, "_battlefields.png"))
      region_battlefield_map <- generate_region_map(processed_data, region, 
                                   region_mapping[[region]]$coordinates,
                                   region_battlefield_path, 
                                   battlefield_mode = TRUE)
      
      if (!is.null(region_battlefield_map)) {
        maps_generated <- maps_generated + 1
      }
      
      # Mode partis politiques 
      region_parties_path <- file.path(output_dir, paste0(region, "_partis.png"))
      region_parties_map <- generate_region_map(processed_data, region, 
                               region_mapping[[region]]$coordinates,
                               region_parties_path, 
                               battlefield_mode = FALSE)
      
      if (!is.null(region_parties_map)) {
        maps_generated <- maps_generated + 1
      }
    }, error = function(e) {
      cat(paste("  ERREUR lors du traitement de la région", region, ":\n"))
      cat(paste("    ", e$message, "\n"))
      cat("  Passage à la région suivante...\n")
    })
  }
  
  cat("\nTraitement terminé! Toutes les images ont été sauvegardées dans le dossier:", output_dir, "\n")
  cat("Nombre total d'images générées:", maps_generated, "\n")
}