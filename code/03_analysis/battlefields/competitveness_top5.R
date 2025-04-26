# Script pour identifier les circonscriptions les plus serrées (top 5)
# Ce script utilise les mêmes données que le script principal

# Charger les données si ce n'est pas déjà fait
if(!exists("df")) {
  df <- read.csv("https://raw.githubusercontent.com/clessn/agregateur_data/main/data/df.csv")
  df$riding_id <- as.character(df$riding_id)
}

# Charger les données géographiques si ce n'est pas déjà fait
if(!exists("map_data")) {
  library(cartessn)  # Assurez-vous que ce package est installé
  map_data <- cartessn::spatial_canada_2022_electoral_ridings_aligned
  map_data$id_riding <- as.character(map_data$id_riding)
}

# Fonction pour préparer les données
prepare_competitive_data <- function(df) {
  # Renommer et calculer les colonnes pertinentes
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
      riding = as.character(riding),
      # Calculer l'intensité de la compétition (plus le pourcentage est bas, plus c'est serré)
      competitiveness = 100 - first_party_percentage
    )
  
  return(df_processed)
}

# Obtenir les noms des circonscriptions
get_riding_names <- function(riding_ids) {
  # Joindre avec les données géographiques pour obtenir les noms
  riding_names <- map_data %>%
    dplyr::select(id_riding, name_riding_fr, name_riding_en) %>%
    dplyr::filter(id_riding %in% riding_ids)
  
  return(riding_names)
}

# Trouver les circonscriptions les plus serrées
find_most_competitive_ridings <- function(df_processed, top_n = 5) {
  most_competitive <- df_processed %>%
    arrange(desc(competitiveness)) %>%
    slice_head(n = top_n)
  
  # Obtenir les noms des circonscriptions
  riding_names <- get_riding_names(most_competitive$riding)
  
  # Joindre les noms aux données
  result <- most_competitive %>%
    left_join(riding_names, by = c("riding" = "id_riding"))
  
  # Formater pour l'affichage
  result_formatted <- result %>%
    mutate(
      competitiveness = round(competitiveness, 1),
      first_party_percentage = round(first_party_percentage, 1),
      display = paste0(
        name_riding_fr, 
        " (", name_riding_en, ")", 
        " - Parti en tête: ", party,
        " avec ", first_party_percentage, "% ",
        "(niveau de compétitivité: ", competitiveness, ")"
      )
    ) %>%
    select(riding, party, first_party_percentage, competitiveness, name_riding_fr, name_riding_en, display)
  
  return(result_formatted)
}

# Fonction pour trouver les circonscriptions les plus serrées par région
find_most_competitive_by_region <- function(df_processed, region_code, top_n = 3) {
  if(exists("region_mapping") && region_code %in% names(region_mapping)) {
    # Filtrer pour les circonscriptions de cette région
    ridings_in_region <- region_mapping[[region_code]]$ridings
    
    region_competitive <- df_processed %>%
      filter(riding %in% ridings_in_region) %>%
      arrange(desc(competitiveness)) %>%
      slice_head(n = top_n)
    
    # Obtenir les noms
    riding_names <- get_riding_names(region_competitive$riding)
    
    # Joindre les noms
    result <- region_competitive %>%
      left_join(riding_names, by = c("riding" = "id_riding"))
    
    # Formater pour l'affichage
    result_formatted <- result %>%
      mutate(
        competitiveness = round(competitiveness, 1),
        first_party_percentage = round(first_party_percentage, 1),
        display = paste0(
          name_riding_fr, 
          " (", name_riding_en, ")", 
          " - Parti en tête: ", party,
          " avec ", first_party_percentage, "% ",
          "(niveau de compétitivité: ", competitiveness, ")"
        )
      ) %>%
      select(riding, party, first_party_percentage, competitiveness, name_riding_fr, name_riding_en, display)
    
    return(result_formatted)
  } else {
    warning(paste("Région inconnue:", region_code))
    return(NULL)
  }
}

# Exécution principale
main_competitive <- function() {
  # Préparer les données
  df_processed <- prepare_competitive_data(df)
  
  # Trouver les 5 circonscriptions les plus serrées dans tout le Canada
  cat("\n=== TOP 5 DES CIRCONSCRIPTIONS LES PLUS SERRÉES AU CANADA ===\n\n")
  top_competitive <- find_most_competitive_ridings(df_processed, top_n = 5)
  
  # Afficher les résultats
  for(i in 1:nrow(top_competitive)) {
    cat(paste0(i, ". ", top_competitive$display[i], "\n"))
  }
  
  # Option: Sauvegarder dans un fichier CSV
  write.csv(top_competitive, "top_competitive_ridings.csv", row.names = FALSE)
  cat("\nLes résultats ont été sauvegardés dans 'top_competitive_ridings.csv'\n")
  
  # Option: Analyser par région (exemple avec Montréal)
  cat("\n=== TOP 3 DES CIRCONSCRIPTIONS LES PLUS SERRÉES AU QUEBEC ===\n\n")
  montreal_competitive <- find_most_competitive_by_region(df_processed, "montreal_region", top_n = 3)
  if(!is.null(montreal_competitive)) {
    for(i in 1:nrow(montreal_competitive)) {
      cat(paste0(i, ". ", montreal_competitive$display[i], "\n"))
    }
  }
  
  # Retourner les données pour utilisation ultérieure si nécessaire
  return(list(
    canada = top_competitive,
    montreal = montreal_competitive
  ))
}

# Exécuter le script
results <- main_competitive()
