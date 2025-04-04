#' Traitement des coefficients du modèle pour export
#' 
#' Ce script extrait les coefficients symétriques du modèle final et les formate
#' pour faciliter leur utilisation et interprétation. Il prépare les données pour 
#' une exportation vers Excel et effectue des vérifications pour s'assurer que
#' l'exportation est correcte.
#'
#' Entrée :
#' - Modèle final (finalmodel_withRTAPredictions_2025-04-15.rds)
#'
#' Sortie :
#' - Fichier Excel des coefficients formatés (model_coefficients_formatted.xlsx)
#'
# Charger les packages nécessaires
library(tidyverse)
library(nnet)

# Charger le modèle final
final_model <- readRDS("_SharedFolder_datagotchi_federal_2024/data/modele/finalmodel_withRTAPredictions_2025-04-15.rds")

# Vérifier si le modèle contient les coefficients symétriques
if (!exists("sym_coef", where = final_model)) {
  cat("Le modèle ne contient pas de coefficients symétriques. Calcul en cours...\n")
  
  # Extraire la matrice des coefficients originaux
  orig_coef <- coef(final_model)
  
  # Récupérer tous les niveaux de la variable réponse
  all_levels <- c(rownames(orig_coef), "bq") # Assurer que "bq" est inclus
  
  # Créer une matrice complète pour les coefficients
  full_coef <- matrix(0, nrow = length(all_levels), ncol = ncol(orig_coef))
  rownames(full_coef) <- all_levels
  colnames(full_coef) <- colnames(orig_coef)
  
  # Remplir full_coef pour les niveaux non de référence
  for (lvl in rownames(orig_coef)) {
    full_coef[lvl, ] <- orig_coef[lvl, ]
  }
  
  # Pour chaque prédicteur, calculer la moyenne des coefficients sur tous les niveaux
  m <- colMeans(full_coef)
  
  # Reparamétrer de manière symétrique
  sym_coef <- full_coef - matrix(rep(m, each = length(all_levels)), nrow = length(all_levels))
} else {
  cat("Utilisation des coefficients symétriques existants.\n")
  sym_coef <- final_model$sym_coef
}

# Afficher les partis disponibles dans les coefficients symétriques
cat("Partis dans les coefficients symétriques:", paste(rownames(sym_coef), collapse=", "), "\n")

# Définir les noms de partis pour l'export
export_parties <- c("lpc", "gpc", "cpc", "bq", "ndp")

# Extraire les variables (sauf l'intercept)
variables <- colnames(sym_coef)[-1]  # Toutes les variables sauf l'intercept

# Créer un dataframe pour stocker les résultats
results_df <- data.frame()

# Traiter l'intercept séparément
intercept_row <- data.frame(
  question_slug = "party",
  Choice = "intercept",
  emoji = "🪬",
  coefficient = "party_intercept",
  CLESSN_Coefficient = NA,
  interaction = NA
)

# Ajouter les valeurs d'intercept pour chaque parti
for (party in export_parties) {
  col_name <- paste0("coef_", party)
  if (party %in% rownames(sym_coef)) {
    intercept_row[[col_name]] <- sym_coef[party, "(Intercept)"]
  } else {
    # Si le parti n'est pas dans sym_coef, utiliser 0 comme valeur neutre
    intercept_row[[col_name]] <- 0
    warning(paste("Parti", party, "non trouvé dans les coefficients symétriques. Utilisation de 0 pour l'intercept."))
  }
}

results_df <- rbind(results_df, intercept_row)

# Fonction pour extraire le nom de la variable à partir du nom du coefficient
extract_variable_name <- function(coef_name) {
  if (grepl("_", coef_name)) {
    parts <- strsplit(coef_name, "_")[[1]]
    if (length(parts) > 1) {
      variable_name <- paste(parts[1:(length(parts)-1)], collapse = "_")
      return(variable_name)
    }
  }
  return(coef_name)
}

# Fonction pour extraire le niveau/choix à partir du nom du coefficient
extract_choice <- function(coef_name) {
  if (grepl("_", coef_name)) {
    parts <- strsplit(coef_name, "_")[[1]]
    if (length(parts) > 1) {
      choice <- parts[length(parts)]
      return(choice)
    }
  }
  return("value")
}

# Identifier les variables RTA et les traiter séparément
rta_vars <- variables[grepl("^prediction_", variables)]
non_rta_vars <- variables[!grepl("^prediction_", variables)]

# Traiter les variables non-RTA
for (var in non_rta_vars) {
  # Extraire le nom de la question/variable et le choix
  question_slug <- extract_variable_name(var)
  choice <- extract_choice(var)
  
  # Déterminer si c'est un binaire
  is_binary <- ifelse(grepl("bin$", var) || choice == "value", "TRUE", "FALSE")
  
  # Coefficient et nom CLESSN
  coef_name <- var
  clessn_coef <- gsub("^rule_", "", var)
  clessn_coef <- gsub("^sport_", "lifestyle_exercise_", clessn_coef)
  clessn_coef <- gsub("^transport_", "lifestyle_typeTransport_", clessn_coef)
  clessn_coef <- gsub("^shopping_", "lifestyle_consClothes_", clessn_coef)
  # Ajouter d'autres transformations selon ton modèle
  
  # Créer la ligne de base
  row <- data.frame(
    question_slug = question_slug,
    Choice = choice,
    emoji = "🪬",
    coefficient = coef_name,
    CLESSN_Coefficient = clessn_coef,
    interaction = NA
  )
  
  # Ajouter les coefficients pour chaque parti
  for (party in export_parties) {
    col_name <- paste0("coef_", party)
    if (party %in% rownames(sym_coef)) {
      row[[col_name]] <- sym_coef[party, var]
    } else {
      # Si le parti n'est pas dans sym_coef, utiliser 0 comme valeur neutre
      row[[col_name]] <- 0
      warning(paste("Parti", party, "non trouvé dans les coefficients symétriques pour la variable", var))
    }
  }
  
  results_df <- rbind(results_df, row)
}

# Traiter les variables RTA
for (var in rta_vars) {
  party_in_var <- gsub("^prediction_", "", var)
  
  row <- data.frame(
    question_slug = "rta_prediction",
    Choice = party_in_var,
    emoji = "🪬",
    coefficient = var,
    CLESSN_Coefficient = NA,
    interaction = NA
  )
  
  # Ajouter les coefficients pour chaque parti
  for (party in export_parties) {
    col_name <- paste0("coef_", party)
    if (party %in% rownames(sym_coef)) {
      row[[col_name]] <- sym_coef[party, var]
    } else {
      # Si le parti n'est pas dans sym_coef, utiliser 0 comme valeur neutre
      row[[col_name]] <- 0
      warning(paste("Parti", party, "non trouvé dans les coefficients symétriques pour la variable", var))
    }
  }
  
  results_df <- rbind(results_df, row)
}

# Exporter le résultat
openxlsx::write.xlsx(results_df, "_SharedFolder_datagotchi_federal_2024/data/modele/model_coefficients_formatted.xlsx", rowNames = FALSE)

cat("Exportation des coefficients terminée!\n")
cat("Fichier sauvegardé: _SharedFolder_datagotchi_federal_2024/data/modele/model_coefficients_formatted.csv\n")

# Après avoir exporté les coefficients, ajouter une vérification

# Charger le fichier exporté pour vérification
exported_coefs <- openxlsx::read.xlsx("_SharedFolder_datagotchi_federal_2024/data/modele/model_coefficients_formatted.xlsx")

# Vérifier quelques coefficients au hasard pour chaque parti
cat("\n--- Vérification des coefficients exportés ---\n")

# Sélectionner quelques variables aléatoires pour vérification
set.seed(123)  # Pour reproduire les mêmes sélections
if (length(variables) > 10) {
  vars_to_check <- sample(variables, 10)
} else {
  vars_to_check <- variables
}

# Vérifier chaque variable pour chaque parti
for (var in vars_to_check) {
  cat("\nVérification pour la variable:", var, "\n")
  
  for (party in intersect(export_parties, rownames(sym_coef))) {
    # Valeur dans le modèle
    model_value <- sym_coef[party, var]
    
    # Trouver la valeur correspondante dans le fichier exporté
    var_in_export <- exported_coefs$coefficient == var
    if (any(var_in_export)) {
      exported_value <- exported_coefs[var_in_export, paste0("coef_", party)]
      
      # Comparer les valeurs (avec une petite marge d'erreur pour les différences d'arrondi)
      is_equal <- abs(model_value - exported_value) < 1e-10
      
      cat(sprintf("  %s: Modèle = %.6f, Exporté = %.6f, %s\n", 
                 party, model_value, exported_value, 
                 ifelse(is_equal, "OK", "DIFFÉRENT!")))
    } else {
      cat(sprintf("  %s: Variable non trouvée dans l'export!\n", party))
    }
  }
}

# Vérifier les intercepts
cat("\nVérification des intercepts:\n")
for (party in intersect(export_parties, rownames(sym_coef))) {
  # Valeur dans le modèle
  model_intercept <- sym_coef[party, "(Intercept)"]
  
  # Trouver la valeur correspondante dans le fichier exporté
  intercept_row <- exported_coefs$coefficient == "party_intercept"
  if (any(intercept_row)) {
    exported_intercept <- exported_coefs[intercept_row, paste0("coef_", party)]
    
    # Comparer les valeurs
    is_equal <- abs(model_intercept - exported_intercept) < 1e-10
    
    cat(sprintf("  %s: Modèle = %.6f, Exporté = %.6f, %s\n", 
               party, model_intercept, exported_intercept, 
               ifelse(is_equal, "OK", "DIFFÉRENT!")))
  } else {
    cat(sprintf("  %s: Intercept non trouvé dans l'export!\n", party))
  }
}

# Vérification des prédictions RTA
if (length(rta_vars) > 0) {
  cat("\nVérification des coefficients pour les prédictions RTA:\n")
  
  for (var in rta_vars) {
    cat("\nVérification pour la variable RTA:", var, "\n")
    
    for (party in intersect(export_parties, rownames(sym_coef))) {
      # Valeur dans le modèle
      model_value <- sym_coef[party, var]
      
      # Trouver la valeur correspondante dans le fichier exporté
      var_in_export <- exported_coefs$coefficient == var
      if (any(var_in_export)) {
        exported_value <- exported_coefs[var_in_export, paste0("coef_", party)]
        
        # Comparer les valeurs
        is_equal <- abs(model_value - exported_value) < 1e-10
        
        cat(sprintf("  %s: Modèle = %.6f, Exporté = %.6f, %s\n", 
                   party, model_value, exported_value, 
                   ifelse(is_equal, "OK", "DIFFÉRENT!")))
      } else {
        cat(sprintf("  %s: Variable RTA non trouvée dans l'export!\n", party))
      }
    }
  }
}

# Résumé final de la vérification
cat("\n--- Résumé de la vérification ---\n")
total_vars <- length(variables) + 1  # +1 pour l'intercept
total_parties <- length(intersect(export_parties, rownames(sym_coef)))
cat(sprintf("Nombre total de variables vérifiées (échantillon): %d\n", length(vars_to_check) + 1))
cat(sprintf("Nombre de partis vérifiés: %d\n", total_parties))
cat("Si tous les tests sont marqués 'OK', l'exportation est réussie!\n")
