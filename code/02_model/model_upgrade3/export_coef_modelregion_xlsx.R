#' Export des coefficients du modèle multinomial pour l'application datagotchi
#' 
#' Ce script charge le modèle RTA amélioré et exporte les coefficients symétriques
#' au format compatible avec l'application Python, incluant le traitement spécial pour le BQ.
#'
#' Entrée :
#' - Modèle final (finalmodel_withRTAPredictions_april3_2025-04-16.rds)
#'
#' Sortie :
#' - Fichier Excel des coefficients formatés pour les développeurs
#' - Fichier CSV des intercepts pour l'application

# Chargement des packages nécessaires
library(tidyverse)
library(nnet)
library(openxlsx)
library(tibble)

# Configuration et chargement du modèle ----------------------------------------

# Définir le chemin vers le modèle
model_path <- "_SharedFolder_datagotchi_federal_2024/data/modele/finalmodel_withRTAPredictions_april3_2025-04-16.rds"

# Vérifier l'existence du fichier
if (!file.exists(model_path)) {
  stop(paste0("Le fichier modèle n'existe pas: ", model_path))
}

# Charger le modèle final
cat("Chargement du modèle:", model_path, "\n")
final_model <- readRDS(model_path)

# Définir l'ordre des partis pour l'export (ordre attendu par les développeurs)
export_parties <- c("lpc", "gpc", "cpc", "bq", "ndp")

# Vérifier la présence des coefficients symétriques ----------------------------

if (!exists("sym_coef", where = final_model)) {
  cat("Calcul des coefficients symétriques...\n")
  
  # Extraire les coefficients originaux
  orig_coef <- coef(final_model)
  
  # Récupérer tous les niveaux de la variable réponse
  # On suppose que "bq" est la référence dans le modèle
  all_levels <- c(rownames(orig_coef), "bq") 
  
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
  
  # Ajouter la matrice symétrique au modèle final
  final_model$sym_coef <- sym_coef
  
  # Sauvegarder le modèle avec les coefficients symétriques
  sym_model_path <- gsub("\\.rds$", "_with_sym_coef.rds", model_path)
  saveRDS(final_model, sym_model_path)
  cat("Modèle sauvegardé avec coefficients symétriques:", sym_model_path, "\n")
} else {
  cat("Le modèle contient déjà des coefficients symétriques\n")
  sym_coef <- final_model$sym_coef
}

# Préparation des données pour l'export ----------------------------------------

# 1. Formater pour l'export développeur
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
  intercept_row[[col_name]] <- sym_coef[party, "(Intercept)"]
}

results_df <- rbind(results_df, intercept_row)

# Fonctions utilitaires --------------------------------------------------------

# Fonction pour déterminer si une variable est une interaction régionale
is_regional_interaction <- function(var_name) {
  return(grepl("^is_", var_name))
}

# Fonction pour extraire la région à partir d'une variable d'interaction
extract_region <- function(var_name) {
  if (grepl("^is_", var_name)) {
    parts <- strsplit(var_name, ":")[[1]]
    if (length(parts) > 0) {
      region_part <- parts[1]
      region <- gsub("^is_", "", region_part)
      return(region)
    }
  }
  return(NA)
}

# Fonction pour extraire la variable de base d'une interaction régionale
extract_base_var <- function(var_name) {
  if (grepl(":", var_name)) {
    parts <- strsplit(var_name, ":")[[1]]
    if (length(parts) > 1) {
      return(parts[2])
    }
  }
  return(var_name)
}

# Traitement de toutes les variables (sauf l'intercept) -----------------------

vars_to_process <- colnames(sym_coef)[colnames(sym_coef) != "(Intercept)"]

for (var in vars_to_process) {
  # Déterminer si c'est une interaction régionale
  is_regional <- is_regional_interaction(var)
  region <- ifelse(is_regional, extract_region(var), NA)
  
  # Extraire la variable de base
  base_var <- extract_base_var(var)
  
  # Déterminer si c'est une variable RTA
  is_rta <- grepl("prediction_", base_var)
  
  if (is_rta) {
    # Pour les variables RTA
    party_in_var <- gsub("^prediction_", "", base_var)
    
    row <- data.frame(
      question_slug = "rta_prediction",
      Choice = party_in_var,
      emoji = "🪬",
      coefficient = var,
      CLESSN_Coefficient = NA,
      interaction = ifelse(is_regional, region, NA)
    )
  } else {
    # Pour les autres variables
    # Essayer d'extraire le nom de la question et le choix
    parts <- strsplit(base_var, "_")[[1]]
    if (length(parts) > 1) {
      question_slug <- paste(parts[1:(length(parts)-1)], collapse="_")
      choice <- parts[length(parts)]
    } else {
      question_slug <- base_var
      choice <- "value"
    }
    
    row <- data.frame(
      question_slug = question_slug,
      Choice = choice,
      emoji = "🪬",
      coefficient = var,
      CLESSN_Coefficient = base_var,
      interaction = ifelse(is_regional, region, NA)
    )
  }
  
  # Ajouter les coefficients pour chaque parti
  for (party in export_parties) {
    col_name <- paste0("coef_", party)
    row[[col_name]] <- sym_coef[party, var]
  }
  
  results_df <- rbind(results_df, row)
}

# Exporter les fichiers pour les développeurs ----------------------------------

# 1. Format tableau complet pour les coefficients
output_file_coef <- "_SharedFolder_datagotchi_federal_2024/data/modele/model_coefficients_formatted.xlsx"
openxlsx::write.xlsx(results_df, output_file_coef, rowNames = FALSE)

# 2. Générer le fichier intercepts.csv pour le script Python
intercepts_df <- data.frame(
  party = export_parties,
  intercept = sapply(export_parties, function(p) sym_coef[p, "(Intercept)"])
)

# Ajouter les colonnes de projection RTA
rta_vars <- vars_to_process[grepl("prediction_", vars_to_process)]
rta_parties <- unique(gsub("^prediction_", "", gsub(":.*$", "", rta_vars)))

# Par défaut, initialiser toutes les projections à 1.0
for (rta_party in rta_parties) {
  for (target_party in export_parties) {
    col_name <- paste0("projection_", target_party)
    if (!(col_name %in% names(intercepts_df))) {
      intercepts_df[[col_name]] <- 1.0  # Valeur par défaut
    }
  }
}

# Ajustement pour tenir compte du traitement spécial du BQ
# La valeur reste 1.0, car le script Python gère la mise à zéro hors Québec

# Exporter le fichier d'intercepts
intercepts_file <- "_SharedFolder_datagotchi_federal_2024/data/modele/intercepts.csv"
write.csv(intercepts_df, intercepts_file, row.names = FALSE)

# Exporter aussi un fichier à format fixe pour débogage
output_file_transposed <- "_SharedFolder_datagotchi_federal_2024/data/modele/coef_matrix.xlsx"
coef_transposed <- sym_coef %>%
  as.data.frame() %>%
  rownames_to_column(var = "Party") %>%
  {t(.[,-1])} %>% # Transposer sans la colonne Party
  as.data.frame() %>%
  set_names(sym_coef %>% rownames()) %>%
  rownames_to_column(var = "Predictor") %>%
  select(Predictor, all_of(export_parties))

write.xlsx(list(
  "Coefficients" = coef_transposed,
  "Intercepts" = intercepts_df
), output_file_transposed)

# Vérification des résultats --------------------------------------------------

cat("\n--- Exportation des fichiers terminée ---\n")
cat("- Format pour les développeurs:", output_file_coef, "\n")
cat("- Fichier intercepts.csv:", intercepts_file, "\n")
cat("- Format transposé pour vérification:", output_file_transposed, "\n")

# Vérification des fichiers exportés
cat("\n--- Vérification des coefficients exportés ---\n")
exported_coefs <- openxlsx::read.xlsx(output_file_coef)
cat("Nombre de lignes exportées:", nrow(exported_coefs), "\n")
cat("Nombre de colonnes exportées:", ncol(exported_coefs), "\n")

# Vérifier quelques coefficients au hasard
set.seed(123)
vars_to_check <- sample(vars_to_process, min(10, length(vars_to_process)))

for (var in vars_to_check) {
  cat("\nVérification pour la variable:", var, "\n")
  
  for (party in export_parties) {
    model_value <- sym_coef[party, var]
    
    var_in_export <- exported_coefs$coefficient == var
    if (any(var_in_export)) {
      exported_value <- as.numeric(exported_coefs[var_in_export, paste0("coef_", party)])
      
      is_equal <- abs(model_value - exported_value) < 1e-10
      cat(sprintf("  %s: Modèle = %.6f, Exporté = %.6f, %s\n", 
                 party, model_value, exported_value, 
                 ifelse(is_equal, "OK", "DIFFÉRENT!")))
    } else {
      cat(sprintf("  %s: Variable non trouvée dans l'export!\n", party))
    }
  }
}

# Résumé final
cat("\n--- Résumé ---\n")
cat(sprintf("Nombre total de variables exportées: %d\n", nrow(results_df)))
cat(sprintf("Nombre de partis exportés: %d\n", length(export_parties)))
cat(sprintf("Partis exportés: %s\n", paste(export_parties, collapse=", ")))
cat("Export terminé avec succès!\n")
