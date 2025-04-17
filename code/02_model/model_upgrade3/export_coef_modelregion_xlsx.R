#' Export des coefficients du modèle multinomial pour l'application datagotchi
#' 
#' Ce script charge le modèle RTA avec interactions régionales, extrait et
#' transforme les coefficients pour les rendre compatibles avec l'application Python.
#' Il utilise la même approche que précédemment pour les coefficients symétriques.
#'
#' Entrée :
#' - Modèle final (finalmodel_withRTAPredictions_6Regions_2025-04-09.rds)
#'
#' Sortie :
#' - Fichier Excel des coefficients formatés (model_coefficients_formatted.xlsx)

# Charger les packages nécessaires
library(tidyverse)
library(nnet)
library(openxlsx)
library(tibble)

# Définir le chemin vers le modèle
model_path <- "_SharedFolder_datagotchi_federal_2024/data/modele/finalmodel_withRTAPredictions_6Regions_2025-04-09.rds"

# Charger le modèle final
cat("Chargement du modèle:", model_path, "\n")
final_model <- readRDS(model_path)

# Vérifier si le modèle contient déjà les coefficients symétriques
if (!exists("sym_coef", where = final_model)) {
  cat("Calcul des coefficients symétriques...\n")
  
  # Extraire la matrice des coefficients originaux
  orig_coef <- coef(final_model)
  
  # Récupérer tous les niveaux de la variable réponse
  all_levels <- c(rownames(orig_coef), "bq") # Assurer que "bq" est inclus
  
  # Créer une matrice complète pour les coefficients
  full_coef <- matrix(0, nrow = length(all_levels), ncol = ncol(orig_coef))
  rownames(full_coef) <- all_levels
  colnames(full_coef) <- colnames(orig_coef)
  
  # Remplir la matrice pour les niveaux non de référence
  for (lvl in rownames(orig_coef)) {
    full_coef[lvl, ] <- orig_coef[lvl, ]
  }
  
  # Pour chaque prédicteur, calculer la moyenne des coefficients sur tous les niveaux
  m <- colMeans(full_coef)
  
  # Reparamétrer de manière symétrique
  sym_coef <- full_coef - matrix(rep(m, each = length(all_levels)), nrow = length(all_levels))
  
  # Ajouter les coefficients symétriques au modèle
  final_model$sym_coef <- sym_coef
  
  # Optionnel: Sauvegarder le modèle avec les coefficients symétriques
  saveRDS(final_model, gsub("\\.rds$", "_with_sym_coef.rds", model_path))
  cat("Modèle sauvegardé avec coefficients symétriques\n")
} else {
  cat("Le modèle contient déjà des coefficients symétriques\n")
  sym_coef <- final_model$sym_coef
}

# Définir l'ordre des partis pour l'export (celui attendu par les développeurs)
export_parties <- c("lpc", "gpc", "cpc", "bq", "ndp")

# EXTRACTION DES COEFFICIENTS --------------------------------------------------

# 1. Fichier avec tous les coefficients (sans intercept)
coef_transposed <- sym_coef %>%
  as.data.frame() %>%
  rownames_to_column(var = "Party") %>%
  {t(.[,-1])} %>% # Transposer sans la colonne Party
  as.data.frame() %>%
  set_names(sym_coef %>% rownames()) %>%
  rownames_to_column(var = "Predictor") %>%
  select(Predictor, all_of(export_parties)) %>%
  filter(Predictor != "(Intercept)")

# 2. Fichier des intercepts seulement
intercept_df <- sym_coef %>%
  as.data.frame() %>%
  rownames_to_column(var = "Party") %>%
  select(Party, Intercept = "(Intercept)") %>%
  filter(Party %in% export_parties) %>%
  mutate(Intercept = format(Intercept, scientific = FALSE)) %>%
  arrange(factor(Party, levels = export_parties))

# Exportation des deux fichiers
output_file <- "_SharedFolder_datagotchi_federal_2024/data/modele/coef_matrix_6Regions.xlsx"
write.xlsx(list(
  "Coefficients" = coef_transposed,
  "Intercepts" = intercept_df
), output_file)

cat("Exportation réussie :\n",
    "- Coefficients dans l'onglet 'Coefficients'\n",
    "- Intercepts dans l'onglet 'Intercepts'\n",
    "Fichier :", output_file, "\n")

# En plus, générer le fichier au format attendu par les développeurs
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

# Traiter toutes les variables (sauf l'intercept)
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

# Exporter le résultat au format attendu par les développeurs
output_file_dev <- "_SharedFolder_datagotchi_federal_2024/data/modele/model_coefficients_formatted_with_bq.xlsx"
openxlsx::write.xlsx(results_df, output_file_dev, rowNames = FALSE)

# Générer aussi un fichier intercepts.csv pour le script Python
intercepts_df <- data.frame(
  party = export_parties,
  intercept = sapply(export_parties, function(p) sym_coef[p, "(Intercept)"])
)

# Ajouter les colonnes de projection RTA si elles existent
rta_vars <- vars_to_process[grepl("prediction_", vars_to_process)]
rta_parties <- unique(gsub("^prediction_", "", gsub(":.*$", "", rta_vars)))
for (p in rta_parties) {
  col_name <- paste0("projection_", p)
  intercepts_df[[col_name]] <- 1.0  # Valeur par défaut
}

# Exporter le fichier d'intercepts
intercepts_file <- "_SharedFolder_datagotchi_federal_2024/data/modele/intercepts.csv"
write.csv(intercepts_df, intercepts_file, row.names = FALSE)

cat("\nExportation des fichiers pour les développeurs :\n")
cat("- Format standard:", output_file_dev, "\n")
cat("- Fichier intercepts:", intercepts_file, "\n")

# Vérification des fichiers exportés
cat("\n--- Vérification des coefficients exportés ---\n")
exported_coefs <- openxlsx::read.xlsx(output_file_dev)
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
cat("Export terminé avec succès!\n")
