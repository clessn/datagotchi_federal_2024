#' Calcul des contributions pré-calculées des RTA aux prédictions par parti
#' 
#' Ce script calcule les contributions de chaque RTA aux prédictions des partis politiques
#' en utilisant les coefficients du modèle multinomial. Il génère une table de référence 
#' pour accélérer les prédictions en production.
#'
#' Entrée :
#' - Prédictions par RTA (rta_predictions_partis.csv)
#' - Modèle final avec coefficients symétriques (finalmodel_withRTAPredictions_april3_2025-04-16.rds)
#'
#' Sortie :
#' - Table de contributions pré-calculées (rta_precalculated_contributions.csv)

# Chargement des packages nécessaires
library(tidyverse)

# Configuration et chemins de fichiers
model_path <- "_SharedFolder_datagotchi_federal_2024/data/modele/finalmodel_withRTAPredictions_april3_2025-04-16.rds"
rta_pred_path <- "_SharedFolder_datagotchi_federal_2024/data/modele/rta_predictions_partis.csv"
output_path <- "_SharedFolder_datagotchi_federal_2024/data/modele/rta_precalculated_contributions.csv"

# Vérification de l'existence des fichiers
if (!file.exists(model_path)) {
  stop(paste0("Le fichier modèle n'existe pas: ", model_path))
}
if (!file.exists(rta_pred_path)) {
  stop(paste0("Le fichier de prédictions RTA n'existe pas: ", rta_pred_path))
}

# Définir l'ordre des partis (cohérent avec l'application Python)
export_parties <- c("lpc", "gpc", "cpc", "bq", "ndp")

# Chargement des données
cat("Chargement du modèle et des prédictions RTA...\n")
final_model <- readRDS(model_path)
rta_predictions <- read.csv(rta_pred_path, stringsAsFactors = FALSE)

# Standardiser les noms de colonnes pour les prédictions RTA
rta_predictions$rta <- toupper(rta_predictions$rta)

# Transformer les noms pour correspondre aux coefficients du modèle
party_mapping <- c("CPC", "LPC", "NDP", "GPC", "BQ")
names(party_mapping) <- paste0("prediction_", party_mapping)

# Vérification et extraction des coefficients symétriques
if (!exists("sym_coef", where = final_model)) {
  cat("Le modèle ne contient pas de coefficients symétriques. Calcul en cours...\n")
  
  # Extraire les coefficients originaux
  orig_coef <- coef(final_model)
  
  # Récupérer tous les niveaux de la variable réponse
  all_levels <- c(rownames(orig_coef), "bq")  # Supposer que "bq" est la référence
  
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
} else {
  sym_coef <- final_model$sym_coef
  cat("Coefficients symétriques extraits du modèle.\n")
}

# Extraction des coefficients spécifiques aux RTA pour chaque parti
cat("Extraction des coefficients RTA pour chaque parti...\n")

# Identifier les colonnes RTA dans la matrice de coefficients
rta_coef_cols <- grep("^prediction_", colnames(sym_coef), value = TRUE)

if (length(rta_coef_cols) == 0) {
  stop("Aucune colonne de prédiction RTA trouvée dans les coefficients du modèle.")
}

# Créer un dataframe des coefficients RTA pour chaque parti
rta_coefficients <- data.frame(
  party = rownames(sym_coef),
  stringsAsFactors = FALSE
)

for (col in rta_coef_cols) {
  rta_coefficients[[col]] <- sym_coef[, col]
}

# S'assurer que les partis sont en minuscules pour correspondre à l'application Python
rta_coefficients$party <- tolower(rta_coefficients$party)

# Vérification des valeurs manquantes dans les prédictions RTA
missing_rtas <- sum(is.na(rta_predictions[, -1]))
if (missing_rtas > 0) {
  cat("ATTENTION:", missing_rtas, "valeurs manquantes dans les prédictions RTA.\n")
  cat("Ces valeurs seront remplacées par la moyenne de la colonne.\n")
  
  # Remplacer les valeurs NA par la moyenne de la colonne
  for (col in names(rta_predictions)[-1]) {  # Toutes sauf la colonne rta
    mean_val <- mean(rta_predictions[[col]], na.rm = TRUE)
    rta_predictions[[col]][is.na(rta_predictions[[col]])] <- mean_val
  }
}

# Fonction pour calculer la contribution pré-calculée pour chaque RTA
calculate_rta_contribution <- function(rta_row, rta_coefs) {
  # Initialiser un dataframe pour les résultats
  results <- data.frame(
    rta = rta_row$rta,
    party = rta_coefs$party,
    contribution = NA_real_,
    stringsAsFactors = FALSE
  )
  
  # Pour chaque parti, calculer la contribution
  for (i in 1:nrow(rta_coefs)) {
    party_row <- rta_coefs[i, ]
    party_name <- party_row$party
    
    # Initialiser la contribution
    contribution <- 0
    
    # Ajouter la contribution de chaque prédiction de parti
    for (col in rta_coef_cols) {
      # Obtenir le nom de parti correspondant dans le fichier rta_predictions
      party_in_pred <- gsub("prediction_", "", col)
      
      if (party_in_pred %in% names(rta_row)) {
        # Multiplier la prédiction RTA par le coefficient correspondant
        contribution <- contribution + (rta_row[[party_in_pred]] * party_row[[col]])
      } else {
        cat("AVERTISSEMENT: La colonne", party_in_pred, "n'existe pas dans les prédictions RTA\n")
      }
    }
    
    # Stocker dans les résultats
    results[results$party == party_name, "contribution"] <- contribution
  }
  
  return(results)
}

# Pré-calculer la contribution pour chaque RTA
cat("Calcul des contributions pour", nrow(rta_predictions), "RTAs...\n")

rta_contributions <- list()
pb <- txtProgressBar(min = 0, max = nrow(rta_predictions), style = 3)

for (i in 1:nrow(rta_predictions)) {
  rta_row <- rta_predictions[i, ]
  contributions <- calculate_rta_contribution(rta_row, rta_coefficients)
  rta_contributions[[i]] <- contributions
  setTxtProgressBar(pb, i)
}
close(pb)

# Combiner tous les résultats
all_contributions <- do.call(rbind, rta_contributions)

# Mettre en forme large pour faciliter l'utilisation
rta_contribution_wide <- all_contributions %>%
  pivot_wider(
    id_cols = rta,
    names_from = party,
    values_from = contribution
  )

# Assurer que tous les partis sont présents dans l'ordre souhaité
missing_parties <- setdiff(export_parties, names(rta_contribution_wide))
if (length(missing_parties) > 0) {
  cat("AVERTISSEMENT: Les partis suivants manquent et seront ajoutés avec des zéros:", 
      paste(missing_parties, collapse=", "), "\n")
  
  for (party in missing_parties) {
    rta_contribution_wide[[party]] <- 0
  }
}

# Calculer les valeurs par défaut pour les RTA non présentes dans la table
default_contributions <- data.frame(
  rta = "DEFAULT", 
  stringsAsFactors = FALSE
)

# Calculer la moyenne pour chaque parti à utiliser comme valeur par défaut
for (party in unique(all_contributions$party)) {
  mean_contribution <- mean(all_contributions[all_contributions$party == party, "contribution"])
  default_contributions[[party]] <- mean_contribution
}

# Ajouter la ligne par défaut à la table
rta_contribution_wide <- rbind(rta_contribution_wide, default_contributions)

# Réorganiser les colonnes selon l'ordre souhaité pour l'export
rta_contribution_wide <- rta_contribution_wide %>%
  select(rta, all_of(export_parties))

# Vérifier que tous les partis sont présents
for (party in export_parties) {
  if (!(party %in% names(rta_contribution_wide))) {
    cat("ERREUR: Le parti", party, "est manquant dans les données de sortie.\n")
    rta_contribution_wide[[party]] <- 0  # Ajouter une colonne de zéros pour éviter l'échec
  }
}

# Sauvegarder les contributions pré-calculées
write.csv(rta_contribution_wide, output_path, row.names = FALSE)

# Afficher un résumé
cat("\nContributions RTA pré-calculées pour", nrow(rta_predictions), "RTAs\n")
cat("Pour", length(export_parties), "partis:", paste(export_parties, collapse=", "), "\n")
cat("Fichier de sortie:", output_path, "\n")

# Exemple d'utilisation de la table de référence (pour les développeurs)
cat("\n--- Exemple d'utilisation pour les développeurs ---\n")
cat("Pseudocode pour intégrer les contributions RTA dans l'application Python:\n\n")

cat("```python
# Charger la table de contributions RTA
rta_contributions = pd.read_csv('rta_precalculated_contributions.csv')

def get_rta_contribution(rta_code, party):
    # Vérifier si le RTA existe dans la table, sinon utiliser les valeurs par défaut
    rta_row = rta_contributions[rta_contributions['rta'] == rta_code]
    if rta_row.empty:
        rta_row = rta_contributions[rta_contributions['rta'] == 'DEFAULT']
    
    # Récupérer la contribution pour le parti spécifique
    return rta_row[party].values[0]

# Dans le flux de prédiction, après avoir calculé eta à partir des intercepts et coefficients
# Ajouter la contribution RTA pour chaque parti
for party in parties:
    eta[party] += get_rta_contribution(user_rta, party)
```\n")

# Vérification finale des résultats
cat("\n--- Vérification des résultats ---\n")

# Sélectionner quelques RTA au hasard pour vérification
set.seed(123)
sample_rtas <- sample(rta_predictions$rta, min(5, nrow(rta_predictions)))

for (rta in sample_rtas) {
  cat("\nVérification pour le RTA:", rta, "\n")
  
  # Obtenir les prédictions RTA pour ce code
  rta_pred <- rta_predictions[rta_predictions$rta == rta, ]
  
  # Obtenir les contributions calculées
  rta_contrib <- rta_contribution_wide[rta_contribution_wide$rta == rta, ]
  
  # Pour chaque parti, recalculer et comparer
  for (party in export_parties) {
    # Recalculer la contribution
    recalculated <- 0
    party_row <- rta_coefficients[rta_coefficients$party == party, ]
    
    for (col in rta_coef_cols) {
      party_in_pred <- gsub("prediction_", "", col)
      if (party_in_pred %in% names(rta_pred)) {
        recalculated <- recalculated + (rta_pred[[party_in_pred]] * party_row[[col]])
      }
    }
    
    # Comparer avec la valeur pré-calculée
    precalculated <- rta_contrib[[party]]
    
    # Afficher la comparaison
    diff <- abs(recalculated - precalculated)
    status <- ifelse(diff < 1e-10, "OK", "ERREUR")
    
    cat(sprintf("  %s: Recalculé = %.6f, Pré-calculé = %.6f, Diff = %.9f, %s\n", 
               party, recalculated, precalculated, diff, status))
  }
}

cat("\nScript terminé avec succès!\n")