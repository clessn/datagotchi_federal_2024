# -----------------------------------------------------------------------
# Script de validation de robustesse du modèle RTA amélioré (version 2)
# Date: 10 avril 2025
# -----------------------------------------------------------------------

# ------------------------------------------------------------------------
# 1) Chargement des packages
# ------------------------------------------------------------------------
library(tidyverse)
library(caret)
library(nnet)
library(yardstick)
library(pbapply)
library(ROCR)
library(boot)
library(e1071)

# ------------------------------------------------------------------------
# 2) Chargement du modèle et des données
# ------------------------------------------------------------------------
# Chargement du modèle final
model_path <- "_SharedFolder_datagotchi_federal_2024/data/modele/finalmodel_withRTAPredictions_2025-04-15.rds"
final_model <- readRDS(model_path)

# Chargement des dummies
dummies_path <- "_SharedFolder_datagotchi_federal_2024/data/modele/dummies_finalmodel_withRTAPredictions_2025-04-15.rds"
dummies_final <- readRDS(dummies_path)

# Si dummies_final est de type character, on doit créer un objet dummyVars approprié
if (is.character(dummies_final)) {
  cat("Attention: dummies_final est un objet de type character. Création d'un nouvel objet dummyVars.\n")
  # On va créer un nouvel objet dummyVars à partir des données de validation
  library(caret)
  
  # Identifier les variables catégorielles
  cat_vars <- sapply(DataPilot_enriched, is.factor)
  cat("Variables catégorielles identifiées:", sum(cat_vars), "\n")
  
  # Créer un nouvel objet dummyVars
  final_vars <- c(model_variables, "prediction_CPC", "prediction_LPC", "prediction_NDP", "prediction_GPC", "prediction_BQ")
  tmp_data <- DataPilot_enriched[, final_vars, drop = FALSE]
  dummies_final <- dummyVars(~ ., data = tmp_data, fullRank = TRUE)
  cat("Nouvel objet dummyVars créé avec succès\n")
}

# Chargement des données pilote
data_pilot_path <- "_SharedFolder_datagotchi_federal_2024/data/pilote/dataClean/datagotchi2025_canada_pilot_20250310.rds"
DataPilot <- readRDS(data_pilot_path)

# Chargement des données application
data_app_path <- "_SharedFolder_datagotchi_federal_2024/data/app/dataClean/datagotchi2025_canada_app_20250314.rds"
DataApp <- readRDS(data_app_path)

# Chargement des prédictions par RTA
rta_path <- "_SharedFolder_datagotchi_federal_2024/data/modele/rta_predictions_partis.csv"
rta_predictions <- read.csv(rta_path, stringsAsFactors = FALSE)

# Chargement des résultats d'entraînement précédents
results_path <- "_SharedFolder_datagotchi_federal_2024/data/modele/resultsTrainV4_31janvier2025.rds"
results_train <- readRDS(results_path)

cat("Modèle et données chargés avec succès\n")

# ------------------------------------------------------------------------
# 3) Préparation des données pour validation
# ------------------------------------------------------------------------
# Standardisation des facteurs pour dv_voteChoice
all_levels <- c("bq", "cpc", "lpc", "ndp", "gpc")

# Convertir les variables vote en facteurs avec les mêmes niveaux
DataPilot$dv_voteChoice <- factor(
  gsub("npd", "ndp", as.character(DataPilot$dv_voteChoice)),
  levels = all_levels
)

DataApp$dv_voteChoice <- factor(
  gsub("npd", "ndp", as.character(DataApp$dv_voteChoice)),
  levels = all_levels
)

# Récupérer les variables du meilleur modèle
best_id <- results_train %>%
  group_by(model_id) %>%
  summarise(
    score_iter_accuracy = first(accuracy_cv),
    .groups = "drop"
  ) %>%
  arrange(desc(score_iter_accuracy)) %>%
  pull(model_id) %>%
  first()

model_variables <- results_train %>%
  filter(model_id == best_id) %>%
  pull(variable) %>%
  unique()

# Fonction pour standardiser les codes postaux et extraire les RTA
standardize_postal_codes <- function(df) {
  df %>%
    mutate(
      ses_postalCode_clean = toupper(as.character(ses_postalCode)),
      rta = substr(ses_postalCode_clean, 1, 3)
    )
}

# Fonction pour enrichir un dataframe avec les prédictions RTA
enrich_with_predictions <- function(df, rta_preds) {
  # Joindre les prédictions par RTA
  df_enriched <- df %>%
    left_join(rta_preds, by = "rta") %>%
    mutate(
      # Utiliser les valeurs de RTA si disponibles, sinon utiliser la moyenne
      prediction_CPC = ifelse(is.na(CPC), mean(rta_preds$CPC, na.rm = TRUE), CPC),
      prediction_LPC = ifelse(is.na(LPC), mean(rta_preds$LPC, na.rm = TRUE), LPC),
      prediction_NDP = ifelse(is.na(NDP), mean(rta_preds$NDP, na.rm = TRUE), NDP),
      prediction_GPC = ifelse(is.na(GPC), mean(rta_preds$GPC, na.rm = TRUE), GPC),
      prediction_BQ = ifelse(is.na(BQ), mean(rta_preds$BQ, na.rm = TRUE), BQ)
    ) %>%
    select(-CPC, -LPC, -NDP, -GPC, -BQ)
  
  return(df_enriched)
}

# Ajouter une variable source
DataPilot$source <- "pilote"
DataApp$source <- "application"

# Standardiser les codes postaux et extraire les RTA
DataPilot <- standardize_postal_codes(DataPilot)
DataApp <- standardize_postal_codes(DataApp)

# Standardiser les RTA dans le fichier de prédictions
rta_predictions$rta <- toupper(rta_predictions$rta)

# Enrichir les données avec les prédictions RTA
DataPilot_enriched <- enrich_with_predictions(DataPilot, rta_predictions)
DataApp_enriched <- enrich_with_predictions(DataApp, rta_predictions)

# Sélection des variables nécessaires
# Pour DataPilot, filtrer seulement les observations avec un choix de vote valide
DataPilot_selected <- DataPilot_enriched %>%
  filter(dv_voteChoice %in% c("bq", "cpc", "lpc", "ndp", "gpc") & !is.na(dv_voteChoice)) %>%
  select(all_of(c(model_variables, "dv_voteChoice", "source", "ses_region", 
                  "prediction_CPC", "prediction_LPC", "prediction_NDP", "prediction_GPC", "prediction_BQ"))) %>%
  drop_na()

# Pour DataApp, filtrer de la même façon que pour DataPilot
DataApp_selected <- DataApp_enriched %>%
  filter(dv_voteChoice %in% c("bq", "cpc", "lpc", "ndp", "gpc") & !is.na(dv_voteChoice)) %>%
  select(all_of(c(model_variables, "dv_voteChoice", "source", "ses_region", 
                  "prediction_CPC", "prediction_LPC", "prediction_NDP", "prediction_GPC", "prediction_BQ"))) %>%
  drop_na()

# Fusionner les données
DataFull <- bind_rows(DataPilot_selected, DataApp_selected)

cat("Données préparées pour validation. Nombre d'observations:", nrow(DataFull), "\n")

# ------------------------------------------------------------------------
# 4) Création de la partition pour la validation
# ------------------------------------------------------------------------
set.seed(42)  # Pour reproductibilité

# Stratifier par parti et par source (comme dans le modèle original)
trainIndex <- createDataPartition(
  interaction(DataFull$dv_voteChoice, DataFull$source), 
  p = 0.7,  # 70% pour l'entraînement, 30% pour la validation
  list = FALSE
)
validation_data <- DataFull[-trainIndex, ]

cat("Ensemble de validation créé. Nombre d'observations:", nrow(validation_data), "\n")
cat("Distribution des votes dans l'ensemble de validation:\n")
print(table(validation_data$dv_voteChoice, validation_data$source))

# ------------------------------------------------------------------------
# 5) Validation de la robustesse du modèle
# ------------------------------------------------------------------------
cat("\n=== VALIDATION DE LA ROBUSTESSE DU MODÈLE ===\n")

# 5.1) Préparation des données pour les prédictions
# Créer les variables dummy pour les données de validation
# Obtenir les variables directement à partir des termes du modèle
model_terms <- terms(final_model)
final_vars <- c(model_variables, "prediction_CPC", "prediction_LPC", "prediction_NDP", "prediction_GPC", "prediction_BQ")
X_validation <- validation_data[, final_vars, drop = FALSE]
y_validation <- validation_data$dv_voteChoice

# Conversion en variables dummy
tryCatch({
  # Essayer d'utiliser l'objet dummies_final
  X_validation_dummy <- predict(dummies_final, newdata = X_validation) %>% as.data.frame()
  cat("Conversion en variables dummy réussie avec l'objet dummies_final existant\n")
}, error = function(e) {
  cat("Erreur lors de la conversion en variables dummy:", e$message, "\n")
  cat("Création manuelle des variables dummy...\n")
  
  # Créer manuellement les dummy variables
  # Cette approche est plus robuste mais peut ne pas correspondre exactement au modèle original
  
  # Identifier les variables catégorielles
  cat_vars <- sapply(X_validation, is.factor)
  numeric_vars <- sapply(X_validation, is.numeric)
  
  # Pour les variables catégorielles, créer des indicateurs manuellement
  X_dummy_list <- list()
  
  # Ajouter les variables numériques directement
  for (var in names(X_validation)[numeric_vars]) {
    X_dummy_list[[var]] <- X_validation[[var]]
  }
  
  # Pour chaque variable catégorielle, créer des variables indicatrices
  for (var in names(X_validation)[cat_vars]) {
    levels_var <- levels(X_validation[[var]])
    # Ignorer le premier niveau (référence)
    for (lvl in levels_var[-1]) {
      col_name <- paste0(var, "_", lvl)
      X_dummy_list[[col_name]] <- as.numeric(X_validation[[var]] == lvl)
    }
  }
  
  # Convertir la liste en data frame
  X_validation_dummy <- as.data.frame(X_dummy_list)
  
  # Vérifier qu'on a des colonnes dans notre jeu de données transformé
  cat("Dimensions des données transformées:", dim(X_validation_dummy), "\n")
  
  # Vérifier que toutes les variables nécessaires pour le modèle sont présentes
  model_vars <- all.vars(terms(final_model))
  model_vars <- model_vars[model_vars != "dv_voteChoice"] # Exclure la variable dépendante
  
  # Identifier les variables manquantes
  missing_vars <- setdiff(model_vars, names(X_validation_dummy))
  if (length(missing_vars) > 0) {
    cat("ATTENTION: Variables manquantes dans les données transformées:", paste(missing_vars, collapse=", "), "\n")
    cat("Ajout de colonnes de zéros pour les variables manquantes...\n")
    
    # Ajouter des colonnes de zéros pour les variables manquantes
    for (var in missing_vars) {
      X_validation_dummy[[var]] <- 0
    }
  }
  
  cat("Conversion en variables dummy terminée avec méthode alternative\n")
})

# 5.2) Prédictions sur l'ensemble de validation
cat("\nCalcul des prédictions...\n")
# Faire des prédictions de classe
pred_class <- predict(final_model, newdata = X_validation_dummy)
# Faire des prédictions de probabilité
pred_prob <- predict(final_model, newdata = X_validation_dummy, type = "probs")

# 5.3) Calcul de l'accuracy globale
accuracy <- mean(pred_class == y_validation, na.rm = TRUE)
cat("\nAccuracy globale:", round(accuracy, 4), "\n")

# 5.4) Matrice de confusion
conf_matrix <- table(Predicted = pred_class, Actual = y_validation)
cat("\nMatrice de confusion:\n")
print(conf_matrix)

# 5.5) Calcul des métriques par parti
parties <- levels(y_validation)
metrics_by_party <- data.frame(
  Party = parties,
  Precision = NA_real_,
  Recall = NA_real_,
  F1_Score = NA_real_,
  Support = NA_integer_
)

for (i in seq_along(parties)) {
  party <- parties[i]
  actual_positives <- y_validation == party
  predicted_positives <- pred_class == party
  
  # Support (nombre d'observations réelles de cette classe)
  support <- sum(actual_positives)
  
  # True Positives
  tp <- sum(predicted_positives & actual_positives)
  
  # False Positives et False Negatives
  fp <- sum(predicted_positives & !actual_positives)
  fn <- sum(!predicted_positives & actual_positives)
  
  # Calcul Precision et Recall
  precision <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
  recall <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
  
  # Calcul F1 Score
  f1 <- ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
  
  # Stockage des métriques
  metrics_by_party$Precision[i] <- precision
  metrics_by_party$Recall[i] <- recall
  metrics_by_party$F1_Score[i] <- f1
  metrics_by_party$Support[i] <- support
}

cat("\nMétriques par parti:\n")
print(metrics_by_party)

# 5.6) Calcul du recall moyen (macro)
macro_recall <- mean(metrics_by_party$Recall, na.rm = TRUE)
cat("\nRecall moyen (macro):", round(macro_recall, 4), "\n")

# 5.7) Calcul du recall pondéré (weighted)
weighted_recall <- sum(metrics_by_party$Recall * metrics_by_party$Support) / sum(metrics_by_party$Support)
cat("Recall pondéré (weighted):", round(weighted_recall, 4), "\n")

# 5.8) Validation du premier ou deuxième choix
# Fonction pour déterminer si la vraie classe est parmi les N premières prédictions
top_n_accuracy <- function(prob_matrix, actual_classes, n = 2) {
  # Vérifier que prob_matrix est bien un dataframe ou une matrice
  if (!is.data.frame(prob_matrix) && !is.matrix(prob_matrix)) {
    stop("prob_matrix doit être un dataframe ou une matrice")
  }
  
  # Convertir en matrice si c'est un dataframe
  if (is.data.frame(prob_matrix)) {
    prob_matrix <- as.matrix(prob_matrix)
  }
  
  # Pour chaque observation, trier les probabilités et obtenir les n premières classes
  if (n == 1) {
    # Cas spécial pour n=1 (éviter les problèmes de dimensionnalité)
    top_n_classes <- apply(prob_matrix, 1, function(x) {
      names(sort(x, decreasing = TRUE))[1]
    })
    
    # Vérifier si la vraie classe est parmi les n premières prédictions
    correct_in_top_n <- sapply(seq_along(actual_classes), function(i) {
      actual_class <- as.character(actual_classes[i])
      actual_class == top_n_classes[i]
    })
  } else {
    # Cas général pour n > 1
    top_n_classes <- t(apply(prob_matrix, 1, function(x) {
      names(sort(x, decreasing = TRUE))[1:min(n, length(x))]
    }))
    
    # Vérifier si la vraie classe est parmi les n premières prédictions
    correct_in_top_n <- sapply(seq_along(actual_classes), function(i) {
      actual_class <- as.character(actual_classes[i])
      actual_class %in% top_n_classes[i, ]
    })
  }
  
  # Calculer la précision
  mean(correct_in_top_n, na.rm = TRUE)
}

# Calcul de l'accuracy du top-1 (doit être égal à l'accuracy standard)
top_1_accuracy <- top_n_accuracy(pred_prob, y_validation, n = 1)
cat("\nAccuracy du premier choix (top-1):", round(top_1_accuracy, 4), "\n")

# Calcul de l'accuracy du top-2
top_2_accuracy <- top_n_accuracy(pred_prob, y_validation, n = 2)
cat("Accuracy du premier ou deuxième choix (top-2):", round(top_2_accuracy, 4), "\n")

# 5.9) Analyse par région
# Validation pour chaque région disponible
if ("ses_region" %in% names(validation_data) && is.factor(validation_data$ses_region)) {
  regions <- levels(validation_data$ses_region)
  region_metric_df <- data.frame(
    Region = regions,
    Observations = NA_integer_,
    Accuracy = NA_real_,
    Top2_Accuracy = NA_real_
  )
  
  cat("\nAnalyse par région:\n")
  
  for (i in seq_along(regions)) {
    region <- regions[i]
    region_indices <- validation_data$ses_region == region
    
    if (sum(region_indices) > 10) {  # Au moins 10 observations pour être significatif
      pred_class_region <- pred_class[region_indices]
      actual_class_region <- y_validation[region_indices]
      
      accuracy_region <- mean(pred_class_region == actual_class_region, na.rm = TRUE)
      
      # Top-2 accuracy pour la région
      pred_prob_region <- pred_prob[region_indices, ]
      top_2_accuracy_region <- top_n_accuracy(pred_prob_region, actual_class_region, n = 2)
      
      # Stocker les métriques
      region_metric_df$Observations[i] <- sum(region_indices)
      region_metric_df$Accuracy[i] <- accuracy_region
      region_metric_df$Top2_Accuracy[i] <- top_2_accuracy_region
      
      cat(sprintf("Région: %s (n=%d) - Accuracy: %.4f, Top-2 Accuracy: %.4f\n", 
                  region, sum(region_indices), accuracy_region, top_2_accuracy_region))
      
      # Afficher également la matrice de confusion par région
      conf_matrix_region <- table(pred_class_region, actual_class_region)
      cat("Matrice de confusion pour", region, ":\n")
      print(conf_matrix_region)
    } else {
      cat("\nRégion:", region, "- Trop peu d'observations (", sum(region_indices), ")\n")
    }
  }
} else {
  cat("\nAnalyse par région impossible: variable ses_region non disponible ou non facteur\n")
}

# ------------------------------------------------------------------------
# 6) Analyse de la stabilité via bootstrap
# ------------------------------------------------------------------------
cat("\n=== ANALYSE DE LA STABILITÉ DU MODÈLE VIA BOOTSTRAP ===\n")

# Fonction pour calculer l'accuracy sur un échantillon bootstrap
boot_accuracy <- function(data, indices) {
  # Créer un échantillon bootstrap
  boot_sample <- data[indices, ]
  
  # Préparer les données pour la prédiction
  X_boot <- boot_sample[, final_vars, drop = FALSE]
  
  # Conversion en variables dummy avec gestion d'erreur
  X_boot_dummy <- tryCatch({
    # Essayer d'utiliser l'objet dummies_final
    predict(dummies_final, newdata = X_boot) %>% as.data.frame()
  }, error = function(e) {
    # Méthode alternative si le predict échoue
    # Identifier les variables catégorielles
    cat_vars <- sapply(X_boot, is.factor)
    numeric_vars <- sapply(X_boot, is.numeric)
    
    # Pour les variables catégorielles, créer des indicateurs manuellement
    X_dummy_list <- list()
    
    # Ajouter les variables numériques directement
    for (var in names(X_boot)[numeric_vars]) {
      X_dummy_list[[var]] <- X_boot[[var]]
    }
    
    # Pour chaque variable catégorielle, créer des variables indicatrices
    for (var in names(X_boot)[cat_vars]) {
      if (length(levels(X_boot[[var]])) > 1) {
        levels_var <- levels(X_boot[[var]])
        # Ignorer le premier niveau (référence)
        for (lvl in levels_var[-1]) {
          col_name <- paste0(var, "_", lvl)
          X_dummy_list[[col_name]] <- as.numeric(X_boot[[var]] == lvl)
        }
      }
    }
    
    # Convertir la liste en data frame
    X_dummy <- as.data.frame(X_dummy_list)
    
    # Vérifier que toutes les variables nécessaires pour le modèle sont présentes
    model_vars <- all.vars(terms(final_model))
    model_vars <- model_vars[model_vars != "dv_voteChoice"] # Exclure la variable dépendante
    
    # Identifier les variables manquantes
    missing_vars <- setdiff(model_vars, names(X_dummy))
    if (length(missing_vars) > 0) {
      # Ajouter des colonnes de zéros pour les variables manquantes
      for (var in missing_vars) {
        X_dummy[[var]] <- 0
      }
    }
    
    return(X_dummy)
  })
  
  # Faire des prédictions
  pred <- predict(final_model, newdata = X_boot_dummy, type = "class")
  
  # Calculer l'accuracy
  mean(pred == boot_sample$dv_voteChoice, na.rm = TRUE)
}

# Exécuter le bootstrap avec 100 répétitions
set.seed(123)
cat("Calcul de la stabilité via bootstrap (100 répétitions)...\n")
# Limiter à un sous-échantillon pour la rapidité si nécessaire
if (nrow(validation_data) > 1000) {
  set.seed(456)
  bootstrap_sample <- validation_data[sample(nrow(validation_data), 1000), ]
} else {
  bootstrap_sample <- validation_data
}

bootstrap_results <- boot(bootstrap_sample, boot_accuracy, R = 100)

# Afficher les résultats
cat("\nAccuracy moyenne (bootstrap):", round(mean(bootstrap_results$t), 4), "\n")
cat("Écart-type de l'accuracy (bootstrap):", round(sd(bootstrap_results$t), 4), "\n")
ci <- boot.ci(bootstrap_results, type = "perc")
cat("Intervalle de confiance à 95% pour l'accuracy:", 
    round(ci$percent[4], 4), "-", round(ci$percent[5], 4), "\n")

# ------------------------------------------------------------------------
# 7) Analyse de la calibration du modèle
# ------------------------------------------------------------------------
cat("\n=== ANALYSE DE LA CALIBRATION DU MODÈLE ===\n")

# Fonction pour évaluer la calibration pour chaque parti
evaluate_calibration <- function(probabilities, actual, party) {
  # Obtenir les probabilités prédites pour ce parti
  pred_prob <- probabilities[, party]
  
  # Vérifier si la prédiction est correcte (1) ou non (0)
  actual_binary <- as.numeric(actual == party)
  
  # Créer 10 bins de probabilité
  bins <- cut(pred_prob, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)
  
  # Calculer la fréquence observée dans chaque bin
  calibration <- aggregate(actual_binary, by = list(bins), FUN = mean)
  names(calibration) <- c("Probability_Bin", "Observed_Frequency")
  
  # Calculer le nombre d'observations dans chaque bin
  bin_counts <- table(bins)
  calibration$Count <- as.numeric(bin_counts[match(calibration$Probability_Bin, names(bin_counts))])
  
  # Calculer la probabilité moyenne prédite dans chaque bin
  avg_predicted <- aggregate(pred_prob, by = list(bins), FUN = mean)
  calibration$Avg_Predicted_Prob <- avg_predicted$x
  
  return(calibration)
}

# Analyser la calibration pour chaque parti
for (party in parties) {
  cat("\nCalibration pour le parti", party, ":\n")
  calib <- evaluate_calibration(pred_prob, y_validation, party)
  print(calib)
}

# ------------------------------------------------------------------------
# 8) Résumé des métriques et sauvegarde des résultats
# ------------------------------------------------------------------------
cat("\n=== RÉSUMÉ DE LA VALIDATION DE ROBUSTESSE ===\n")

# Créer un dataframe de résumé
robustness_summary <- data.frame(
  Metric = c("Accuracy globale", 
             "Accuracy premier ou deuxième choix", 
             "Recall moyen (macro)", 
             "Recall pondéré (weighted)")
)

# Ajouter les valeurs calculées
robustness_summary$Value <- c(accuracy, 
                             top_2_accuracy, 
                             macro_recall, 
                             weighted_recall)

# Afficher le résumé
cat("\nRésumé des métriques de robustesse:\n")
print(robustness_summary)

# Créer un résumé par parti
party_summary <- metrics_by_party %>%
  select(Party, Precision, Recall, F1_Score, Support) %>%
  mutate(
    Top2_Accuracy = NA_real_  # À remplir manuellement pour chaque parti
  )

# Calculer le Top-2 Accuracy pour chaque parti
for (party in parties) {
  party_indices <- y_validation == party
  if (sum(party_indices) > 0) {
    pred_prob_party <- pred_prob[party_indices, ]
    actual_class_party <- y_validation[party_indices]
    top_2_accuracy_party <- top_n_accuracy(pred_prob_party, actual_class_party, n = 2)
    
    party_summary$Top2_Accuracy[party_summary$Party == party] <- top_2_accuracy_party
  }
}

cat("\nRésumé par parti:\n")
print(party_summary)

# Sauvegarder les résultats
results_path <- "_SharedFolder_datagotchi_federal_2024/data/modele/robustness_validation_model_upgrade2_results_2025-04-10.rds"
results_list <- list(
  robustness_summary = robustness_summary,
  party_summary = party_summary,
  confusion_matrix = conf_matrix,
  region_metrics = if(exists("region_metric_df")) region_metric_df else NULL,
  bootstrap_results = bootstrap_results
)
saveRDS(results_list, results_path)
cat("\nRésultats de validation sauvegardés dans:", results_path, "\n")

# Afficher un message final
cat("\n=== VALIDATION DE ROBUSTESSE COMPLÉTÉE ===\n")
cat("Le modèle a été évalué pour sa robustesse et sa capacité à prédire correctement le premier ou deuxième choix.\n")
cat("Les métriques clés sont:\n")
cat("- Accuracy globale:", round(accuracy, 4), "\n")
cat("- Accuracy du premier ou deuxième choix:", round(top_2_accuracy, 4), "\n")
cat("- Recall moyen (macro):", round(macro_recall, 4), "\n")
cat("- Stabilité (intervalle de confiance à 95%):", round(ci$percent[4], 4), "-", round(ci$percent[5], 4), "\n")