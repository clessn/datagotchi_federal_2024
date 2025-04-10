# -----------------------------------------------------------------------
# Script de validation de robustesse du modèle RTA avec interactions 6 régions
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
model_path <- "_SharedFolder_datagotchi_federal_2024/data/modele/finalmodel_withRTAPredictions_6Regions_2025-04-09.rds"
final_model <- readRDS(model_path)

# Chargement de la formule
formula_path <- "_SharedFolder_datagotchi_federal_2024/data/modele/formula_finalmodel_withRTAPredictions_6Regions_2025-04-09.rds"
formula_model <- readRDS(formula_path)

# Chargement des données pilote
data_pilot_path <- "_SharedFolder_datagotchi_federal_2024/data/pilote/dataClean/datagotchi2025_canada_pilot_20250322.rds"
DataPilot <- readRDS(data_pilot_path)

# Chargement des données application
data_app_path <- "_SharedFolder_datagotchi_federal_2024/data/app/dataClean/datagotchi2025_canada_app_20250408.rds"
DataApp <- readRDS(data_app_path)

# Chargement des prédictions par RTA
rta_path <- "_SharedFolder_datagotchi_federal_2024/data/modele/rta_predictions_partis.csv"
rta_predictions <- read.csv(rta_path, stringsAsFactors = FALSE)

# Chargement de la configuration de fusion des régions
region_config_path <- "_SharedFolder_datagotchi_federal_2024/data/modele/region_mapping_6Regions_2025-04-09.rds"
region_config <- readRDS(region_config_path)

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

# Définir les 6 régions du Canada
region_levels <- c("ontario", "quebec", "british_columbia", "prairie", "atlantic", "territories")

# Ajouter les variables d'indicateurs régionaux pour les 6 régions
add_region_indicators <- function(df) {
  # Créer les indicateurs régionaux
  df <- df %>%
    mutate(
      region_model = as.character(ses_region),
      # Appliquer la fusion des régions si configurée
      is_ontario = as.numeric(ses_region == "ontario" | (ses_region == "atlantic" & "atlantic" %in% region_config$original_region[region_config$model_region == "ontario"])),
      is_quebec = as.numeric(ses_region == "quebec"),
      is_british_columbia = as.numeric(ses_region == "british_columbia"),
      is_prairie = as.numeric(ses_region == "prairie" | (ses_region == "territories" & "territories" %in% region_config$original_region[region_config$model_region == "prairie"])),
      is_atlantic = as.numeric(ses_region == "atlantic" & !("atlantic" %in% region_config$original_region[region_config$model_region == "ontario"])),
      is_territories = as.numeric(ses_region == "territories" & !("territories" %in% region_config$original_region[region_config$model_region == "prairie"]))
    )
  
  return(df)
}

# Appliquer les indicateurs régionaux
DataPilot_enriched <- add_region_indicators(DataPilot_enriched)
DataApp_enriched <- add_region_indicators(DataApp_enriched)

# Convertir les types de données pour qu'ils soient cohérents
DataPilot_enriched$ses_riding_id <- as.character(DataPilot_enriched$ses_riding_id)
DataApp_enriched$ses_riding_id <- as.character(DataApp_enriched$ses_riding_id)

# Fusionner les données
DataFull <- bind_rows(DataPilot_enriched, DataApp_enriched)

# Filtrer pour ne garder que les observations avec valeurs de vote valides
DataFull_filtered <- DataFull %>%
  filter(dv_voteChoice %in% c("bq", "cpc", "lpc", "ndp", "gpc") & !is.na(dv_voteChoice))

# Identification des variables nécessaires dans la formule
formula_vars <- all.vars(formula_model)
response_var <- formula_vars[1]  # dv_voteChoice
predictor_vars <- formula_vars[-1]  # Toutes les variables prédictives

# Vérifier quelles variables de la formule sont disponibles dans les données
available_vars <- predictor_vars[predictor_vars %in% names(DataFull_filtered)]
missing_vars <- predictor_vars[!predictor_vars %in% names(DataFull_filtered)]

if (length(missing_vars) > 0) {
  cat("ATTENTION: Variables manquantes dans les données:", paste(missing_vars, collapse=", "), "\n")
}

# Sélectionner uniquement les variables nécessaires pour la prédiction
DataValidation <- DataFull_filtered %>%
  select(all_of(c(response_var, available_vars, "source", "ses_region"))) %>%
  drop_na()

cat("Données préparées pour validation. Nombre d'observations:", nrow(DataValidation), "\n")

# ------------------------------------------------------------------------
# 4) Création de la partition pour la validation
# ------------------------------------------------------------------------
set.seed(42)  # Pour reproductibilité

# Stratifier par parti, région et source (comme dans le modèle original)
trainIndex <- createDataPartition(
  interaction(DataValidation$dv_voteChoice, DataValidation$ses_region, DataValidation$source), 
  p = 0.7,  # 70% pour l'entraînement, 30% pour la validation
  list = FALSE
)
validation_data <- DataValidation[-trainIndex, ]

cat("Ensemble de validation créé. Nombre d'observations:", nrow(validation_data), "\n")
cat("Distribution des votes dans l'ensemble de validation:\n")
print(table(validation_data$dv_voteChoice, validation_data$source))

# ------------------------------------------------------------------------
# 5) Validation de la robustesse du modèle
# ------------------------------------------------------------------------
cat("\n=== VALIDATION DE LA ROBUSTESSE DU MODÈLE ===\n")

# 5.1) Prédictions sur l'ensemble de validation
cat("\nCalcul des prédictions...\n")
# Faire des prédictions de classe
pred_class <- predict(final_model, newdata = validation_data, type = "class")
# Faire des prédictions de probabilité
pred_prob <- predict(final_model, newdata = validation_data, type = "probs")

# 5.2) Calcul de l'accuracy globale
accuracy <- mean(pred_class == validation_data$dv_voteChoice, na.rm = TRUE)
cat("\nAccuracy globale:", round(accuracy, 4), "\n")

# 5.3) Matrice de confusion
conf_matrix <- table(Predicted = pred_class, Actual = validation_data$dv_voteChoice)
cat("\nMatrice de confusion:\n")
print(conf_matrix)

# 5.4) Calcul des métriques par parti
parties <- levels(validation_data$dv_voteChoice)
metrics_by_party <- data.frame(
  Party = parties,
  Precision = NA_real_,
  Recall = NA_real_,
  F1_Score = NA_real_,
  Support = NA_integer_
)

for (i in seq_along(parties)) {
  party <- parties[i]
  actual_positives <- validation_data$dv_voteChoice == party
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

# 5.5) Calcul du recall moyen (macro)
macro_recall <- mean(metrics_by_party$Recall, na.rm = TRUE)
cat("\nRecall moyen (macro):", round(macro_recall, 4), "\n")

# 5.6) Calcul du recall pondéré (weighted)
weighted_recall <- sum(metrics_by_party$Recall * metrics_by_party$Support) / sum(metrics_by_party$Support)
cat("Recall pondéré (weighted):", round(weighted_recall, 4), "\n")

# 5.7) Validation du premier ou deuxième choix
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
  if (n == a) {
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

# Corriger l'erreur de typo (a -> 1)
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
top_1_accuracy <- top_n_accuracy(pred_prob, validation_data$dv_voteChoice, n = 1)
cat("\nAccuracy du premier choix (top-1):", round(top_1_accuracy, 4), "\n")

# Calcul de l'accuracy du top-2
top_2_accuracy <- top_n_accuracy(pred_prob, validation_data$dv_voteChoice, n = 2)
cat("Accuracy du premier ou deuxième choix (top-2):", round(top_2_accuracy, 4), "\n")

# 5.8) Analyse par région
# Validation pour chaque région du modèle
regions_model <- c("ontario", "quebec", "british_columbia", "prairie", "atlantic", "territories")
region_metric_df <- data.frame(
  Region = regions_model,
  Observations = NA_integer_,
  Accuracy = NA_real_,
  Top2_Accuracy = NA_real_
)

for (i in seq_along(regions_model)) {
  region <- regions_model[i]
  region_var <- paste0("is_", region)
  region_indices <- validation_data[[region_var]] == 1
  
  if (sum(region_indices) > 0) {
    pred_class_region <- pred_class[region_indices]
    actual_class_region <- validation_data$dv_voteChoice[region_indices]
    
    accuracy_region <- mean(pred_class_region == actual_class_region, na.rm = TRUE)
    
    # Top-2 accuracy pour la région
    pred_prob_region <- pred_prob[region_indices, ]
    top_2_accuracy_region <- top_n_accuracy(pred_prob_region, actual_class_region, n = 2)
    
    # Stocker les métriques
    region_metric_df$Observations[i] <- sum(region_indices)
    region_metric_df$Accuracy[i] <- accuracy_region
    region_metric_df$Top2_Accuracy[i] <- top_2_accuracy_region
    
    cat(sprintf("\nRégion: %s (n=%d) - Accuracy: %.4f, Top-2 Accuracy: %.4f\n", 
                 region, sum(region_indices), accuracy_region, top_2_accuracy_region))
    
    # Afficher également la matrice de confusion par région
    conf_matrix_region <- table(pred_class_region, actual_class_region)
    cat("Matrice de confusion pour", region, ":\n")
    print(conf_matrix_region)
  } else {
    cat("\nRégion:", region, "- Aucune observation\n")
  }
}

# 5.9) Analyse détaillée par région originale (avant fusion)
if ("ses_region" %in% names(validation_data) && is.factor(validation_data$ses_region)) {
  original_regions <- levels(validation_data$ses_region)
  cat("\nAnalyse par région géographique (avant fusion):\n")
  
  for (region in original_regions) {
    region_indices <- validation_data$ses_region == region
    if (sum(region_indices) > 10) {  # Au moins 10 observations pour être significatif
      pred_class_region <- pred_class[region_indices]
      actual_class_region <- validation_data$dv_voteChoice[region_indices]
      
      accuracy_region <- mean(pred_class_region == actual_class_region, na.rm = TRUE)
      
      # Top-2 accuracy pour la région
      pred_prob_region <- pred_prob[region_indices, ]
      top_2_accuracy_region <- top_n_accuracy(pred_prob_region, actual_class_region, n = 2)
      
      cat(sprintf("Région géographique: %s (n=%d) - Accuracy: %.4f, Top-2 Accuracy: %.4f\n", 
                   region, sum(region_indices), accuracy_region, top_2_accuracy_region))
    }
  }
}

# ------------------------------------------------------------------------
# 6) Analyse de la stabilité via bootstrap
# ------------------------------------------------------------------------
cat("\n=== ANALYSE DE LA STABILITÉ DU MODÈLE VIA BOOTSTRAP ===\n")

# Fonction pour calculer l'accuracy sur un échantillon bootstrap
boot_accuracy <- function(data, indices) {
  # Créer un échantillon bootstrap
  boot_sample <- data[indices, ]
  
  # Faire des prédictions
  pred <- predict(final_model, newdata = boot_sample, type = "class")
  
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
  calib <- evaluate_calibration(pred_prob, validation_data$dv_voteChoice, party)
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
             "Recall pondéré (weighted)",
             region_metric_df$Region %>% paste("Accuracy", .)),
  Value = c(accuracy, 
            top_2_accuracy, 
            macro_recall, 
            weighted_recall,
            region_metric_df$Accuracy)
)

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
  party_indices <- validation_data$dv_voteChoice == party
  if (sum(party_indices) > 0) {
    pred_prob_party <- pred_prob[party_indices, ]
    actual_class_party <- validation_data$dv_voteChoice[party_indices]
    top_2_accuracy_party <- top_n_accuracy(pred_prob_party, actual_class_party, n = 2)
    
    party_summary$Top2_Accuracy[party_summary$Party == party] <- top_2_accuracy_party
  }
}

cat("\nRésumé par parti:\n")
print(party_summary)

# Sauvegarder les résultats
results_path <- "_SharedFolder_datagotchi_federal_2024/data/modele/robustness_validation_6regions_results_2025-04-10.rds"
results_list <- list(
  robustness_summary = robustness_summary,
  party_summary = party_summary,
  confusion_matrix = conf_matrix,
  region_metrics = region_metric_df,
  bootstrap_results = bootstrap_results
)
saveRDS(results_list, results_path)
cat("\nRésultats de validation sauvegardés dans:", results_path, "\n")

# Afficher un message final
cat("\n=== VALIDATION DE ROBUSTESSE COMPLÉTÉE ===\n")
cat("Le modèle à 6 régions a été évalué pour sa robustesse et sa capacité à prédire correctement le premier ou deuxième choix.\n")
cat("Les métriques clés sont:\n")
cat("- Accuracy globale:", round(accuracy, 4), "\n")
cat("- Accuracy du premier ou deuxième choix:", round(top_2_accuracy, 4), "\n")
cat("- Recall moyen (macro):", round(macro_recall, 4), "\n")
cat("- Stabilité (intervalle de confiance à 95%):", round(ci$percent[4], 4), "-", round(ci$percent[5], 4), "\n")

