# -----------------------------------------------------------------------
# Script de validation de robustesse du modèle RTA upgrade 2
# Date: 10 avril 2025
# Auteur: Claude
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

# Si dummies_final est de type character, on va créer un nouvel objet dummyVars
if(is.character(dummies_final)) {
  cat("dummies_final est un caractère, création d'un nouvel objet dummyVars...\n")
  # On devra reconstruire l'objet dummyVars plus tard après préparation des données
}

# Chargement des données pilote
data_pilot_path <- "_SharedFolder_datagotchi_federal_2024/data/pilote/dataClean/datagotchi2025_canada_pilot_20250322.rds"
DataPilot <- readRDS(data_pilot_path)

# Chargement des données application
data_app_path <- "_SharedFolder_datagotchi_federal_2024/data/app/dataClean/_previous/datagotchi2025_canada_app_20250314.rds"
DataApp <- readRDS(data_app_path)

# Chargement des prédictions par RTA
rta_path <- "_SharedFolder_datagotchi_federal_2024/data/modele/rta_predictions_partis.csv"
rta_predictions <- read.csv(rta_path, stringsAsFactors = FALSE)

# Chargement des résultats d'entraînement précédents (pour récupérer les variables)
results_path <- "_SharedFolder_datagotchi_federal_2024/data/modele/_previous/resultsTrainV4_31janvier2025.rds"
results_train <- readRDS(results_path)

cat("Modèle et données chargés avec succès\n")

# ------------------------------------------------------------------------
# 3) Identifications des variables du modèle
# ------------------------------------------------------------------------
# Identifier le meilleur modèle selon l'accuracy
best_iterations <- results_train %>%
  group_by(model_id) %>%
  summarise(
    score_iter_accuracy = first(accuracy_cv),
    .groups = "drop"
  ) %>%
  arrange(desc(score_iter_accuracy))

best_id <- best_iterations$model_id[1]
cat("Meilleur modèle trouvé (ID) =", best_id, "\n")

# Récupérer les variables du meilleur modèle
best_config <- results_train %>%
  filter(model_id == best_id)

model_variables <- unique(best_config$variable)
cat("Variables du modèle précédent:", length(model_variables), "\n")

# ------------------------------------------------------------------------
# 4) Préparation des données pour validation
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

# Harmonisation des facteurs
harmonize_factor <- function(x_pilot, x_app) {
  # Convertir en caractères pour s'assurer de la compatibilité
  x_pilot_char <- as.character(x_pilot)
  x_app_char <- as.character(x_app)
  
  # Identifier tous les niveaux uniques
  all_levels <- unique(c(x_pilot_char, x_app_char))
  
  # Reconvertir en facteurs avec les mêmes niveaux
  x_pilot_new <- factor(x_pilot_char, levels = all_levels)
  x_app_new <- factor(x_app_char, levels = all_levels)
  
  return(list(pilot = x_pilot_new, app = x_app_new))
}

# Identifier les facteurs dans le modèle et les harmoniser
model_factor_vars <- model_variables[model_variables %in% names(DataPilot_enriched)]
pilot_factors <- names(DataPilot_enriched)[sapply(DataPilot_enriched, is.factor)]
model_factors <- intersect(model_factor_vars, pilot_factors)

for (f in model_factors) {
  if (f %in% names(DataPilot_enriched) && f %in% names(DataApp_enriched)) {
    cat("Harmonisation du facteur:", f, "\n")
    
    # Vérifier et harmoniser
    harmonized <- harmonize_factor(DataPilot_enriched[[f]], DataApp_enriched[[f]])
    DataPilot_enriched[[f]] <- harmonized$pilot
    DataApp_enriched[[f]] <- harmonized$app
  }
}

# Variables finales à utiliser pour le modèle
final_vars <- c(model_variables, "prediction_CPC", "prediction_LPC", "prediction_NDP", "prediction_GPC", "prediction_BQ")

# Sélection des variables nécessaires
# Pour DataPilot, filtrer seulement les observations avec un choix de vote valide
DataPilot_selected <- DataPilot_enriched %>%
  filter(dv_voteChoice %in% all_levels & !is.na(dv_voteChoice)) %>%
  select(all_of(c(final_vars, "dv_voteChoice", "source", "ses_region"))) %>%
  drop_na()

# Pour DataApp, filtrer de la même façon que pour DataPilot
DataApp_selected <- DataApp_enriched %>%
  filter(dv_voteChoice %in% all_levels & !is.na(dv_voteChoice)) %>%
  select(all_of(c(final_vars, "dv_voteChoice", "source", "ses_region"))) %>%
  drop_na()

# Fusionner les données
DataFull <- bind_rows(DataPilot_selected, DataApp_selected)

cat("Données préparées pour validation. Nombre d'observations:", nrow(DataFull), "\n")

# ------------------------------------------------------------------------
# 5) Création de la partition pour la validation
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
# 6) Validation de la robustesse du modèle
# ------------------------------------------------------------------------
cat("\n=== VALIDATION DE LA ROBUSTESSE DU MODÈLE ===\n")

# 6.1) Préparation des données pour les prédictions
# Extraire les X pour la validation
X_validation <- validation_data[, final_vars, drop = FALSE]
y_validation <- validation_data$dv_voteChoice

# Créer les variables dummy avec dummyVars
cat("Création des variables dummy pour la validation...\n")

# Si dummies_final est de type character, on doit créer un nouvel objet dummyVars
if(is.character(dummies_final) || !inherits(dummies_final, "dummyVars")) {
  cat("Recréation de l'objet dummyVars...\n")
  # Créer l'objet dummyVars avec les données validées
  dummies_final <- dummyVars(" ~ .", data = X_validation, fullRank = TRUE, sep = "_")
  cat("Nouvel objet dummyVars créé\n")
}

# Génération des variables dummy
X_validation_dummy <- predict(dummies_final, newdata = X_validation) %>% as.data.frame()
cat("Dimensions des données dummy pour validation:", dim(X_validation_dummy), "\n")

# 6.2) Prédictions sur l'ensemble de validation
cat("\nCalcul des prédictions...\n")

# Vérifier les noms des variables du modèle
model_var_names <- colnames(coef(final_model))
cat("Nombre de variables dans le modèle:", length(model_var_names), "\n")

# Vérifier les noms des variables dans X_validation_dummy
dummy_var_names <- colnames(X_validation_dummy)
cat("Nombre de variables dans les données dummy:", length(dummy_var_names), "\n")

# Identifier les variables manquantes
missing_vars <- setdiff(model_var_names, dummy_var_names)
if(length(missing_vars) > 0) {
  cat("Variables manquantes dans les données dummy:", length(missing_vars), "\n")
  
  # Ajouter les variables manquantes avec des valeurs de 0
  for(var in missing_vars) {
    X_validation_dummy[[var]] <- 0
    cat("Ajout de la variable", var, "avec des zéros\n")
  }
}

# Vérifier les variables supplémentaires qui pourraient poser problème
extra_vars <- setdiff(dummy_var_names, model_var_names)
if(length(extra_vars) > 0) {
  cat("Variables supplémentaires dans les données dummy:", length(extra_vars), "\n")
  # On garde ces variables car elles ne devraient pas poser de problème pour la prédiction
}

# Faire des prédictions de classe
pred_class <- predict(final_model, newdata = X_validation_dummy, type = "class")
# Faire des prédictions de probabilité
pred_prob <- predict(final_model, newdata = X_validation_dummy, type = "probs")

# 6.3) Calcul de l'accuracy globale
accuracy <- mean(pred_class == y_validation, na.rm = TRUE)
cat("\nAccuracy globale:", round(accuracy, 4), "\n")

# 6.4) Matrice de confusion
conf_matrix <- table(Predicted = pred_class, Actual = y_validation)
cat("\nMatrice de confusion:\n")
print(conf_matrix)

# 6.5) Calcul des métriques par parti
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

# 6.6) Calcul du recall moyen (macro)
macro_recall <- mean(metrics_by_party$Recall, na.rm = TRUE)
cat("\nRecall moyen (macro):", round(macro_recall, 4), "\n")

# 6.7) Calcul du recall pondéré (weighted)
weighted_recall <- sum(metrics_by_party$Recall * metrics_by_party$Support) / sum(metrics_by_party$Support)
cat("Recall pondéré (weighted):", round(weighted_recall, 4), "\n")

# 6.8) Validation du premier ou deuxième choix
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

# 6.9) Analyse par région
# Vérifier que ses_region est présent et est un facteur
if ("ses_region" %in% names(validation_data)) {
  # S'assurer que c'est un facteur
  if (!is.factor(validation_data$ses_region)) {
    validation_data$ses_region <- factor(validation_data$ses_region)
  }
  
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
  cat("\nAnalyse par région impossible: variable ses_region non disponible\n")
}

# ------------------------------------------------------------------------
# 7) Analyse de la stabilité via bootstrap
# ------------------------------------------------------------------------
cat("\n=== ANALYSE DE LA STABILITÉ DU MODÈLE VIA BOOTSTRAP ===\n")

# Fonction pour calculer l'accuracy sur un échantillon bootstrap
boot_accuracy <- function(data, indices) {
  # Créer un échantillon bootstrap
  boot_sample <- data[indices, ]
  
  # Préparer les données pour la prédiction
  X_boot <- boot_sample[, final_vars, drop = FALSE]
  
  # Vérifier que dummies_final est bien un objet dummyVars
  if(is.character(dummies_final) || !inherits(dummies_final, "dummyVars")) {
    X_boot_dummy <- model.matrix(~ ., data = X_boot)[, -1] %>% as.data.frame()
  } else {
    X_boot_dummy <- predict(dummies_final, newdata = X_boot) %>% as.data.frame()
  }
  
  # Vérifier et ajouter les variables manquantes requises par le modèle
  model_var_names <- colnames(coef(final_model))
  dummy_var_names <- colnames(X_boot_dummy)
  missing_vars <- setdiff(model_var_names, dummy_var_names)
  
  # Ajouter les variables manquantes
  for(var in missing_vars) {
    X_boot_dummy[[var]] <- 0
  }
  
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
# 8) Analyse de la calibration du modèle
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
# 9) Résumé des métriques et sauvegarde des résultats
# ------------------------------------------------------------------------
cat("\n=== RÉSUMÉ DE LA VALIDATION DE ROBUSTESSE ===\n")

# Créer un dataframe de résumé
robustness_summary <- data.frame(
  Metric = c("Accuracy globale", 
             "Accuracy premier ou deuxième choix", 
             "Recall moyen (macro)", 
             "Recall pondéré (weighted)"),
  Value = c(accuracy, 
            top_2_accuracy, 
            macro_recall, 
            weighted_recall)
)

# Ajouter les résultats par région si disponibles
if (exists("region_metric_df")) {
  region_results <- data.frame(
    Metric = paste("Accuracy", region_metric_df$Region),
    Value = region_metric_df$Accuracy
  )
  robustness_summary <- rbind(robustness_summary, region_results)
}

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
cat("Le modèle upgrade 2 a été évalué pour sa robustesse et sa capacité à prédire correctement le premier ou deuxième choix.\n")
cat("Les métriques clés sont:\n")
cat("- Accuracy globale:", round(accuracy, 4), "\n")
cat("- Accuracy du premier ou deuxième choix:", round(top_2_accuracy, 4), "\n")
cat("- Recall moyen (macro):", round(macro_recall, 4), "\n")
cat("- Stabilité (intervalle de confiance à 95%):", round(ci$percent[4], 4), "-", round(ci$percent[5], 4), "\n")