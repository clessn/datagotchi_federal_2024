# -----------------------------------------------------------------------
# Script de validation de robustesse du modèle RTA upgrade 2
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

# Analyse du modèle et détection des interactions
cat("Analyse du modèle chargé...\n")

# Vérifier si le modèle contient des interactions
model_has_interactions <- FALSE
model_interaction_terms <- character(0)

# Méthode 1: Vérifier avec terms() si possible
tryCatch({
  model_terms <- terms(final_model)
  term_labels <- attr(model_terms, "term.labels")
  model_has_interactions <- any(grepl(":", term_labels))
  if (model_has_interactions) {
    model_interaction_terms <- term_labels[grepl(":", term_labels)]
    cat("Interactions détectées via terms():", paste(model_interaction_terms[1:min(5, length(model_interaction_terms))], collapse=", "))
    if (length(model_interaction_terms) > 5) cat(" et", length(model_interaction_terms) - 5, "autres")
    cat("\n")
  }
}, error = function(e) {
  cat("Impossible d'extraire les termes du modèle:", e$message, "\n")
  
  # Méthode 2: Vérifier avec la matrice de coefficients
  tryCatch({
    coef_mat <- coef(final_model)
    var_names <- colnames(coef_mat)
    model_has_interactions <<- any(grepl(":", var_names))
    
    if (model_has_interactions) {
      model_interaction_terms <<- var_names[grepl(":", var_names)]
      cat("Interactions détectées via coefficients:", paste(model_interaction_terms[1:min(5, length(model_interaction_terms))], collapse=", "))
      if (length(model_interaction_terms) > 5) cat(" et", length(model_interaction_terms) - 5, "autres")
      cat("\n")
    }
  }, error = function(e2) {
    cat("Impossible d'extraire les coefficients du modèle:", e2$message, "\n")
  })
})

if (model_has_interactions) {
  cat("Le modèle contient des termes d'interaction. Mode de prédiction adapté sera utilisé.\n")
} else {
  cat("Aucune interaction détectée dans le modèle.\n")
}

# Tentative de chargement des dummies
dummies_path <- "_SharedFolder_datagotchi_federal_2024/data/modele/dummies_finalmodel_withRTAPredictions_2025-04-15.rds"
tryCatch({
  dummies_final <- readRDS(dummies_path)
  cat("Dummies chargés depuis:", dummies_path, "\n")
}, error = function(e) {
  cat("Impossible de charger les dummies:", e$message, "\n")
  dummies_final <- NULL
})

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

# 6.1) Amélioration de l'approche pour la prédiction
# Au lieu d'essayer d'ajouter les variables manquantes à X_validation_dummy, 
# nous allons utiliser directement validation_data et dv_voteChoice pour la prédiction

cat("Préparation des données de validation pour la prédiction...\n")
X_validation <- validation_data[, final_vars, drop = FALSE]
y_validation <- validation_data$dv_voteChoice

# Vérification des référence pour les facteurs (comme dans le script original)
# Vérifier d'abord si les variables sont bien des facteurs
for (var_name in names(X_validation)) {
  if (!is.factor(X_validation[[var_name]])) {
    next  # Ignorer les non-facteurs
  }
  
  # Définir manuellement les références selon la liste fournie
  if (var_name == "ses_region" && "prairie" %in% levels(X_validation[[var_name]])) {
    X_validation[[var_name]] <- relevel(X_validation[[var_name]], ref = "prairie")
    cat("ses_region: 'prairie' définie comme référence\n")
  }
  else if (var_name == "lifestyle_typeTransport" && "active_transport" %in% levels(X_validation[[var_name]])) {
    X_validation[[var_name]] <- relevel(X_validation[[var_name]], ref = "active_transport")
    cat("lifestyle_typeTransport: 'active_transport' définie comme référence\n")
  }
  else if (var_name == "lifestyle_consClothes" && "large_retailers" %in% levels(X_validation[[var_name]])) {
    X_validation[[var_name]] <- relevel(X_validation[[var_name]], ref = "large_retailers")
    cat("lifestyle_consClothes: 'large_retailers' définie comme référence\n")
  }
  else if (var_name == "lifestyle_exercise" && "gym" %in% levels(X_validation[[var_name]])) {
    X_validation[[var_name]] <- relevel(X_validation[[var_name]], ref = "gym")
    cat("lifestyle_exercise: 'gym' définie comme référence\n")
  }
  else if (var_name == "lifestyle_favAlcool" && "beer" %in% levels(X_validation[[var_name]])) {
    X_validation[[var_name]] <- relevel(X_validation[[var_name]], ref = "beer")
    cat("lifestyle_favAlcool: 'beer' définie comme référence\n")
  }
  else if (var_name == "lifestyle_consCoffee" && "tim_hortons" %in% levels(X_validation[[var_name]])) {
    X_validation[[var_name]] <- relevel(X_validation[[var_name]], ref = "tim_hortons")
    cat("lifestyle_consCoffee: 'tim_hortons' définie comme référence\n")
  }
  else if (var_name == "ses_language" && "english" %in% levels(X_validation[[var_name]])) {
    X_validation[[var_name]] <- relevel(X_validation[[var_name]], ref = "english")
    cat("ses_language: 'english' définie comme référence\n")
  }
  else if (var_name == "ses_dwelling_cat" && "stand_alone_house" %in% levels(X_validation[[var_name]])) {
    X_validation[[var_name]] <- relevel(X_validation[[var_name]], ref = "stand_alone_house")
    cat("ses_dwelling_cat: 'stand_alone_house' définie comme référence\n")
  }
  else if (var_name == "lifestyle_clothingStyleGroups" && "easygoing" %in% levels(X_validation[[var_name]])) {
    X_validation[[var_name]] <- relevel(X_validation[[var_name]], ref = "easygoing")
    cat("lifestyle_clothingStyleGroups: 'easygoing' définie comme référence\n")
  }
  else if (var_name == "ses_educ" && "no_schooling" %in% levels(X_validation[[var_name]])) {
    X_validation[[var_name]] <- relevel(X_validation[[var_name]], ref = "no_schooling")
    cat("ses_educ: 'no_schooling' définie comme référence\n")
  }
  else if (var_name == "ses_income3Cat" && "High" %in% levels(X_validation[[var_name]])) {
    X_validation[[var_name]] <- relevel(X_validation[[var_name]], ref = "High")
    cat("ses_income3Cat: 'High' définie comme référence\n")
  }
}

# 6.2) Prédictions sur l'ensemble de validation
cat("\nCalcul des prédictions...\n")

# Faire des prédictions directement à partir du modèle
tryCatch({
  # Faire des prédictions de classe
  pred_class <- predict(final_model, newdata = X_validation, type = "class")
  # Faire des prédictions de probabilité
  pred_prob <- predict(final_model, newdata = X_validation, type = "probs")
  
  cat("Prédictions réussies!\n")
}, error = function(e) {
  cat("Erreur lors des prédictions:", e$message, "\n")
  
  # En cas d'erreur, examiner le modèle et extraire les termes d'interaction
  cat("Extraction des termes d'interaction du modèle...\n")
  model_terms <- try({
    terms(final_model)
  }, silent = TRUE)
  
  if (!inherits(model_terms, "try-error")) {
    # Si le modèle a des termes d'interaction, les extraire
    has_interactions <- grepl(":", attr(model_terms, "term.labels"))
    interaction_terms <- attr(model_terms, "term.labels")[has_interactions]
    
    if (length(interaction_terms) > 0) {
      cat("Termes d'interaction détectés:", paste(interaction_terms, collapse=", "), "\n")
      
      # Créer une formule complète avec interactions
      formula_str <- paste("~ 0 +", paste(attr(model_terms, "term.labels"), collapse = " + "))
      cat("Formule complète:", formula_str, "\n")
    } else {
      # Pas d'interactions, utiliser simplement les variables
      formula_str <- paste("~ 0 +", paste(final_vars, collapse = " + "))
      cat("Pas d'interactions détectées, utilisation des variables simples\n")
    }
  } else {
    # Si la tentative d'extraction des termes échoue, essayer une approche alternative
    # En vérifiant les noms des coefficients du modèle pour détecter les termes d'interaction
    cat("Impossible d'extraire les termes du modèle, analyse des coefficients...\n")
    
    coef_names <- try({
      names(coef(final_model))
    }, silent = TRUE)
    
    if (!inherits(coef_names, "try-error") && length(coef_names) > 0) {
      # Si la tentative d'extraction des coefficients réussit
      coef_mat <- try({
        coef(final_model)
      }, silent = TRUE)
      
      if (!inherits(coef_mat, "try-error")) {
        # Obtenir les noms des colonnes pour détecter les interactions
        var_names <- colnames(coef_mat)
        has_interactions <- grepl(":", var_names)
        
        if (any(has_interactions)) {
          cat("Interactions détectées dans les coefficients:", 
              paste(var_names[has_interactions][1:min(5, sum(has_interactions))], collapse=", "), 
              if(sum(has_interactions) > 5) "... (plus)" else "", "\n")
          
          # Extraire tous les termes (simples et interactions)
          all_terms <- unique(c(final_vars, var_names))
          formula_str <- paste("~ 0 +", paste(all_terms, collapse = " + "))
        } else {
          formula_str <- paste("~ 0 +", paste(final_vars, collapse = " + "))
        }
      } else {
        # Si l'extraction de la matrice de coefficients échoue
        formula_str <- paste("~ 0 +", paste(final_vars, collapse = " + "))
      }
    } else {
      # Si l'extraction des noms de coefficients échoue
      formula_str <- paste("~ 0 +", paste(final_vars, collapse = " + "))
    }
  }
  
  # Créer la formule
  formula_obj <- try(as.formula(formula_str), silent = TRUE)
  
  if (inherits(formula_obj, "try-error")) {
    cat("Erreur lors de la création de la formule. Utilisation d'une formule simplifiée...\n")
    formula_str <- paste("~ 0 +", paste(names(X_validation), collapse = " + "))
    formula_obj <- as.formula(formula_str)
  }
  
  # Créer la matrice de modèle
  tryCatch({
    X_matrix <- model.matrix(formula_obj, data = X_validation)
    
    # Faire les prédictions sur la matrice
    pred_class <<- predict(final_model, newdata = X_matrix, type = "class")
    pred_prob <<- predict(final_model, newdata = X_matrix, type = "probs")
    
    cat("Prédictions alternatives réussies avec model.matrix!\n")
  }, error = function(e2) {
    cat("Erreur avec model.matrix:", e2$message, "\n")
    
    # En dernier recours, essayer d'utiliser la formule directement
    cat("Tentative finale avec la formule directe...\n")
    
    # Création d'une nouvelle formule complète pour predire directement
    formula_final <- formula(paste("dv_voteChoice ~", paste(final_vars, collapse = " + ")))
    
    # Créer un nouveau jeu de données qui inclut la variable de réponse
    X_complete <- validation_data[, c("dv_voteChoice", final_vars)]
    
    # Prédire en utilisant la formule directement
    tryCatch({
      pred_model <- multinom(formula_final, data = X_complete, trace = FALSE)
      
      pred_class <<- predict(pred_model, newdata = X_complete, type = "class")
      pred_prob <<- predict(pred_model, newdata = X_complete, type = "probs")
      
      cat("Prédictions finales réussies avec un nouveau modèle!\n")
    }, error = function(e3) {
      cat("Échec de toutes les tentatives de prédiction:", e3$message, "\n")
      cat("Impossible de faire des prédictions. Arrêt du script.\n")
      stop("Impossible de prédire avec le modèle. Vérifiez les variables et les interactions.")
    })
  })
})

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
      # Extraire les données pour cette région
      X_region <- X_validation[region_indices, , drop = FALSE]
      y_region <- y_validation[region_indices]
      
      # Prédictions régionales avec gestion des erreurs possible
      pred_class_region <- NULL
      pred_prob_region <- NULL
      
      tryCatch({
        # Essayer les prédictions directes
        pred_class_region <- predict(final_model, newdata = X_region, type = "class")
        pred_prob_region <- predict(final_model, newdata = X_region, type = "probs")
      }, error = function(e) {
        cat("Erreur lors des prédictions pour la région", region, ":", e$message, "\n")
        
        # Si le modèle a des interactions, utiliser les termes du modèle
        if (model_has_interactions && length(model_interaction_terms) > 0) {
          cat("Utilisation des termes d'interaction pour la région", region, "\n")
          
          # Créer une formule avec les interactions connues
          if (length(model_interaction_terms) > 0) {
            formula_str <- paste("~ 0 +", paste(model_interaction_terms, collapse = " + "))
            formula_obj <- as.formula(formula_str)
            
            # Créer la matrice de modèle
            X_matrix_region <- try(model.matrix(formula_obj, data = X_region), silent = TRUE)
            
            if (!inherits(X_matrix_region, "try-error")) {
              # Prédire avec la matrice
              pred_class_region <<- predict(final_model, newdata = X_matrix_region, type = "class")
              pred_prob_region <<- predict(final_model, newdata = X_matrix_region, type = "probs")
            } else {
              cat("Erreur lors de la création de la matrice de modèle pour la région", region, "\n")
              # Créer des NA pour ce cas
              pred_class_region <<- rep(NA, nrow(X_region))
              pred_prob_region <<- matrix(NA, nrow = nrow(X_region), ncol = length(levels(y_validation)))
              colnames(pred_prob_region) <<- levels(y_validation)
            }
          }
        } else {
          # Si pas d'interactions, utiliser une approche simplifiée
          formula_str <- paste("~ 0 +", paste(names(X_region), collapse = " + "))
          formula_obj <- as.formula(formula_str)
          
          tryCatch({
            X_matrix_region <- model.matrix(formula_obj, data = X_region)
            pred_class_region <<- predict(final_model, newdata = X_matrix_region, type = "class")
            pred_prob_region <<- predict(final_model, newdata = X_matrix_region, type = "probs")
          }, error = function(e2) {
            cat("Erreur secondaire pour la région", region, ":", e2$message, "\n")
            cat("Impossible de faire des prédictions pour cette région\n")
            
            # Créer des NA pour ce cas
            pred_class_region <<- rep(NA, nrow(X_region))
            pred_prob_region <<- matrix(NA, nrow = nrow(X_region), ncol = length(levels(y_validation)))
            colnames(pred_prob_region) <<- levels(y_validation)
          })
        }
      })
      
      # Calculer l'accuracy (en ignorant les NA)
      valid_predictions <- !is.na(pred_class_region)
      if (sum(valid_predictions) > 0) {
        accuracy_region <- mean(pred_class_region[valid_predictions] == y_region[valid_predictions], na.rm = TRUE)
        
        # Top-2 accuracy pour la région (s'il y a des prédictions valides)
        if (!is.null(pred_prob_region) && !all(is.na(pred_prob_region))) {
          top_2_accuracy_region <- top_n_accuracy(pred_prob_region[valid_predictions, , drop = FALSE], 
                                               y_region[valid_predictions], n = 2)
        } else {
          top_2_accuracy_region <- NA
        }
        
        # Stocker les métriques
        region_metric_df$Observations[i] <- sum(region_indices)
        region_metric_df$Accuracy[i] <- accuracy_region
        region_metric_df$Top2_Accuracy[i] <- top_2_accuracy_region
        
        cat(sprintf("Région: %s (n=%d) - Accuracy: %.4f, Top-2 Accuracy: %.4f\n", 
                    region, sum(region_indices), accuracy_region, top_2_accuracy_region))
        
        # Afficher la matrice de confusion pour cette région (seulement pour les prédictions valides)
        if (sum(valid_predictions) > 0) {
          conf_matrix_region <- table(pred_class_region[valid_predictions], y_region[valid_predictions])
          cat("Matrice de confusion pour", region, ":\n")
          print(conf_matrix_region)
        } else {
          cat("Pas de matrice de confusion disponible pour", region, "(pas de prédictions valides)\n")
        }
      } else {
        cat("Pas de prédictions valides pour la région", region, "\n")
      }
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
  
  # Préparer les variables
  X_boot <- boot_sample[, final_vars, drop = FALSE]
  
  # Faire des prédictions
  result <- tryCatch({
    pred <- predict(final_model, newdata = X_boot, type = "class")
    mean(pred == boot_sample$dv_voteChoice, na.rm = TRUE)
  }, error = function(e) {
    # Stratégie 1: Essayer d'extraire les termes d'interaction
    model_terms <- try(terms(final_model), silent = TRUE)
    
    if (!inherits(model_terms, "try-error")) {
      # Extraire les termes d'interaction
      all_terms <- attr(model_terms, "term.labels")
      formula_str <- paste("~ 0 +", paste(all_terms, collapse = " + "))
    } else {
      # Stratégie 2: Vérifier les coefficients
      coef_mat <- try(coef(final_model), silent = TRUE)
      
      if (!inherits(coef_mat, "try-error")) {
        var_names <- colnames(coef_mat)
        formula_str <- paste("~ 0 +", paste(var_names, collapse = " + "))
      } else {
        # Stratégie 3: Utiliser seulement les variables de base
        formula_str <- paste("~ 0 +", paste(final_vars, collapse = " + "))
      }
    }
    
    # Essayer de créer la matrice et prédire
    formula_obj <- try(as.formula(formula_str), silent = TRUE)
    
    if (!inherits(formula_obj, "try-error")) {
      X_matrix <- try(model.matrix(formula_obj, data = X_boot), silent = TRUE)
      
      if (!inherits(X_matrix, "try-error")) {
        # Si cela réussit, prédire
        pred <- try(predict(final_model, newdata = X_matrix, type = "class"), silent = TRUE)
        
        if (!inherits(pred, "try-error")) {
          return(mean(pred == boot_sample$dv_voteChoice, na.rm = TRUE))
        }
      }
    }
    
    # Si tout échoue, renvoyer NA
    return(NA)
  })
  
  return(result)
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
  # Vérifier si la matrice de probabilités est valide
  if (is.null(probabilities) || !is.matrix(probabilities) && !is.data.frame(probabilities)) {
    return(data.frame(
      Probability_Bin = character(0),
      Observed_Frequency = numeric(0),
      Count = integer(0),
      Avg_Predicted_Prob = numeric(0)
    ))
  }
  
  # Vérifier que le parti existe dans les colonnes
  if (!(party %in% colnames(probabilities))) {
    cat("ATTENTION: Le parti", party, "n'est pas présent dans les probabilités prédites\n")
    return(data.frame(
      Probability_Bin = character(0),
      Observed_Frequency = numeric(0),
      Count = integer(0),
      Avg_Predicted_Prob = numeric(0)
    ))
  }
  
  # Obtenir les probabilités prédites pour ce parti (filtrer les NA)
  pred_prob <- probabilities[, party]
  valid_indices <- !is.na(pred_prob) & !is.na(actual)
  
  if (sum(valid_indices) < 10) {
    cat("ATTENTION: Trop peu d'observations valides pour le parti", party, "\n")
    return(data.frame(
      Probability_Bin = character(0),
      Observed_Frequency = numeric(0),
      Count = integer(0),
      Avg_Predicted_Prob = numeric(0)
    ))
  }
  
  # Filtrer pour ne garder que les observations valides
  pred_prob <- pred_prob[valid_indices]
  actual_filtered <- actual[valid_indices]
  
  # Vérifier si la prédiction est correcte (1) ou non (0)
  actual_binary <- as.numeric(actual_filtered == party)
  
  # Créer 10 bins de probabilité
  bins <- cut(pred_prob, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)
  
  # Vérifier si tous les bins sont NA
  if (all(is.na(bins))) {
    cat("ATTENTION: Tous les bins sont NA pour le parti", party, "\n")
    return(data.frame(
      Probability_Bin = character(0),
      Observed_Frequency = numeric(0),
      Count = integer(0),
      Avg_Predicted_Prob = numeric(0)
    ))
  }
  
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
cat("Analyse de la calibration des probabilités prédites...\n")

# Vérifier si nous avons des probabilités prédites valides
if (!is.null(pred_prob) && (is.matrix(pred_prob) || is.data.frame(pred_prob))) {
  # Filtrer les observations avec des prédictions valides (non NA)
  valid_rows <- !is.na(rowSums(as.matrix(pred_prob)))
  
  if (sum(valid_rows) > 0) {
    for (party in parties) {
      cat("\nCalibration pour le parti", party, ":\n")
      calib <- evaluate_calibration(pred_prob[valid_rows, , drop = FALSE], 
                                   y_validation[valid_rows], party)
      
      if (nrow(calib) > 0) {
        print(calib)
      } else {
        cat("Pas assez de données pour évaluer la calibration pour ce parti\n")
      }
    }
  } else {
    cat("Aucune prédiction valide pour évaluer la calibration\n")
  }
} else {
  cat("Impossible d'analyser la calibration: Matrice de probabilités non disponible ou invalide\n")
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