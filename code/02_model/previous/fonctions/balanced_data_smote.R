# ------------------------------------------------------------------------
# 1) Chargement des packages
# ------------------------------------------------------------------------
library(parallel)
library(doParallel)
library(nnet)
library(caret)
library(tidyverse)
library(dplyr)

# ------------------------------------------------------------------------
# 2) Configuration du parallélisme
# ------------------------------------------------------------------------
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

# ------------------------------------------------------------------------
# 3) Chargement des données
# ------------------------------------------------------------------------
# Charger le modèle original et les résultats d'entraînement
original_model <- readRDS("_SharedFolder_datagotchi_federal_2024/data/modele/finalmodel_withOutInteractions.rds")
results_train <- readRDS("_SharedFolder_datagotchi_federal_2024/data/modele/resultsTrainV4_31janvier2025.rds")

# Charger les données pilotes et les données de l'application
DataPilote <- readRDS("_SharedFolder_datagotchi_federal_2024/data/pilote/dataClean/datagotchi2025_canada_pilot_20250310.rds")
DataApp <- readRDS("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/datagotchi2025_canada_app_20250314.rds")

# Charger les prédictions RTA
rta_predictions <- read.csv("_SharedFolder_datagotchi_federal_2024/data/modele/rta_predictions_partis.csv", 
                           stringsAsFactors = FALSE)

# ------------------------------------------------------------------------
# 4) Préparation des données
# ------------------------------------------------------------------------
prepare_data <- function(data) {
  # Extraction des RTA des codes postaux
  data$rta <- substr(data$ses_postalCode, 1, 3)
  
  # Convertir en majuscules pour assurer la correspondance avec rta_predictions
  data$rta <- toupper(data$rta)
  
  # Correction pour NDP/NPD si dv_voteChoice existe
  if ("dv_voteChoice" %in% names(data)) {
    # Convertir en character pour modifier les valeurs
    data$dv_voteChoice <- as.character(data$dv_voteChoice)
    
    # Remplacer "npd" par "ndp" pour uniformiser
    data$dv_voteChoice[data$dv_voteChoice == "npd"] <- "ndp"
    
    # Reconvertir en facteur
    data$dv_voteChoice <- factor(data$dv_voteChoice)
    
    # Filtrer "other" de dv_voteChoice
    if ("other" %in% levels(data$dv_voteChoice)) {
      data <- data %>% filter(dv_voteChoice != "other")
    }
  }
  
  # Convertir les facteurs ordonnés en facteurs non-ordonnés
  data <- data %>%
    mutate(across(where(is.ordered), ~ factor(., ordered = FALSE)))
  
  # Convertir certaines variables en facteur comme dans le script original
  if ("lifestyle_consCoffee" %in% names(data)) {
    data$lifestyle_consCoffee <- factor(data$lifestyle_consCoffee, ordered = FALSE)
  }
  
  if ("lifestyle_consClothes" %in% names(data)) {
    data$lifestyle_consClothes <- factor(data$lifestyle_consClothes, ordered = FALSE)
  }
  
  return(data)
}

# Appliquer la préparation aux deux jeux de données
DataPilote <- prepare_data(DataPilote)
DataApp <- prepare_data(DataApp)

# Vérifier les niveaux de dv_voteChoice dans les deux jeux de données
cat("Niveaux de dv_voteChoice dans les données pilotes:", 
    paste(levels(DataPilote$dv_voteChoice), collapse=", "), "\n")
cat("Niveaux de dv_voteChoice dans les nouvelles données:", 
    paste(levels(DataApp$dv_voteChoice), collapse=", "), "\n")

# ------------------------------------------------------------------------
# 5) Harmonisation des facteurs entre les jeux de données
# ------------------------------------------------------------------------
# Identifier toutes les variables facteur
all_vars <- names(DataPilote)[sapply(DataPilote, is.factor)]

# Harmoniser les niveaux des facteurs entre les deux jeux de données
for (var in all_vars) {
  # Vérifier si la variable existe dans les deux jeux de données
  if (var %in% names(DataApp)) {
    # Vérifier si les deux sont bien des facteurs
    if (is.factor(DataPilote[[var]]) && is.factor(DataApp[[var]])) {
      # Obtenir tous les niveaux uniques des deux jeux de données
      all_levels <- unique(c(levels(DataPilote[[var]]), levels(DataApp[[var]])))
      
      # Vérifier que all_levels n'est pas vide
      if (length(all_levels) > 0) {
        # Appliquer les mêmes niveaux aux deux jeux de données
        DataPilote[[var]] <- factor(DataPilote[[var]], levels = all_levels)
        DataApp[[var]] <- factor(DataApp[[var]], levels = all_levels)
      } else {
        cat("Attention: La variable", var, "a 0 niveaux. Conversion en character.\n")
        DataPilote[[var]] <- as.character(DataPilote[[var]])
        DataApp[[var]] <- as.character(DataApp[[var]])
      }
    } else {
      cat("Attention: La variable", var, "n'est pas un facteur dans les deux jeux de données.\n")
    }
  } else {
    cat("Attention: La variable", var, "n'existe pas dans DataApp. Ignorée pour l'harmonisation.\n")
  }
}

# ------------------------------------------------------------------------
# 6) Fusion des jeux de données
# ------------------------------------------------------------------------
# Ajouter une colonne source pour identifier l'origine des données
DataPilote$source <- "pilot"
DataApp$source <- "app"

# Fusionner les jeux de données
DataMerged <- bind_rows(DataPilote, DataApp)

# Harmoniser les valeurs de dv_voteChoice (ndp vs npd)
if ("dv_voteChoice" %in% names(DataMerged)) {
  # Convertir en character pour modification
  DataMerged$dv_voteChoice <- as.character(DataMerged$dv_voteChoice)
  
  # Harmoniser "npd" et "ndp" vers "ndp"
  DataMerged$dv_voteChoice[DataMerged$dv_voteChoice == "npd"] <- "ndp"
  
  # Reconvertir en facteur
  DataMerged$dv_voteChoice <- factor(DataMerged$dv_voteChoice)
  
  # Afficher la distribution après correction
  cat("\nDistribution de dv_voteChoice après harmonisation:\n")
  print(table(DataMerged$dv_voteChoice))
}

# Appliquer un contraste somme à dv_voteChoice
DataMerged$dv_voteChoice <- factor(DataMerged$dv_voteChoice)
contrasts(DataMerged$dv_voteChoice) <- contr.sum(nlevels(DataMerged$dv_voteChoice))

# ------------------------------------------------------------------------
# 7) Préparation des prédictions RTA
# ------------------------------------------------------------------------
# Renommer les colonnes pour plus de clarté
print(head(rta_predictions))
colnames(rta_predictions) <- c("rta", "pred_rta_CPC", "pred_rta_LPC", "pred_rta_NDP", "pred_rta_GPC", "pred_rta_BQ")

# Joindre les prédictions RTA au jeu de données fusionné
DataMerged <- left_join(DataMerged, rta_predictions, by = "rta")

# Vérifier les RTA sans correspondance
missing_rta <- is.na(DataMerged$pred_rta_CPC)
cat("Nombre d'observations avec RTA sans correspondance:", sum(missing_rta), "\n")

# Remplacer les valeurs manquantes par les moyennes
if (sum(missing_rta) > 0) {
  # Calculer les valeurs moyennes pour chaque parti
  mean_CPC <- mean(DataMerged$pred_rta_CPC, na.rm = TRUE)
  mean_LPC <- mean(DataMerged$pred_rta_LPC, na.rm = TRUE)
  mean_NDP <- mean(DataMerged$pred_rta_NDP, na.rm = TRUE)
  mean_GPC <- mean(DataMerged$pred_rta_GPC, na.rm = TRUE)
  mean_BQ <- mean(DataMerged$pred_rta_BQ, na.rm = TRUE)
  
  # Remplacer les valeurs manquantes par les moyennes
  DataMerged$pred_rta_CPC[missing_rta] <- mean_CPC
  DataMerged$pred_rta_LPC[missing_rta] <- mean_LPC
  DataMerged$pred_rta_NDP[missing_rta] <- mean_NDP
  DataMerged$pred_rta_GPC[missing_rta] <- mean_GPC
  DataMerged$pred_rta_BQ[missing_rta] <- mean_BQ
}

# ------------------------------------------------------------------------
# 8) Identification des meilleures variables du modèle précédent
# ------------------------------------------------------------------------
# Récupérer la configuration du meilleur modèle
best_iterations <- results_train %>%
  group_by(model_id) %>%
  summarise(
    score_iter_accuracy = first(accuracy_cv),
    .groups = "drop"
  ) %>%
  arrange(desc(score_iter_accuracy))

best_id <- best_iterations$model_id[1]
best_config <- results_train %>% filter(model_id == best_id)

# Récupérer les variables du meilleur modèle
final_vars <- best_config$variable

# Vérifier quelles variables sont disponibles dans DfTrain
available_vars <- names(DfTrain)

# Filtrer final_vars pour ne garder que les variables disponibles
final_vars_available <- final_vars[final_vars %in% available_vars]
cat("Variables originales:", length(final_vars), "\n")
cat("Variables disponibles:", length(final_vars_available), "\n")
cat("Variables originales non disponibles:", 
    paste(setdiff(final_vars, final_vars_available), collapse=", "), "\n")

# Ajouter les variables RTA
rta_vars <- c("pred_rta_CPC", "pred_rta_LPC", "pred_rta_NDP", "pred_rta_GPC", "pred_rta_BQ")
final_vars_with_rta <- unique(c(final_vars_available, rta_vars))

cat("Nombre de variables avec RTA:", length(final_vars_with_rta), "\n")

# ------------------------------------------------------------------------
# 9) Division Train/Test
# ------------------------------------------------------------------------
set.seed(42)
trainIndex <- createDataPartition(DataMerged$dv_voteChoice, p = 0.8, 
                                list = FALSE, 
                                times = 1)
DfTrain <- DataMerged[trainIndex, ]
DfTest <- DataMerged[-trainIndex, ]

# ------------------------------------------------------------------------
# 10) Définition de la fonction d'optimisation des hyperparamètres
# ------------------------------------------------------------------------
# Fonction pour trouver le meilleur paramètre de decay par validation croisée
optimize_decay <- function(balanced_data_for_training, folds = 5) {
  set.seed(42)
  # Diviser les données en folds
  fold_indices <- createFolds(balanced_data_for_training$dv_voteChoice, k = folds)
  
  # Valeurs de decay à tester
  decay_values <- c(0, 0.001, 0.01, 0.05, 0.1)
  
  # Dataframe pour stocker les résultats
  results <- data.frame(
    decay = decay_values,
    accuracy = numeric(length(decay_values)),
    stringsAsFactors = FALSE
  )
  
  # Pour chaque valeur de decay
  for (i in seq_along(decay_values)) {
    current_decay <- decay_values[i]
    cat("Essai avec decay =", current_decay, "\n")
    
    # Cross-validation
    fold_accuracies <- numeric(folds)
    for (f in seq_along(fold_indices)) {
      # Créer les ensembles train/valid
      valid_indices <- fold_indices[[f]]
      train_cv <- balanced_data_for_training[-valid_indices, ]
      valid_cv <- balanced_data_for_training[valid_indices, ]
      
      # Entraîner le modèle
      model_cv <- nnet::multinom(
        dv_voteChoice ~ .,
        data = train_cv,
        trace = FALSE,
        maxit = 200,
        decay = current_decay,
        MaxNWts = 100000
      )
      
      # Prédire et calculer l'accuracy
      pred_cv <- predict(model_cv, newdata = valid_cv)
      fold_accuracies[f] <- mean(pred_cv == valid_cv$dv_voteChoice, na.rm = TRUE)
    }
    
    # Moyenne des accuracy de tous les folds
    results$accuracy[i] <- mean(fold_accuracies)
  }
  
  # Trouver le meilleur decay
  best_index <- which.max(results$accuracy)
  best_decay <- results$decay[best_index]
  
  cat("Meilleur decay trouvé:", best_decay, "avec accuracy:", results$accuracy[best_index], "\n")
  return(best_decay)
}

# ------------------------------------------------------------------------
# 11) Fonction pour la classification avec seuils personnalisés
# ------------------------------------------------------------------------
# Fonction pour classifier avec des seuils personnalisés
custom_classify <- function(probs, thresholds) {
  n <- nrow(probs)
  classes <- colnames(probs)
  result <- character(n)
  
  for (i in 1:n) {
    # Normaliser les probabilités pour qu'elles somment à 1
    normalized_probs <- probs[i,] / sum(probs[i,])
    
    # Appliquer les seuils personnalisés
    over_threshold <- names(normalized_probs)[normalized_probs > thresholds[names(normalized_probs)]]
    
    if (length(over_threshold) == 0) {
      # Si aucune classe ne dépasse son seuil, utiliser la classification standard
      result[i] <- names(normalized_probs)[which.max(normalized_probs)]
    } else {
      # Parmi les classes dépassant leur seuil, prendre celle avec la plus haute probabilité
      result[i] <- over_threshold[which.max(normalized_probs[over_threshold])]
    }
  }
  
  return(factor(result, levels = classes))
}

# ------------------------------------------------------------------------
# 12) Entraînement du modèle avec RTA et améliorations
# ------------------------------------------------------------------------
cat("\n========== APPROCHE MODIFIÉE AVEC RTA ET AMÉLIORATIONS ==========\n")

cat("\nPréparation des données pour l'entraînement avec variables RTA...\n")
X_train_final <- DfTrain[, final_vars_with_rta, drop = FALSE]
y_train_final <- DfTrain$dv_voteChoice

# Création des variables dummy
dummies_final <- dummyVars(" ~ .", data = X_train_final, fullRank = TRUE, sep = "_")
X_train_dummy <- predict(dummies_final, newdata = X_train_final) %>% as.data.frame()

cat("\nÉquilibrage des données pour améliorer la précision du modèle...\n")
# Modifier la fonction balanced_data_smote pour favoriser GPC et LPC
# La fonction est maintenant modifiée dans le fichier séparé avec target_prop=1.0 pour GPC
# et target_prop=1.2 pour LPC
balanced_data <- balanced_data_smote(DfTrain, "dv_voteChoice", target_prop = 1.0)

# Vérifier la distribution après équilibrage
new_class_counts <- table(balanced_data$dv_voteChoice)
cat("\nDistribution des classes après équilibrage amélioré:\n")
print(new_class_counts)

# Préparer les données équilibrées pour l'entraînement
X_balanced <- balanced_data[, final_vars_with_rta, drop = FALSE]
y_balanced <- balanced_data$dv_voteChoice

# Créer les variables dummy pour les données équilibrées
dummies_balanced <- dummyVars(" ~ .", data = X_balanced, fullRank = TRUE, sep = "_")
X_balanced_dummy <- predict(dummies_balanced, newdata = X_balanced) %>% as.data.frame()
balanced_data_for_training <- cbind(dv_voteChoice = y_balanced, X_balanced_dummy)

# Option: optimiser le paramètre de decay par validation croisée
# Décommenter pour utiliser l'optimisation (prend plus de temps)
# best_decay <- optimize_decay(balanced_data_for_training)
# Si non optimisé, utiliser la valeur suggérée de 0.01
best_decay <- 0.01

cat("\nEntraînement du modèle multinom avec variables RTA et régularisation...\n")
set.seed(42)
multinom_model <- tryCatch({
  nnet::multinom(
    dv_voteChoice ~ .,
    data = balanced_data_for_training,
    trace = TRUE,
    maxit = 500,         # Augmenter le nombre d'itérations (200 → 500)
    decay = best_decay,  # Ajouter de la régularisation pour éviter le surapprentissage
    MaxNWts = 100000
  )
}, error = function(e) {
  cat("Erreur dans l'entraînement du modèle:", e$message, "\n")
  # Si erreur, essayer sans les variables RTA
  cat("Tentative sans les variables RTA...\n")
  X_train_without_rta <- DfTrain[, final_vars, drop = FALSE]
  dummies_without_rta <- dummyVars(" ~ .", data = X_train_without_rta, fullRank = TRUE, sep = "_")
  X_train_dummy_without_rta <- predict(dummies_without_rta, newdata = X_train_without_rta) %>% as.data.frame()
  
  train_data_without_rta <- cbind(dv_voteChoice = y_train_final, X_train_dummy_without_rta)
  return(nnet::multinom(
    dv_voteChoice ~ .,
    data = train_data_without_rta,
    trace = TRUE,
    maxit = 500,
    decay = best_decay,
    MaxNWts = 100000
  ))
})

# ------------------------------------------------------------------------
# 13) Évaluation du modèle sur les données de test
# ------------------------------------------------------------------------
cat("\nÉvaluation du modèle multinom avec RTA et améliorations...\n")
# S'assurer d'utiliser les mêmes variables que pour l'entraînement
X_test_final <- DfTest[, final_vars_with_rta, drop = FALSE]
y_test_final <- DfTest$dv_voteChoice

# Filtrer les valeurs NA dans y_test_final
na_test <- is.na(y_test_final)
if (any(na_test)) {
  cat("Attention: Il y a", sum(na_test), "observations avec valeurs manquantes pour dv_voteChoice dans le jeu de test. Elles seront filtrées.\n")
  X_test_final <- X_test_final[!na_test, ]
  y_test_final <- y_test_final[!na_test]
}

# Appliquer la même transformation dummy aux données de test
X_test_dummy <- predict(dummies_balanced, newdata = X_test_final) %>% as.data.frame()

# S'assurer que toutes les colonnes nécessaires sont présentes dans X_test_dummy
missing_cols <- setdiff(colnames(X_balanced_dummy), colnames(X_test_dummy))
if (length(missing_cols) > 0) {
  cat("Attention: Colonnes manquantes dans les données de test:", paste(missing_cols, collapse=", "), "\n")
  # Ajouter les colonnes manquantes avec des zéros
  for (col in missing_cols) {
    X_test_dummy[[col]] <- 0
  }
}

# Réordonner les colonnes pour qu'elles correspondent à celles utilisées lors de l'entraînement
X_test_dummy <- X_test_dummy[, colnames(X_balanced_dummy), drop = FALSE]

# Prédictions sur les données de test (probabilités)
pred_test_final_prob <- predict(multinom_model, newdata = X_test_dummy, type = "probs")

# Appliquer des seuils personnalisés pour améliorer la classification
# Créer des seuils personnalisés par classe
custom_thresholds <- c(bq = 0.3, cpc = 0.25, gpc = 0.15, lpc = 0.25, ndp = 0.25)

# Utiliser la classification personnalisée
custom_pred_class <- custom_classify(pred_test_final_prob, custom_thresholds)
pred_test_final_class <- custom_pred_class

# Assurer que les prédictions et les valeurs réelles ont les mêmes niveaux de facteur
y_test_final <- factor(y_test_final, levels = levels(y_balanced))
pred_test_final_class <- factor(pred_test_final_class, levels = levels(y_balanced))

# Calcul de l'accuracy
acc_multinom <- mean(pred_test_final_class == y_test_final, na.rm = TRUE)
cat("Accuracy du modèle multinom avec RTA et améliorations:", round(acc_multinom * 100, 2), "%\n")

# Calcul du LogLoss amélioré
levelz <- levels(y_test_final)
n_test <- length(y_test_final)
logloss_test <- 0
logloss_counter <- 0  # Compteur pour savoir combien de prédictions ont été utilisées

for (i in seq_len(n_test)) {
  obs_class <- as.character(y_test_final[i])
  if (!is.na(obs_class)) {
    class_idx <- which(levelz == obs_class)
    if (length(class_idx) > 0) {
      p <- pred_test_final_prob[i, class_idx]
      if (!is.na(p)) {  # Vérifier que p n'est pas NA
        p <- max(p, 1e-15)
        logloss_test <- logloss_test - log(p)
        logloss_counter <- logloss_counter + 1
      }
    }
  }
}

# Normaliser par le nombre réel de calculs effectués
if (logloss_counter > 0) {
  logloss_test <- logloss_test / logloss_counter
} else {
  logloss_test <- NA
}
cat("LogLoss (test) :", logloss_test, "\n")

# Performance par parti améliorée
party_performance <- data.frame(
  party = character(),
  count = numeric(),
  correct = numeric(),
  accuracy = numeric(),
  stringsAsFactors = FALSE
)

# Utiliser y_test_final (filtré) au lieu de DfTest$dv_voteChoice (non filtré)
for (party in levels(y_test_final)) {
  party_indices <- which(y_test_final == party)
  party_count <- length(party_indices)
  
  if (party_count > 0) {
    # Compter les prédictions correctes pour ce parti
    correct <- sum(pred_test_final_class[party_indices] == party, na.rm = TRUE)
    
    party_performance <- rbind(party_performance, data.frame(
      party = party,
      count = party_count,
      correct = correct,
      accuracy = round(correct / party_count * 100, 1),
      stringsAsFactors = FALSE
    ))
  }
}

cat("\nPerformance par parti avec améliorations:\n")
print(party_performance)

# ------------------------------------------------------------------------
# 14) Sauvegarde du modèle amélioré
# ------------------------------------------------------------------------
# Récupérer des informations du modèle original pour assurer la compatibilité
if (!is.null(original_model$sym_coef)) {
  # Extraire la matrice des coefficients du nouveau modèle
  orig_coef <- coef(multinom_model)
  # Récupérer tous les niveaux de la variable réponse
  all_levels <- levels(y_balanced)
  
  # Créer une matrice complète pour les coefficients
  full_coef <- matrix(0, nrow = length(all_levels), ncol = ncol(orig_coef))
  rownames(full_coef) <- all_levels
  colnames(full_coef) <- colnames(orig_coef)
  
  # Remplir full_coef pour les niveaux non de référence
  for (lvl in rownames(orig_coef)) {
    if (lvl %in% all_levels) {
      full_coef[lvl, ] <- orig_coef[lvl, ]
    }
  }
  
  # Pour chaque prédicteur, calculer la moyenne des coefficients sur tous les niveaux
  m <- colMeans(full_coef)
  
  # Reparamétrer de manière symétrique
  sym_coef <- full_coef - matrix(rep(m, each = length(all_levels)), nrow = length(all_levels))
  
  # Ajouter la matrice symétrique au modèle final
  multinom_model$sym_coef <- sym_coef
}

# Nommer le modèle final pour le distinguer
multinom_model$model_name <- "modele_ameliore_gpc_lpc_avec_seuils_personnalises"

# Sauvegarder les seuils personnalisés avec le modèle
multinom_model$custom_thresholds <- custom_thresholds

# Sauvegarder le modèle final
saveRDS(multinom_model, "_SharedFolder_datagotchi_federal_2024/data/modele/finalmodel_ameliore_gpc_lpc.rds")
cat("\nNouveau modèle amélioré sauvegardé avec succès.\n")

# Fermer le cluster de parallélisation
stopCluster(cl)