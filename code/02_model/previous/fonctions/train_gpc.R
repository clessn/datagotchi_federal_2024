train_gpc_detector <- function(train_data, all_vars) {
  # Vérifier le déséquilibre
  gpc_count <- sum(train_data$dv_voteChoice == "gpc")
  total_count <- nrow(train_data)
  gpc_percent <- gpc_count / total_count * 100
  
  cat("Analyse de la classe GPC:", gpc_count, "sur", total_count, 
      "observations (", round(gpc_percent, 2), "%)\n")
  
  # Si moins de 1% des données sont GPC, utiliser un sur-échantillonnage spécifique
  if(gpc_percent < 1) {
    cat("Classe GPC très minoritaire. Application d'un sur-échantillonnage spécifique...\n")
    
    # Séparer les observations GPC et non-GPC
    gpc_data <- train_data[train_data$dv_voteChoice == "gpc", ]
    non_gpc_data <- train_data[train_data$dv_voteChoice != "gpc", ]
    
    # Sous-échantillonner les non-GPC (maximum 10 fois le nombre de GPC)
    max_non_gpc <- min(nrow(non_gpc_data), gpc_count * 10)
    non_gpc_sample <- non_gpc_data[sample(nrow(non_gpc_data), max_non_gpc), ]
    
    # Sur-échantillonner les GPC (pour avoir environ 1/3 des données)
    target_gpc_count <- max(gpc_count, floor(max_non_gpc / 2))
    
    # Si besoin de plus d'échantillons GPC
    if(target_gpc_count > gpc_count) {
      # Nombre de copies nécessaires
      additional_needed <- target_gpc_count - gpc_count
      # Échantillonner avec remplacement
      additional_gpc <- gpc_data[sample(nrow(gpc_data), additional_needed, replace = TRUE), ]
      # Combiner
      gpc_sample <- rbind(gpc_data, additional_gpc)
    } else {
      gpc_sample <- gpc_data
    }
    
    # Combiner pour l'entraînement
    balanced_data <- rbind(gpc_sample, non_gpc_sample)
    
    cat("Données rééquilibrées pour l'entraînement du détecteur GPC:", 
        nrow(gpc_sample), "GPC vs", nrow(non_gpc_sample), "non-GPC\n")
    
    train_data <- balanced_data
  }
  
  # Créer une variable binaire pour GPC vs non-GPC
  train_data$is_gpc <- ifelse(train_data$dv_voteChoice == "gpc", 1, 0)
  
  # Utiliser une régression logistique améliorée pour la détection GPC
  # Sélectionner des prédicteurs pertinents environnementaux et idéologiques (variables les plus importantes pour GPC)
  potential_predictors <- c()
  
  # Variables connues comme étant pertinentes pour le vote GPC
  important_gpc_vars <- c(
    # Chercher des variables liées à l'environnement
    grep("^(environment|climat|ecolo|carbon|develop|nuclear|renewab)", all_vars, value = TRUE, ignore.case = TRUE),
    # Chercher des variables idéologiques/valeurs
    grep("^(values|ideol|progre|conserv|left|right)", all_vars, value = TRUE, ignore.case = TRUE),
    # Ajouter des variables socio-démographiques
    grep("^(age|education|income|urban|rural)", all_vars, value = TRUE, ignore.case = TRUE)
  )
  
  # Ajouter les variables importantes identifiées
  potential_predictors <- unique(c(potential_predictors, important_gpc_vars))
  
  # Filtrer pour garder uniquement les variables qui existent dans le dataset
  potential_predictors <- intersect(potential_predictors, names(train_data))
  
  # S'il n'y a pas assez de prédicteurs spécifiques, utiliser d'autres variables numériques et facteurs simples
  if(length(potential_predictors) < 5) {
    for(var in all_vars) {
      if(is.numeric(train_data[[var]])) {
        potential_predictors <- c(potential_predictors, var)
      } else if(is.factor(train_data[[var]]) && length(levels(train_data[[var]])) <= 5) {
        potential_predictors <- c(potential_predictors, var)
      }
    }
    # Limiter le nombre de prédicteurs si nécessaire
    if(length(potential_predictors) > 20) {
      potential_predictors <- potential_predictors[1:20]
    }
  }
  
  if(length(potential_predictors) == 0) {
    cat("Pas de variables appropriées disponibles pour le détecteur GPC\n")
    return(NULL)
  }
  
  # Créer la formule
  formula_gpc <- as.formula(paste("is_gpc ~", paste(potential_predictors, collapse = " + ")))
  
  # Entraîner le modèle avec des hyperparamètres optimisés
  gpc_model <- tryCatch({
    # Utiliser une régression logistique avec régularisation (poids plus élevés pour la classe minoritaire)
    weights <- ifelse(train_data$is_gpc == 1, 
                     sum(train_data$is_gpc == 0) / sum(train_data$is_gpc == 1), 
                     1)
    
    glm_model <- glm(
      formula = formula_gpc,
      data = train_data,
      family = binomial(),
      weights = weights
    )
    
    # Évaluer la performance sur les données d'entraînement
    train_probs <- predict(glm_model, train_data, type = "response")
    
    # Trouver le seuil optimal (maximisant F1-score)
    thresholds <- seq(0.1, 0.9, by = 0.05)
    best_f1 <- 0
    best_threshold <- 0.5
    
    for(threshold in thresholds) {
      train_pred <- ifelse(train_probs > threshold, 1, 0)
      precision <- sum(train_pred == 1 & train_data$is_gpc == 1) / sum(train_pred == 1)
      recall <- sum(train_pred == 1 & train_data$is_gpc == 1) / sum(train_data$is_gpc == 1)
      
      if(!is.nan(precision) && !is.nan(recall) && (precision + recall) > 0) {
        f1 <- 2 * precision * recall / (precision + recall)
        if(f1 > best_f1) {
          best_f1 <- f1
          best_threshold <- threshold
        }
      }
    }
    
    cat("Seuil optimal trouvé:", best_threshold, "avec F1-score:", round(best_f1, 3), "\n")
    
    list(
      model = glm_model,
      variables = potential_predictors,
      threshold = best_threshold
    )
  }, error = function(e) {
    cat("Erreur dans l'entraînement du détecteur GPC:", e$message, "\n")
    return(NULL)
  })
  
  if(is.null(gpc_model)) {
    return(NULL)
  }
  
  # Évaluer les performances finales sur les données d'entraînement
  train_probs <- predict(gpc_model$model, train_data, type = "response")
  train_pred <- ifelse(train_probs > gpc_model$threshold, 1, 0)
  accuracy <- mean(train_pred == train_data$is_gpc)
  
  # Calculer la précision et le rappel pour les électeurs GPC
  true_gpc <- train_data$is_gpc == 1
  pred_gpc <- train_pred == 1
  
  precision <- sum(true_gpc & pred_gpc) / sum(pred_gpc)
  recall <- sum(true_gpc & pred_gpc) / sum(true_gpc)
  
  cat("Détecteur GPC entraîné avec accuracy:", round(accuracy * 100, 1), "%\n")
  cat("Précision:", round(precision * 100, 1), "%, Rappel:", 
      round(recall * 100, 1), "%\n")
  
  return(gpc_model)
}