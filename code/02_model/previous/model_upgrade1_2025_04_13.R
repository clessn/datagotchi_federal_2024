# ------------------------------------------------------------------------
# Modèle Multinom amélioré avec interactions sélectives et correction idéologique optimisée
# ------------------------------------------------------------------------

# 1) Chargement des packages nécessaires
library(tidyverse)
library(caret)
library(nnet)
library(doParallel)
library(pROC)

# Configurer le parallélisme
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

# 2) Chargement des données et du modèle original
# Charger le modèle final original
original_model <- readRDS("_SharedFolder_datagotchi_federal_2024/data/modele/finalmodel_withOutInteractions.rds")

# Charger les données fusionnées
DataMerged <- readRDS("_SharedFolder_datagotchi_federal_2024/data/DataMerged_pilot_app.rds")

# Charger les résultats d'entraînement pour les variables
results_train <- readRDS("_SharedFolder_datagotchi_federal_2024/data/modele/resultsTrainV4_31janvier2025.rds")

# Identifier le meilleur modèle
best_iterations <- results_train %>%
  group_by(model_id) %>%
  summarise(
    score_iter_accuracy = first(accuracy_cv),
    .groups = "drop"
  ) %>%
  arrange(desc(score_iter_accuracy))

best_id <- best_iterations$model_id[1]
best_config <- results_train %>% filter(model_id == best_id)
final_vars <- best_config$variable

# 3) Vérification des classes dans les données
original_classes <- levels(DataMerged$dv_voteChoice)
cat("Classes du modèle original:", paste(original_classes, collapse=", "), "\n")

# 4) Séparation Train/Test stratifiée
set.seed(42)
trainIndex <- createDataPartition(DataMerged$dv_voteChoice, p = 0.8, 
                                list = FALSE, 
                                times = 1)
DfTrain <- DataMerged[trainIndex, ]
DfTest <- DataMerged[-trainIndex, ]

# 5) Validation set pour la matrice idéologique
set.seed(123)
val_indices <- sample(1:nrow(DfTest), nrow(DfTest)/2)
DfVal <- DfTest[val_indices, ]
DfFinalTest <- DfTest[-val_indices, ]

# 6) Fonction pour évaluer systématiquement les interactions
evaluate_interaction_importance <- function(train_data, base_vars, response_var = "dv_voteChoice", 
                                           max_levels_product = 30, top_n = 10) {
  interaction_scores <- list()
  
  # Créer toutes les interactions possibles par paires
  all_pairs <- combn(base_vars, 2, simplify = FALSE)
  
  cat("Évaluation de", length(all_pairs), "interactions potentielles...\n")
  
  for(i in 1:length(all_pairs)) {
    pair <- all_pairs[[i]]
    var1 <- pair[1]
    var2 <- pair[2]
    
    # Vérifier si les deux variables sont catégorielles
    is_cat1 <- is.factor(train_data[[var1]])
    is_cat2 <- is.factor(train_data[[var2]])
    
    # Si les deux sont catégorielles, vérifier le nombre de niveaux
    if(is_cat1 && is_cat2) {
      levels1 <- length(levels(train_data[[var1]]))
      levels2 <- length(levels(train_data[[var2]]))
      
      # Si le produit des niveaux est trop élevé, sauter cette interaction
      if(levels1 * levels2 > max_levels_product) {
        next
      }
    }
    
    interaction_term <- paste(var1, var2, sep = ":")
    
    # Formule avec variables de base + interaction
    formula_str <- paste(
      response_var, "~", 
      paste(base_vars, collapse = " + "), "+", 
      interaction_term
    )
    
    tryCatch({
      # Utiliser un échantillon pour accélérer l'évaluation
      sample_indices <- sample(1:nrow(train_data), min(5000, nrow(train_data)))
      sample_data <- train_data[sample_indices, ]
      
      # Évaluer avec un modèle simple et AIC
      model_with_interaction <- nnet::multinom(
        as.formula(formula_str),
        data = sample_data,
        decay = 0.05,  # Régularisation plus forte
        maxit = 100,   # Nombre réduit d'itérations pour accélérer
        trace = FALSE
      )
      
      # Calculer l'AIC comme score (plus bas = meilleur)
      interaction_scores[[interaction_term]] <- AIC(model_with_interaction)
      
      if(i %% 10 == 0 || i == length(all_pairs)) {
        cat("Progrès:", i, "/", length(all_pairs), "\n")
      }
      
    }, error = function(e) {
      cat("Erreur avec l'interaction", interaction_term, ":", e$message, "\n")
      interaction_scores[[interaction_term]] <- Inf
    })
  }
  
  # Convertir en data frame et trier (AIC plus bas = meilleur)
  scores_df <- data.frame(
    interaction = names(interaction_scores),
    aic_score = unlist(interaction_scores),
    stringsAsFactors = FALSE
  )
  
  scores_df <- scores_df[order(scores_df$aic_score), ]
  
  # Retourner les meilleures interactions
  best_interactions <- head(scores_df$interaction, top_n)
  
  cat("\nMeilleures interactions identifiées:\n")
  print(head(scores_df, top_n))
  
  return(best_interactions)
}

# 7) Fonction améliorée pour l'équilibrage des données
balanced_data_smote <- function(train_data, response_var = "dv_voteChoice", 
                               use_actual_smote = FALSE, k = 5) {
  # Si ROSE est disponible et demandé, l'utiliser pour SMOTE
  if(use_actual_smote && require("ROSE", quietly = TRUE)) {
    # Utiliser ROSE pour un échantillonnage équilibré sophistiqué
    formula_str <- paste(response_var, "~ .")
    balanced <- ROSE::ovun.sample(
      formula = as.formula(formula_str),
      data = train_data,
      method = "both",  # both = combinaison de over et under-sampling
      p = 0.5,          # proportion de la classe minoritaire
      k = k,            # nombre de voisins pour SMOTE
      seed = 42
    )
    return(balanced$data)
  } else {
    # Sinon, utiliser l'approche de sur-échantillonnage améliorée
    # Identifier la distribution des classes
    class_counts <- table(train_data[[response_var]])
    target_count <- max(class_counts)  # Viser la taille de la plus grande classe
    
    # Créer un ensemble de données équilibré
    balanced_data <- train_data[0, ]  # Dataframe vide avec même structure
    
    # Pour chaque classe
    for(class_name in names(class_counts)) {
      # Extraire les données de cette classe
      class_data <- train_data[train_data[[response_var]] == class_name, ]
      class_size <- nrow(class_data)
      
      # Si besoin de sur-échantillonner
      if(class_size < target_count) {
        # Nombre d'échantillons à ajouter
        samples_needed <- target_count - class_size
        
        # Si beaucoup d'échantillonnage nécessaire, utiliser une approche stratifiée
        if(samples_needed > 2 * class_size) {
          n_copies <- floor(samples_needed / class_size)
          remainder <- samples_needed %% class_size
          
          # Ajouter des copies complètes
          for(i in 1:n_copies) {
            balanced_data <- rbind(balanced_data, class_data)
          }
          
          # Ajouter le reste avec un échantillonnage aléatoire
          if(remainder > 0) {
            sampled_indices <- sample(1:class_size, remainder, replace = TRUE)
            balanced_data <- rbind(balanced_data, class_data[sampled_indices, ])
          }
        } else {
          # Échantillonnage simple avec remplacement
          sampled_indices <- sample(1:class_size, samples_needed, replace = TRUE)
          balanced_data <- rbind(balanced_data, class_data, class_data[sampled_indices, ])
        }
      } else {
        # Si la classe a déjà assez d'échantillons, l'ajouter telle quelle
        balanced_data <- rbind(balanced_data, class_data)
      }
    }
    
    # Mélanger les données
    balanced_data <- balanced_data[sample(1:nrow(balanced_data)), ]
    return(balanced_data)
  }
}

# 8) Fonction améliorée pour optimiser la matrice idéologique
source("code/02_model/fonctions/optimize_ideological_matrix.R")

# 9) Fonction pour optimiser les poids par parti
source("code/02_model/fonctions/optimize_ideology_weights_by_party.R")

# Définir la méthode predict.multinom_enhanced pour les modèles avec correction idéologique
if(!exists("predict.multinom_enhanced", mode = "function")) {
  predict.multinom_enhanced <- function(object, newdata, type = "class", use_ideology = TRUE, ...) {
    # Vérifier si le modèle a la matrice de proximité idéologique
    if (use_ideology && is.null(object$ideological_proximity)) {
      warning("La matrice de proximité idéologique n'est pas définie. Continuera sans correction idéologique.")
      use_ideology <- FALSE
    }
    
    # Faire la prédiction standard de multinom
    if (type == "class") {
      # Pour les classes, on utilise le comportement standard
      pred_class <- NextMethod("predict")
      if (!use_ideology) {
        return(pred_class)
      }
      # Pour appliquer l'idéologie, on a besoin des probabilités
      pred_probs <- NextMethod("predict", type = "probs")
    } else if (type == "probs") {
      # Pour les probabilités, on utilise le comportement standard
      pred_probs <- NextMethod("predict", type = "probs")
      if (!use_ideology) {
        return(pred_probs)
      }
    } else {
      stop("Type de prédiction non supporté: ", type)
    }
    
    # Appliquer la logique idéologique pour lisser les probabilités
    if (use_ideology) {
      smoothed_probs <- matrix(0, nrow = nrow(pred_probs), ncol = ncol(pred_probs))
      colnames(smoothed_probs) <- colnames(pred_probs)
      
      # Paramètre d'équilibre entre prédiction originale et lissage idéologique
      original_weight <- object$ideology_params$original_weight  # Généralement 0.7
      ideology_weight <- 1 - original_weight  # Généralement 0.3
      
      for (i in 1:nrow(pred_probs)) {
        for (j in 1:ncol(pred_probs)) {
          party_j <- tolower(colnames(pred_probs)[j])
          
          # Pondérer par la proximité idéologique
          weighted_sum <- 0
          for (k in 1:ncol(pred_probs)) {
            party_k <- tolower(colnames(pred_probs)[k])
            # Récupérer le facteur de proximité (ou utiliser 0 si non défini)
            prox_factor <- object$ideological_proximity[party_j, party_k]
            weighted_sum <- weighted_sum + (pred_probs[i, k] * prox_factor)
          }
          
          # Combiner probabilité originale et lissage idéologique
          smoothed_probs[i, j] <- pred_probs[i, j] * original_weight + weighted_sum * ideology_weight
        }
      }
      
      # Renormaliser pour que la somme des probabilités soit 1
      row_sums <- rowSums(smoothed_probs)
      smoothed_probs <- smoothed_probs / row_sums
      pred_probs <- smoothed_probs
    }
    
    if (type == "class") {
      # Retourner les classes prédites basées sur les probabilités modifiées
      predicted_indices <- max.col(pred_probs)
      return(factor(colnames(pred_probs)[predicted_indices], levels = colnames(pred_probs)))
    } else {
      # Retourner les probabilités modifiées
      return(pred_probs)
    }
  }
  
  # Assigner dans l'espace de noms approprié
  assignInNamespace("predict.multinom_enhanced", predict.multinom_enhanced, ns = "package:nnet")
}

# Définir la fonction predict.multinom_enhanced_v2 pour les poids par parti
predict.multinom_enhanced_v2 <- function(object, newdata, type = "class", use_ideology = TRUE, ...) {
  # Vérifier si le modèle a la matrice de proximité idéologique
  if (use_ideology && is.null(object$ideological_proximity)) {
    warning("La matrice de proximité idéologique n'est pas définie. Continuera sans correction idéologique.")
    use_ideology <- FALSE
  }
  
  # Faire la prédiction standard de multinom
  if (type == "class") {
    # Pour les classes, on utilise le comportement standard
    pred_class <- NextMethod("predict")
    if (!use_ideology) {
      return(pred_class)
    }
    # Pour appliquer l'idéologie, on a besoin des probabilités
    pred_probs <- NextMethod("predict", type = "probs")
  } else if (type == "probs") {
    # Pour les probabilités, on utilise le comportement standard
    pred_probs <- NextMethod("predict", type = "probs")
    if (!use_ideology) {
      return(pred_probs)
    }
  } else {
    stop("Type de prédiction non supporté: ", type)
  }
  
  # Appliquer la logique idéologique pour lisser les probabilités
  if (use_ideology) {
    smoothed_probs <- matrix(0, nrow = nrow(pred_probs), ncol = ncol(pred_probs))
    colnames(smoothed_probs) <- colnames(pred_probs)
    
    # Récupérer les poids par parti s'ils existent
    if (!is.null(object$ideology_params$party_weights)) {
      party_weights <- object$ideology_params$party_weights
    } else {
      # Sinon utiliser le poids original pour tous les partis
      party_weights <- rep(object$ideology_params$original_weight, ncol(pred_probs))
      names(party_weights) <- colnames(pred_probs)
    }
    
    for (i in 1:nrow(pred_probs)) {
      for (j in 1:ncol(pred_probs)) {
        party_j <- colnames(pred_probs)[j]
        
        # Utiliser le poids spécifique à ce parti
        original_weight <- party_weights[party_j]
        ideology_weight <- 1 - original_weight
        
        # Pondérer par la proximité idéologique
        weighted_sum <- 0
        for (k in 1:ncol(pred_probs)) {
          party_k <- colnames(pred_probs)[k]
          
          # Récupérer le facteur de proximité (ou utiliser 0 si non défini)
          prox_factor <- object$ideological_proximity[tolower(party_j), tolower(party_k)]
          weighted_sum <- weighted_sum + (pred_probs[i, k] * prox_factor)
        }
        
        # Combiner probabilité originale et lissage idéologique
        smoothed_probs[i, j] <- pred_probs[i, j] * original_weight + weighted_sum * ideology_weight
      }
    }
    
    # Renormaliser pour que la somme des probabilités soit 1
    row_sums <- rowSums(smoothed_probs)
    smoothed_probs <- smoothed_probs / row_sums
    pred_probs <- smoothed_probs
  }
  
  if (type == "class") {
    # Retourner les classes prédites basées sur les probabilités modifiées
    predicted_indices <- max.col(pred_probs)
    return(factor(colnames(pred_probs)[predicted_indices], levels = colnames(pred_probs)))
  } else {
    # Retourner les probabilités modifiées
    return(pred_probs)
  }
}

# Au lieu d'assigner dans l'espace de noms, nous allons simplement définir la fonction 
# et utiliser la méthode S3 normale pour l'enregistrement
# La fonction sera disponible dans l'environnement global
# On n'a pas besoin de l'assigner explicitement

# 12) Approche principale optimisée
cat("\n========== APPROCHE OPTIMISÉE ==========\n")

# Identifier systématiquement les meilleures interactions
cat("Évaluation systématique des interactions...\n")
selected_interactions <- evaluate_interaction_importance(
  DfTrain, 
  final_vars, 
  "dv_voteChoice", 
  max_levels_product = 30, 
  top_n = 10
)

# Construire la formule avec les interactions sélectionnées
formula_base <- paste("dv_voteChoice ~", paste(final_vars, collapse = " + "))

if(length(selected_interactions) > 0) {
  interactions_formula <- paste(selected_interactions, collapse = " + ")
  formula_final <- paste(formula_base, "+", interactions_formula)
} else {
  formula_final <- formula_base
}

cat("\nFormule du modèle finale:", formula_final, "\n")

# Équilibrer les données avec l'approche améliorée
cat("\nÉquilibrage des données avec l'approche améliorée...\n")
balanced_data <- balanced_data_smote(DfTrain, "dv_voteChoice")

# Vérifier le nouvel équilibrage
new_class_counts <- table(balanced_data$dv_voteChoice)
cat("\nDistribution des classes après équilibrage amélioré:\n")
print(new_class_counts)

# Entraîner le modèle multinom avec les interactions optimisées
cat("\nEntraînement du modèle multinom avec interactions optimisées...\n")
set.seed(42)  # Pour la reproductibilité

multinom_model <- tryCatch({
  nnet::multinom(
    as.formula(formula_final),
    data = balanced_data,
    decay = 0.01,    # Régularisation pour éviter le surajustement
    maxit = 1000,    # Nombre maximum d'itérations
    trace = TRUE
  )
}, error = function(e) {
  cat("Erreur dans l'entraînement du modèle:", e$message, "\n")
  
  # Si l'erreur est liée à trop de poids, réduire les interactions
  if(grepl("too many", e$message)) {
    cat("Tentative avec moins d'interactions...\n")
    if(length(selected_interactions) > 5) {
      reduced_interactions <- selected_interactions[1:5]
      reduced_formula <- paste(formula_base, "+", paste(reduced_interactions, collapse = " + "))
      
      return(nnet::multinom(
        as.formula(reduced_formula),
        data = balanced_data,
        decay = 0.01,
        maxit = 1000,
        trace = TRUE
      ))
    } else {
      cat("Utilisation du modèle sans interactions...\n")
      return(nnet::multinom(
        as.formula(formula_base),
        data = balanced_data,
        decay = 0.01,
        maxit = 1000,
        trace = TRUE
      ))
    }
  }
  
  stop(e)  # Propager l'erreur si ce n'est pas lié au nombre de poids
})

# Évaluer le modèle multinom de base
cat("\nÉvaluation du modèle multinom de base...\n")
y_pred_multinom <- predict(multinom_model, newdata = DfFinalTest)
acc_multinom <- mean(y_pred_multinom == DfFinalTest$dv_voteChoice)
cat("Accuracy du modèle multinom de base:", round(acc_multinom * 100, 2), "%\n")

# Créer une matrice de confusion
conf_mat_multinom <- table(
  predicted = y_pred_multinom, 
  actual = DfFinalTest$dv_voteChoice
)
cat("\nMatrice de confusion du modèle multinom de base:\n")
print(conf_mat_multinom)

# Optimiser la matrice idéologique avec l'approche améliorée
cat("\nOptimisation de la matrice idéologique avec l'approche améliorée...\n")

# Initialiser la matrice idéologique
parties <- tolower(levels(DfTrain$dv_voteChoice))
n_parties <- length(parties)

# Créer une matrice de base
base_proximity <- matrix(0, nrow = n_parties, ncol = n_parties)
rownames(base_proximity) <- parties
colnames(base_proximity) <- parties

# Remplir la diagonale avec 1
for (i in 1:n_parties) {
  base_proximity[i, i] <- 1.0
}

# Définir les proximités initiales (repris de l'existant)
# Proximités pour Bloc Québécois (bq)
base_proximity["bq", "cpc"] <- 0.2; base_proximity["cpc", "bq"] <- 0.2
base_proximity["bq", "gpc"] <- 0.3; base_proximity["gpc", "bq"] <- 0.3
base_proximity["bq", "lpc"] <- 0.4; base_proximity["lpc", "bq"] <- 0.4
base_proximity["bq", "ndp"] <- 0.5; base_proximity["ndp", "bq"] <- 0.5

# Proximités pour Parti conservateur (cpc)
base_proximity["cpc", "gpc"] <- 0.3; base_proximity["gpc", "cpc"] <- 0.3
base_proximity["cpc", "lpc"] <- 0.5; base_proximity["lpc", "cpc"] <- 0.5
base_proximity["cpc", "ndp"] <- 0.3; base_proximity["ndp", "cpc"] <- 0.3

# Proximités pour Parti vert (gpc)
base_proximity["gpc", "lpc"] <- 0.6; base_proximity["lpc", "gpc"] <- 0.6
base_proximity["gpc", "ndp"] <- 0.7; base_proximity["ndp", "gpc"] <- 0.7

# Proximités pour Parti libéral (lpc) et NPD (ndp)
base_proximity["lpc", "ndp"] <- 0.7; base_proximity["ndp", "lpc"] <- 0.7

# Ajouter la matrice et les métadonnées au modèle
multinom_model$ideological_proximity <- base_proximity
multinom_model$ideology_params <- list(original_weight = 0.7)  # 70% prédiction originale, 30% idéologie

# Enregistrer la méthode personnalisée
class(multinom_model) <- c("multinom_enhanced", class(multinom_model))

# Optimiser la matrice idéologique avec l'approche améliorée
optimal_proximity <- optimize_ideological_matrix_improved(
  multinom_model, 
  DfVal, 
  DfVal$dv_voteChoice,
  parties = parties
)

# Si l'optimisation a échoué, garder la matrice initiale
if (optimal_proximity$accuracy == -1) {
  cat("L'optimisation de la matrice a échoué, conservation de la matrice initiale\n")
  optimal_proximity$matrix <- multinom_model$ideological_proximity
}

# Ajouter la matrice optimisée au modèle
multinom_model$ideological_proximity <- optimal_proximity$matrix

# Optimiser les poids par parti
cat("\nOptimisation des poids idéologiques par parti...\n")

# Changer la classe du modèle pour utiliser la version avec poids par parti
class(multinom_model) <- c("multinom_enhanced_v2", 
                          class(multinom_model)[!class(multinom_model) %in% 
                                               c("multinom_enhanced", "multinom_enhanced_v2")])

# Optimiser les poids spécifiques à chaque parti
optimal_weights <- optimize_ideology_weights_by_party(
  multinom_model,
  DfVal,
  DfVal$dv_voteChoice,
  weight_range = c(0.5, 0.95),
  step = 0.05
)

# Mettre à jour le modèle avec les poids optimisés
multinom_model$ideology_params$party_weights <- optimal_weights$party_weights

# Évaluation finale avec toutes les améliorations
cat("\nÉvaluation finale du modèle avec poids par parti...\n")

# Prédictions avec les poids optimisés par parti
y_pred_optimal <- predict(multinom_model, DfFinalTest, use_ideology = TRUE)

# Calculer l'accuracy globale
acc_optimal <- mean(y_pred_optimal == DfFinalTest$dv_voteChoice)
cat("Accuracy du modèle avec poids optimisés par parti:", round(acc_optimal * 100, 2), "%\n")

# Créer une matrice de confusion
conf_mat_optimal <- table(
  predicted = y_pred_optimal, 
  actual = DfFinalTest$dv_voteChoice
)
cat("\nMatrice de confusion du modèle avec poids optimisés par parti:\n")
print(conf_mat_optimal)

# Comparer les performances
cat("\n===== COMPARAISON DES PERFORMANCES =====\n")
cat("Accuracy du modèle multinom de base (sans correction):", 
    round(acc_multinom * 100, 2), "%\n")
cat("Accuracy du modèle avec poids optimisés par parti:", 
    round(acc_optimal * 100, 2), "%\n")
cat("Amélioration totale:", round((acc_optimal - acc_multinom) * 100, 2), "%\n")

# Analyser par parti
party_improvements <- data.frame(
  party = character(),
  base_correct = numeric(),
  optimal_correct = numeric(),
  improvement_pct = numeric(),
  stringsAsFactors = FALSE
)

for (party in levels(DfFinalTest$dv_voteChoice)) {
  party_indices <- which(DfFinalTest$dv_voteChoice == party)
  party_count <- length(party_indices)
  
  base_correct <- sum(y_pred_multinom[party_indices] == party)
  optimal_correct <- sum(y_pred_optimal[party_indices] == party)
  
  party_improvements <- rbind(party_improvements, data.frame(
    party = party,
    base_correct = base_correct,
    optimal_correct = optimal_correct,
    correct_rate_base = round(base_correct / party_count * 100, 1),
    correct_rate_optimal = round(optimal_correct / party_count * 100, 1),
    improvement_pct = round((optimal_correct - base_correct) / party_count * 100, 1),
    stringsAsFactors = FALSE
  ))
}

cat("\nAméliorations par parti:\n")
print(party_improvements)

# Sauvegarder le modèle final avec toutes ses composantes
multinom_model$train_accuracy <- acc_multinom
multinom_model$optimal_accuracy <- acc_optimal
multinom_model$improvement <- round((acc_optimal - acc_multinom) * 100, 2)
multinom_model$party_improvements <- party_improvements
multinom_model$formula <- formula_final
multinom_model$final_vars <- final_vars
multinom_model$selected_interactions <- selected_interactions

# S'assurer que la classe du modèle est correctement définie pour utiliser les poids par parti
class(multinom_model) <- c("multinom_enhanced_v2", 
                          class(multinom_model)[!class(multinom_model) %in% 
                                               c("multinom_enhanced", "multinom_enhanced_v2")])

# Créer un guide d'utilisation complet
usage_guide <- paste(
  "# Guide d'utilisation du modèle électoral optimisé\n\n",
  "Ce modèle multinom a été optimisé avec:\n",
  "- Interactions sélectionnées systématiquement\n",
  "- Équilibrage des données amélioré\n",
  "- Matrice idéologique optimisée\n",
  "- Poids idéologiques spécifiques par parti\n\n",
  
  "## Chargement du modèle\n",
  "```r\n",
  "model <- readRDS('_SharedFolder_datagotchi_federal_2024/data/modele/multinom_optimized_model.rds')\n",
  "```\n\n",
  
  "## Prédiction standard (sans correction idéologique)\n",
  "```r\n",
  "predictions <- predict(model, newdata = new_data, use_ideology = FALSE)\n",
  "```\n\n",
  
  "## Prédiction avec correction idéologique et poids par parti\n",
  "```r\n",
  "predictions <- predict(model, newdata = new_data, use_ideology = TRUE)\n",
  "```\n\n",
  
  "## Obtention des probabilités\n",
  "```r\n",
  "probabilities <- predict(model, newdata = new_data, type = 'probs', use_ideology = TRUE)\n",
  "```\n\n",
  
  "## Paramètres du modèle\n",
  "- Poids idéologiques par parti:\n",
  paste(capture.output(print(multinom_model$ideology_params$party_weights)), collapse = "\n"),
  "\n\n",
  
  "## Performance\n",
  "- Accuracy sans correction: ", round(acc_multinom * 100, 2), "%\n",
  "- Accuracy avec poids optimisés: ", round(acc_optimal * 100, 2), "%\n",
  "- Amélioration: ", round((acc_optimal - acc_multinom) * 100, 2), "%\n\n",
  
  "## Performances par parti\n",
  paste(capture.output(print(party_improvements)), collapse = "\n"),
  "\n",
  sep = ""
)

multinom_model$usage_guide <- usage_guide

# Sauvegarder le modèle final
saveRDS(multinom_model, "_SharedFolder_datagotchi_federal_2024/data/modele/multinom_optimized_model_2025-04-13.rds")

# Documentation pour les développeurs
cat("\n========== INSTRUCTIONS POUR LES DÉVELOPPEURS ==========\n")
cat("Le nouveau modèle optimisé (multinom_optimized_model.rds) offre plusieurs améliorations:\n")
cat("1. Sélection systématique des interactions pertinentes\n")
cat("2. Équilibrage des données amélioré\n")
cat("3. Matrice idéologique optimisée avec une approche itérative avancée\n")
cat("4. Poids idéologiques spécifiques à chaque parti\n\n")

cat("Pour utiliser le modèle sans correction idéologique:\n")
cat("model <- readRDS('_SharedFolder_datagotchi_federal_2024/data/modele/multinom_optimized_model.rds')\n")
cat("predictions <- predict(model, newdata = new_data, use_ideology = FALSE)\n\n")

cat("Pour utiliser le modèle avec correction idéologique et poids par parti:\n")
cat("model <- readRDS('_SharedFolder_datagotchi_federal_2024/data/modele/multinom_optimized_model.rds')\n")
cat("predictions <- predict(model, newdata = new_data, use_ideology = TRUE)\n\n")

cat("L'amélioration globale attendue est d'environ", 
    round((acc_optimal - acc_multinom) * 100, 1), 
    "points de pourcentage.\n")

# Arrêter le cluster parallèle
stopCluster(cl)
