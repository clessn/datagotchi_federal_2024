#' Construction du modèle RTA amélioré (version 2, 15 avril 2025)
#' 
#' Ce script construit un modèle multinomial qui intègre les prédictions basées sur
#' les RTA (Forward Sortation Area) et ajoute des interactions entre variables clés.
#' Il fusionne les données pilotes et d'application, harmonise les facteurs entre les jeux 
#' de données, ajoute les prédictions par RTA, et évalue les performances du modèle.
#'
#' Entrée :
#' - Données pilote nettoyées (datagotchi2025_canada_pilot_20250322.rds)
#' - Données d'application nettoyées (datagotchi2025_canada_app_20250403.rds)
#' - Prédictions par RTA (rta_predictions_partis.csv)
#' - Résultats d'entraînement précédents (resultsTrainV4_31janvier2025.rds)
#' - Modèle précédent (finalmodel_withOutInteractions.rds)
#'
#' Sortie :
#' - Modèle final avec prédictions RTA (finalmodel_withRTAPredictions_2025-04-15.rds)
#' - Variables dummy pour prédictions futures (dummies_finalmodel_withRTAPredictions_2025-04-15.rds)
#'
# ------------------------------------------------------------------------
# 1) Chargement des packages
# ------------------------------------------------------------------------
library(parallel)
library(nnet)
library(caret)
library(tidyverse)
library(yardstick)
library(pbapply)

# ------------------------------------------------------------------------
# 2) Chargement des données (pilote et application)
# ------------------------------------------------------------------------
# Données pilote (déjà nettoyées)
DataPilot <- readRDS("_SharedFolder_datagotchi_federal_2024/data/pilote/dataClean/datagotchi2025_canada_pilot_20250322.rds")

# Nouvelles données de l'application
DataApp <- readRDS("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/datagotchi2025_canada_app_20250403.rds")

# Chargement des prédictions par RTA
rta_predictions <- read.csv("_SharedFolder_datagotchi_federal_2024/data/modele/rta_predictions_partis.csv",
                           stringsAsFactors = FALSE)

# Charger le modèle précédent et les résultats pour obtenir les variables du modèle
results_train <- readRDS("_SharedFolder_datagotchi_federal_2024/data/modele/_previous/resultsTrainV4_31janvier2025.rds")
previous_model <- readRDS("_SharedFolder_datagotchi_federal_2024/data/modele/_previous/finalmodel_withOutInteractions.rds")

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
cat("Liste des variables:", paste(model_variables, collapse=", "), "\n")

# ------------------------------------------------------------------------
# 3) Vérification et harmonisation des facteurs
# ------------------------------------------------------------------------
# Vérifier le statut de dv_voteChoice dans les deux jeux de données
cat("Class de dv_voteChoice dans DataPilot:", class(DataPilot$dv_voteChoice), "\n")
cat("Class de dv_voteChoice dans DataApp:", class(DataApp$dv_voteChoice), "\n")

# Vérification des valeurs uniques dans DataPilot
unique_values_pilot <- unique(as.character(DataPilot$dv_voteChoice))
cat("Valeurs uniques dans DataPilot:", paste(unique_values_pilot, collapse=", "), "\n")

# Vérification des valeurs uniques dans DataApp
unique_values_app <- unique(as.character(DataApp$dv_voteChoice))
cat("Valeurs uniques dans DataApp:", paste(unique_values_app, collapse=", "), "\n")

# Standardisation: convertir explicitement les deux variables en facteurs avec les mêmes niveaux
# Identifier tous les niveaux possibles (excluant 'other' qui sera filtré)
all_levels <- unique(c(
  unique_values_pilot[unique_values_pilot != "other"],
  unique_values_app[unique_values_app != "other"]
))

# Standardiser les noms de partis (npd -> ndp si nécessaire)
all_levels <- unique(gsub("npd", "ndp", all_levels))

cat("Niveaux standardisés à utiliser:", paste(all_levels, collapse=", "), "\n")

# Convertir DataPilot$dv_voteChoice en facteur avec les niveaux standardisés
DataPilot$dv_voteChoice <- factor(
  gsub("npd", "ndp", as.character(DataPilot$dv_voteChoice)),
  levels = all_levels
)

# Convertir DataApp$dv_voteChoice en facteur avec les mêmes niveaux
DataApp$dv_voteChoice <- factor(
  gsub("npd", "ndp", as.character(DataApp$dv_voteChoice)),
  levels = all_levels
)

# Vérifier que les niveaux sont maintenant correctement définis
cat("Niveaux de dv_voteChoice dans DataPilot:", 
    paste(levels(DataPilot$dv_voteChoice), collapse=", "), "\n")
cat("Niveaux de dv_voteChoice dans DataApp:", 
    paste(levels(DataApp$dv_voteChoice), collapse=", "), "\n")

# Fonction pour harmoniser les autres facteurs
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

# Identifier les variables du modèle qui sont des facteurs
model_factor_vars <- model_variables[model_variables %in% names(DataPilot)]
cat("Variables facteurs potentielles:", paste(model_factor_vars, collapse=", "), "\n")

# Identifier les facteurs dans DataPilot
pilot_factors <- names(DataPilot)[sapply(DataPilot, is.factor)]
cat("Facteurs dans DataPilot:", paste(pilot_factors, collapse=", "), "\n")

# Obtenir l'intersection
model_factors <- intersect(model_factor_vars, pilot_factors)
cat("Facteurs du modèle à harmoniser:", paste(model_factors, collapse=", "), "\n")

# Harmoniser les facteurs importants
for (f in model_factors) {
  if (f %in% names(DataPilot) && f %in% names(DataApp)) {
    cat("Harmonisation du facteur:", f, "\n")
    
    # Vérifier et harmoniser
    harmonized <- harmonize_factor(DataPilot[[f]], DataApp[[f]])
    DataPilot[[f]] <- harmonized$pilot
    DataApp[[f]] <- harmonized$app
    
    cat("  Niveaux harmonisés:", paste(levels(DataPilot[[f]]), collapse=", "), "\n")
  }
}

# ------------------------------------------------------------------------
# 4) Préparation des données
# ------------------------------------------------------------------------
# Ajouter une variable source pour identifier l'origine des données
DataPilot$source <- "pilote"
DataApp$source <- "application"

# Sélection des variables nécessaires
# Pour DataPilot, filtrer seulement les observations avec des valeurs autres que "other"
DataPilot_selected <- DataPilot %>%
  filter(dv_voteChoice != "other" & !is.na(dv_voteChoice)) %>%
  select(all_of(c(model_variables, "dv_voteChoice", "source", "ses_postalCode"))) %>%
  drop_na()

# Pour DataApp, sélectionner les variables et ajouter ses_postalCode
DataApp_selected <- DataApp %>%
  filter(dv_voteChoice != "other" & !is.na(dv_voteChoice)) %>%
  select(all_of(c(model_variables, "dv_voteChoice", "source", "ses_postalCode"))) %>%
  drop_na()

# ------------------------------------------------------------------------
# 5) Enrichissement avec les prédictions par RTA
# ------------------------------------------------------------------------
DataPilot_selected <- DataPilot_selected %>%
  mutate(
    # Standardiser les codes postaux (majuscules)
    ses_postalCode_clean = toupper(as.character(ses_postalCode)),
    # Extraire les RTA (premiers 3 caractères)
    rta = substr(ses_postalCode_clean, 1, 3)
  )

DataApp_selected <- DataApp_selected %>%
  mutate(
    # Standardiser les codes postaux (majuscules)
    ses_postalCode_clean = toupper(as.character(ses_postalCode)),
    # Extraire les RTA (premiers 3 caractères)
    rta = substr(ses_postalCode_clean, 1, 3)
  )

# Standardiser les RTA dans le fichier de prédictions (pour être sûr)
rta_predictions$rta <- toupper(rta_predictions$rta)

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

# Enrichir les deux jeux de données
DataPilot_enriched <- enrich_with_predictions(DataPilot_selected, rta_predictions)
DataApp_enriched <- enrich_with_predictions(DataApp_selected, rta_predictions)

# ------------------------------------------------------------------------
# 6) Fusion des données et préparation pour modélisation
# ------------------------------------------------------------------------
# Fusionner les données
DataModel <- bind_rows(DataPilot_enriched, DataApp_enriched)

# Vérifier les prédictions par source
prediction_summary <- DataModel %>%
  group_by(source) %>%
  summarise(
    n = n(),
    missing_rta = sum(is.na(rta)),
    matched_rta = sum(rta %in% rta_predictions$rta),
    avg_prediction_CPC = mean(prediction_CPC, na.rm = TRUE),
    avg_prediction_LPC = mean(prediction_LPC, na.rm = TRUE),
    avg_prediction_NDP = mean(prediction_NDP, na.rm = TRUE)
  ) %>%
  mutate(
    pct_matched_rta = matched_rta / n * 100
  )

print(prediction_summary)

# Vérifier que les colonnes importantes sont présentes
cat("Vérification des colonnes importantes dans DataModel:\n")
cat("- prediction_CPC présente:", "prediction_CPC" %in% names(DataModel), "\n")
cat("- prediction_LPC présente:", "prediction_LPC" %in% names(DataModel), "\n")
cat("- prediction_NDP présente:", "prediction_NDP" %in% names(DataModel), "\n")
cat("- prediction_GPC présente:", "prediction_GPC" %in% names(DataModel), "\n")
cat("- prediction_BQ présente:", "prediction_BQ" %in% names(DataModel), "\n")
cat("- rta présente:", "rta" %in% names(DataModel), "\n")
cat("- ses_postalCode présente:", "ses_postalCode" %in% names(DataModel), "\n")

# ------------------------------------------------------------------------
# 7) Séparation Train/Test avec stratification par source
# ------------------------------------------------------------------------
set.seed(42)
# Stratifier par parti et par source pour maintenir la distribution
trainIndex <- createDataPartition(
  interaction(DataModel$dv_voteChoice, DataModel$source), 
  p = 0.8, 
  list = FALSE
)
DfTrain <- DataModel[trainIndex, ]
DfTest <- DataModel[-trainIndex, ]

# Vérification de la distribution dans les ensembles train et test
cat("Distribution dans l'ensemble d'entraînement:", "\n")
print(table(DfTrain$dv_voteChoice, DfTrain$source))

cat("Distribution dans l'ensemble de test:", "\n")
print(table(DfTest$dv_voteChoice, DfTest$source))

# ------------------------------------------------------------------------
# 8) Fonction d'évaluation multiclasse
# ------------------------------------------------------------------------
multiClassSummary2 <- function(data, lev = NULL, model = NULL) {
  # 1) Accuracy
  acc <- yardstick::accuracy_vec(data$obs, data$pred)
  
  # 2) Kappa
  kap <- yardstick::kap_vec(data$obs, data$pred)
  
  # 3) LogLoss
  eps <- 1e-15
  n <- nrow(data)
  ll <- 0
  for (i in seq_len(n)) {
    obs_class <- as.character(data$obs[i])
    prob_col <- paste0("prob.", obs_class)
    p <- data[[prob_col]][i]
    p <- max(p, eps)
    ll <- ll - log(p)
  }
  ll <- ll / n
  
  # 4) F1-score macro
  classes <- if (!is.null(lev)) lev else levels(data$obs)
  f1s <- sapply(classes, function(cl) {
    tp <- sum(data$obs == cl & data$pred == cl)
    fp <- sum(data$obs != cl & data$pred == cl)
    fn <- sum(data$obs == cl & data$pred != cl)
    precision <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
    recall <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
    if (precision + recall == 0) 0 else 2 * precision * recall / (precision + recall)
  })
  f1_macro <- mean(f1s)
  
  # 5) Calcul d'une pénalité personnalisée pour les erreurs critiques
  conf <- table(data$pred, data$obs)
  penalty <- 0
  penalty_value <- 5  # Coefficient de pénalité
  if ("ndp" %in% lev && "cpc" %in% lev) {
    if ("cpc" %in% rownames(conf) && "ndp" %in% colnames(conf)) {
      penalty <- penalty + penalty_value * conf["cpc", "ndp"]
    }
  }
  if ("gpc" %in% lev && "cpc" %in% lev) {
    if ("cpc" %in% rownames(conf) && "gpc" %in% colnames(conf)) {
      penalty <- penalty + penalty_value * conf["cpc", "gpc"]
    }
  }
  composite_score <- acc - (penalty / n)
  
  out <- c(accuracy = acc, kappa = kap, logLoss = ll, f1 = f1_macro, composite_score = composite_score)
  return(out)
}

# ------------------------------------------------------------------------
# 8.5) Validation des interactions individuelles
# ------------------------------------------------------------------------

# Fonction pour tester une interaction spécifique
test_interaction <- function(interaction_term, X_train, y_train, X_test, y_test) {
  # Modèle de base sans interactions
  base_data <- X_train[, !grepl(":", colnames(X_train))]
  base_model <- multinom(y_train ~ ., data = base_data, trace = FALSE, MaxNWts = 100000)
  
  # Prédictions du modèle de base
  base_test_data <- X_test[, !grepl(":", colnames(X_test))]
  base_preds <- predict(base_model, newdata = base_test_data)
  base_acc <- mean(base_preds == y_test)
  
  # Colonnes pour cette interaction spécifique
  interaction_cols <- grep(paste0(interaction_term, "|", gsub(":", ".*:", interaction_term)), 
                           colnames(X_train), value = TRUE)
  
  # Si aucune colonne trouvée, retourner résultat nul
  if (length(interaction_cols) == 0) {
    return(list(interaction = interaction_term, base_acc = base_acc, 
                interaction_acc = NA, diff = NA, n_params = 0))
  }
  
  # Modèle avec l'interaction spécifique
  interaction_data <- cbind(base_data, X_train[, interaction_cols, drop = FALSE])
  interaction_model <- multinom(y_train ~ ., data = interaction_data, trace = FALSE, MaxNWts = 100000)
  
  # Prédictions du modèle avec interaction
  interaction_test_data <- cbind(base_test_data, X_test[, interaction_cols, drop = FALSE])
  interaction_preds <- predict(interaction_model, newdata = interaction_test_data)
  interaction_acc <- mean(interaction_preds == y_test)
  
  # Retourner les résultats
  return(list(interaction = interaction_term, 
              base_acc = base_acc, 
              interaction_acc = interaction_acc,
              diff = interaction_acc - base_acc,
              n_params = length(interaction_cols)))
}

# Tester chaque interaction individuellement
cat("Évaluation des interactions individuelles:\n")
interaction_results <- list()

for (interaction in interactions_to_add) {
  cat("Évaluation de", interaction, "...\n")
  result <- test_interaction(interaction, X_train_dummy, y_train_final, X_test_dummy, y_test_final)
  interaction_results[[interaction]] <- result
  cat("  Accuracy de base:", result$base_acc, "\n")
  cat("  Accuracy avec interaction:", result$interaction_acc, "\n")
  cat("  Différence:", result$diff, "\n")
  cat("  Nombre de paramètres ajoutés:", result$n_params, "\n\n")
}

# Résumer les résultats
interaction_summary <- data.frame(
  interaction = sapply(interaction_results, function(x) x$interaction),
  base_acc = sapply(interaction_results, function(x) x$base_acc),
  interaction_acc = sapply(interaction_results, function(x) x$interaction_acc),
  difference = sapply(interaction_results, function(x) x$diff),
  n_params = sapply(interaction_results, function(x) x$n_params)
)

interaction_summary <- interaction_summary[order(-interaction_summary$difference), ]
print(interaction_summary)

# Identifier les interactions bénéfiques
beneficial_interactions <- interaction_summary$interaction[interaction_summary$difference > 0]
cat("Interactions qui améliorent le modèle:", paste(beneficial_interactions, collapse=", "), "\n")

# Suggestion: Ne conserver que les interactions bénéfiques
if (length(beneficial_interactions) > 0) {
  cat("Suggestion: Ne conserver que les interactions suivantes dans le modèle final:\n")
  cat(paste(beneficial_interactions, collapse="\n"), "\n")
}

# ------------------------------------------------------------------------
# 9) Construction du modèle enrichi avec les variables RTA et les interactions ciblées
# ------------------------------------------------------------------------
# Variables du modèle précédent + nouvelles variables de prédiction RTA
final_vars <- c(model_variables, "prediction_CPC", "prediction_LPC", "prediction_NDP", "prediction_GPC", "prediction_BQ")

# Préparation des données pour le modèle
X_train_final <- DfTrain[, final_vars, drop = FALSE]
y_train_final <- DfTrain$dv_voteChoice

# Création des variables dummy
dummies_final <- dummyVars(" ~ .", data = X_train_final, fullRank = TRUE, sep = "_")
X_train_dummy <- predict(dummies_final, newdata = X_train_final) %>% as.data.frame()

# Vérification des dimensions de la matrice
cat("Dimensions de la matrice d'entraînement:", dim(X_train_dummy), "\n")

# Définir les termes d'interaction - VERSION SIMPLIFIÉE
interactions_to_add <- c(
  "ses_region:ses_language",           # Interaction région et langue
  "ses_region:ses_educ",               # Interaction région et éducation
  "ses_educ:ses_income3Cat"            # Interaction éducation et revenu
)

# Vérifier la présence des variables d'interaction
interaction_vars <- unique(unlist(strsplit(gsub(":", " ", paste(interactions_to_add, collapse=" ")), " ")))
missing_vars <- setdiff(interaction_vars, names(X_train_final))

if (length(missing_vars) > 0) {
  cat("ATTENTION: Variables manquantes pour les interactions:", paste(missing_vars, collapse=", "), "\n")
} else {
  cat("Toutes les variables nécessaires pour les interactions sont présentes\n")
}

# Ajouter les termes d'interaction manuellement pour conserver les mêmes références
# Pour chaque interaction, on crée toutes les combinaisons possibles des niveaux
for (interaction_term in interactions_to_add) {
  # Extraire les noms des variables
  var_names <- unlist(strsplit(interaction_term, ":"))
  var1 <- var_names[1]
  var2 <- var_names[2]
  
  cat("Création de l'interaction entre", var1, "et", var2, "\n")
  
  # Vérifier que les deux variables sont des facteurs
  if (is.factor(X_train_final[[var1]]) && is.factor(X_train_final[[var2]])) {
    # Obtenez les niveaux des facteurs (sauf le niveau de référence pour fullRank=TRUE)
    levels_var1 <- levels(X_train_final[[var1]])[-1]  # Tous sauf le premier (référence)
    levels_var2 <- levels(X_train_final[[var2]])[-1]  # Tous sauf le premier (référence)
    
    # Créer une colonne pour chaque combinaison de niveaux
    for (lvl1 in levels_var1) {
      for (lvl2 in levels_var2) {
        # Nom de la colonne d'interaction
        col_name <- paste0(var1, "_", lvl1, ":", var2, "_", lvl2)
        
        # Créer la colonne d'interaction
        X_train_dummy[[col_name]] <- (X_train_final[[var1]] == lvl1) & (X_train_final[[var2]] == lvl2)
        
        # Convertir en numérique (0/1)
        X_train_dummy[[col_name]] <- as.numeric(X_train_dummy[[col_name]])
      }
    }
  } else if (is.numeric(X_train_final[[var1]]) && is.factor(X_train_final[[var2]])) {
    # Interaction entre variable numérique et facteur
    levels_var2 <- levels(X_train_final[[var2]])[-1]  # Tous sauf le premier (référence)
    
    for (lvl2 in levels_var2) {
      col_name <- paste0(var1, ":", var2, "_", lvl2)
      X_train_dummy[[col_name]] <- X_train_final[[var1]] * (X_train_final[[var2]] == lvl2)
    }
  } else if (is.factor(X_train_final[[var1]]) && is.numeric(X_train_final[[var2]])) {
    # Interaction entre facteur et variable numérique
    levels_var1 <- levels(X_train_final[[var1]])[-1]  # Tous sauf le premier (référence)
    
    for (lvl1 in levels_var1) {
      col_name <- paste0(var1, "_", lvl1, ":", var2)
      X_train_dummy[[col_name]] <- (X_train_final[[var1]] == lvl1) * X_train_final[[var2]]
    }
  } else if (is.numeric(X_train_final[[var1]]) && is.numeric(X_train_final[[var2]])) {
    # Interaction entre deux variables numériques
    col_name <- paste0(var1, ":", var2)
    X_train_dummy[[col_name]] <- X_train_final[[var1]] * X_train_final[[var2]]
  }
}

# Vérification des dimensions de la matrice après ajout des interactions
cat("Dimensions de la matrice d'entraînement avec interactions:", dim(X_train_dummy), "\n")

# AJOUT : Compter le nombre d'interactions ajoutées
interactions_columns <- grep(":", colnames(X_train_dummy), value = TRUE)
cat("Nombre d'interactions ajoutées:", length(interactions_columns), "\n")
cat("Exemples d'interactions ajoutées:", head(interactions_columns), "...\n")

# ------------------------------------------------------------------------
# Fixation manuelle des valeurs de référence des variables
# ------------------------------------------------------------------------
cat("Configuration manuelle des catégories de référence pour correspondre au modèle original...\n")

# 1. D'abord, fixer la classe de référence pour la variable dépendante dv_voteChoice
# Vérifier d'abord les niveaux actuels
cat("Niveaux actuels de dv_voteChoice:", paste(levels(y_train_final), collapse=", "), "\n")

# Supposons que "bq" est la référence dans le modèle original (à ajuster selon vos données)
if ("bq" %in% levels(y_train_final)) {
  y_train_final <- relevel(y_train_final, ref = "bq")
  cat("Variable dépendante: 'bq' définie comme référence\n")
} else {
  cat("Impossible de définir 'bq' comme référence car ce niveau n'existe pas\n")
}

# Entraînement du modèle final avec interactions
final_model <- multinom(
  y_train_final ~ ., 
  data = X_train_dummy, 
  trace = FALSE,
  MaxNWts = 100000
)

# Symétrisation des coefficients
all_levels <- levels(y_train_final)
orig_coef <- coef(final_model)

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

# ------------------------------------------------------------------------
# 10) Évaluation du modèle
# ------------------------------------------------------------------------
# Préparation des données de test
X_test_final <- DfTest[, final_vars, drop = FALSE]
y_test_final <- DfTest$dv_voteChoice

# Utiliser la même transformation dummyVars que pour les données d'entraînement
X_test_dummy <- predict(dummies_final, newdata = X_test_final) %>% as.data.frame()

# Ajouter les termes d'interaction de manière cohérente avec l'entraînement
for (interaction_term in interactions_to_add) {
  # Extraire les noms des variables
  var_names <- unlist(strsplit(interaction_term, ":"))
  var1 <- var_names[1]
  var2 <- var_names[2]
  
  # Vérifier que les deux variables sont des facteurs
  if (is.factor(X_test_final[[var1]]) && is.factor(X_test_final[[var2]])) {
    # Obtenez les niveaux des facteurs (sauf le niveau de référence pour fullRank=TRUE)
    levels_var1 <- levels(X_test_final[[var1]])[-1]  # Tous sauf le premier (référence)
    levels_var2 <- levels(X_test_final[[var2]])[-1]  # Tous sauf le premier (référence)
    
    # Créer une colonne pour chaque combinaison de niveaux
    for (lvl1 in levels_var1) {
      for (lvl2 in levels_var2) {
        # Nom de la colonne d'interaction
        col_name <- paste0(var1, "_", lvl1, ":", var2, "_", lvl2)
        
        # Créer la colonne d'interaction
        X_test_dummy[[col_name]] <- (X_test_final[[var1]] == lvl1) & (X_test_final[[var2]] == lvl2)
        
        # Convertir en numérique (0/1)
        X_test_dummy[[col_name]] <- as.numeric(X_test_dummy[[col_name]])
      }
    }
  } else if (is.numeric(X_test_final[[var1]]) && is.factor(X_test_final[[var2]])) {
    # Interaction entre variable numérique et facteur
    levels_var2 <- levels(X_test_final[[var2]])[-1]  # Tous sauf le premier (référence)
    
    for (lvl2 in levels_var2) {
      col_name <- paste0(var1, ":", var2, "_", lvl2)
      X_test_dummy[[col_name]] <- X_test_final[[var1]] * (X_test_final[[var2]] == lvl2)
    }
  } else if (is.factor(X_test_final[[var1]]) && is.numeric(X_test_final[[var2]])) {
    # Interaction entre facteur et variable numérique
    levels_var1 <- levels(X_test_final[[var1]])[-1]  # Tous sauf le premier (référence)
    
    for (lvl1 in levels_var1) {
      col_name <- paste0(var1, "_", lvl1, ":", var2)
      X_test_dummy[[col_name]] <- (X_test_final[[var1]] == lvl1) * X_test_final[[var2]]
    }
  } else if (is.numeric(X_test_final[[var1]]) && is.numeric(X_test_final[[var2]])) {
    # Interaction entre deux variables numériques
    col_name <- paste0(var1, ":", var2)
    X_test_dummy[[col_name]] <- X_test_final[[var1]] * X_test_final[[var2]]
  }
}

# Vérifier que toutes les colonnes du modèle sont présentes dans les données de test
missing_cols <- setdiff(colnames(X_train_dummy), colnames(X_test_dummy))
if (length(missing_cols) > 0) {
  cat("ATTENTION: Colonnes manquantes dans les données de test:", length(missing_cols), "\n")
  # Ajouter les colonnes manquantes avec des zéros
  for (col in missing_cols) {
    X_test_dummy[[col]] <- 0
  }
}

# S'assurer que l'ordre des colonnes est le même que celui utilisé lors de l'entraînement
X_test_dummy <- X_test_dummy[, colnames(X_train_dummy)]

# Prédictions
pred_test_final_class <- predict(final_model, newdata = X_test_dummy)
pred_test_final_prob <- predict(final_model, newdata = X_test_dummy, type = "probs")

# Évaluation globale
acc_test_final <- mean(pred_test_final_class == y_test_final)
cat("Accuracy globale (test) :", acc_test_final, "\n")

# Évaluation séparée par source
DfTest_with_preds <- DfTest %>%
  mutate(predicted = pred_test_final_class)

# Performance sur les données pilotes
acc_pilot <- DfTest_with_preds %>%
  filter(source == "pilote") %>%
  summarise(accuracy = mean(predicted == dv_voteChoice)) %>%
  pull(accuracy)
cat("Accuracy sur données pilotes :", acc_pilot, "\n")

# Performance sur les données de l'application
acc_app <- DfTest_with_preds %>%
  filter(source == "application") %>%
  summarise(accuracy = mean(predicted == dv_voteChoice)) %>%
  pull(accuracy)
cat("Accuracy sur données application :", acc_app, "\n")

# Matrice de confusion
table_test <- table(
  predicted = pred_test_final_class,
  actual = y_test_final
)
print(table_test)

# Matrices de confusion séparées par source
table_test_pilot <- with(
  DfTest_with_preds %>% filter(source == "pilote"),
  table(predicted = predicted, actual = dv_voteChoice)
)
cat("Matrice de confusion (données pilotes) :\n")
print(table_test_pilot)

table_test_app <- with(
  DfTest_with_preds %>% filter(source == "application"),
  table(predicted = predicted, actual = dv_voteChoice)
)
cat("Matrice de confusion (données application) :\n")
print(table_test_app)

# ------------------------------------------------------------------------
# 11) Sauvegarde du modèle final enrichi
# ------------------------------------------------------------------------
# Enrichir les dummies avec les informations d'interaction pour les futures prédictions
dummies_info <- list(
  dummies_obj = dummies_final,
  interaction_terms = interactions_to_add,
  model_vars = final_vars
)

# Sauvegarder le modèle
saveRDS(final_model, "_SharedFolder_datagotchi_federal_2024/data/modele/finalmodel_withRTAPredictions_2025-04-15.rds")

# Sauvegarder également les dummies pour les futures prédictions
saveRDS(dummies_info, "_SharedFolder_datagotchi_federal_2024/data/modele/dummies_finalmodel_withRTAPredictions_2025-04-15.rds")

cat("Modèle enrichi avec prédictions RTA et interactions sauvegardé avec succès.\n")

# Comparer les performances avec le modèle précédent
cat("Comparaison des performances avec le modèle précédent :\n")

# Examiner la structure des coefficients du modèle précédent
previous_coef <- coef(previous_model)
cat("Dimensions des coefficients du modèle précédent:", dim(previous_coef), "\n")
cat("Noms des lignes (classes):", rownames(previous_coef), "\n")
cat("Nombre de variables:", ncol(previous_coef), "\n")
cat("Quelques noms de variables:", head(colnames(previous_coef)), "...\n")

# Examiner la structure des coefficients du nouveau modèle
new_coef <- coef(final_model)
cat("Dimensions des coefficients du nouveau modèle:", dim(new_coef), "\n")
cat("Noms des lignes (classes):", rownames(new_coef), "\n")
cat("Nombre de variables:", ncol(new_coef), "\n")
cat("Quelques noms de variables:", head(colnames(new_coef)), "...\n")

# Vérifier les différences entre les variables
variables_previous <- colnames(previous_coef)
variables_new <- colnames(new_coef)

# Variables présentes dans l'ancien modèle mais pas dans le nouveau
only_in_previous <- setdiff(variables_previous, variables_new)
cat("Variables présentes uniquement dans l'ancien modèle:", length(only_in_previous), "\n")
if (length(only_in_previous) > 0) {
  cat("Exemples:", head(only_in_previous), "...\n")
}

# Variables présentes dans le nouveau modèle mais pas dans l'ancien
only_in_new <- setdiff(variables_new, variables_previous)
cat("Variables présentes uniquement dans le nouveau modèle:", length(only_in_new), "\n")
if (length(only_in_new) > 0) {
  cat("Exemples:", head(only_in_new), "...\n")
}

# Variables communes aux deux modèles
common_variables <- intersect(variables_previous, variables_new)
cat("Variables communes aux deux modèles:", length(common_variables), "\n")

# ------------------------------------------------------------------------
# 11.5) Évaluation comparative des modèles avec et sans interactions
# ------------------------------------------------------------------------

# Modèle sans interactions (uniquement avec les prédictions RTA)
X_train_sans_interactions <- X_train_dummy[, !grepl(":", colnames(X_train_dummy))]
cat("Dimensions de la matrice d'entraînement sans interactions:", dim(X_train_sans_interactions), "\n")

# Entraînement du modèle sans interactions
model_sans_interactions <- multinom(
  y_train_final ~ ., 
  data = X_train_sans_interactions, 
  trace = FALSE,
  MaxNWts = 100000
)

# Évaluation sur les données de test
X_test_sans_interactions <- X_test_dummy[, !grepl(":", colnames(X_test_dummy))]

# Prédictions du modèle sans interactions
pred_test_sans_interactions <- predict(model_sans_interactions, newdata = X_test_sans_interactions)
acc_test_sans_interactions <- mean(pred_test_sans_interactions == y_test_final)

# Comparaison des performances
cat("\n=== Comparaison des performances ===\n")
cat("Accuracy du modèle avec interactions    :", acc_test_final, "\n")
cat("Accuracy du modèle sans interactions    :", acc_test_sans_interactions, "\n")
cat("Différence d'accuracy                   :", acc_test_final - acc_test_sans_interactions, "\n")

# Analyse détaillée par source de données
results_comparison <- data.frame(
  source = DfTest$source,
  actual = DfTest$dv_voteChoice,
  pred_avec_interactions = pred_test_final_class,
  pred_sans_interactions = pred_test_sans_interactions
)

# Performance par source
source_comparison <- results_comparison %>%
  group_by(source) %>%
  summarise(
    n = n(),
    accuracy_avec = mean(pred_avec_interactions == actual),
    accuracy_sans = mean(pred_sans_interactions == actual),
    difference = accuracy_avec - accuracy_sans
  )

print(source_comparison)

# Analyse par parti politique
party_comparison <- results_comparison %>%
  group_by(actual) %>%
  summarise(
    n = n(),
    accuracy_avec = mean(pred_avec_interactions == actual),
    accuracy_sans = mean(pred_sans_interactions == actual),
    difference = accuracy_avec - accuracy_sans
  )

print(party_comparison)

# Test statistique pour voir si la différence est significative
# McNemar test pour comparer les modèles
cat("\nTest statistique de McNemar pour comparer les modèles:\n")
contingency_table <- table(
  avec_correct = pred_test_final_class == y_test_final,
  sans_correct = pred_test_sans_interactions == y_test_final
)
print(contingency_table)

# Si les packages nécessaires sont disponibles
if (requireNamespace("stats", quietly = TRUE)) {
  mcnemar_test <- stats::mcnemar.test(contingency_table)
  print(mcnemar_test)
  
  # Interpréter le résultat
  alpha <- 0.05
  if (mcnemar_test$p.value < alpha) {
    cat("La différence de performance entre les modèles est statistiquement significative (p <", alpha, ")\n")
  } else {
    cat("La différence de performance entre les modèles n'est pas statistiquement significative (p >=", alpha, ")\n")
  }
}
