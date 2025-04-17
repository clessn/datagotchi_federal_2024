#' Construction du modèle RTA amélioré (version 2, 15 avril 2025)
#' 
#' Ce script construit un modèle multinomial qui intègre les prédictions basées sur
#' les RTA (Forward Sortation Area). Il fusionne les données pilotes et d'application,
#' harmonise les facteurs entre les jeux de données, ajoute les prédictions par RTA,
#' et évalue les performances du modèle.
#'
#' Entrée :
#' - Données pilote nettoyées (datagotchi2025_canada_pilot_20250310.rds)
#' - Données d'application nettoyées (datagotchi2025_canada_app_20250314.rds)
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
DataApp <- readRDS("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/20250413_n78871datagotchi2025_canada_app.rds")

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
# 3) Filtrer les données pour retirer "other" dès le début
# ------------------------------------------------------------------------
# Filtrer les données pour retirer "other" avant la standardisation
DataPilot <- DataPilot %>% 
  filter(dv_voteChoice != "other" & !is.na(dv_voteChoice))

DataApp <- DataApp %>% 
  filter(dv_voteChoice != "other" & !is.na(dv_voteChoice))

# ------------------------------------------------------------------------
# 4) Vérification et harmonisation des facteurs
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
# Identifier tous les niveaux possibles (excluant 'other' qui a déjà été filtré)
all_levels <- unique(c(unique_values_pilot, unique_values_app))

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
# 4.1) Préparation des données
# ------------------------------------------------------------------------
# Ajouter une variable source pour identifier l'origine des données
DataPilot$source <- "pilote"
DataApp$source <- "application"

# Sélection des variables nécessaires (plus besoin de filtrer "other" car déjà fait)
DataPilot_selected <- DataPilot %>%
  select(all_of(c(model_variables, "dv_voteChoice", "source", "ses_postalCode"))) %>%
  drop_na()

DataApp_selected <- DataApp %>%
  select(all_of(c(model_variables, "dv_voteChoice", "source", "ses_postalCode"))) %>%
  drop_na()

# ------------------------------------------------------------------------
# 5) Enrichissement avec les prédictions par RTA
# ------------------------------------------------------------------------

# Ensuite, vous pouvez continuer avec votre code initial
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
# Fonction pour harmoniser les types de données entre deux dataframes
harmonize_column_types <- function(df1, df2) {
  common_cols <- intersect(names(df1), names(df2))
  
  for (col in common_cols) {
    type1 <- class(df1[[col]])[1]
    type2 <- class(df2[[col]])[1]
    
    if (type1 != type2) {
      cat("Différence de type détectée pour", col, ":", type1, "vs", type2, "\n")
      
      # Convertir en character si l'un des deux est character
      if ("character" %in% c(type1, type2)) {
        cat("Conversion en character pour", col, "\n")
        df1[[col]] <- as.character(df1[[col]])
        df2[[col]] <- as.character(df2[[col]])
      }
      # Convertir en numeric si possible
      else if (all(c("numeric", "integer") %in% c(type1, type2))) {
        cat("Conversion en numeric pour", col, "\n")
        df1[[col]] <- as.numeric(df1[[col]])
        df2[[col]] <- as.numeric(df2[[col]])
      }
      # Pour les facteurs, convertir en character puis fusionner les niveaux
      else if ("factor" %in% c(type1, type2)) {
        cat("Conversion de facteur pour", col, "\n")
        df1[[col]] <- as.character(df1[[col]])
        df2[[col]] <- as.character(df2[[col]])
      }
    }
  }
  
  return(list(df1 = df1, df2 = df2))
}

# Harmoniser les types avant fusion
harmonized <- harmonize_column_types(DataPilot_enriched, DataApp_enriched)
DataPilot_harmonized <- harmonized$df1
DataApp_harmonized <- harmonized$df2

# Essayer la fusion avec les dataframes harmonisés
DataModel <- bind_rows(DataPilot_harmonized, DataApp_harmonized)

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
# 9) Construction du modèle enrichi avec les variables RTA
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

# 2. Fixer les références pour les variables catégorielles dans X_train_final
# Vérifier d'abord si les variables sont bien des facteurs
for (var_name in names(X_train_final)) {
  if (!is.factor(X_train_final[[var_name]])) {
    cat("La variable", var_name, "n'est pas un facteur, ignorer\n")
    next
  }
  
  # Définir manuellement les références selon la liste fournie
  if (var_name == "ses_region" && "prairie" %in% levels(X_train_final[[var_name]])) {
    X_train_final[[var_name]] <- relevel(X_train_final[[var_name]], ref = "prairie")
    cat("ses_region: 'prairie' définie comme référence\n")
  }
  else if (var_name == "lifestyle_typeTransport" && "active_transport" %in% levels(X_train_final[[var_name]])) {
    X_train_final[[var_name]] <- relevel(X_train_final[[var_name]], ref = "active_transport")
    cat("lifestyle_typeTransport: 'active_transport' définie comme référence\n")
  }
  else if (var_name == "lifestyle_consClothes" && "large_retailers" %in% levels(X_train_final[[var_name]])) {
    X_train_final[[var_name]] <- relevel(X_train_final[[var_name]], ref = "large_retailers")
    cat("lifestyle_consClothes: 'large_retailers' définie comme référence\n")
  }
  else if (var_name == "lifestyle_exercise" && "gym" %in% levels(X_train_final[[var_name]])) {
    X_train_final[[var_name]] <- relevel(X_train_final[[var_name]], ref = "gym")
    cat("lifestyle_exercise: 'gym' définie comme référence\n")
  }
  else if (var_name == "lifestyle_favAlcool" && "beer" %in% levels(X_train_final[[var_name]])) {
    X_train_final[[var_name]] <- relevel(X_train_final[[var_name]], ref = "beer")
    cat("lifestyle_favAlcool: 'beer' définie comme référence\n")
  }
  else if (var_name == "lifestyle_consCoffee" && "tim_hortons" %in% levels(X_train_final[[var_name]])) {
    X_train_final[[var_name]] <- relevel(X_train_final[[var_name]], ref = "tim_hortons")
    cat("lifestyle_consCoffee: 'tim_hortons' définie comme référence\n")
  }
  else if (var_name == "ses_language" && "english" %in% levels(X_train_final[[var_name]])) {
    X_train_final[[var_name]] <- relevel(X_train_final[[var_name]], ref = "english")
    cat("ses_language: 'english' définie comme référence\n")
  }
  else if (var_name == "ses_dwelling_cat" && "stand_alone_house" %in% levels(X_train_final[[var_name]])) {
    X_train_final[[var_name]] <- relevel(X_train_final[[var_name]], ref = "stand_alone_house")
    cat("ses_dwelling_cat: 'stand_alone_house' définie comme référence\n")
  }
  else if (var_name == "lifestyle_clothingStyleGroups" && "easygoing" %in% levels(X_train_final[[var_name]])) {
    X_train_final[[var_name]] <- relevel(X_train_final[[var_name]], ref = "easygoing")
    cat("lifestyle_clothingStyleGroups: 'easygoing' définie comme référence\n")
  }
  else if (var_name == "ses_educ" && "no_schooling" %in% levels(X_train_final[[var_name]])) {
    X_train_final[[var_name]] <- relevel(X_train_final[[var_name]], ref = "no_schooling")
    cat("ses_educ: 'no_schooling' définie comme référence\n")
  }
  else if (var_name == "ses_income3Cat" && "High" %in% levels(X_train_final[[var_name]])) {
    X_train_final[[var_name]] <- relevel(X_train_final[[var_name]], ref = "High")
    cat("ses_income3Cat: 'High' définie comme référence\n")
  }
}

# 3. Recréer la matrice de dummy variables après avoir réordonnancé les facteurs
cat("Recréation des variables dummy après recodage des références...\n")
dummies_final <- dummyVars(" ~ .", data = X_train_final, fullRank = TRUE, sep = "_")
X_train_dummy <- predict(dummies_final, newdata = X_train_final) %>% as.data.frame()

# Vérification de la nouvelle matrice dummy
cat("Nouvelles dimensions de la matrice d'entraînement:", dim(X_train_dummy), "\n")

# Vérifier les variables binaires qui doivent avoir 0 comme référence
# Pour ces variables, nous n'avons pas besoin de faire de changement car 
# la référence est déjà 0 par défaut avec dummyVars et fullRank=TRUE

# Entraînement du modèle final
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

X_test_dummy <- predict(dummies_final, newdata = X_test_final) %>% as.data.frame()

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
# Sauvegarder le modèle
saveRDS(final_model, "_SharedFolder_datagotchi_federal_2024/data/modele/finalmodel_withRTAPredictions_2025-04-15.rds")

# Sauvegarder également les dummies pour les futures prédictions
saveRDS(dummies_final, "_SharedFolder_datagotchi_federal_2024/data/modele/dummies_finalmodel_withRTAPredictions_2025-04-15.rds")

cat("Modèle enrichi avec prédictions RTA sauvegardé avec succès.\n")

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
