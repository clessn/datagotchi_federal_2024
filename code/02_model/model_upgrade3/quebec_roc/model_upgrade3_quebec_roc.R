#' Construction du modèle RTA amélioré avec interactions régionales (version 4, 09 avril 2025)
#' 
#' Ce script construit un modèle multinomial qui intègre les prédictions basées sur
#' les RTA (Forward Sortation Area) et ajoute des modèles distincts pour:
#' 1. Le Québec
#' 2. Le Reste du Canada (ROC)
#' 
#' Entrée :
#' - Données pilote nettoyées (datagotchi2025_canada_pilot_20250322.rds)
#' - Données d'application nettoyées (datagotchi2025_canada_app_20250408.rds)
#' - Prédictions par RTA (rta_predictions_partis.csv)
#' - Résultats d'entraînement précédents (resultsTrainV4_31janvier2025.rds)
#' - Modèle précédent (finalmodel_withRTAPredictions_2025-04-15.rds)
#'
#' Sortie :
#' - Modèle final avec prédictions RTA et interactions régionales (finalmodel_withRTAPredictions_RegionalInteractions_2025-04-09.rds)
#' - Variables dummy pour prédictions futures (dummies_finalmodel_withRTAPredictions_RegionalInteractions_2025-04-09.rds)
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
DataApp <- readRDS("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/datagotchi2025_canada_app_20250408.rds")

# Chargement des prédictions par RTA
rta_predictions <- read.csv("_SharedFolder_datagotchi_federal_2024/data/modele/rta_predictions_partis.csv",
                           stringsAsFactors = FALSE)

# Charger le modèle précédent et les résultats pour obtenir les variables du modèle
results_train <- readRDS("_SharedFolder_datagotchi_federal_2024/data/modele/_previous/resultsTrainV4_31janvier2025.rds")
previous_model <- readRDS("_SharedFolder_datagotchi_federal_2024/data/modele/finalmodel_withRTAPredictions_2025-04-15.rds")

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
# Éliminer explicitement 'other' et NA s'ils sont présents
all_levels <- all_levels[!all_levels %in% c("other", "NA")]

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
# Pour DataPilot, filtrer seulement les observations avec des valeurs appartenant aux principaux partis
DataPilot_selected <- DataPilot %>%
  filter(dv_voteChoice %in% c("bq", "cpc", "lpc", "ndp", "gpc") & !is.na(dv_voteChoice)) %>%
  select(all_of(c(model_variables, "dv_voteChoice", "source", "ses_postalCode"))) %>%
  drop_na()

# Pour DataApp, filtrer de la même façon que pour DataPilot
DataApp_selected <- DataApp %>%
  filter(dv_voteChoice %in% c("bq", "cpc", "lpc", "ndp", "gpc") & !is.na(dv_voteChoice)) %>%
  select(all_of(c(model_variables, "dv_voteChoice", "source", "ses_postalCode"))) %>%
  drop_na()

# ------------------------------------------------------------------------
# 5) Enrichissement avec les prédictions par RTA
# ------------------------------------------------------------------------
# Standardiser les codes postaux et extraire les RTA
DataPilot_selected <- DataPilot_selected %>%
  mutate(
    ses_postalCode_clean = toupper(as.character(ses_postalCode)),
    rta = substr(ses_postalCode_clean, 1, 3)
  )

DataApp_selected <- DataApp_selected %>%
  mutate(
    ses_postalCode_clean = toupper(as.character(ses_postalCode)),
    rta = substr(ses_postalCode_clean, 1, 3)
  )

# Standardiser les RTA dans le fichier de prédictions
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
# 6) Création des variables régionales et interactions
# ------------------------------------------------------------------------
# NOUVELLE APPROCHE: Créer DEUX variables booléennes pour Québec et ROC
DataPilot_enriched <- DataPilot_enriched %>%
  mutate(
    is_quebec = as.numeric(ses_region == "quebec"),
    is_roc = as.numeric(ses_region != "quebec")
  )

DataApp_enriched <- DataApp_enriched %>%
  mutate(
    is_quebec = as.numeric(ses_region == "quebec"),
    is_roc = as.numeric(ses_region != "quebec")
  )

# Assurer que la variable de région (ses_region) est bien un facteur avec les niveaux appropriés
region_levels <- c("ontario", "quebec", "british_columbia", "prairie", "atlantic", "territories")

DataPilot_enriched <- DataPilot_enriched %>%
  mutate(
    ses_region = factor(ses_region, levels = region_levels)
  )

DataApp_enriched <- DataApp_enriched %>%
  mutate(
    ses_region = factor(ses_region, levels = region_levels)
  )

# Fusion des données pour la modélisation
DataModel <- bind_rows(DataPilot_enriched, DataApp_enriched)

# Vérifier les prédictions RTA par source
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

# ------------------------------------------------------------------------
# 7) Analyse exploratoire des régions
# ------------------------------------------------------------------------
# Observer la distribution des votes par région
region_vote_distribution <- DataModel %>%
  group_by(ses_region, dv_voteChoice) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(ses_region) %>%
  mutate(pct = n / sum(n) * 100) %>%
  arrange(ses_region, desc(pct))

print(region_vote_distribution)

# Visualiser les différences régionales dans les variables clés
regional_differences <- DataModel %>%
  group_by(ses_region) %>%
  summarise(
    n = n(),
    avg_age = mean(as.numeric(gsub("[^0-9]", "", ses_age)), na.rm = TRUE),
    pct_french = mean(ses_language == "french", na.rm = TRUE) * 100,
    pct_urban = mean(ses_dwelling_cat %in% c("apartment", "condo"), na.rm = TRUE) * 100
  )

print(regional_differences)

# ------------------------------------------------------------------------
# 8) Séparation Train/Test avec stratification par source et vote
# ------------------------------------------------------------------------
# Vérifier qu'il n'y a pas de valeurs "other" qui pourraient causer des problèmes
cat("Valeurs uniques de dv_voteChoice dans DataModel:", paste(unique(as.character(DataModel$dv_voteChoice)), collapse=", "), "\n")

# S'assurer qu'il n'y a pas de valeurs 'other' dans les données
if ("other" %in% DataModel$dv_voteChoice) {
  cat("ATTENTION: Valeurs 'other' trouvées dans DataModel, elles seront filtrées\n")
  DataModel <- DataModel %>% filter(dv_voteChoice != "other")
}

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
# 9) Fonction d'évaluation multiclasse
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
  
  # 5) F1-score pour les libéraux spécifiquement
  lpc_f1 <- 0
  if ("lpc" %in% classes) {
    tp_lpc <- sum(data$obs == "lpc" & data$pred == "lpc")
    fp_lpc <- sum(data$obs != "lpc" & data$pred == "lpc")
    fn_lpc <- sum(data$obs == "lpc" & data$pred != "lpc")
    precision_lpc <- ifelse(tp_lpc + fp_lpc == 0, 0, tp_lpc / (tp_lpc + fp_lpc))
    recall_lpc <- ifelse(tp_lpc + fn_lpc == 0, 0, tp_lpc / (tp_lpc + fn_lpc))
    lpc_f1 <- ifelse(precision_lpc + recall_lpc == 0, 0, 2 * precision_lpc * recall_lpc / (precision_lpc + recall_lpc))
  }
  
  # 6) Calcul d'une pénalité personnalisée pour les erreurs critiques
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
  
  out <- c(accuracy = acc, kappa = kap, logLoss = ll, f1 = f1_macro, lpc_f1 = lpc_f1, composite_score = composite_score)
  return(out)
}

# ------------------------------------------------------------------------
# 10) Construction du modèle avec variables distinctes pour Québec et ROC
# ------------------------------------------------------------------------
# Définition des variables de base
base_vars <- c(
  "ses_immigrant", 
  "lifestyle_typeTransport", 
  "lifestyle_consClothes", 
  "lifestyle_exercise", 
  "lifestyle_eatMeatFreq", 
  "lifestyle_favAlcool", 
  "lifestyle_consCoffee", 
  "ses_language", 
  "lifestyle_smokeFreq", 
  "ses_age", 
  "ses_dwelling_cat", 
  "ses_ethnicityWhite", 
  "ses_sexOrientationHetero", 
  "ses_genderFemale", 
  "lifestyle_clothingStyleGroups", 
  "lifestyle_goHuntingFreq_numeric", 
  "lifestyle_goFishingFreq_bin", 
  "lifestyle_goMuseumsFreq_bin", 
  "lifestyle_volunteeringFreq", 
  "lifestyle_motorizedActFreq_bin", 
  "lifestyle_hasTattoos", 
  "ses_educ", 
  "ses_income3Cat", 
  "lifestyle_ownPet_bin", 
  "prediction_CPC", 
  "prediction_LPC", 
  "prediction_NDP", 
  "prediction_GPC", 
  "prediction_BQ"
)

# Vérifier que toutes les variables sont disponibles dans le jeu de données
available_vars <- base_vars[base_vars %in% names(DfTrain)]
cat("Variables disponibles:", length(available_vars), "sur", length(base_vars), "\n")
missing_vars <- base_vars[!base_vars %in% names(DfTrain)]
if (length(missing_vars) > 0) {
  cat("Variables manquantes:", paste(missing_vars, collapse=", "), "\n")
}

# Construire les termes pour Québec (is_quebec:variable)
quebec_terms <- paste0("is_quebec:", available_vars)

# Construire les termes pour ROC (is_roc:variable)
roc_terms <- paste0("is_roc:", available_vars)

# Combiner tous les termes dans la formule finale
all_terms <- c(quebec_terms, roc_terms)

# Formule pour le modèle final
formula_model <- as.formula(paste("dv_voteChoice ~", paste(all_terms, collapse = " + ")))

cat("Formule du modèle final:", "\n")
print(formula_model)

# Traiter les facteurs et fixer les références
cat("Configuration manuelle des catégories de référence pour correspondre au modèle original...\n")

# 1. D'abord, fixer la classe de référence pour la variable dépendante dv_voteChoice
cat("Niveaux actuels de dv_voteChoice:", paste(levels(DfTrain$dv_voteChoice), collapse=", "), "\n")

if ("bq" %in% levels(DfTrain$dv_voteChoice)) {
  DfTrain$dv_voteChoice <- relevel(DfTrain$dv_voteChoice, ref = "bq")
  DfTest$dv_voteChoice <- relevel(DfTest$dv_voteChoice, ref = "bq")
  cat("Variable dépendante: 'bq' définie comme référence\n")
} else {
  cat("Impossible de définir 'bq' comme référence car ce niveau n'existe pas\n")
}

# 2. Fixer les références pour les variables catégorielles
reference_mapping <- list(
  ses_region = "ontario",
  lifestyle_typeTransport = "active_transport",
  lifestyle_consClothes = "large_retailers",
  lifestyle_exercise = "gym",
  lifestyle_favAlcool = "beer",
  lifestyle_consCoffee = "tim_hortons",
  ses_language = "english",
  ses_dwelling_cat = "stand_alone_house",
  lifestyle_clothingStyleGroups = "easygoing",
  ses_educ = "no_schooling",
  ses_income3Cat = "High"
)

# Appliquer les références
for (var_name in names(reference_mapping)) {
  ref_value <- reference_mapping[[var_name]]
  
  if (var_name %in% names(DfTrain) && is.factor(DfTrain[[var_name]]) && ref_value %in% levels(DfTrain[[var_name]])) {
    DfTrain[[var_name]] <- relevel(DfTrain[[var_name]], ref = ref_value)
    DfTest[[var_name]] <- relevel(DfTest[[var_name]], ref = ref_value)
    cat(var_name, ": '", ref_value, "' définie comme référence\n", sep="")
  } else if (var_name %in% names(DfTrain)) {
    cat("Impossible de définir '", ref_value, "' comme référence pour ", var_name, 
        " (soit n'est pas un facteur, soit la valeur n'existe pas)\n", sep="")
  }
}

# ------------------------------------------------------------------------
# 11) Entraînement du modèle
# ------------------------------------------------------------------------
cat("Entraînement du modèle...\n")
model <- multinom(
  formula_model, 
  data = DfTrain, 
  trace = FALSE,
  MaxNWts = 300000
)

# ------------------------------------------------------------------------
# 12) Évaluation du modèle
# ------------------------------------------------------------------------
cat("\n--- Évaluation du modèle ---\n")

# Prédictions
pred_class <- predict(model, newdata = DfTest)
pred_prob <- predict(model, newdata = DfTest, type = "probs")

# Évaluation globale
acc <- mean(pred_class == DfTest$dv_voteChoice, na.rm = TRUE)
cat("Accuracy globale:", acc, "\n")

# Évaluation par parti
conf_matrix <- table(pred_class, DfTest$dv_voteChoice)
cat("Matrice de confusion:\n")
print(conf_matrix)

# Calcul du F1-score par parti
parties <- unique(DfTest$dv_voteChoice)
f1_scores <- sapply(parties, function(p) {
  tp <- sum(pred_class == p & DfTest$dv_voteChoice == p, na.rm = TRUE)
  fp <- sum(pred_class == p & DfTest$dv_voteChoice != p, na.rm = TRUE)
  fn <- sum(pred_class != p & DfTest$dv_voteChoice == p, na.rm = TRUE)
  precision <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
  recall <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
  if (is.na(precision + recall) || precision + recall == 0) {
    return(0)
  } else {
    return(2 * precision * recall / (precision + recall))
  }
})
names(f1_scores) <- parties
cat("F1-scores par parti:\n")
print(f1_scores)

# Evaluation par région (pour le Québec vs ROC)
# Pour le Québec
quebec_indices <- DfTest$is_quebec == 1
acc_quebec <- mean(pred_class[quebec_indices] == DfTest$dv_voteChoice[quebec_indices], na.rm = TRUE)
cat("Accuracy pour le Québec:", acc_quebec, "\n")

# Pour le ROC
roc_indices <- DfTest$is_roc == 1
acc_roc <- mean(pred_class[roc_indices] == DfTest$dv_voteChoice[roc_indices], na.rm = TRUE)
cat("Accuracy pour le ROC:", acc_roc, "\n")

# Évaluations détaillées par région si disponible
if ("ses_region" %in% names(DfTest) && is.factor(DfTest$ses_region)) {
  regions <- levels(DfTest$ses_region)
  acc_by_region <- sapply(regions, function(r) {
    region_indices <- DfTest$ses_region == r
    if (sum(region_indices) > 0) {
      return(mean(pred_class[region_indices] == DfTest$dv_voteChoice[region_indices], na.rm = TRUE))
    } else {
      return(NA)
    }
  })
  names(acc_by_region) <- regions
  cat("Accuracy par région détaillée:\n")
  print(acc_by_region)
}

# ------------------------------------------------------------------------
# 13) Analyse des coefficients par région
# ------------------------------------------------------------------------
cat("\n--- Analyse des coefficients par région ---\n")

# Extraire les coefficients du modèle
model_coef <- coef(model)

# Identifier les coefficients pour is_quebec et is_roc
quebec_coefs <- model_coef[, grepl("is_quebec", colnames(model_coef))]
roc_coefs <- model_coef[, grepl("is_roc", colnames(model_coef))]

# Comparer les coefficients entre Québec et ROC
cat("Différences de coefficients entre Québec et ROC pour les principales variables:\n")
for (var in available_vars) {
  quebec_col <- paste0("is_quebec:", var)
  roc_col <- paste0("is_roc:", var)
  
  if (quebec_col %in% colnames(model_coef) && roc_col %in% colnames(model_coef)) {
    cat(var, ":\n")
    cat("  Québec:", model_coef[1, quebec_col], "\n")
    cat("  ROC:", model_coef[1, roc_col], "\n")
    cat("  Différence:", model_coef[1, quebec_col] - model_coef[1, roc_col], "\n\n")
  }
}

# ------------------------------------------------------------------------
# 14) Sauvegarde du modèle final
# ------------------------------------------------------------------------
# Créer un objet dummies pour de futures prédictions
train_subset <- DfTrain[, all.vars(formula_model)[-1]]  # Exclure la variable dépendante
dummies_final <- dummyVars(" ~ .", data = train_subset, fullRank = TRUE, sep = "_")

# Sauvegarder le modèle final
model_file_name <- "_SharedFolder_datagotchi_federal_2024/data/modele/finalmodel_withRTAPredictions_RegionalInteractions_2025-04-09.rds"
saveRDS(model, model_file_name)
cat("\nModèle final sauvegardé dans:", model_file_name, "\n")

# Sauvegarder les dummies pour les futures prédictions
dummies_file_name <- "_SharedFolder_datagotchi_federal_2024/data/modele/dummies_finalmodel_withRTAPredictions_RegionalInteractions_2025-04-09.rds"
saveRDS(dummies_final, dummies_file_name)
cat("Dummies sauvegardés dans:", dummies_file_name, "\n")

# Sauvegarder la formule finale
formula_file_name <- "_SharedFolder_datagotchi_federal_2024/data/modele/formula_finalmodel_withRTAPredictions_RegionalInteractions_2025-04-09.rds"
saveRDS(formula_model, formula_file_name)
cat("Formule sauvegardée dans:", formula_file_name, "\n")

# Résumé des résultats
model_results <- data.frame(
  Model = "Modèle Québec/ROC",
  Accuracy = acc,
  Accuracy_Quebec = acc_quebec,
  Accuracy_ROC = acc_roc
)

# Sauvegarder les résultats
results_file_name <- "_SharedFolder_datagotchi_federal_2024/data/modele/results_model_comparison_2025-04-09.rds"
saveRDS(model_results, results_file_name)
cat("Résultats du modèle sauvegardés dans:", results_file_name, "\n")

# ------------------------------------------------------------------------
# 15) Résumé des améliorations apportées au modèle initial
# ------------------------------------------------------------------------
cat("\n=============== RÉSUMÉ DES AMÉLIORATIONS ===============\n")
cat("1. Implémentation d'un modèle avec coefficients distincts pour:\n")
cat("   - Le Québec (via les termes is_quebec:variable)\n")
cat("   - Le Reste du Canada (via les termes is_roc:variable)\n\n")

cat("2. Évaluation détaillée par région:\n")
cat("   - Matrices de confusion spécifiques par région\n")
cat("   - Performance différenciée entre Québec et ROC\n")
cat("   - Performance détaillée pour chaque région (Ontario, Prairies, etc.)\n\n")

cat("3. Analyse des coefficients régionaux:\n")
cat("   - Identification des facteurs qui varient le plus entre le Québec et le ROC\n")
cat("   - Quantification des différences d'effets entre régions\n\n")

cat("\nModèle prêt pour le déploiement!\n")