#' Construction du modèle RTA amélioré avec interactions (version 3, 09 avril 2025)
#' 
#' Ce script construit un modèle multinomial qui intègre les prédictions basées sur
#' les RTA (Forward Sortation Area) et ajoute des interactions avec la région.
#' Il fusionne les données pilotes et d'application, harmonise les facteurs,
#' ajoute les prédictions par RTA, crée des variables d'interaction avec la région,
#' et évalue les performances du modèle.
#'
#' Entrée :
#' - Données pilote nettoyées (datagotchi2025_canada_pilot_20250310.rds)
#' - Données d'application nettoyées (datagotchi2025_canada_app_20250314.rds)
#' - Prédictions par RTA (rta_predictions_partis.csv)
#' - Résultats d'entraînement précédents (resultsTrainV4_31janvier2025.rds)
#' - Modèle précédent (finalmodel_withRTAPredictions_2025-04-15.rds)
#'
#' Sortie :
#' - Modèle final avec prédictions RTA et interactions (finalmodel_withRTAPredictions_Interactions_2025-04-09.rds)
#' - Variables dummy pour prédictions futures (dummies_finalmodel_withRTAPredictions_Interactions_2025-04-09.rds)
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
# 6) Création de la variable ROC vs Québec et interactions régionales
# ------------------------------------------------------------------------
# Créer une nouvelle variable binaire pour distinguer Québec du reste du Canada (ROC)
DataPilot_enriched <- DataPilot_enriched %>%
  mutate(
    is_quebec = ifelse(ses_region == "quebec", 1, 0),
    roc_vs_quebec = factor(ifelse(ses_region == "quebec", "quebec", "roc"))
  )

DataApp_enriched <- DataApp_enriched %>%
  mutate(
    is_quebec = ifelse(ses_region == "quebec", 1, 0),
    roc_vs_quebec = factor(ifelse(ses_region == "quebec", "quebec", "roc"))
  )

# Définir les variables avec lesquelles nous voulons créer des interactions avec la région
interaction_vars <- c(
  "lifestyle_typeTransport", 
  "lifestyle_consClothes", 
  "lifestyle_exercise", 
  "lifestyle_favAlcool", 
  "lifestyle_consCoffee",
  "ses_language",
  "ses_dwelling_cat",
  "lifestyle_clothingStyleGroups",
  "ses_educ",
  "ses_income3Cat"
)

# Vérifier que ces variables existent dans nos données
existing_interaction_vars <- interaction_vars[interaction_vars %in% names(DataPilot_enriched)]
cat("Variables d'interaction disponibles:", paste(existing_interaction_vars, collapse=", "), "\n")

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
# 7) Séparation Train/Test avec stratification par source et vote
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
# 9) Construction du modèle enrichi avec interactions régionales
# ------------------------------------------------------------------------
# Créer la formule pour le modèle avec les interactions
# Commencer avec les variables de base et les prédictions RTA
base_vars <- c(model_variables, "prediction_CPC", "prediction_LPC", "prediction_NDP", "prediction_GPC", "prediction_BQ")

# Ajouter la variable roc_vs_quebec
base_vars <- c(base_vars, "roc_vs_quebec")

# Créer des termes d'interaction
interaction_terms <- c()
for (var in existing_interaction_vars) {
  interaction_terms <- c(interaction_terms, paste0("roc_vs_quebec:", var))
}

# Formule finale pour le modèle
final_formula <- as.formula(paste("dv_voteChoice ~", 
                                paste(c(base_vars, interaction_terms), collapse = " + ")))

# Traiter les facteurs et fixer les références
cat("Configuration manuelle des catégories de référence pour correspondre au modèle original...\n")

# 1. D'abord, fixer la classe de référence pour la variable dépendante dv_voteChoice
cat("Niveaux actuels de dv_voteChoice:", paste(levels(DfTrain$dv_voteChoice), collapse=", "), "\n")

if ("bq" %in% levels(DfTrain$dv_voteChoice)) {
  DfTrain$dv_voteChoice <- relevel(DfTrain$dv_voteChoice, ref = "bq")
  cat("Variable dépendante: 'bq' définie comme référence\n")
} else {
  cat("Impossible de définir 'bq' comme référence car ce niveau n'existe pas\n")
}

# 2. Fixer les références pour les variables catégorielles
reference_mapping <- list(
  ses_region = "prairie",
  lifestyle_typeTransport = "active_transport",
  lifestyle_consClothes = "large_retailers",
  lifestyle_exercise = "gym",
  lifestyle_favAlcool = "beer",
  lifestyle_consCoffee = "tim_hortons",
  ses_language = "english",
  ses_dwelling_cat = "stand_alone_house",
  lifestyle_clothingStyleGroups = "easygoing",
  ses_educ = "no_schooling",
  ses_income3Cat = "High",
  roc_vs_quebec = "roc"  # Définir "roc" comme référence pour la variable ROC vs Québec
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

# Création du modèle
cat("Entraînement du modèle avec interactions régionales...\n")
final_model <- multinom(
  final_formula, 
  data = DfTrain, 
  trace = FALSE,
  MaxNWts = 200000  # Augmenter cette valeur car les interactions augmentent le nombre de poids
)

# Symétrisation des coefficients
all_levels <- levels(DfTrain$dv_voteChoice)
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
# Prédictions
pred_test_class <- predict(final_model, newdata = DfTest)
pred_test_prob <- predict(final_model, newdata = DfTest, type = "probs")

# Évaluation globale
acc_test <- mean(pred_test_class == DfTest$dv_voteChoice)
cat("Accuracy globale (test) :", acc_test, "\n")

# Évaluation séparée par région (ROC vs Québec)
DfTest_with_preds <- DfTest %>%
  mutate(predicted = pred_test_class)

# Performance par région
acc_by_region <- DfTest_with_preds %>%
  group_by(roc_vs_quebec) %>%
  summarise(accuracy = mean(predicted == dv_voteChoice)) %>%
  arrange(roc_vs_quebec)

cat("Accuracy par région:\n")
print(acc_by_region)

# Performance par source
acc_by_source <- DfTest_with_preds %>%
  group_by(source) %>%
  summarise(accuracy = mean(predicted == dv_voteChoice)) %>%
  arrange(source)

cat("Accuracy par source:\n")
print(acc_by_source)

# Matrice de confusion globale
table_test <- table(
  predicted = pred_test_class,
  actual = DfTest$dv_voteChoice
)
print(table_test)

# Matrices de confusion par région
for (region in unique(DfTest_with_preds$roc_vs_quebec)) {
  table_region <- with(
    DfTest_with_preds %>% filter(roc_vs_quebec == region),
    table(predicted = predicted, actual = dv_voteChoice)
  )
  cat("Matrice de confusion (région: ", region, ") :\n", sep="")
  print(table_region)
}

# ------------------------------------------------------------------------
# 11) Analyse des interactions
# ------------------------------------------------------------------------
# Extraire les coefficients des interactions pour analyse
interaction_coeffs <- summary(final_model)$coefficients[, grepl("roc_vs_quebec", colnames(summary(final_model)$coefficients))]

cat("Coefficients des interactions avec la région (roc_vs_quebec):\n")
print(interaction_coeffs)

# Calculer l'importance des interactions en utilisant les valeurs absolues
interaction_importance <- apply(abs(interaction_coeffs), 2, mean)
sorted_importance <- sort(interaction_importance, decreasing = TRUE)

cat("Importance des interactions (moyenne des valeurs absolues):\n")
print(sorted_importance)

# Extraire les termes d'interaction significatifs
# Note: Pour les modèles multinom, nous utilisons l'amplitude des coefficients comme indication
significant_threshold <- quantile(abs(as.vector(interaction_coeffs)), 0.75)  # Seuil au 3e quartile
significant_interactions <- which(abs(interaction_coeffs) > significant_threshold, arr.ind = TRUE)

cat("Interactions significatives (> ", round(significant_threshold, 4), "):\n", sep="")
for (i in 1:nrow(significant_interactions)) {
  row_idx <- significant_interactions[i, 1]
  col_idx <- significant_interactions[i, 2]
  party <- rownames(interaction_coeffs)[row_idx]
  interaction_term <- colnames(interaction_coeffs)[col_idx]
  coef_value <- interaction_coeffs[row_idx, col_idx]
  
  cat(sprintf("%s × %s: %.4f\n", party, interaction_term, coef_value))
}

# ------------------------------------------------------------------------
# 12) Sauvegarde du modèle final avec interactions
# ------------------------------------------------------------------------
# Créer un objet dummies pour de futures prédictions (si nécessaire)
# Utiliser un sous-ensemble de DfTrain pour cela
train_subset <- DfTrain[, all.vars(final_formula)[-1]]  # Exclure la variable dépendante
dummies_final <- dummyVars(" ~ .", data = train_subset, fullRank = TRUE, sep = "_")

# Sauvegarder le modèle
saveRDS(final_model, "_SharedFolder_datagotchi_federal_2024/data/modele/finalmodel_withRTAPredictions_Interactions_2025-04-09.rds")

# Sauvegarder également les dummies pour les futures prédictions
saveRDS(dummies_final, "_SharedFolder_datagotchi_federal_2024/data/modele/dummies_finalmodel_withRTAPredictions_Interactions_2025-04-09.rds")

# Sauvegarder la formule finale
saveRDS(final_formula, "_SharedFolder_datagotchi_federal_2024/data/modele/formula_finalmodel_withRTAPredictions_Interactions_2025-04-09.rds")

cat("Modèle enrichi avec prédictions RTA et interactions régionales sauvegardé avec succès.\n")



final_formula
final_model
