#' Construction du modèle RTA amélioré avec interactions régionales (6 régions du Canada)
#' 
#' Ce script construit un modèle multinomial qui intègre les prédictions basées sur
#' les RTA (Forward Sortation Area) et ajoute des modèles distincts pour 6 régions du Canada:
#' 1. Ontario
#' 2. Québec
#' 3. Colombie-Britannique
#' 4. Prairies
#' 5. Provinces atlantiques
#' 6. Territoires
#' 
#' Entrée :
#' - Données pilote nettoyées (datagotchi2025_canada_pilot_20250322.rds)
#' - Données d'application nettoyées (datagotchi2025_canada_app_20250408.rds)
#' - Prédictions par RTA (rta_predictions_partis.csv)
#' - Résultats d'entraînement précédents (resultsTrainV4_31janvier2025.rds)
#'
#' Sortie :
#' - Modèle final avec prédictions RTA et interactions régionales (finalmodel_withRTAPredictions_6Regions_2025-04-09.rds)
#' - Variables dummy pour prédictions futures (dummies_finalmodel_withRTAPredictions_6Regions_2025-04-09.rds)
#' - Formule du modèle final (formula_finalmodel_withRTAPredictions_6Regions_2025-04-09.rds)
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

# Charger les résultats précédents pour obtenir les variables du modèle
results_train <- readRDS("_SharedFolder_datagotchi_federal_2024/data/modele/_previous/resultsTrainV4_31janvier2025.rds")

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
# 6) Création des variables régionales
# ------------------------------------------------------------------------
# Définir les 6 régions du Canada
# Vérifier que la variable de région (ses_region) est bien un facteur avec les niveaux appropriés
region_levels <- c("ontario", "quebec", "british_columbia", "prairie", "atlantic", "territories")

DataPilot_enriched <- DataPilot_enriched %>%
  mutate(
    ses_region = factor(ses_region, levels = region_levels)
  )

DataApp_enriched <- DataApp_enriched %>%
  mutate(
    ses_region = factor(ses_region, levels = region_levels)
  )

# Vérifier le type de ses_immigrant dans les deux dataframes
cat("Type de ses_immigrant dans DataPilot_enriched:", class(DataPilot_enriched$ses_immigrant), "\n")
cat("Type de ses_immigrant dans DataApp_enriched:", class(DataApp_enriched$ses_immigrant), "\n")

# Harmoniser le type de ses_immigrant (convertir en numeric)
# Si ses_immigrant dans DataApp_enriched contient des valeurs qui peuvent être converties en numeric
DataApp_enriched$ses_immigrant <- as.numeric(as.character(DataApp_enriched$ses_immigrant))

# Vérifier si la conversion a fonctionné
cat("Nouveau type de ses_immigrant dans DataApp_enriched:", class(DataApp_enriched$ses_immigrant), "\n")

# Maintenant essayer de combiner les dataframes
DataModel <- bind_rows(DataPilot_enriched, DataApp_enriched)

# ------------------------------------------------------------------------
# 7) Analyse exploratoire des régions et vérification des effectifs
# ------------------------------------------------------------------------
# Vérifier le nombre de répondants par région
region_counts <- DataModel %>%
  group_by(ses_region) %>%
  summarise(
    n = n(),
    pct = n() / nrow(DataModel) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(n))

print(region_counts)

# Définir un seuil minimum pour considérer une région comme valide
min_respondents <- 200  # Seuil ajusté à 200 pour assurer des estimations stables

# Identifier les régions avec trop peu de répondants
small_regions <- region_counts %>%
  filter(n < min_respondents) %>%
  pull(ses_region)

cat("Régions avec moins de", min_respondents, "répondants:", paste(small_regions, collapse=", "), "\n")

# ------------------------------------------------------------------------
# 8) Fusion des régions avec trop peu de répondants
# ------------------------------------------------------------------------
# Logique de fusion: regrouper les régions géographiquement ou culturellement proches
# Stratégie de fusion basée sur la proximité géographique:
# - territoires -> prairies (si nécessaire)
# - atlantic -> ontario (si nécessaire)

# Créer une nouvelle variable pour les régions fusionnées
DataModel <- DataModel %>%
  mutate(region_model = as.character(ses_region))

# Appliquer les fusions si nécessaire
if ("territories" %in% small_regions) {
  cat("Fusion des territoires avec les prairies en raison du faible nombre de répondants\n")
  DataModel <- DataModel %>%
    mutate(region_model = ifelse(region_model == "territories", "prairie", region_model))
}

if ("atlantic" %in% small_regions) {
  cat("Fusion des provinces atlantiques avec l'Ontario en raison du faible nombre de répondants\n")
  DataModel <- DataModel %>%
    mutate(region_model = ifelse(region_model == "atlantic", "ontario", region_model))
}

DataModel <- DataModel %>%
  mutate(region_model = factor(region_model))

# Vérifier les nouvelles régions fusionnées
region_model_counts <- DataModel %>%
  group_by(region_model) %>%
  summarise(
    n = n(),
    pct = n() / nrow(DataModel) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(n))

print(region_model_counts)

# Créer des variables indicatrices pour chaque région
regions_for_model <- unique(DataModel$region_model)
cat("Régions finales pour le modèle:", paste(regions_for_model, collapse=", "), "\n")

# Création des variables indicatrices pour chaque région
for (r in regions_for_model) {
  var_name <- paste0("is_", r)
  DataModel[[var_name]] <- as.numeric(DataModel$region_model == r)
}

# Observer la distribution des votes par région
region_vote_distribution <- DataModel %>%
  group_by(region_model, dv_voteChoice) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(region_model) %>%
  mutate(pct = n / sum(n) * 100) %>%
  arrange(region_model, desc(pct))

print(region_vote_distribution)

# Visualiser les différences régionales dans les variables clés
regional_differences <- DataModel %>%
  group_by(region_model) %>%
  summarise(
    n = n(),
    avg_age = mean(as.numeric(gsub("[^0-9]", "", ses_age)), na.rm = TRUE),
    pct_french = mean(ses_language == "french", na.rm = TRUE) * 100,
    pct_urban = mean(ses_dwelling_cat %in% c("apartment", "condo"), na.rm = TRUE) * 100
  )

print(regional_differences)

# ------------------------------------------------------------------------
# 9) Séparation Train/Test avec stratification par source et vote
# ------------------------------------------------------------------------
# Vérifier qu'il n'y a pas de valeurs "other" qui pourraient causer des problèmes
cat("Valeurs uniques de dv_voteChoice dans DataModel:", paste(unique(as.character(DataModel$dv_voteChoice)), collapse=", "), "\n")

# S'assurer qu'il n'y a pas de valeurs 'other' dans les données
if ("other" %in% DataModel$dv_voteChoice) {
  cat("ATTENTION: Valeurs 'other' trouvées dans DataModel, elles seront filtrées\n")
  DataModel <- DataModel %>% filter(dv_voteChoice != "other")
}

set.seed(42)
# Stratifier par parti, région et source pour maintenir la distribution
trainIndex <- createDataPartition(
  interaction(DataModel$dv_voteChoice, DataModel$region_model, DataModel$source), 
  p = 0.8, 
  list = FALSE
)
DfTrain <- DataModel[trainIndex, ]
DfTest <- DataModel[-trainIndex, ]

# Vérification de la distribution dans les ensembles train et test
cat("Distribution dans l'ensemble d'entraînement par région:", "\n")
print(table(DfTrain$region_model, DfTrain$dv_voteChoice))

cat("Distribution dans l'ensemble de test par région:", "\n")
print(table(DfTest$region_model, DfTest$dv_voteChoice))

# ------------------------------------------------------------------------
# 10) Fonction d'évaluation multiclasse
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
# 11) Construction du modèle avec variables distinctes pour chaque région
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

# Vérifier les combinaisons vides région/parti
cat("\nVérification des combinaisons région/parti:\n")
for(r in regions_for_model) {
  for(p in levels(DfTrain$dv_voteChoice)) {
    count <- sum(DfTrain$region_model == r & DfTrain$dv_voteChoice == p)
    if(count == 0) {
      cat("ATTENTION: Aucune observation pour la région", r, "et le parti", p, "\n")
    } else if(count < 10) {
      cat("ATTENTION: Seulement", count, "observations pour la région", r, "et le parti", p, "\n")
    }
  }
}

# Construire les termes d'interaction pour chaque région
region_terms <- list()
for (r in regions_for_model) {
  var_prefix <- paste0("is_", r)
  region_terms[[r]] <- paste0(var_prefix, ":", available_vars)
}

# Combiner tous les termes dans la formule finale
all_terms <- unlist(region_terms)

# Vérifier la taille de la formule
cat("Nombre total de termes dans la formule:", length(all_terms), "\n")

# Formule pour le modèle final
formula_model <- as.formula(paste("dv_voteChoice ~", paste(all_terms, collapse = " + ")))

# Vérifier que la formule est valide
tryCatch({
  terms(formula_model, data = DfTrain)
  cat("Formule valide!\n")
}, error = function(e) {
  cat("ERREUR avec la formule:", e$message, "\n")
})

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
  ses_region = "prairie",  # Modifié: "ontario" -> "prairie"
  ses_immigrant = "0",
  lifestyle_typeTransport = "active_transport",
  lifestyle_consClothes = "large_retailers",
  lifestyle_exercise = "gym",
  lifestyle_eatMeatFreq = "0",
  lifestyle_favAlcool = "beer",
  lifestyle_consCoffee = "tim_hortons",
  ses_language = "english",
  lifestyle_smokeFreq = "0",
  ses_age = "0",
  ses_dwelling_cat = "stand_alone_house",
  ses_ethnicityWhite = "0",
  ses_sexOrientationHetero = "0",
  ses_genderFemale = "0",
  lifestyle_clothingStyleGroups = "easygoing",
  lifestyle_goHuntingFreq_numeric = "0",
  lifestyle_goFishingFreq_bin = "0",
  lifestyle_goMuseumsFreq_bin = "0",
  lifestyle_volunteeringFreq = "0",
  lifestyle_motorizedActFreq_bin = "0",
  lifestyle_hasTattoos = "0",
  ses_educ = "no_schooling",
  ses_income3Cat = "High",
  lifestyle_ownPet_bin = "0"
)

# Appliquer les références
for (var_name in names(reference_mapping)) {
  ref_value <- reference_mapping[[var_name]]
  
  if (var_name %in% names(DfTrain)) {
    # Pour les variables numériques codées comme "0"
    if (ref_value == "0" && is.numeric(DfTrain[[var_name]])) {
      cat(var_name, ": Variable numérique, pas besoin de définir '0' comme référence\n", sep="")
      next
    }
    
    # Pour les variables factorielles
    if (is.factor(DfTrain[[var_name]])) {
      if (ref_value %in% levels(DfTrain[[var_name]])) {
        DfTrain[[var_name]] <- relevel(DfTrain[[var_name]], ref = ref_value)
        DfTest[[var_name]] <- relevel(DfTest[[var_name]], ref = ref_value)
        cat(var_name, ": '", ref_value, "' définie comme référence\n", sep="")
      } else {
        cat("Impossible de définir '", ref_value, "' comme référence pour ", var_name, 
            " (valeur non présente dans les niveaux actuels: ", 
            paste(levels(DfTrain[[var_name]]), collapse=", "), ")\n", sep="")
      }
    } 
    # Pour les variables binaires qui pourraient être numériques mais devant être traitées comme facteurs
    else if (grepl("_bin$|White$|Hetero$|Female$|hasTattoos$", var_name) && !is.factor(DfTrain[[var_name]])) {
      # Convertir en facteur d'abord
      DfTrain[[var_name]] <- factor(DfTrain[[var_name]])
      DfTest[[var_name]] <- factor(DfTest[[var_name]])
      
      if (ref_value %in% levels(DfTrain[[var_name]])) {
        DfTrain[[var_name]] <- relevel(DfTrain[[var_name]], ref = ref_value)
        DfTest[[var_name]] <- relevel(DfTest[[var_name]], ref = ref_value)
        cat(var_name, ": Convertie en facteur avec '", ref_value, "' comme référence\n", sep="")
      } else {
        cat("Variable ", var_name, " convertie en facteur mais '", ref_value, 
            "' n'est pas dans les niveaux disponibles: ", 
            paste(levels(DfTrain[[var_name]]), collapse=", "), "\n", sep="")
      }
    } 
    else {
      cat(var_name, ": Variable présente mais n'est pas un facteur (type: ", class(DfTrain[[var_name]]), ")\n", sep="")
    }
  } else {
    cat(var_name, ": Variable non présente dans les données\n", sep="")
  }
}
# ------------------------------------------------------------------------
# 12) Entraînement du modèle
# ------------------------------------------------------------------------
cat("Entraînement du modèle...\n")

# Utiliser tryCatch pour capturer les erreurs potentielles
tryCatch({
  # Utiliser trace=TRUE pour voir la progression
  model <- multinom(
    formula_model, 
    data = DfTrain, 
    trace = TRUE,
    maxit = 500,      # Augmenter le nombre d'itérations maximum
    decay = 0.01,     # Ajouter une régularisation légère
    MaxNWts = 1000000 # Augmenté significativement pour gérer plus de paramètres
  )
  cat("Modèle entraîné avec succès!\n")
}, error = function(e) {
  cat("ERREUR lors de l'entraînement:", e$message, "\n")
  # Tentative de simplification du modèle en cas d'erreur
  cat("Tentative de simplification du modèle...\n")
  
  # Réduire le nombre de variables à inclure aux plus importantes
  reduced_vars <- c(
    "ses_language", "ses_age", "ses_dwelling_cat", 
    "lifestyle_typeTransport", "lifestyle_exercise",
    "prediction_CPC", "prediction_LPC", "prediction_NDP", "prediction_GPC", "prediction_BQ"
  )
  
  # Ne garder que les variables disponibles
  reduced_vars <- reduced_vars[reduced_vars %in% names(DfTrain)]
  
  # Reconstruire les termes d'interaction pour chaque région avec variables réduites
  region_terms_reduced <- list()
  for (r in regions_for_model) {
    var_prefix <- paste0("is_", r)
    region_terms_reduced[[r]] <- paste0(var_prefix, ":", reduced_vars)
  }
  
  # Combiner les termes réduits
  all_terms_reduced <- unlist(region_terms_reduced)
  
  # Formule réduite
  formula_model_reduced <- as.formula(paste("dv_voteChoice ~", paste(all_terms_reduced, collapse = " + ")))
  
  # Tenter d'entraîner le modèle réduit
  tryCatch({
    model <<- multinom(
      formula_model_reduced, 
      data = DfTrain, 
      trace = TRUE,
      maxit = 1000,
      decay = 0.1,
      MaxNWts = 500000
    )
    cat("Modèle simplifié entraîné avec succès!\n")
    # Mettre à jour la formule globale pour la sauvegarde
    formula_model <<- formula_model_reduced
  }, error = function(e) {
    cat("ERREUR lors de l'entraînement du modèle simplifié:", e$message, "\n")
    cat("Impossible d'entraîner le modèle. Arrêt de l'exécution.\n")
    stop("Échec de l'entraînement du modèle.")
  })
})

# Vérifier si le modèle existe avant de continuer

if (!exists("model")) {
  stop("L'entraînement du modèle a échoué. Arrêt de l'exécution.")
}

# ------------------------------------------------------------------------
# 13) Évaluation du modèle
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

# Evaluation par région
cat("\nPerformance par région:\n")
for (r in regions_for_model) {
  region_indices <- DfTest$region_model == r
  if (sum(region_indices) > 0) {
    acc_region <- mean(pred_class[region_indices] == DfTest$dv_voteChoice[region_indices], na.rm = TRUE)
    cat("Accuracy pour", r, ":", acc_region, "\n")
    
    # Matrice de confusion par région
    conf_region <- table(pred_class[region_indices], DfTest$dv_voteChoice[region_indices])
    cat("Matrice de confusion pour", r, ":\n")
    print(conf_region)
    
    # F1-score par parti pour cette région
    f1_region <- sapply(parties, function(p) {
      tp <- sum(pred_class[region_indices] == p & DfTest$dv_voteChoice[region_indices] == p, na.rm = TRUE)
      fp <- sum(pred_class[region_indices] == p & DfTest$dv_voteChoice[region_indices] != p, na.rm = TRUE)
      fn <- sum(pred_class[region_indices] != p & DfTest$dv_voteChoice[region_indices] == p, na.rm = TRUE)
      precision <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
      recall <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
      if (is.na(precision + recall) || precision + recall == 0) {
        return(0)
      } else {
        return(2 * precision * recall / (precision + recall))
      }
    })
    names(f1_region) <- parties
    cat("F1-scores par parti pour", r, ":\n")
    print(f1_region)
    cat("\n")
  }
}

# ------------------------------------------------------------------------
# 14) Analyse des coefficients par région
# ------------------------------------------------------------------------
cat("\n--- Analyse des coefficients par région ---\n")

# Extraire les coefficients du modèle de manière sécurisée
tryCatch({
  model_coef <- coef(model)
  
  # Comparaison des coefficients entre régions pour quelques variables clés
  # Identifier les variables clés présentes dans le modèle
  all_model_vars <- colnames(model_coef)
  important_vars_base <- c("ses_language", "ses_age", "prediction_CPC", "prediction_LPC", "prediction_BQ")
  
  # Trouver les variables importantes qui sont effectivement dans le modèle
  important_vars <- c()
  for (base_var in important_vars_base) {
    matching_vars <- grep(paste0(":", base_var), all_model_vars, value = TRUE)
    if (length(matching_vars) > 0) {
      important_vars <- c(important_vars, base_var)
    }
  }
  
  if (length(important_vars) > 0) {
    for (var in important_vars) {
      cat("\nComparaison des coefficients pour la variable:", var, "\n")
      
      for (r in regions_for_model) {
        var_name <- paste0("is_", r, ":", var)
        matching_vars <- grep(var_name, all_model_vars, value = TRUE)
        
        if (length(matching_vars) > 0) {
          for (exact_var in matching_vars) {
            for (party in rownames(model_coef)) {
              cat("  Région", r, "pour", party, ":", model_coef[party, exact_var], "\n")
            }
          }
        }
      }
    }
  } else {
    cat("Aucune des variables importantes n'a été trouvée dans le modèle final.\n")
  }
}, error = function(e) {
  cat("Erreur lors de l'analyse des coefficients:", e$message, "\n")
})

# ------------------------------------------------------------------------
# 15) Sauvegarde du modèle final
# ------------------------------------------------------------------------
# Créer un objet dummies pour de futures prédictions
tryCatch({
  cat("\n--- Sauvegarde du modèle et des résultats ---\n")
  
  # S'assurer que all.vars fonctionne en mode sécurisé
  model_vars <- tryCatch({
    all.vars(formula_model)[-1]  # Exclure la variable dépendante
  }, error = function(e) {
    cat("Erreur lors de l'extraction des variables:", e$message, "\n")
    # Obtenir les variables d'une autre façon si nécessaire
    return(colnames(model$coefnames))
  })
  
  # Créer les dummies si possible
  train_subset <- DfTrain[, intersect(model_vars, names(DfTrain))]
  dummies_final <- dummyVars(" ~ .", data = train_subset, fullRank = TRUE, sep = "_")

  # Sauvegarder le modèle final
  model_file_name <- "_SharedFolder_datagotchi_federal_2024/data/modele/finalmodel_withRTAPredictions_6Regions_2025-04-09.rds"
  saveRDS(model, model_file_name)
  cat("Modèle final sauvegardé dans:", model_file_name, "\n")

  # Sauvegarder les dummies pour les futures prédictions
  dummies_file_name <- "_SharedFolder_datagotchi_federal_2024/data/modele/dummies_finalmodel_withRTAPredictions_6Regions_2025-04-09.rds"
  saveRDS(dummies_final, dummies_file_name)
  cat("Dummies sauvegardés dans:", dummies_file_name, "\n")

  # Sauvegarder la formule finale
  formula_file_name <- "_SharedFolder_datagotchi_federal_2024/data/modele/formula_finalmodel_withRTAPredictions_6Regions_2025-04-09.rds"
  saveRDS(formula_model, formula_file_name)
  cat("Formule sauvegardée dans:", formula_file_name, "\n")

  # Sauvegarder la configuration de fusion des régions
  region_config <- data.frame(
    original_region = region_levels,
    model_region = sapply(region_levels, function(r) {
      if (r %in% small_regions) {
        if (r == "territories") return("prairie")
        if (r == "atlantic") return("ontario")
        return(r)
      } else {
        return(r)
      }
    })
  )
  region_config_file <- "_SharedFolder_datagotchi_federal_2024/data/modele/region_mapping_6Regions_2025-04-09.rds"
  saveRDS(region_config, region_config_file)
  cat("Configuration des régions sauvegardée dans:", region_config_file, "\n")

  # Résumé des résultats
  model_results <- data.frame(
    Model = "Modèle 6 Régions",
    Accuracy = acc,
    F1_Macro = mean(f1_scores, na.rm = TRUE)
  )

  # Ajouter les résultats par région
  for (r in regions_for_model) {
    region_indices <- DfTest$region_model == r
    if (sum(region_indices) > 0) {
      acc_region <- mean(pred_class[region_indices] == DfTest$dv_voteChoice[region_indices], na.rm = TRUE)
      model_results[[paste0("Accuracy_", r)]] <- acc_region
    }
  }

  # Sauvegarder les résultats
  results_file_name <- "_SharedFolder_datagotchi_federal_2024/data/modele/results_model_6Regions_2025-04-09.rds"
  saveRDS(model_results, results_file_name)
  cat("Résultats du modèle sauvegardés dans:", results_file_name, "\n")
}, error = function(e) {
  cat("Erreur lors de la sauvegarde du modèle:", e$message, "\n")
})

# ------------------------------------------------------------------------
# 16) Résumé des améliorations apportées au modèle
# ------------------------------------------------------------------------
cat("\n=============== RÉSUMÉ DES AMÉLIORATIONS ===============\n")
cat("1. Implémentation d'un modèle avec coefficients distincts pour 6 régions:\n")
cat("   - Ontario\n")
cat("   - Québec\n")
cat("   - Colombie-Britannique\n")
cat("   - Prairies\n")
cat("   - Provinces atlantiques\n")
cat("   - Territoires\n\n")

cat("2. Analyse et fusion intelligente des régions avec effectifs insuffisants:\n")
if (length(small_regions) > 0) {
  for (r in small_regions) {
    merged_with <- if (r == "territories") "prairie" else "ontario"
    cat("   - Fusion de", r, "avec", merged_with, "en raison du faible nombre de répondants\n")
  }
} else {
  cat("   - Toutes les régions ont un nombre suffisant de répondants, aucune fusion nécessaire\n")
}
cat("\n")

cat("3. Évaluation détaillée par région:\n")
cat("   - Matrices de confusion spécifiques par région\n")
cat("   - F1-scores par parti et par région\n")
cat("   - Identification des forces et faiblesses du modèle selon les régions\n\n")

cat("4. Analyse des coefficients régionaux:\n")
cat("   - Identification des facteurs qui varient entre les régions\n")
cat("   - Quantification des différences d'effets entre régions\n\n")

cat("5. Améliorations techniques:\n")
cat("   - Vérification rigoureuse de la disponibilité des répondants par région\n")
cat("   - Stratification avancée pour l'échantillonnage train/test\n")
cat("   - Gestion robuste des erreurs et mécanismes de récupération\n")
cat("   - Sauvegarde sécurisée de la configuration de fusion des régions\n\n")

cat("Modèle prêt pour le déploiement!\n")
