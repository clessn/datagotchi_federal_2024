#' Construction du modèle RTA amélioré avec données depuis le 3 avril 2025
#' 
#' Ce script construit un modèle multinomial qui intègre les prédictions basées sur
#' les RTA (Forward Sortation Area), en utilisant uniquement les répondants depuis le 3 avril 2025.
#'
#' Entrée :
#' - Données pilote nettoyées (datagotchi2025_canada_pilot_20250322.rds)
#' - Données d'application nettoyées (20250415_n81812datagotchi2025_canada_app.rds)
#' - Prédictions par RTA (rta_predictions_partis.csv)
#' - Résultats d'entraînement précédents (resultsTrainV4_31janvier2025.rds)
#' - Modèle précédent (finalmodel_withOutInteractions.rds)
#'
#' Sortie :
#' - Modèle final avec prédictions RTA (finalmodel_withRTAPredictions_april3_2025-04-16.rds)
#' - Variables dummy pour prédictions futures (dummies_finalmodel_withRTAPredictions_april3_2025-04-16.rds)
#' - Journal d'exécution (log_modele_RTA_april3_2025-04-16.txt)
#'

# ------------------------------------------------------------------------
# 0) Initialisation et configuration
# ------------------------------------------------------------------------
# Date d'exécution pour les noms de fichiers
date_execution <- format(Sys.Date(), "%Y-%m-%d")
log_file <- paste0("_SharedFolder_datagotchi_federal_2024/logs/log_modele_RTA_april3_", date_execution, ".txt")

# Fonction pour écrire dans le journal
write_log <- function(message, file = log_file, print_console = TRUE) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_message <- paste0("[", timestamp, "] ", message)
  
  # Créer le répertoire des logs s'il n'existe pas
  log_dir <- dirname(file)
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }
  
  # Écrire dans le fichier
  write(log_message, file = file, append = TRUE)
  
  # Afficher dans la console si demandé
  if (print_console) {
    cat(log_message, "\n")
  }
}

# Fonction pour gérer les erreurs et continuer l'exécution
safe_operation <- function(expr, error_message = "Une erreur s'est produite", default = NULL) {
  tryCatch(
    expr,
    error = function(e) {
      write_log(paste0(error_message, ": ", e$message))
      return(default)
    },
    warning = function(w) {
      write_log(paste0("Avertissement: ", w$message))
      return(eval(expr))
    }
  )
}

# ------------------------------------------------------------------------
# 1) Chargement des packages
# ------------------------------------------------------------------------
write_log("Début de l'exécution du script")
write_log("Chargement des packages")

required_packages <- c("parallel", "nnet", "caret", "tidyverse", "yardstick", "pbapply")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    write_log(paste0("Installation du package manquant: ", pkg))
    install.packages(pkg, quiet = TRUE)
  }
  
  safe_operation(
    library(pkg, character.only = TRUE),
    paste0("Erreur lors du chargement du package ", pkg)
  )
}

# ------------------------------------------------------------------------
# 2) Chargement des données (pilote et application)
# ------------------------------------------------------------------------
write_log("Chargement des données")

# Chemins des fichiers
fichier_pilot <- "_SharedFolder_datagotchi_federal_2024/data/pilote/dataClean/datagotchi2025_canada_pilot_20250322.rds"
fichier_app <- "_SharedFolder_datagotchi_federal_2024/data/app/dataClean/20250421_n89416datagotchi2025_canada_app.rds"
fichier_rta <- "_SharedFolder_datagotchi_federal_2024/data/modele/rta_predictions_partis.csv"
fichier_resultats <- "_SharedFolder_datagotchi_federal_2024/data/modele/_previous/resultsTrainV4_31janvier2025.rds"
fichier_modele_prev <- "_SharedFolder_datagotchi_federal_2024/data/modele/_previous/finalmodel_withOutInteractions.rds"

# Vérification de l'existence des fichiers
fichiers <- c(fichier_pilot, fichier_app, fichier_rta, fichier_resultats, fichier_modele_prev)
for (f in fichiers) {
  if (!file.exists(f)) {
    write_log(paste0("ERREUR: Le fichier ", f, " n'existe pas."))
    stop(paste0("Fichier manquant: ", f))
  }
}

# Chargement des données avec gestion d'erreurs
DataPilot <- safe_operation(
  readRDS(fichier_pilot),
  paste0("Erreur lors du chargement des données pilote: ", fichier_pilot)
)

DataApp <- safe_operation(
  readRDS(fichier_app),
  paste0("Erreur lors du chargement des données application: ", fichier_app)
)

rta_predictions <- safe_operation(
  read.csv(fichier_rta, stringsAsFactors = FALSE),
  paste0("Erreur lors du chargement des prédictions RTA: ", fichier_rta)
)

results_train <- safe_operation(
  readRDS(fichier_resultats),
  paste0("Erreur lors du chargement des résultats d'entraînement: ", fichier_resultats)
)

previous_model <- safe_operation(
  readRDS(fichier_modele_prev),
  paste0("Erreur lors du chargement du modèle précédent: ", fichier_modele_prev)
)

# Vérifier que les données ont été chargées correctement
for (data_obj in c("DataPilot", "DataApp", "rta_predictions", "results_train", "previous_model")) {
  if (!exists(data_obj) || is.null(get(data_obj))) {
    write_log(paste0("ERREUR: L'objet ", data_obj, " n'a pas été chargé correctement."))
    stop(paste0("Échec du chargement: ", data_obj))
  }
}

# ------------------------------------------------------------------------
# 2.1) Récupération de la variable X_time à partir des données brutes
# ------------------------------------------------------------------------
write_log("Récupération de la variable X_time à partir des données brutes")

# Définir le chemin vers le fichier de données brutes
fichier_raw <- "_SharedFolder_datagotchi_federal_2024/data/app/dataRaw/ECAN25_Data_20250305-20250421.csv"

# Vérifier l'existence du fichier
if (!file.exists(fichier_raw)) {
  write_log(paste0("ERREUR: Le fichier de données brutes ", fichier_raw, " n'existe pas."))
  stop(paste0("Fichier manquant: ", fichier_raw))
}

# Chargement des données brutes
DataRaw <- safe_operation(
  read.csv(fichier_raw, stringsAsFactors = FALSE),
  paste0("Erreur lors du chargement des données brutes: ", fichier_raw)
)

# Vérifier que DataRaw a été correctement chargé
if (is.null(DataRaw)) {
  write_log("ERREUR: Les données brutes n'ont pas été chargées correctement")
  stop("Échec du chargement des données brutes")
}

# Vérifier que X_time existe dans les données brutes
if (!"X_time" %in% names(DataRaw)) {
  write_log("ERREUR: La variable X_time n'existe pas dans les données brutes")
  stop("Variable X_time manquante dans les données brutes")
}

# Alternative si les jointures ne fonctionnent pas
if (nrow(DataApp) == nrow(DataRaw)) {
  DataApp$X_time <- DataRaw$X_time
  write_log("Variable X_time ajoutée directement (même nombre de lignes)")
} else {
  write_log(paste0("AVERTISSEMENT: Nombre de lignes différent - DataRaw: ", 
                nrow(DataRaw), ", DataApp: ", nrow(DataApp)))
  
  # Si les lignes sont différentes, on peut essayer une approche alternative
  write_log("Tentative d'extraction d'un sous-ensemble des données brutes correspondant aux données de l'application")
  
  # Si X_id existe dans DataRaw et id existe dans DataApp, essayer de faire correspondre
  if ("X_id" %in% names(DataRaw) && "id" %in% names(DataApp)) {
    write_log("Tentative de correspondance par identifiants")
    
    # Renommer X_id en id pour faciliter la correspondance
    DataRaw_subset <- DataRaw
    names(DataRaw_subset)[names(DataRaw_subset) == "X_id"] <- "id"
    
    # Créer un vecteur de correspondance
    match_indices <- match(DataApp$id, DataRaw_subset$id)
    
    # Ajouter X_time en utilisant les indices correspondants
    DataApp$X_time <- DataRaw_subset$X_time[match_indices]
    
    # Vérifier combien de valeurs manquantes ont été créées
    na_count <- sum(is.na(DataApp$X_time))
    write_log(paste0("Variable X_time ajoutée par correspondance d'identifiants. Valeurs manquantes: ", 
                    na_count, " (", round(na_count/nrow(DataApp)*100, 2), "%)"))
  } else {
    write_log("ERREUR: Impossible de faire correspondre les identifiants entre les deux sources de données")
    stop("Échec de l'ajout de X_time")
  }
}

# Convertir X_time en format Date pour les manipulations ultérieures
DataApp$X_time <- safe_operation(
  as.Date(DataApp$X_time),
  "Erreur lors de la conversion de X_time en Date"
)

# Afficher la distribution des dates
date_distribution_raw <- table(DataApp$X_time)
write_log("Distribution des dates dans l'ensemble des données App:")
print(date_distribution_raw)
capture.output(date_distribution_raw, file = log_file, append = TRUE)

# ------------------------------------------------------------------------
# 2.2) Sélection des données récentes pour DataApp
# ------------------------------------------------------------------------
write_log("Sélection des 30,000 répondants les plus récents")

# Vérifier que la variable X_time existe
if (!"X_time" %in% names(DataApp)) {
  write_log("ERREUR: La variable X_time n'existe pas dans DataApp")
  stop("Variable X_time manquante")
}

# Convertir X_time en Date si ce n'est pas déjà le cas
if (!inherits(DataApp$X_time, "Date")) {
  DataApp$X_time <- as.Date(DataApp$X_time)
  write_log("Conversion de X_time en format Date")
}

# Trier les données par date (du plus récent au plus ancien)
DataApp_sorted <- DataApp %>%
  arrange(desc(X_time))

# Sélectionner les 30,000 premières lignes (les plus récentes)
DataApp_recent <- DataApp_sorted %>%
  head(30000)

# Afficher la distribution des dates dans l'échantillon sélectionné
date_distribution <- table(DataApp_recent$X_time)
write_log("Distribution des dates dans l'échantillon de 30,000 répondants:")
print(date_distribution)
capture.output(date_distribution, file = log_file, append = TRUE)

# Remplacer l'ensemble de données original par la sélection récente
DataApp <- DataApp_recent

write_log(paste0("Nombre de lignes dans DataApp après sélection: ", nrow(DataApp)))

# Afficher la distribution des votes dans le nouvel échantillon
vote_distribution <- table(DataApp$dv_voteChoice)
write_log("Distribution de dv_voteChoice dans les données récentes:")
print(vote_distribution)
capture.output(vote_distribution, file = log_file, append = TRUE)

# Identifier le meilleur modèle selon l'accuracy
best_iterations <- results_train %>%
  group_by(model_id) %>%
  summarise(
    score_iter_accuracy = first(accuracy_cv),
    .groups = "drop"
  ) %>%
  arrange(desc(score_iter_accuracy))

best_id <- best_iterations$model_id[1]
write_log(paste0("Meilleur modèle trouvé (ID) = ", best_id))

# Récupérer les variables du meilleur modèle
best_config <- results_train %>%
  filter(model_id == best_id)

model_variables <- unique(best_config$variable)
write_log(paste0("Variables du modèle précédent: ", length(model_variables)))
write_log(paste0("Liste des variables: ", paste(model_variables, collapse=", ")))

# ------------------------------------------------------------------------
# 3) Analyse préliminaire des données
# ------------------------------------------------------------------------
write_log("Analyse préliminaire des données")

# Fonction pour analyser un dataframe
analyze_dataframe <- function(df, name) {
  write_log(paste0("Analyse de ", name, ":"))
  write_log(paste0("  - Dimensions: ", nrow(df), " lignes, ", ncol(df), " colonnes"))
  
  # Vérifier les valeurs manquantes
  na_counts <- colSums(is.na(df))
  na_cols <- names(na_counts[na_counts > 0])
  
  if (length(na_cols) > 0) {
    write_log(paste0("  - Colonnes avec valeurs manquantes: ", 
                    length(na_cols), " sur ", ncol(df)))
    for (col in na_cols) {
      pct_na <- round(na_counts[col] / nrow(df) * 100, 2)
      write_log(paste0("    * ", col, ": ", na_counts[col], " (", pct_na, "%)"))
    }
  } else {
    write_log("  - Aucune valeur manquante détectée")
  }
  
  # Vérifier la variable cible (si elle existe)
  if ("dv_voteChoice" %in% names(df)) {
    vote_counts <- table(df$dv_voteChoice)
    write_log("  - Distribution de dv_voteChoice:")
    
    for (party in names(vote_counts)) {
      pct_party <- round(vote_counts[party] / sum(vote_counts) * 100, 2)
      write_log(paste0("    * ", party, ": ", vote_counts[party], " (", pct_party, "%)"))
    }
  }
}

# Analyser les dataframes
analyze_dataframe(DataPilot, "DataPilot")
analyze_dataframe(DataApp, "DataApp (données récentes)")
analyze_dataframe(rta_predictions, "RTA Predictions")

# ------------------------------------------------------------------------
# 4) Prétraitement des données: filtrage et standardisation
# ------------------------------------------------------------------------
write_log("Prétraitement des données: filtrage et standardisation")

# Préserver des copies des données originales
DataPilot_original <- DataPilot
DataApp_original <- DataApp

# Filtrer les données pour retirer "other" et NA
DataPilot <- DataPilot %>% 
  filter(dv_voteChoice != "other" & !is.na(dv_voteChoice))

DataApp <- DataApp %>% 
  filter(dv_voteChoice != "other" & !is.na(dv_voteChoice))

# Vérification des valeurs uniques
unique_values_pilot <- unique(as.character(DataPilot$dv_voteChoice))
write_log(paste0("Valeurs uniques dans DataPilot: ", paste(unique_values_pilot, collapse=", ")))

unique_values_app <- unique(as.character(DataApp$dv_voteChoice))
write_log(paste0("Valeurs uniques dans DataApp: ", paste(unique_values_app, collapse=", ")))

# Standardisation: convertir en facteurs avec les mêmes niveaux
all_levels <- unique(c(unique_values_pilot, unique_values_app))

# Standardiser les noms de partis (npd -> ndp si nécessaire)
all_levels <- unique(gsub("npd", "ndp", all_levels))
write_log(paste0("Niveaux standardisés à utiliser: ", paste(all_levels, collapse=", ")))

# Convertir les variables en facteurs avec les niveaux standardisés
safe_operation({
  DataPilot$dv_voteChoice <- factor(
    gsub("npd", "ndp", as.character(DataPilot$dv_voteChoice)),
    levels = all_levels
  )
  
  DataApp$dv_voteChoice <- factor(
    gsub("npd", "ndp", as.character(DataApp$dv_voteChoice)),
    levels = all_levels
  )
}, "Erreur lors de la standardisation des niveaux de facteurs")

# Vérifier que les niveaux sont maintenant correctement définis
write_log(paste0("Niveaux de dv_voteChoice dans DataPilot: ", 
                paste(levels(DataPilot$dv_voteChoice), collapse=", ")))
write_log(paste0("Niveaux de dv_voteChoice dans DataApp: ", 
                paste(levels(DataApp$dv_voteChoice), collapse=", ")))

# ------------------------------------------------------------------------
# 5) Harmonisation des facteurs
# ------------------------------------------------------------------------
write_log("Harmonisation des facteurs entre les jeux de données")

# Fonction améliorée pour harmoniser les facteurs
harmonize_factor <- function(x_pilot, x_app, var_name = "inconnu") {
  # Convertir en caractères pour s'assurer de la compatibilité
  x_pilot_char <- as.character(x_pilot)
  x_app_char <- as.character(x_app)
  
  # Identifier tous les niveaux uniques
  all_levels <- unique(c(x_pilot_char, x_app_char))
  write_log(paste0("  - Variable ", var_name, ": ", length(all_levels), " niveaux uniques"))
  
  # Reconvertir en facteurs avec les mêmes niveaux
  x_pilot_new <- factor(x_pilot_char, levels = all_levels)
  x_app_new <- factor(x_app_char, levels = all_levels)
  
  return(list(pilot = x_pilot_new, app = x_app_new))
}

# Identifier les variables du modèle qui sont des facteurs
model_factor_vars <- model_variables[model_variables %in% names(DataPilot)]
write_log(paste0("Variables facteurs potentielles: ", paste(model_factor_vars, collapse=", ")))

# Identifier les facteurs dans DataPilot
pilot_factors <- names(DataPilot)[sapply(DataPilot, is.factor)]
write_log(paste0("Facteurs dans DataPilot: ", paste(pilot_factors, collapse=", ")))

# Obtenir l'intersection
model_factors <- intersect(model_factor_vars, pilot_factors)
write_log(paste0("Facteurs du modèle à harmoniser: ", paste(model_factors, collapse=", ")))

# Harmoniser les facteurs importants
for (f in model_factors) {
  if (f %in% names(DataPilot) && f %in% names(DataApp)) {
    write_log(paste0("Harmonisation du facteur: ", f))
    
    # Vérifier et harmoniser
    harmonized <- safe_operation(
      harmonize_factor(DataPilot[[f]], DataApp[[f]], f),
      paste0("Erreur lors de l'harmonisation du facteur ", f)
    )
    
    if (!is.null(harmonized)) {
      DataPilot[[f]] <- harmonized$pilot
      DataApp[[f]] <- harmonized$app
      write_log(paste0("  - Niveaux harmonisés: ", paste(levels(DataPilot[[f]]), collapse=", ")))
    }
  }
}

# ------------------------------------------------------------------------
# 6) Préparation des données pour le modèle
# ------------------------------------------------------------------------
write_log("Préparation des données pour le modèle")

# Ajouter une variable source pour identifier l'origine des données
DataPilot$source <- "pilote"
DataApp$source <- "application"

# Sélection des variables nécessaires
DataPilot_selected <- safe_operation(
  DataPilot %>%
    select(all_of(c(model_variables, "dv_voteChoice", "source", "ses_postalCode"))) %>%
    drop_na(),
  "Erreur lors de la sélection des variables de DataPilot"
)

DataApp_selected <- safe_operation(
  DataApp %>%
    select(all_of(c(model_variables, "dv_voteChoice", "source", "ses_postalCode"))) %>%
    drop_na(),
  "Erreur lors de la sélection des variables de DataApp"
)

# Vérifier les dimensions après sélection
write_log(paste0("Dimensions après sélection - DataPilot: ", 
                nrow(DataPilot_selected), " lignes, ", 
                ncol(DataPilot_selected), " colonnes"))
write_log(paste0("Dimensions après sélection - DataApp: ", 
                nrow(DataApp_selected), " lignes, ", 
                ncol(DataApp_selected), " colonnes"))

# ------------------------------------------------------------------------
# 7) Extraction des RTA et enrichissement avec les prédictions
# ------------------------------------------------------------------------
write_log("Extraction des RTA et enrichissement")

# Extraire les RTA des codes postaux
DataPilot_selected <- safe_operation(
  DataPilot_selected %>%
    mutate(
      ses_postalCode_clean = toupper(as.character(ses_postalCode)),
      rta = substr(ses_postalCode_clean, 1, 3)
    ),
  "Erreur lors de l'extraction des RTA de DataPilot"
)

DataApp_selected <- safe_operation(
  DataApp_selected %>%
    mutate(
      ses_postalCode_clean = toupper(as.character(ses_postalCode)),
      rta = substr(ses_postalCode_clean, 1, 3)
    ),
  "Erreur lors de l'extraction des RTA de DataApp"
)

# Standardiser les RTA dans le fichier de prédictions
rta_predictions$rta <- toupper(rta_predictions$rta)

# Fonction pour enrichir un dataframe avec les prédictions RTA (avec gestion d'erreurs)
enrich_with_predictions <- function(df, rta_preds) {
  # Vérifier les RTAs manquantes
  missing_rtas <- sum(!df$rta %in% rta_preds$rta)
  total_rtas <- nrow(df)
  write_log(paste0("RTAs manquantes: ", missing_rtas, "/", total_rtas, 
                  " (", round(missing_rtas/total_rtas*100, 2), "%)"))
  
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
DataPilot_enriched <- safe_operation(
  enrich_with_predictions(DataPilot_selected, rta_predictions),
  "Erreur lors de l'enrichissement de DataPilot avec les prédictions RTA"
)

DataApp_enriched <- safe_operation(
  enrich_with_predictions(DataApp_selected, rta_predictions),
  "Erreur lors de l'enrichissement de DataApp avec les prédictions RTA"
)

# ------------------------------------------------------------------------
# 8) Harmonisation des types de données et fusion
# ------------------------------------------------------------------------
write_log("Harmonisation des types de données et fusion")

# Fonction pour harmoniser les types de données entre deux dataframes
harmonize_column_types <- function(df1, df2) {
  common_cols <- intersect(names(df1), names(df2))
  
  for (col in common_cols) {
    type1 <- class(df1[[col]])[1]
    type2 <- class(df2[[col]])[1]
    
    if (type1 != type2) {
      write_log(paste0("Différence de type détectée pour ", col, " : ", type1, " vs ", type2))
      
      # Convertir en character si l'un des deux est character
      if ("character" %in% c(type1, type2)) {
        write_log(paste0("Conversion en character pour ", col))
        df1[[col]] <- as.character(df1[[col]])
        df2[[col]] <- as.character(df2[[col]])
      }
      # Convertir en numeric si possible
      else if (all(c("numeric", "integer") %in% c(type1, type2))) {
        write_log(paste0("Conversion en numeric pour ", col))
        df1[[col]] <- as.numeric(df1[[col]])
        df2[[col]] <- as.numeric(df2[[col]])
      }
      # Pour les facteurs, convertir en character puis fusionner les niveaux
      else if ("factor" %in% c(type1, type2)) {
        write_log(paste0("Conversion de facteur pour ", col))
        df1[[col]] <- as.character(df1[[col]])
        df2[[col]] <- as.character(df2[[col]])
      }
    }
  }
  
  return(list(df1 = df1, df2 = df2))
}

# Harmoniser les types avant fusion
harmonized <- safe_operation(
  harmonize_column_types(DataPilot_enriched, DataApp_enriched),
  "Erreur lors de l'harmonisation des types de colonnes"
)

if (!is.null(harmonized)) {
  DataPilot_harmonized <- harmonized$df1
  DataApp_harmonized <- harmonized$df2
  
  # Essayer la fusion avec les dataframes harmonisés
  DataModel <- safe_operation(
    bind_rows(DataPilot_harmonized, DataApp_harmonized),
    "Erreur lors de la fusion des jeux de données"
  )
  
  if (is.null(DataModel)) {
    write_log("ERREUR CRITIQUE: Échec de la fusion des données, arrêt du script")
    stop("Échec de la fusion des données")
  }
} else {
  write_log("ERREUR CRITIQUE: Échec de l'harmonisation des types, arrêt du script")
  stop("Échec de l'harmonisation des types")
}

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

write_log("Résumé des prédictions par source:")
print(prediction_summary)
capture.output(prediction_summary, file = log_file, append = TRUE)

# Vérifier que les colonnes importantes sont présentes
cols_to_check <- c("prediction_CPC", "prediction_LPC", "prediction_NDP", 
                   "prediction_GPC", "prediction_BQ", "rta", "ses_postalCode")

write_log("Vérification des colonnes importantes dans DataModel:")
for (col in cols_to_check) {
  write_log(paste0("- ", col, " présente: ", col %in% names(DataModel)))
}

# ------------------------------------------------------------------------
# 9) Séparation Train/Test avec stratification par source et parti
# ------------------------------------------------------------------------
write_log("Séparation Train/Test avec stratification")

set.seed(42)  # Pour la reproductibilité

# Stratifier par parti et par source pour maintenir la distribution
trainIndex <- safe_operation(
  createDataPartition(
    interaction(DataModel$dv_voteChoice, DataModel$source), 
    p = 0.8, 
    list = FALSE
  ),
  "Erreur lors de la création de l'index d'entraînement"
)

if (!is.null(trainIndex)) {
  DfTrain <- DataModel[trainIndex, ]
  DfTest <- DataModel[-trainIndex, ]
  
  # Vérification de la distribution dans les ensembles train et test
  write_log("Distribution dans l'ensemble d'entraînement:")
  train_dist <- table(DfTrain$dv_voteChoice, DfTrain$source)
  print(train_dist)
  capture.output(train_dist, file = log_file, append = TRUE)
  
  write_log("Distribution dans l'ensemble de test:")
  test_dist <- table(DfTest$dv_voteChoice, DfTest$source)
  print(test_dist)
  capture.output(test_dist, file = log_file, append = TRUE)
} else {
  write_log("ERREUR CRITIQUE: Échec de la création de l'index d'entraînement, arrêt du script")
  stop("Échec de la séparation train/test")
}

# ------------------------------------------------------------------------
# 10) Fonction d'évaluation multiclasse
# ------------------------------------------------------------------------
write_log("Définition de la fonction d'évaluation multiclasse")

multiClassSummary2 <- function(data, lev = NULL, model = NULL) {
  # S'assurer que les données sont au bon format
  if (!all(c("obs", "pred") %in% names(data))) {
    stop("Les données doivent contenir les colonnes 'obs' et 'pred'")
  }
  
  # Classes à utiliser
  classes <- if (!is.null(lev)) lev else levels(data$obs)
  
  # 1) Accuracy
  acc <- tryCatch(
    yardstick::accuracy_vec(data$obs, data$pred),
    error = function(e) {
      warning("Erreur dans le calcul d'accuracy: ", e$message)
      return(NA)
    }
  )
  
  # 2) Kappa
  kap <- tryCatch(
    yardstick::kap_vec(data$obs, data$pred),
    error = function(e) {
      warning("Erreur dans le calcul de kappa: ", e$message)
      return(NA)
    }
  )
  
  # 3) LogLoss
  eps <- 1e-15
  n <- nrow(data)
  ll <- tryCatch({
    ll_val <- 0
    for (i in seq_len(n)) {
      obs_class <- as.character(data$obs[i])
      prob_col <- paste0("prob.", obs_class)
      if (prob_col %in% names(data)) {
        p <- data[[prob_col]][i]
        p <- max(p, eps)
        ll_val <- ll_val - log(p)
      } else {
        warning("Colonne de probabilité manquante: ", prob_col)
      }
    }
    ll_val / n
  }, error = function(e) {
    warning("Erreur dans le calcul de logLoss: ", e$message)
    return(NA)
  })
  
  # 4) F1-score macro
  f1s <- sapply(classes, function(cl) {
    tp <- sum(data$obs == cl & data$pred == cl)
    fp <- sum(data$obs != cl & data$pred == cl)
    fn <- sum(data$obs == cl & data$pred != cl)
    precision <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
    recall <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
    if (precision + recall == 0) 0 else 2 * precision * recall / (precision + recall)
  })
  f1_macro <- mean(f1s, na.rm = TRUE)
  
  # 5) Calcul d'une pénalité personnalisée pour les erreurs critiques
  conf <- table(data$pred, data$obs)
  penalty <- 0
  penalty_value <- 5  # Coefficient de pénalité
  if ("ndp" %in% classes && "cpc" %in% classes) {
    if ("cpc" %in% rownames(conf) && "ndp" %in% colnames(conf)) {
      penalty <- penalty + penalty_value * conf["cpc", "ndp"]
    }
  }
  if ("gpc" %in% classes && "cpc" %in% classes) {
    if ("cpc" %in% rownames(conf) && "gpc" %in% colnames(conf)) {
      penalty <- penalty + penalty_value * conf["cpc", "gpc"]
    }
  }
  composite_score <- acc - (penalty / n)
  
  out <- c(accuracy = acc, kappa = kap, logLoss = ll, f1 = f1_macro, composite_score = composite_score)
  return(out)
}

# ------------------------------------------------------------------------
# 11) Construction du modèle enrichi avec les variables RTA
# ------------------------------------------------------------------------
write_log("Construction du modèle enrichi avec les variables RTA")

# Variables du modèle précédent + nouvelles variables de prédiction RTA
final_vars <- c(model_variables, "prediction_CPC", "prediction_LPC", "prediction_NDP", "prediction_GPC", "prediction_BQ")

# Préparation des données pour le modèle
X_train_final <- DfTrain[, final_vars, drop = FALSE]
y_train_final <- DfTrain$dv_voteChoice

# Création des variables dummy
dummies_final <- safe_operation(
  dummyVars(" ~ .", data = X_train_final, fullRank = TRUE, sep = "_"),
  "Erreur lors de la création des variables dummy"
)

if (is.null(dummies_final)) {
  write_log("ERREUR CRITIQUE: Échec de la création des variables dummy, arrêt du script")
  stop("Échec de la création des variables dummy")
}

X_train_dummy <- safe_operation(
  predict(dummies_final, newdata = X_train_final) %>% as.data.frame(),
  "Erreur lors de la génération de la matrice dummy"
)

if (is.null(X_train_dummy)) {
  write_log("ERREUR CRITIQUE: Échec de la génération de la matrice dummy, arrêt du script")
  stop("Échec de la génération de la matrice dummy")
}

# Vérification des dimensions de la matrice
write_log(paste0("Dimensions de la matrice d'entraînement: ", 
                nrow(X_train_dummy), " lignes, ", 
                ncol(X_train_dummy), " colonnes"))

# ------------------------------------------------------------------------
# 12) Fixation manuelle des valeurs de référence des variables
# ------------------------------------------------------------------------
write_log("Configuration manuelle des catégories de référence")

# 1. Fixer la classe de référence pour la variable dépendante dv_voteChoice
write_log(paste0("Niveaux actuels de dv_voteChoice: ", paste(levels(y_train_final), collapse=", ")))

# Définir "bq" comme référence si possible
if ("bq" %in% levels(y_train_final)) {
  y_train_final <- safe_operation(
    relevel(y_train_final, ref = "bq"),
    "Erreur lors de la définition de 'bq' comme référence pour dv_voteChoice"
  )
  write_log("Variable dépendante: 'bq' définie comme référence")
} else {
  write_log("Impossible de définir 'bq' comme référence car ce niveau n'existe pas")
}

# 2. Fixer les références pour les variables catégorielles dans X_train_final
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
  ses_income3Cat = "High"
)

# Boucle pour définir les références
for (var_name in names(X_train_final)) {
  if (!is.factor(X_train_final[[var_name]])) {
    write_log(paste0("La variable ", var_name, " n'est pas un facteur, ignorer"))
    next
  }
  
  # Obtenir la référence souhaitée
  ref_value <- reference_mapping[[var_name]]
  if (!is.null(ref_value) && ref_value %in% levels(X_train_final[[var_name]])) {
    X_train_final[[var_name]] <- safe_operation(
      relevel(X_train_final[[var_name]], ref = ref_value),
      paste0("Erreur lors de la définition de '", ref_value, "' comme référence pour ", var_name)
    )
    write_log(paste0(var_name, ": '", ref_value, "' définie comme référence"))
  }
}

# 3. Recréer la matrice de dummy variables après avoir réordonnancé les facteurs
write_log("Recréation des variables dummy après recodage des références")

dummies_final <- safe_operation(
  dummyVars(" ~ .", data = X_train_final, fullRank = TRUE, sep = "_"),
  "Erreur lors de la recréation des variables dummy"
)

X_train_dummy <- safe_operation(
  predict(dummies_final, newdata = X_train_final) %>% as.data.frame(),
  "Erreur lors de la génération de la nouvelle matrice dummy"
)

if (is.null(X_train_dummy)) {
  write_log("ERREUR CRITIQUE: Échec de la génération de la nouvelle matrice dummy, arrêt du script")
  stop("Échec de la génération de la nouvelle matrice dummy")
}

write_log(paste0("Nouvelles dimensions de la matrice d'entraînement: ", 
                nrow(X_train_dummy), " lignes, ", 
                ncol(X_train_dummy), " colonnes"))

# ------------------------------------------------------------------------
# 13) Entraînement du modèle
# ------------------------------------------------------------------------
write_log("Entraînement du modèle final")

# Définir un temps maximum pour l'entraînement (timeout en secondes)
timeout_seconds <- 3600  # 1 heure par défaut

# Fonction pour entraîner avec timeout
train_with_timeout <- function(formula, data, trace, MaxNWts, timeout) {
  result <- NULL
  
  # Fonction d'entraînement à exécuter avec timeout
  train_func <- function() {
    multinom(formula, data = data, trace = trace, MaxNWts = MaxNWts)
  }
  
  # Essayer avec un timeout
  tryCatch({
    # Utiliser R.utils::withTimeout si disponible
    if (requireNamespace("R.utils", quietly = TRUE)) {
      result <- R.utils::withTimeout(train_func(), timeout = timeout)
    } else {
      # Fallback sans timeout si R.utils n'est pas disponible
      result <- train_func()
    }
  }, TimeoutException = function(e) {
    write_log(paste0("AVERTISSEMENT: L'entraînement a dépassé le délai de ", timeout, " secondes"))
    stop("Timeout pendant l'entraînement du modèle")
  })
  
  return(result)
}

# Entraînement avec gestion d'erreurs et timeout
final_model <- safe_operation({
  # Essayer d'abord avec timeout si R.utils est disponible
  if (requireNamespace("R.utils", quietly = TRUE)) {
    write_log("Entraînement avec timeout activé")
    train_with_timeout(
      y_train_final ~ ., 
      data = X_train_dummy, 
      trace = FALSE,
      MaxNWts = 100000,
      timeout = timeout_seconds
    )
  } else {
    # Fallback sans timeout
    write_log("Entraînement sans timeout (R.utils non disponible)")
    multinom(
      y_train_final ~ ., 
      data = X_train_dummy, 
      trace = FALSE,
      MaxNWts = 100000
    )
  }
}, "Erreur lors de l'entraînement du modèle multinomial")

if (is.null(final_model)) {
  write_log("ERREUR CRITIQUE: Échec de l'entraînement du modèle final, arrêt du script")
  stop("Échec de l'entraînement du modèle")
}

write_log("Modèle final entraîné avec succès")

# ------------------------------------------------------------------------
# 14) Symmétrisation des coefficients
# ------------------------------------------------------------------------
write_log("Symmétrisation des coefficients du modèle")

# Récupérer les coefficients originaux
all_levels <- levels(y_train_final)
orig_coef <- coef(final_model)
write_log(paste0("Dimensions des coefficients originaux: ", nrow(orig_coef), " x ", ncol(orig_coef)))

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
write_log("Coefficients symétriques ajoutés au modèle")

# ------------------------------------------------------------------------
# 15) Évaluation du modèle
# ------------------------------------------------------------------------
write_log("Évaluation du modèle")

# Préparation des données de test
X_test_final <- DfTest[, final_vars, drop = FALSE]
y_test_final <- DfTest$dv_voteChoice

X_test_dummy <- safe_operation(
  predict(dummies_final, newdata = X_test_final) %>% as.data.frame(),
  "Erreur lors de la génération de la matrice dummy pour les données de test"
)

if (is.null(X_test_dummy)) {
  write_log("ERREUR lors de la génération de la matrice dummy pour les données de test")
  # Ne pas arrêter le script, mais ne pas faire l'évaluation
} else {
  # Prédictions
  pred_test_final_class <- safe_operation(
    predict(final_model, newdata = X_test_dummy),
    "Erreur lors de la prédiction des classes"
  )
  
  pred_test_final_prob <- safe_operation(
    predict(final_model, newdata = X_test_dummy, type = "probs"),
    "Erreur lors de la prédiction des probabilités"
  )
  
  if (!is.null(pred_test_final_class) && !is.null(pred_test_final_prob)) {
    # Évaluation globale
    acc_test_final <- mean(pred_test_final_class == y_test_final)
    write_log(paste0("Accuracy globale (test) : ", round(acc_test_final, 4)))
    
    # Évaluation séparée par source
    DfTest_with_preds <- DfTest %>%
      mutate(predicted = pred_test_final_class)
    
    # Performance sur les données pilotes
    acc_pilot <- DfTest_with_preds %>%
      filter(source == "pilote") %>%
      summarise(accuracy = mean(predicted == dv_voteChoice)) %>%
      pull(accuracy)
    write_log(paste0("Accuracy sur données pilotes : ", round(acc_pilot, 4)))
    
    # Performance sur les données de l'application
    acc_app <- DfTest_with_preds %>%
      filter(source == "application") %>%
      summarise(accuracy = mean(predicted == dv_voteChoice)) %>%
      pull(accuracy)
    write_log(paste0("Accuracy sur données application : ", round(acc_app, 4)))
    
    # Matrice de confusion
    table_test <- table(
      predicted = pred_test_final_class,
      actual = y_test_final
    )
    write_log("Matrice de confusion globale :")
    print(table_test)
    capture.output(table_test, file = log_file, append = TRUE)
    
    # Matrices de confusion séparées par source
    table_test_pilot <- with(
      DfTest_with_preds %>% filter(source == "pilote"),
      table(predicted = predicted, actual = dv_voteChoice)
    )
    write_log("Matrice de confusion (données pilotes) :")
    print(table_test_pilot)
    capture.output(table_test_pilot, file = log_file, append = TRUE)
    
    table_test_app <- with(
      DfTest_with_preds %>% filter(source == "application"),
      table(predicted = predicted, actual = dv_voteChoice)
    )
    write_log("Matrice de confusion (données application) :")
    print(table_test_app)
    capture.output(table_test_app, file = log_file, append = TRUE)
    
    # Calcul des métriques supplémentaires
    # Précision par classe
    precision_by_class <- sapply(levels(y_test_final), function(cl) {
      tp <- sum(y_test_final == cl & pred_test_final_class == cl)
      fp <- sum(y_test_final != cl & pred_test_final_class == cl)
      ifelse(tp + fp == 0, 0, tp / (tp + fp))
    })
    
    # Rappel par classe
    recall_by_class <- sapply(levels(y_test_final), function(cl) {
      tp <- sum(y_test_final == cl & pred_test_final_class == cl)
      fn <- sum(y_test_final == cl & pred_test_final_class != cl)
      ifelse(tp + fn == 0, 0, tp / (tp + fn))
    })
    
    # F1-score par classe
    f1_by_class <- sapply(levels(y_test_final), function(cl) {
      prec <- precision_by_class[cl]
      rec <- recall_by_class[cl]
      ifelse(prec + rec == 0, 0, 2 * prec * rec / (prec + rec))
    })
    
    # Afficher les métriques détaillées
    write_log("Métriques détaillées par classe :")
    metrics_df <- data.frame(
      Party = levels(y_test_final),
      Precision = round(precision_by_class, 4),
      Recall = round(recall_by_class, 4),
      F1_Score = round(f1_by_class, 4)
    )
    print(metrics_df)
    capture.output(metrics_df, file = log_file, append = TRUE)
  } else {
    write_log("Impossible d'évaluer le modèle en raison d'erreurs dans les prédictions")
  }
}

# ------------------------------------------------------------------------
# 16) Sauvegarde du modèle final
# ------------------------------------------------------------------------
write_log("Sauvegarde du modèle final")

# Créer les chemins de fichiers de sortie
output_model_path <- paste0("_SharedFolder_datagotchi_federal_2024/data/modele/finalmodel_withRTAPredictions_april3_", date_execution, ".rds")
output_dummies_path <- paste0("_SharedFolder_datagotchi_federal_2024/data/modele/dummies_finalmodel_withRTAPredictions_april3_", date_execution, ".rds")

# Sauvegarder le modèle
save_result <- safe_operation(
  saveRDS(final_model, output_model_path),
  paste0("Erreur lors de la sauvegarde du modèle final: ", output_model_path)
)

if (is.null(save_result)) {
  write_log(paste0("Modèle sauvegardé avec succès: ", output_model_path))
} else {
  write_log(paste0("AVERTISSEMENT: Problème lors de la sauvegarde du modèle: ", output_model_path))
}

# Sauvegarder les dummies
save_dummies_result <- safe_operation(
  saveRDS(dummies_final, output_dummies_path),
  paste0("Erreur lors de la sauvegarde des dummies: ", output_dummies_path)
)

if (is.null(save_dummies_result)) {
  write_log(paste0("Dummies sauvegardées avec succès: ", output_dummies_path))
} else {
  write_log(paste0("AVERTISSEMENT: Problème lors de la sauvegarde des dummies: ", output_dummies_path))
}

# ------------------------------------------------------------------------
# 17) Comparaison avec le modèle précédent
# ------------------------------------------------------------------------
write_log("Comparaison avec le modèle précédent")

# Examiner la structure des coefficients du modèle précédent
previous_coef <- safe_operation(
  coef(previous_model),
  "Erreur lors de l'extraction des coefficients du modèle précédent"
)

if (!is.null(previous_coef)) {
  write_log(paste0("Dimensions des coefficients du modèle précédent: ", 
                  nrow(previous_coef), " x ", ncol(previous_coef)))
  write_log(paste0("Noms des lignes (classes): ", paste(rownames(previous_coef), collapse=", ")))
  write_log(paste0("Nombre de variables: ", ncol(previous_coef)))
  write_log(paste0("Quelques noms de variables: ", paste(head(colnames(previous_coef)), collapse=", "), "..."))
  
  # Examiner la structure des coefficients du nouveau modèle
  new_coef <- coef(final_model)
  write_log(paste0("Dimensions des coefficients du nouveau modèle: ", 
                  nrow(new_coef), " x ", ncol(new_coef)))
  write_log(paste0("Noms des lignes (classes): ", paste(rownames(new_coef), collapse=", ")))
  write_log(paste0("Nombre de variables: ", ncol(new_coef)))
  write_log(paste0("Quelques noms de variables: ", paste(head(colnames(new_coef)), collapse=", "), "..."))
  
  # Vérifier les différences entre les variables
  variables_previous <- colnames(previous_coef)
  variables_new <- colnames(new_coef)
  
  # Variables présentes dans l'ancien modèle mais pas dans le nouveau
  only_in_previous <- setdiff(variables_previous, variables_new)
  write_log(paste0("Variables présentes uniquement dans l'ancien modèle: ", length(only_in_previous)))
  if (length(only_in_previous) > 0) {
    write_log(paste0("Exemples: ", paste(head(only_in_previous), collapse=", "), "..."))
  }
  
  # Variables présentes dans le nouveau modèle mais pas dans l'ancien
  only_in_new <- setdiff(variables_new, variables_previous)
  write_log(paste0("Variables présentes uniquement dans le nouveau modèle: ", length(only_in_new)))
  if (length(only_in_new) > 0) {
    write_log(paste0("Exemples: ", paste(head(only_in_new), collapse=", "), "..."))
  }
  
  # Variables communes aux deux modèles
  common_variables <- intersect(variables_previous, variables_new)
  write_log(paste0("Variables communes aux deux modèles: ", length(common_variables)))
}

# ------------------------------------------------------------------------
# 18) Vérification finale et conclusion
# ------------------------------------------------------------------------
write_log("Vérification finale et conclusion")

# Vérifier que tous les objets essentiels ont été créés
essential_objects <- c("final_model", "dummies_final", "acc_test_final", "acc_pilot", "acc_app")
missing_objects <- essential_objects[!sapply(essential_objects, exists)]

if (length(missing_objects) > 0) {
  write_log(paste0("AVERTISSEMENT: Certains objets essentiels n'ont pas été créés: ", 
                  paste(missing_objects, collapse=", ")))
} else {
  write_log("Tous les objets essentiels ont été créés avec succès")
}

# Résumé des performances
if (exists("acc_test_final") && !is.null(acc_test_final)) {
  write_log("\nRÉSUMÉ DES PERFORMANCES:")
  write_log(paste0("Accuracy globale: ", round(acc_test_final, 4)))
  write_log(paste0("Accuracy sur données pilotes: ", round(acc_pilot, 4)))
  write_log(paste0("Accuracy sur données application: ", round(acc_app, 4)))
  
  write_log("\nChemin des fichiers de sortie:")
  write_log(paste0("- Modèle: ", output_model_path))
  write_log(paste0("- Dummies: ", output_dummies_path))
  write_log(paste0("- Journal: ", log_file))
}

write_log("Exécution du script terminée avec succès")
