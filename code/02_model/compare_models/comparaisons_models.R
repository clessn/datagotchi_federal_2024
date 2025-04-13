# ------------------------------------------------------------------------
# 1) Chargement des packages
# ------------------------------------------------------------------------
library(tidyverse)
library(nnet)
library(caret)
library(yardstick)
library(knitr)
library(kableExtra)
library(ggplot2)
library(gridExtra)
library(pbapply)


# ------------------------------------------------------------------------
# 2) Chargement des modèles et des données
# ------------------------------------------------------------------------
# Modèle original (sans RTA)
model_original <- readRDS("_SharedFolder_datagotchi_federal_2024/data/modele/_previous/finalmodel_withOutInteractions.rds")

# Modèle amélioré (avec RTA)
model_improved <- readRDS("_SharedFolder_datagotchi_federal_2024/data/modele/finalmodel_withRTAPredictions_2025-04-15.rds")
dummies_improved <- readRDS("_SharedFolder_datagotchi_federal_2024/data/modele/dummies_finalmodel_withRTAPredictions_2025-04-15.rds")

# Charger les résultats du modèle original pour obtenir les variables
results_train_original <- readRDS("_SharedFolder_datagotchi_federal_2024/data/modele/_previous/resultsTrainV4_31janvier2025.rds")

# Identifier le meilleur modèle selon l'accuracy
best_iterations <- results_train_original %>%
  group_by(model_id) %>%
  summarise(
    score_iter_accuracy = first(accuracy_cv),
    .groups = "drop"
  ) %>%
  arrange(desc(score_iter_accuracy))

best_id <- best_iterations$model_id[1]
cat("Meilleur modèle original trouvé (ID) =", best_id, "\n")

# Récupérer les variables du meilleur modèle original
best_config <- results_train_original %>%
  filter(model_id == best_id)

original_variables <- unique(best_config$variable)
cat("Variables du modèle original:", length(original_variables), "\n")
cat("Liste des variables:", paste(original_variables, collapse=", "), "\n")

# Charger les prédictions par RTA
rta_predictions <- read.csv("_SharedFolder_datagotchi_federal_2024/data/modele/rta_predictions_partis_previous.csv",
                           stringsAsFactors = FALSE)

# Charger les données
DataPilot <- readRDS("_SharedFolder_datagotchi_federal_2024/data/pilote/dataClean/datagotchi2025_canada_pilot_20250322.rds")
DataApp <- readRDS("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/_previous/datagotchi2025_canada_app_20250314.rds")

# ------------------------------------------------------------------------
# 3) Préparation des données selon la méthode du modèle original (Script 1)
# ------------------------------------------------------------------------
# Traitement comme dans le Script 1
DataModel_Original <- DataPilot %>% 
  select(all_of(c(original_variables, "dv_voteChoice"))) %>%
  drop_na() %>%
  filter(dv_voteChoice != "other")

# Convert all ordered factors to unordered factors
DataModel_Original <- DataModel_Original %>%
  mutate(across(where(is.ordered), ~ factor(., ordered = FALSE)))

# Conversion de quelques variables en facteur
DataModel_Original$dv_voteChoice <- factor(DataModel_Original$dv_voteChoice)

# Appliquer un contraste somme à dv_voteChoice pour le modèle original
contrasts(DataModel_Original$dv_voteChoice) <- contr.sum(nlevels(DataModel_Original$dv_voteChoice))

DataModel_Original$lifestyle_consCoffee <- factor(DataModel_Original$lifestyle_consCoffee, ordered = FALSE)
DataModel_Original$lifestyle_consClothes <- factor(DataModel_Original$lifestyle_consClothes, ordered = FALSE)

# Séparation Train/Test avec le même seed que le script original
set.seed(42)
trainIndex_Original <- createDataPartition(DataModel_Original$dv_voteChoice, p = 0.8, list = FALSE)
DfTrain_Original <- DataModel_Original[trainIndex_Original, ]
DfTest_Original <- DataModel_Original[-trainIndex_Original, ]

# ------------------------------------------------------------------------
# 4) Préparation des données selon la méthode du modèle amélioré (Script 2)
# ------------------------------------------------------------------------
# Standardisation des dv_voteChoice comme dans le Script 2
unique_values_pilot <- unique(as.character(DataPilot$dv_voteChoice))
unique_values_app <- unique(as.character(DataApp$dv_voteChoice))

all_levels <- unique(c(
  unique_values_pilot[unique_values_pilot != "other"],
  unique_values_app
))

all_levels <- unique(gsub("npd", "ndp", all_levels))
cat("Niveaux standardisés à utiliser:", paste(all_levels, collapse=", "), "\n")

# Standardiser et convertir en facteur
DataPilot$dv_voteChoice <- factor(
  gsub("npd", "ndp", as.character(DataPilot$dv_voteChoice)),
  levels = all_levels
)

DataApp$dv_voteChoice <- factor(
  gsub("npd", "ndp", as.character(DataApp$dv_voteChoice)),
  levels = all_levels
)

# Fonction pour harmoniser les facteurs comme dans le Script 2
harmonize_factor <- function(x_pilot, x_app) {
  x_pilot_char <- as.character(x_pilot)
  x_app_char <- as.character(x_app)
  
  all_levels <- unique(c(x_pilot_char, x_app_char))
  
  x_pilot_new <- factor(x_pilot_char, levels = all_levels)
  x_app_new <- factor(x_app_char, levels = all_levels)
  
  return(list(pilot = x_pilot_new, app = x_app_new))
}

# Identifier les variables du modèle qui sont des facteurs
model_factor_vars <- original_variables[original_variables %in% names(DataPilot)]
pilot_factors <- names(DataPilot)[sapply(DataPilot, is.factor)]
model_factors <- intersect(model_factor_vars, pilot_factors)
cat("Facteurs du modèle à harmoniser:", paste(model_factors, collapse=", "), "\n")

# Harmoniser les facteurs importants
for (f in model_factors) {
  if (f %in% names(DataPilot) && f %in% names(DataApp)) {
    cat("Harmonisation du facteur:", f, "\n")
    harmonized <- harmonize_factor(DataPilot[[f]], DataApp[[f]])
    DataPilot[[f]] <- harmonized$pilot
    DataApp[[f]] <- harmonized$app
  }
}

# Ajouter une variable source pour identifier l'origine des données
DataPilot$source <- "pilote"
DataApp$source <- "application"

# Récupérer les variables du modèle amélioré (originales + RTA)
improved_variables <- c(original_variables, 
                        "prediction_CPC", "prediction_LPC", 
                        "prediction_NDP", "prediction_GPC", "prediction_BQ")

# Pour DataPilot, filtrer et sélectionner comme dans Script 2
DataPilot_selected <- DataPilot %>%
  filter(dv_voteChoice != "other" & !is.na(dv_voteChoice)) %>%
  select(all_of(c(original_variables, "dv_voteChoice", "source", "ses_postalCode"))) %>%
  drop_na()

# Pour DataApp, sélectionner comme dans Script 2
DataApp_selected <- DataApp %>%
  filter(!is.na(dv_voteChoice)) %>%
  select(all_of(c(original_variables, "dv_voteChoice", "source", "ses_postalCode"))) %>%
  drop_na()

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

# Fonction pour enrichir avec les prédictions RTA comme dans Script 2
enrich_with_predictions <- function(df, rta_preds) {
  df_enriched <- df %>%
    left_join(rta_preds, by = "rta") %>%
    mutate(
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

# Fusionner les données
DataModel_Improved <- bind_rows(DataPilot_enriched, DataApp_enriched)

# Séparation Train/Test avec stratification par source et parti
set.seed(42)
trainIndex_Improved <- createDataPartition(
  interaction(DataModel_Improved$dv_voteChoice, DataModel_Improved$source), 
  p = 0.8, 
  list = FALSE
)
DfTrain_Improved <- DataModel_Improved[trainIndex_Improved, ]
DfTest_Improved <- DataModel_Improved[-trainIndex_Improved, ]

# Vérification de la distribution dans les ensembles test
cat("Distribution dans l'ensemble de test du modèle original:", "\n")
print(table(DfTest_Original$dv_voteChoice))

cat("Distribution dans l'ensemble de test du modèle amélioré:", "\n")
print(table(DfTest_Improved$dv_voteChoice, DfTest_Improved$source))

# ------------------------------------------------------------------------
# 5) Préparation pour les prédictions avec le modèle original
# ------------------------------------------------------------------------
# Utiliser la fonction dummyVars pour créer les variables dummy pour le modèle original
dummies_original <- dummyVars(" ~ .", data = DfTrain_Original[, original_variables, drop = FALSE], 
                             fullRank = TRUE, sep = "_")

# Appliquer aux données de test
X_test_original_dummy <- predict(dummies_original, 
                               newdata = DfTest_Original[, original_variables, drop = FALSE]) %>% 
                               as.data.frame()

# Prédictions avec le modèle original
pred_original_class <- predict(model_original, newdata = X_test_original_dummy)
pred_original_prob <- predict(model_original, newdata = X_test_original_dummy, type = "probs")

# Fonction pour trouver le deuxième choix le plus probable
get_second_choice <- function(prob_matrix) {
  apply(prob_matrix, 1, function(row) {
    names(sort(row, decreasing = TRUE))[2]
  })
}

second_choice_original <- get_second_choice(pred_original_prob)

# Créer un dataframe avec les résultats du modèle original
results_original <- data.frame(
  actual = DfTest_Original$dv_voteChoice,
  predicted_1st = pred_original_class,
  predicted_2nd = second_choice_original,
  correct_1st = pred_original_class == DfTest_Original$dv_voteChoice,
  correct_either = (pred_original_class == DfTest_Original$dv_voteChoice) | 
                   (second_choice_original == DfTest_Original$dv_voteChoice),
  model = "Original",
  source = "pilote"  # Toutes les données sont du pilote pour le modèle original
)

# ------------------------------------------------------------------------
# 6) Préparation pour les prédictions avec le modèle amélioré
# ------------------------------------------------------------------------
# Préparation des variables pour le modèle amélioré
X_test_improved <- DfTest_Improved[, improved_variables, drop = FALSE]

# Utiliser les dummies du modèle amélioré
X_test_improved_dummy <- predict(dummies_improved, newdata = X_test_improved) %>% as.data.frame()

# Prédictions avec le modèle amélioré
pred_improved_class <- predict(model_improved, newdata = X_test_improved_dummy)
pred_improved_prob <- predict(model_improved, newdata = X_test_improved_dummy, type = "probs")

# Trouver le deuxième choix le plus probable
second_choice_improved <- get_second_choice(pred_improved_prob)

# Créer un dataframe avec les résultats du modèle amélioré
results_improved <- data.frame(
  actual = DfTest_Improved$dv_voteChoice,
  predicted_1st = pred_improved_class,
  predicted_2nd = second_choice_improved,
  correct_1st = pred_improved_class == DfTest_Improved$dv_voteChoice,
  correct_either = (pred_improved_class == DfTest_Improved$dv_voteChoice) | 
                   (second_choice_improved == DfTest_Improved$dv_voteChoice),
  model = "Amélioré",
  source = DfTest_Improved$source
)

# ------------------------------------------------------------------------
# 7) Comparaison des performances
# ------------------------------------------------------------------------
# Calcul des métriques globales pour le modèle original
original_metrics <- results_original %>%
  summarise(
    accuracy_1st = mean(correct_1st),
    accuracy_either = mean(correct_either),
    n = n()
  )

# Calcul des métriques globales pour le modèle amélioré
improved_metrics_global <- results_improved %>%
  summarise(
    accuracy_1st = mean(correct_1st),
    accuracy_either = mean(correct_either),
    n = n()
  )

# Calcul des métriques par source pour le modèle amélioré
improved_metrics_by_source <- results_improved %>%
  group_by(source) %>%
  summarise(
    accuracy_1st = mean(correct_1st),
    accuracy_either = mean(correct_either),
    n = n(),
    .groups = "drop"
  )

# Calcul des métriques par parti pour les deux modèles
original_metrics_by_party <- results_original %>%
  group_by(actual) %>%
  summarise(
    accuracy_1st = mean(correct_1st),
    accuracy_either = mean(correct_either),
    n = n(),
    model = "Original",
    .groups = "drop"
  )

improved_metrics_by_party <- results_improved %>%
  group_by(actual) %>%
  summarise(
    accuracy_1st = mean(correct_1st),
    accuracy_either = mean(correct_either),
    n = n(),
    model = "Amélioré",
    .groups = "drop"
  )

# Combiner les métriques par parti pour les deux modèles
party_comparison <- bind_rows(original_metrics_by_party, improved_metrics_by_party)

# Affichage des métriques globales
cat("\nMétriques globales pour le modèle original:\n")
print(original_metrics)

cat("\nMétriques globales pour le modèle amélioré:\n")
print(improved_metrics_global)

cat("\nMétriques par source pour le modèle amélioré:\n")
print(improved_metrics_by_source)

cat("\nMétriques par parti:\n")
print(party_comparison)

# ------------------------------------------------------------------------
# 8) Matrice de confusion pour les deux modèles
# ------------------------------------------------------------------------
# Matrice de confusion pour le modèle original
confusion_original <- table(
  predicted = results_original$predicted_1st,
  actual = results_original$actual
)

# Matrice de confusion pour le modèle amélioré (global)
confusion_improved <- table(
  predicted = results_improved$predicted_1st,
  actual = results_improved$actual
)

# Matrices de confusion par source pour le modèle amélioré
confusion_improved_pilot <- with(
  results_improved %>% filter(source == "pilote"),
  table(predicted = predicted_1st, actual = actual)
)

confusion_improved_app <- with(
  results_improved %>% filter(source == "application"),
  table(predicted = predicted_1st, actual = actual)
)

cat("\nMatrice de confusion - Modèle original:\n")
print(confusion_original)

cat("\nMatrice de confusion - Modèle amélioré (global):\n")
print(confusion_improved)

cat("\nMatrice de confusion - Modèle amélioré (pilote):\n")
print(confusion_improved_pilot)

cat("\nMatrice de confusion - Modèle amélioré (application):\n")
print(confusion_improved_app)

# ------------------------------------------------------------------------
# 9) Visualisations
# ------------------------------------------------------------------------
# Graphique comparatif de l'accuracy (1er choix)
accuracy_comparison <- data.frame(
  Model = c("Original", "Amélioré (Global)", "Amélioré (Pilote)", "Amélioré (App)"),
  Accuracy = c(
    original_metrics$accuracy_1st,
    improved_metrics_global$accuracy_1st,
    improved_metrics_by_source$accuracy_1st[improved_metrics_by_source$source == "pilote"],
    improved_metrics_by_source$accuracy_1st[improved_metrics_by_source$source == "application"]
  )
)

plot_accuracy <- ggplot(accuracy_comparison, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", Accuracy * 100)), vjust = -0.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  theme_minimal() +
  labs(title = "Comparaison de l'accuracy (1er choix)", 
       y = "Accuracy", 
       x = "") +
  theme(legend.position = "none")

# Graphique comparatif de l'accuracy (1er ou 2e choix)
accuracy_either_comparison <- data.frame(
  Model = c("Original", "Amélioré (Global)", "Amélioré (Pilote)", "Amélioré (App)"),
  Accuracy = c(
    original_metrics$accuracy_either,
    improved_metrics_global$accuracy_either,
    improved_metrics_by_source$accuracy_either[improved_metrics_by_source$source == "pilote"],
    improved_metrics_by_source$accuracy_either[improved_metrics_by_source$source == "application"]
  )
)

plot_accuracy_either <- ggplot(accuracy_either_comparison, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", Accuracy * 100)), vjust = -0.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  theme_minimal() +
  labs(title = "Comparaison de l'accuracy (1er ou 2e choix)", 
       y = "Accuracy", 
       x = "") +
  theme(legend.position = "none")

# Graphique comparatif par parti (1er choix)
plot_by_party <- ggplot(party_comparison, aes(x = actual, y = accuracy_1st, fill = model)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.1f%%", accuracy_1st * 100)), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  theme_minimal() +
  labs(title = "Accuracy par parti (1er choix)", 
       y = "Accuracy", 
       x = "Parti",
       fill = "Modèle")

# Graphique comparatif par parti (1er ou 2e choix)
plot_by_party_either <- ggplot(party_comparison, aes(x = actual, y = accuracy_either, fill = model)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.1f%%", accuracy_either * 100)), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  theme_minimal() +
  labs(title = "Accuracy par parti (1er ou 2e choix)", 
       y = "Accuracy", 
       x = "Parti",
       fill = "Modèle")

# Créer un dossier pour les résultats si nécessaire
dir.create("_SharedFolder_datagotchi_federal_2024/data/modele/comparison", showWarnings = FALSE, recursive = TRUE)
dir.create("_SharedFolder_datagotchi_federal_2024/reports", showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------------
# 10) Sauvegarde des résultats et des graphiques
# ------------------------------------------------------------------------
# Sauvegarder les résultats des comparaisons
saveRDS(list(
  original_metrics = original_metrics,
  improved_metrics_global = improved_metrics_global,
  improved_metrics_by_source = improved_metrics_by_source,
  original_metrics_by_party = original_metrics_by_party,
  improved_metrics_by_party = improved_metrics_by_party,
  party_comparison = party_comparison,
  confusion_original = confusion_original,
  confusion_improved = confusion_improved,
  confusion_improved_pilot = confusion_improved_pilot,
  confusion_improved_app = confusion_improved_app
), "_SharedFolder_datagotchi_federal_2024/data/modele/comparison/results.rds")

# Sauvegarder les graphiques
ggsave("_SharedFolder_datagotchi_federal_2024/data/modele/comparison/plot_accuracy.png", plot_accuracy, width = 8, height = 6)
ggsave("_SharedFolder_datagotchi_federal_2024/data/modele/comparison/plot_accuracy_either.png", plot_accuracy_either, width = 8, height = 6)
ggsave("_SharedFolder_datagotchi_federal_2024/data/modele/comparison/plot_by_party.png", plot_by_party, width = 10, height = 6)
ggsave("_SharedFolder_datagotchi_federal_2024/data/modele/comparison/plot_by_party_either.png", plot_by_party_either, width = 10, height = 6)


# ------------------------------------------------------------------------
# 11) Analyse de la redirection des erreurs de prédiction
# ------------------------------------------------------------------------

# Fonction pour créer une matrice de redirection des erreurs
create_error_redirection_matrix <- function(results_df, title) {
  # Conserver uniquement les prédictions incorrectes
  incorrect_predictions <- results_df %>%
    filter(!correct_1st)
  
  # Créer une matrice de confusion pour les erreurs uniquement
  error_matrix <- table(
    predicted = incorrect_predictions$predicted_1st,
    actual = incorrect_predictions$actual
  )
  
  # Convertir en pourcentage par ligne (parti réel)
  error_matrix_pct <- prop.table(error_matrix, margin = 2)
  
  # Créer un dataframe pour ggplot
  error_df <- as.data.frame(error_matrix_pct)
  names(error_df) <- c("Predicted", "Actual", "Proportion")
  
  # Mettre en forme les pourcentages
  error_df$Percentage <- sprintf("%.1f%%", error_df$Proportion * 100)
  
  # Filtrer les valeurs 0 pour éviter d'encombrer le graphique
  error_df <- error_df %>% filter(Proportion > 0)
  
  # Créer une heatmap avec fond blanc
  heatmap <- ggplot(error_df, aes(x = Actual, y = Predicted, fill = Proportion)) +
    geom_tile() +
    geom_text(aes(label = Percentage), size = 3) +
    scale_fill_gradient(low = "white", high = "red", labels = scales::percent) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA)
    ) +
    labs(
      title = title,
      subtitle = "Quand le modèle se trompe, vers quel parti redirige-t-il la prédiction?",
      x = "Parti réel (actual)",
      y = "Parti prédit (predicted)",
      fill = "Proportion"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(list(matrix = error_matrix, pct_matrix = error_matrix_pct, plot = heatmap))
}

# Créer les matrices de redirection des erreurs pour les deux modèles
error_redirect_original <- create_error_redirection_matrix(
  results_original, 
  "Redirection des erreurs - Modèle original"
)

error_redirect_improved <- create_error_redirection_matrix(
  results_improved, 
  "Redirection des erreurs - Modèle amélioré (global)"
)

# Créer les matrices de redirection des erreurs par source pour le modèle amélioré
error_redirect_improved_pilot <- create_error_redirection_matrix(
  results_improved %>% filter(source == "pilote"), 
  "Redirection des erreurs - Modèle amélioré (pilote)"
)

error_redirect_improved_app <- create_error_redirection_matrix(
  results_improved %>% filter(source == "application"), 
  "Redirection des erreurs - Modèle amélioré (application)"
)

# Afficher les matrices
cat("\nMatrice de redirection des erreurs - Modèle original:\n")
print(error_redirect_original$pct_matrix)

cat("\nMatrice de redirection des erreurs - Modèle amélioré (global):\n")
print(error_redirect_improved$pct_matrix)

# Analyse plus spécifique des erreurs pour chaque parti
analyze_party_errors <- function(results_df, party_code) {
  # Filtrer les données pour ce parti
  party_data <- results_df %>%
    filter(actual == party_code)
  
  # Calculer les prédictions incorrectes
  incorrect <- party_data %>%
    filter(!correct_1st)
  
  # Distribution des prédictions incorrectes
  incorrect_dist <- table(incorrect$predicted_1st)
  incorrect_pct <- prop.table(incorrect_dist) * 100
  
  # Nombre total de cas pour ce parti
  total_cases <- nrow(party_data)
  incorrect_cases <- nrow(incorrect)
  accuracy <- 1 - (incorrect_cases / total_cases)
  
  # Créer un résumé
  cat(sprintf("\nAnalyse des erreurs pour le parti %s:", party_code))
  cat(sprintf("\nAccuracy: %.1f%% (%d corrects sur %d)", 
              accuracy * 100, total_cases - incorrect_cases, total_cases))
  cat(sprintf("\nLorsque le modèle se trompe (%d cas), il prédit:", incorrect_cases))
  
  for (party in names(incorrect_pct)) {
    cat(sprintf("\n  - %s: %.1f%%", party, incorrect_pct[party]))
  }
  
  return(list(
    accuracy = accuracy,
    incorrect_dist = incorrect_dist,
    incorrect_pct = incorrect_pct
  ))
}

# Analyser les erreurs pour chaque parti pour le modèle original
cat("\n\n===== ANALYSE DÉTAILLÉE DES ERREURS PAR PARTI - MODÈLE ORIGINAL =====")
parties <- levels(results_original$actual)
error_analyses_original <- list()
for (party in parties) {
  error_analyses_original[[party]] <- analyze_party_errors(results_original, party)
}

# Analyser les erreurs pour chaque parti pour le modèle amélioré
cat("\n\n===== ANALYSE DÉTAILLÉE DES ERREURS PAR PARTI - MODÈLE AMÉLIORÉ =====")
parties <- levels(results_improved$actual)
error_analyses_improved <- list()
for (party in parties) {
  error_analyses_improved[[party]] <- analyze_party_errors(results_improved, party)
}

# Créer un graphique comparatif des redirections d'erreurs pour un parti spécifique (exemple avec CPC)
create_error_comparison_plot <- function(party_code) {
  # Extraire les données pour le parti spécifié de chaque modèle
  original_errors <- error_analyses_original[[party_code]]$incorrect_pct
  improved_errors <- error_analyses_improved[[party_code]]$incorrect_pct
  
  # Combiner les données pour la comparaison
  all_parties <- unique(c(names(original_errors), names(improved_errors)))
  comparison_data <- data.frame(
    Party = all_parties,
    Original = 0,
    Improved = 0
  )
  
  # Remplir avec les pourcentages d'erreur
  for (party in all_parties) {
    if (party %in% names(original_errors)) {
      comparison_data[comparison_data$Party == party, "Original"] <- original_errors[party]
    }
    if (party %in% names(improved_errors)) {
      comparison_data[comparison_data$Party == party, "Improved"] <- improved_errors[party]
    }
  }
  
  # Convertir en format long pour ggplot
  comparison_long <- comparison_data %>%
    pivot_longer(cols = c("Original", "Improved"), 
                 names_to = "Model", 
                 values_to = "Percentage")
  
  # Créer le graphique avec fond blanc
  plot <- ggplot(comparison_long, aes(x = Party, y = Percentage, fill = Model)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
              position = position_dodge(width = 0.9), vjust = -0.5) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA)
    ) +
    labs(
      title = sprintf("Comparaison des redirections d'erreurs - Parti %s", party_code),
      subtitle = "Quand le modèle se trompe, vers quels partis redirige-t-il la prédiction?",
      x = "Parti prédit incorrectement",
      y = "Pourcentage des erreurs",
      fill = "Modèle"
    )
  
  return(plot)
}

# Créer des graphiques de comparaison pour tous les partis
error_comparison_plots <- list()
for (party in parties) {
  error_comparison_plots[[party]] <- create_error_comparison_plot(party)
}

# Sauvegarder les graphiques
for (party in parties) {
  ggsave(
    sprintf("_SharedFolder_datagotchi_federal_2024/data/modele/comparison/error_redirect_%s.png", party),
    error_comparison_plots[[party]],
    width = 10, height = 6
  )
}

# Sauvegarder les matrices de redirection d'erreurs
ggsave("_SharedFolder_datagotchi_federal_2024/data/modele/comparison/error_redirect_original.png", 
       error_redirect_original$plot, width = 10, height = 8)
ggsave("_SharedFolder_datagotchi_federal_2024/data/modele/comparison/error_redirect_improved.png", 
       error_redirect_improved$plot, width = 10, height = 8)
ggsave("_SharedFolder_datagotchi_federal_2024/data/modele/comparison/error_redirect_improved_pilot.png", 
       error_redirect_improved_pilot$plot, width = 10, height = 8)
ggsave("_SharedFolder_datagotchi_federal_2024/data/modele/comparison/error_redirect_improved_app.png", 
       error_redirect_improved_app$plot, width = 10, height = 8)

# Modifier également les graphiques existants dans le code original pour avoir un fond blanc
# Cela devra être ajouté après les définitions des graphiques dans la section 9

# Ajout d'un fond blanc aux graphiques existants
plot_accuracy <- plot_accuracy + 
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  )

plot_accuracy_either <- plot_accuracy_either + 
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  )

plot_by_party <- plot_by_party + 
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  )

plot_by_party_either <- plot_by_party_either + 
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  )

# Ajouter à la liste des résultats
saveRDS(
  list(
    error_redirect_original = error_redirect_original,
    error_redirect_improved = error_redirect_improved,
    error_redirect_improved_pilot = error_redirect_improved_pilot,
    error_redirect_improved_app = error_redirect_improved_app,
    error_analyses_original = error_analyses_original,
    error_analyses_improved = error_analyses_improved
  ),
  "_SharedFolder_datagotchi_federal_2024/data/modele/comparison/error_analyses.rds"
)

