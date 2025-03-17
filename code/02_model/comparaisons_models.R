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
library(quarto)

# ------------------------------------------------------------------------
# 2) Chargement des modèles et des données
# ------------------------------------------------------------------------
# Modèle original (sans RTA)
model_original <- readRDS("_SharedFolder_datagotchi_federal_2024/data/modele/finalmodel_withOutInteractions.rds")

# Modèle amélioré (avec RTA)
model_improved <- readRDS("_SharedFolder_datagotchi_federal_2024/data/modele/finalmodel_withRTAPredictions_2025-04-15.rds")
dummies_improved <- readRDS("_SharedFolder_datagotchi_federal_2024/data/modele/dummies_finalmodel_withRTAPredictions_2025-04-15.rds")

# Charger les résultats du modèle original pour obtenir les variables
results_train_original <- readRDS("_SharedFolder_datagotchi_federal_2024/data/modele/resultsTrainV4_31janvier2025.rds")

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
rta_predictions <- read.csv("_SharedFolder_datagotchi_federal_2024/data/modele/rta_predictions_partis.csv",
                           stringsAsFactors = FALSE)

# Charger les données
DataPilot <- readRDS("_SharedFolder_datagotchi_federal_2024/data/pilote/dataClean/datagotchi2025_canada_pilot_20250310.rds")
DataApp <- readRDS("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/datagotchi2025_canada_app_20250314.rds")

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

