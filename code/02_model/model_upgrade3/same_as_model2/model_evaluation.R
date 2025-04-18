#' Évaluation de la robustesse du modèle RTA amélioré
#' 
#' Ce script évalue la robustesse du modèle multinomial avec prédictions RTA
#' en analysant différentes métriques: accuracy, recall, stabilité,
#' accuracy pour les 1er et 2ème choix, métriques par parti et par région.
#'
#' Dépend de:
#' - Modèle final avec prédictions RTA (finalmodel_withRTAPredictions_2025-04-15.rds)
#' - Variables dummy (dummies_finalmodel_withRTAPredictions_2025-04-15.rds)
#' - Données pilote et application nettoyées
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
library(ggplot2)
library(patchwork)
library(forcats)
library(pROC)

# ------------------------------------------------------------------------
# 2) Chargement des données et du modèle
# ------------------------------------------------------------------------
# Données pilote
DataPilot <- readRDS("_SharedFolder_datagotchi_federal_2024/data/pilote/dataClean/datagotchi2025_canada_pilot_20250322.rds")

# Données de l'application
DataApp <- readRDS("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/20250415_n81812datagotchi2025_canada_app.rds")

# Chargement des prédictions par RTA
rta_predictions <- read.csv("_SharedFolder_datagotchi_federal_2024/data/modele/rta_predictions_partis.csv",
                          stringsAsFactors = FALSE)

# Chargement des résultats d'entraînement précédents pour obtenir les vraies variables du modèle original
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

# Obtenir les vraies variables originales du modèle
original_model_variables <- unique(best_config$variable)
cat("Variables du modèle précédent:", length(original_model_variables), "\n")
cat("Liste des premières variables:", paste(head(original_model_variables, 10), collapse=", "), "...\n")

# Chargement du modèle actuel et des variables dummy
model_final <- readRDS("_SharedFolder_datagotchi_federal_2024/data/modele/finalmodel_withRTAPredictions_2025-04-15.rds")
dummies_final <- readRDS("_SharedFolder_datagotchi_federal_2024/data/modele/dummies_finalmodel_withRTAPredictions_2025-04-15.rds")

# ------------------------------------------------------------------------
# 3) Préparation des données similaire au script principal
# ------------------------------------------------------------------------
# Standardisation des facteurs
all_levels <- unique(c(
  unique(as.character(DataPilot$dv_voteChoice[DataPilot$dv_voteChoice != "other"])),
  unique(as.character(DataApp$dv_voteChoice))
))
all_levels <- unique(gsub("npd", "ndp", all_levels))

# Convertir en facteurs avec les mêmes niveaux
DataPilot$dv_voteChoice <- factor(
  gsub("npd", "ndp", as.character(DataPilot$dv_voteChoice)),
  levels = all_levels
)
DataApp$dv_voteChoice <- factor(
  gsub("npd", "ndp", as.character(DataApp$dv_voteChoice)),
  levels = all_levels
)

# Ajouter la source
DataPilot$source <- "pilote"
DataApp$source <- "application"

# Fonction pour harmoniser les autres facteurs
harmonize_factor <- function(x_pilot, x_app) {
  x_pilot_char <- as.character(x_pilot)
  x_app_char <- as.character(x_app)
  all_levels <- unique(c(x_pilot_char, x_app_char))
  x_pilot_new <- factor(x_pilot_char, levels = all_levels)
  x_app_new <- factor(x_app_char, levels = all_levels)
  return(list(pilot = x_pilot_new, app = x_app_new))
}

# Identifier les facteurs du modèle
pilot_factors <- names(DataPilot)[sapply(DataPilot, is.factor)]
model_factor_vars <- names(DataPilot)[names(DataPilot) %in% original_model_variables]
model_factors <- intersect(model_factor_vars, pilot_factors)

# Harmoniser les facteurs
for (f in model_factors) {
  if (f %in% names(DataPilot) && f %in% names(DataApp)) {
    harmonized <- harmonize_factor(DataPilot[[f]], DataApp[[f]])
    DataPilot[[f]] <- harmonized$pilot
    DataApp[[f]] <- harmonized$app
  }
}

# On n'a pas besoin de redéfinir final_vars ici
# final_vars sera défini après la création de DataModel

# Filtrer et sélectionner les données
# Assurons-nous de sélectionner toutes les variables qui pourraient être utiles
keep_vars <- c(original_model_variables, "dv_voteChoice", "source", "ses_postalCode", "ses_region")

DataPilot_selected <- DataPilot %>%
  filter(dv_voteChoice != "other" & !is.na(dv_voteChoice)) %>%
  select(any_of(keep_vars)) %>%
  drop_na()

DataApp_selected <- DataApp %>%
  filter(!is.na(dv_voteChoice)) %>%
  select(any_of(keep_vars)) %>%
  drop_na()

# Extraction des RTA
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

# CORRECTION: Résoudre le problème de type pour ses_immigrant avant d'aller plus loin
# Vérifier si la variable existe dans les deux jeux de données
if("ses_immigrant" %in% names(DataPilot_selected) && "ses_immigrant" %in% names(DataApp_selected)) {
  # Afficher les types actuels
  cat("Type de ses_immigrant dans DataPilot_selected:", class(DataPilot_selected$ses_immigrant)[1], "\n")
  cat("Type de ses_immigrant dans DataApp_selected:", class(DataApp_selected$ses_immigrant)[1], "\n")
  
  # Convertir en character dans les deux jeux de données
  DataPilot_selected$ses_immigrant <- as.character(DataPilot_selected$ses_immigrant)
  DataApp_selected$ses_immigrant <- as.character(DataApp_selected$ses_immigrant)
  
  # Vérifier après conversion
  cat("Après conversion - Type de ses_immigrant dans DataPilot_selected:", class(DataPilot_selected$ses_immigrant)[1], "\n")
  cat("Après conversion - Type de ses_immigrant dans DataApp_selected:", class(DataApp_selected$ses_immigrant)[1], "\n")
}

# Standardiser les RTA dans le fichier de prédictions
rta_predictions$rta <- toupper(rta_predictions$rta)

# Fonction pour enrichir avec les prédictions RTA
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

# Enrichir les données
DataPilot_enriched <- enrich_with_predictions(DataPilot_selected, rta_predictions)
DataApp_enriched <- enrich_with_predictions(DataApp_selected, rta_predictions)

# Fusionner les données
DataModel <- bind_rows(DataPilot_enriched, DataApp_enriched)

# ------------------------------------------------------------------------
# 4) Séparation pour évaluation
# ------------------------------------------------------------------------
set.seed(123)
# Stratifier par parti et par source
trainIndex <- createDataPartition(
  interaction(DataModel$dv_voteChoice, DataModel$source), 
  p = 0.8, 
  list = FALSE
)
DfTrain <- DataModel[trainIndex, ]
DfTest <- DataModel[-trainIndex, ]

# Pour obtenir une évaluation complète et précise, nous allons reproduire
# l'approche du script original model_upgrade2_2025_03_15.R

# Vérifier quelles variables originales sont disponibles dans nos données actuelles
# et créer un sous-ensemble des variables originales qui sont disponibles
cat("Vérification de la disponibilité des variables originales...\n")
available_original_vars <- intersect(original_model_variables, names(DfTrain))
cat("Variables originales disponibles:", length(available_original_vars), "sur", length(original_model_variables), "\n")
cat("Liste des 10 premières variables disponibles:", paste(head(available_original_vars, 10), collapse=", "), "...\n")

# Ajouter les variables de prédiction RTA aux variables disponibles
final_vars <- c(available_original_vars, "prediction_CPC", "prediction_LPC", "prediction_NDP", "prediction_GPC", "prediction_BQ")
cat("Nombre total de variables utilisées:", length(final_vars), "\n")

# Préparation des données pour le modèle
X_train_final <- DfTrain[, final_vars, drop = FALSE]
y_train_final <- DfTrain$dv_voteChoice

# Fixer la classe de référence pour la variable dépendante dv_voteChoice
if ("bq" %in% levels(y_train_final)) {
  y_train_final <- relevel(y_train_final, ref = "bq")
}

# Fixer les références pour les variables catégorielles
for (var_name in names(X_train_final)) {
  if (!is.factor(X_train_final[[var_name]])) next
  
  if (var_name == "ses_region" && "prairie" %in% levels(X_train_final[[var_name]])) {
    X_train_final[[var_name]] <- relevel(X_train_final[[var_name]], ref = "prairie")
  }
  else if (var_name == "lifestyle_typeTransport" && "active_transport" %in% levels(X_train_final[[var_name]])) {
    X_train_final[[var_name]] <- relevel(X_train_final[[var_name]], ref = "active_transport")
  }
  else if (var_name == "lifestyle_consClothes" && "large_retailers" %in% levels(X_train_final[[var_name]])) {
    X_train_final[[var_name]] <- relevel(X_train_final[[var_name]], ref = "large_retailers")
  }
  else if (var_name == "lifestyle_exercise" && "gym" %in% levels(X_train_final[[var_name]])) {
    X_train_final[[var_name]] <- relevel(X_train_final[[var_name]], ref = "gym")
  }
  else if (var_name == "lifestyle_favAlcool" && "beer" %in% levels(X_train_final[[var_name]])) {
    X_train_final[[var_name]] <- relevel(X_train_final[[var_name]], ref = "beer")
  }
  else if (var_name == "lifestyle_consCoffee" && "tim_hortons" %in% levels(X_train_final[[var_name]])) {
    X_train_final[[var_name]] <- relevel(X_train_final[[var_name]], ref = "tim_hortons")
  }
  else if (var_name == "ses_language" && "english" %in% levels(X_train_final[[var_name]])) {
    X_train_final[[var_name]] <- relevel(X_train_final[[var_name]], ref = "english")
  }
  else if (var_name == "ses_dwelling_cat" && "stand_alone_house" %in% levels(X_train_final[[var_name]])) {
    X_train_final[[var_name]] <- relevel(X_train_final[[var_name]], ref = "stand_alone_house")
  }
  else if (var_name == "lifestyle_clothingStyleGroups" && "easygoing" %in% levels(X_train_final[[var_name]])) {
    X_train_final[[var_name]] <- relevel(X_train_final[[var_name]], ref = "easygoing")
  }
  else if (var_name == "ses_educ" && "no_schooling" %in% levels(X_train_final[[var_name]])) {
    X_train_final[[var_name]] <- relevel(X_train_final[[var_name]], ref = "no_schooling")
  }
  else if (var_name == "ses_income3Cat" && "High" %in% levels(X_train_final[[var_name]])) {
    X_train_final[[var_name]] <- relevel(X_train_final[[var_name]], ref = "High")
  }
}

# Création des variables dummy
dummies_new <- dummyVars(" ~ .", data = X_train_final, fullRank = TRUE, sep = "_")
X_train_dummy <- predict(dummies_new, newdata = X_train_final) %>% as.data.frame()

# Entraînement du modèle final
final_model_new <- multinom(
  y_train_final ~ ., 
  data = X_train_dummy, 
  trace = FALSE,
  MaxNWts = 100000
)

# Préparation des données de test
X_test_final <- DfTest[, final_vars, drop = FALSE]
y_test_final <- DfTest$dv_voteChoice

# Prédictions avec le modèle
X_test_dummy <- predict(dummies_new, newdata = X_test_final) %>% as.data.frame()
predictions <- predict(final_model_new, newdata = X_test_dummy)
probs <- predict(final_model_new, newdata = X_test_dummy, type = "probs")

# Ajout des prédictions aux données de test
DfTest <- DfTest %>%
  mutate(
    predicted_class = predictions
  )

# Ajouter les probabilités pour chaque classe
DfTest <- cbind(DfTest, probs)

# ------------------------------------------------------------------------
# 5) Métriques globales
# ------------------------------------------------------------------------
# Nous n'utiliserons pas cette fonction car elle cause des problèmes de compatibilité

# Calculer les métriques globales
# Calculons d'abord l'accuracy
accuracy_val <- mean(DfTest$predicted_class == DfTest$dv_voteChoice)

# Pour le top-2 accuracy, utilisons une approche plus simple
top2_acc <- numeric(nrow(DfTest))
for (i in 1:nrow(DfTest)) {
  # Obtenir les 2 classes avec les probabilités les plus élevées pour cette observation
  prob_row <- as.numeric(probs[i,])
  top2_indices <- order(prob_row, decreasing = TRUE)[1:2]
  top2_classes <- colnames(probs)[top2_indices]
  # Vérifier si la vraie classe est dans le top 2
  top2_acc[i] <- as.character(DfTest$dv_voteChoice[i]) %in% top2_classes
}
top2_accuracy_val <- mean(top2_acc)

# Pour le balanced accuracy, calculons-le manuellement pour chaque classe
classes <- levels(DfTest$dv_voteChoice)
sensitivities <- numeric(length(classes))

for (i in 1:length(classes)) {
  class_name <- classes[i]
  # Calculer la sensibilité pour cette classe
  true_positives <- sum(DfTest$dv_voteChoice == class_name & DfTest$predicted_class == class_name)
  class_total <- sum(DfTest$dv_voteChoice == class_name)
  
  # Éviter la division par zéro
  if (class_total > 0) {
    sensitivities[i] <- true_positives / class_total
  } else {
    sensitivities[i] <- 0
  }
}

# La balanced accuracy est la moyenne des sensibilités par classe
bal_acc_val <- mean(sensitivities)

global_metrics <- tibble(
  accuracy = accuracy_val,
  balanced_accuracy = bal_acc_val,
  top2_accuracy = top2_accuracy_val
)

# Print des résultats globaux
cat("MÉTRIQUES GLOBALES:\n")
cat("Accuracy:", global_metrics$accuracy, "\n")
cat("Balanced Accuracy:", global_metrics$balanced_accuracy, "\n")
cat("Top-2 Accuracy:", global_metrics$top2_accuracy, "\n")

# ------------------------------------------------------------------------
# 6) Métriques par source de données
# ------------------------------------------------------------------------
# Calculer les métriques par source
source_metrics <- DfTest %>%
  group_by(source) %>%
  summarise(
    n = n(),
    accuracy = mean(predicted_class == dv_voteChoice),
    .groups = "drop"
  )

# Print des résultats par source
cat("\nMÉTRIQUES PAR SOURCE:\n")
print(source_metrics)

# ------------------------------------------------------------------------
# 7) Métriques par parti
# ------------------------------------------------------------------------
# Calculer les métriques par parti
party_metrics <- DfTest %>%
  group_by(dv_voteChoice) %>%
  summarise(
    n = n(),
    accuracy = mean(predicted_class == dv_voteChoice),
    precision = sum(predicted_class == dv_voteChoice) / sum(predicted_class == first(dv_voteChoice)),
    recall = sum(predicted_class == dv_voteChoice) / n(),
    f1_score = ifelse(precision + recall > 0, 2 * (precision * recall) / (precision + recall), 0),
    .groups = "drop"
  )

# Pour les partis avec très peu d'exemples, certaines métriques peuvent être NA ou Inf.
# Nous allons les remplacer par 0.
party_metrics <- party_metrics %>%
  mutate(
    precision = replace_na(precision, 0),
    recall = replace_na(recall, 0),
    f1_score = replace_na(f1_score, 0),
    f1_score = ifelse(is.infinite(f1_score), 0, f1_score)
  )

# Print des résultats par parti
cat("\nMÉTRIQUES PAR PARTI:\n")
print(party_metrics)

# ------------------------------------------------------------------------
# 8) Métriques par région
# ------------------------------------------------------------------------
# Calculer les métriques par région
region_metrics <- DfTest %>%
  group_by(ses_region) %>%
  summarise(
    n = n(),
    accuracy = mean(predicted_class == dv_voteChoice),
    .groups = "drop"
  )

# Print des résultats par région
cat("\nMÉTRIQUES PAR RÉGION:\n")
print(region_metrics)

# ------------------------------------------------------------------------
# 9) Matrice de confusion
# ------------------------------------------------------------------------
conf_matrix <- confusionMatrix(factor(DfTest$predicted_class, levels=levels(DfTest$dv_voteChoice)), 
                              DfTest$dv_voteChoice)
cat("\nMATRICE DE CONFUSION:\n")
print(conf_matrix$table)
cat("\nSTATISTIQUES DE LA MATRICE DE CONFUSION:\n")
print(conf_matrix$overall)

# ------------------------------------------------------------------------
# 10) Stabilité du modèle avec bootstrap
# ------------------------------------------------------------------------
# Fonction pour calculer l'accuracy sur un échantillon bootstrap
calc_bootstrap_accuracy <- function(indices) {
  # Créer un échantillon bootstrap
  boot_sample <- DfTest[indices, ]
  
  # Calculer l'accuracy
  accuracy <- mean(boot_sample$predicted_class == boot_sample$dv_voteChoice)
  
  return(accuracy)
}

# Nombre d'itérations bootstrap
n_boot <- 500

# Réaliser le bootstrap
set.seed(456)
boot_indices <- replicate(n_boot, sample(1:nrow(DfTest), replace = TRUE), simplify = FALSE)
boot_results <- lapply(boot_indices, calc_bootstrap_accuracy)
boot_results <- unlist(boot_results)

# Calculer l'intervalle de confiance à 95%
boot_ci <- quantile(boot_results, c(0.025, 0.975))

# Print des résultats du bootstrap
cat("\nSTABILITÉ DU MODÈLE (BOOTSTRAP):\n")
cat("Accuracy moyenne:", mean(boot_results), "\n")
cat("Intervalle de confiance à 95%:", boot_ci[1], "à", boot_ci[2], "\n")
cat("Écart-type:", sd(boot_results), "\n")

# ------------------------------------------------------------------------
# 11) Analyse des erreurs de prédiction
# ------------------------------------------------------------------------
# Ajouter une colonne pour indiquer si la prédiction est correcte
DfTest <- DfTest %>%
  mutate(
    is_correct = predicted_class == dv_voteChoice,
    error_type = case_when(
      is_correct ~ "correct",
      dv_voteChoice == "cpc" & predicted_class == "lpc" ~ "cpc_to_lpc",
      dv_voteChoice == "lpc" & predicted_class == "cpc" ~ "lpc_to_cpc",
      dv_voteChoice == "ndp" & predicted_class == "lpc" ~ "ndp_to_lpc",
      dv_voteChoice == "lpc" & predicted_class == "ndp" ~ "lpc_to_ndp",
      dv_voteChoice == "bq" & predicted_class != "bq" ~ paste0("bq_to_", predicted_class),
      dv_voteChoice == "gpc" & predicted_class != "gpc" ~ paste0("gpc_to_", predicted_class),
      TRUE ~ paste0(dv_voteChoice, "_to_", predicted_class)
    )
  )

# Calculer les types d'erreurs les plus fréquents
error_summary <- DfTest %>%
  filter(!is_correct) %>%
  count(error_type, sort = TRUE) %>%
  mutate(percentage = n / sum(n) * 100)

# Print des résultats d'analyse d'erreurs
cat("\nANALYSE DES ERREURS DE PRÉDICTION:\n")
cat("Types d'erreurs les plus fréquents:\n")
print(head(error_summary, 10))

# ------------------------------------------------------------------------
# 12) Visualisation des résultats
# ------------------------------------------------------------------------
# 1. Graphique de l'accuracy par parti
p1 <- party_metrics %>%
  mutate(dv_voteChoice = fct_reorder(dv_voteChoice, accuracy)) %>%
  ggplot(aes(x = dv_voteChoice, y = accuracy, fill = dv_voteChoice)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.3f", accuracy)), vjust = -0.5) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Accuracy par parti", x = "Parti", y = "Accuracy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. Graphique de l'accuracy par région
p2 <- region_metrics %>%
  mutate(ses_region = fct_reorder(ses_region, accuracy)) %>%
  ggplot(aes(x = ses_region, y = accuracy, fill = ses_region)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.3f", accuracy)), vjust = -0.5) +
  labs(title = "Accuracy par région", x = "Région", y = "Accuracy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Distribution des probabilités prédites pour chaque parti
prob_data <- DfTest %>%
  select(dv_voteChoice, all_of(colnames(probs))) %>%
  pivot_longer(cols = all_of(colnames(probs)), 
               names_to = "predicted_party", 
               values_to = "probability")

p3 <- prob_data %>%
  ggplot(aes(x = probability, fill = dv_voteChoice == predicted_party)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  facet_wrap(~predicted_party, scales = "free_y") +
  labs(title = "Distribution des probabilités prédites", 
       x = "Probabilité", y = "Compte",
       fill = "Prédiction correcte") +
  theme_minimal()

# 4. Heatmap de la matrice de confusion
conf_data <- as.data.frame(conf_matrix$table)
colnames(conf_data) <- c("prediction", "actual", "count")

p4 <- conf_data %>%
  ggplot(aes(x = prediction, y = actual, fill = count)) +
  geom_tile() +
  geom_text(aes(label = count), color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Matrice de confusion", x = "Prédiction", y = "Réel") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Sauvegarder les graphiques
pdf("_SharedFolder_datagotchi_federal_2024/data/modele/model_evaluation_plots.pdf", width = 12, height = 10)
print((p1 | p2) / (p3 | p4))
dev.off()

# ------------------------------------------------------------------------
# 13) Évaluation de la calibration du modèle
# ------------------------------------------------------------------------
# Fonction pour calculer le Brier score
brier_score <- function(actual, predicted_probs) {
  # Convertir actual en matrice one-hot
  parties <- colnames(predicted_probs)
  actual_mat <- matrix(0, nrow = length(actual), ncol = length(parties))
  for (i in seq_along(actual)) {
    j <- which(parties == actual[i])
    if (length(j) > 0) actual_mat[i, j] <- 1
  }
  
  # Calculer le Brier score
  mean(rowSums((actual_mat - predicted_probs)^2))
}

# Calculer le Brier score
bs <- brier_score(as.character(DfTest$dv_voteChoice), as.matrix(probs))
cat("\nCALIBRATION DU MODÈLE:\n")
cat("Brier Score:", bs, "(plus proche de 0 est meilleur)\n")

# ------------------------------------------------------------------------
# 14) Test de robustesse avec données bootstrappées par région
# ------------------------------------------------------------------------
# Fonction pour calculer les métriques sur un échantillon par région
calc_region_metrics <- function(region) {
  region_data <- DfTest %>% filter(ses_region == region)
  
  # S'il y a trop peu de données pour cette région, retourner NA
  if (nrow(region_data) < 10) {
    return(tibble(
      region = region,
      n = nrow(region_data),
      accuracy = NA,
      balanced_accuracy = NA
    ))
  }
  
  # Calculer les métriques pour cette région
  accuracy <- mean(region_data$predicted_class == region_data$dv_voteChoice)
  
  # S'il y a au moins 2 classes différentes, calculer le balanced accuracy
  if (length(unique(region_data$dv_voteChoice)) >= 2) {
    # Calculer la sensibilité pour chaque classe manuellement
    classes <- levels(region_data$dv_voteChoice)
    region_sensitivities <- numeric(length(classes))
    
    for (j in 1:length(classes)) {
      class_name <- classes[j]
      # Calculer la sensibilité pour cette classe
      true_positives <- sum(region_data$dv_voteChoice == class_name & 
                            region_data$predicted_class == class_name)
      class_total <- sum(region_data$dv_voteChoice == class_name)
      
      # Éviter la division par zéro
      if (class_total > 0) {
        region_sensitivities[j] <- true_positives / class_total
      } else {
        region_sensitivities[j] <- 0
      }
    }
    
    # La balanced accuracy est la moyenne des sensibilités par classe
    balanced_acc <- mean(region_sensitivities)
  } else {
    balanced_acc <- NA
  }
  
  tibble(
    region = region,
    n = nrow(region_data),
    accuracy = accuracy,
    balanced_accuracy = balanced_acc
  )
}

# Obtenir la liste des régions
regions <- unique(DfTest$ses_region)

# Calculer les métriques pour chaque région
region_robust_metrics <- lapply(regions, calc_region_metrics) %>% bind_rows()

cat("\nROBUSTESSE PAR RÉGION:\n")
print(region_robust_metrics)

# ------------------------------------------------------------------------
# 15) Résumé des résultats
# ------------------------------------------------------------------------
cat("\nRÉSUMÉ DES RÉSULTATS:\n")
cat("Accuracy globale:", global_metrics$accuracy, "\n")
cat("Top-2 Accuracy:", global_metrics$top2_accuracy, "\n")
cat("Stabilité (intervalle de confiance à 95%):", boot_ci[1], "à", boot_ci[2], "\n")
cat("Meilleur parti prédit:", party_metrics$dv_voteChoice[which.max(party_metrics$accuracy)], 
    "avec accuracy =", max(party_metrics$accuracy), "\n")
cat("Pire parti prédit:", party_metrics$dv_voteChoice[which.min(party_metrics$accuracy)], 
    "avec accuracy =", min(party_metrics$accuracy), "\n")
cat("Meilleure région:", region_metrics$ses_region[which.max(region_metrics$accuracy)], 
    "avec accuracy =", max(region_metrics$accuracy), "\n")
cat("Pire région:", region_metrics$ses_region[which.min(region_metrics$accuracy)], 
    "avec accuracy =", min(region_metrics$accuracy), "\n")
cat("Type d'erreur le plus fréquent:", error_summary$error_type[1], 
    "(", error_summary$percentage[1], "%)\n")
cat("Calibration (Brier Score):", bs, "\n")

# Sauvegarder les résultats
write.csv(global_metrics, "_SharedFolder_datagotchi_federal_2024/data/modele/global_metrics.csv", row.names = FALSE)
write.csv(party_metrics, "_SharedFolder_datagotchi_federal_2024/data/modele/party_metrics.csv", row.names = FALSE)
write.csv(region_metrics, "_SharedFolder_datagotchi_federal_2024/data/modele/region_metrics.csv", row.names = FALSE)
write.csv(error_summary, "_SharedFolder_datagotchi_federal_2024/data/modele/error_summary.csv", row.names = FALSE)
write.csv(region_robust_metrics, "_SharedFolder_datagotchi_federal_2024/data/modele/region_robust_metrics.csv", row.names = FALSE)

cat("\nToutes les métriques ont été calculées et sauvegardées avec succès.\n")