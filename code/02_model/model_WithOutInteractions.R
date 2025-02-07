# ------------------------------------------------------------------------
# 1) Chargement des packages
# ------------------------------------------------------------------------
library(parallel)
library(nnet)
library(caret)
library(tidyverse)
library(yardstick)  # Pour calculer les métriques multiclasses
library(pbapply)    # Pour afficher une barre de progression (optionnel)
# ------------------------------------------------------------------------
# 2) Chargement et préparation des données
# ------------------------------------------------------------------------
DataModel <- readRDS("_SharedFolder_datagotchi_federal_2024/data/pilote/DataCleanPilot_2025Janv30.rds")

DataModel <- DataModel %>% 
  select(
    id, 
    ses_immigrant,
    ses_region,
    ses_age, ses_age_4Cat, ses_ageGroup5Years, 
    lifestyle_hasTattoos, lifestyle_numberTattoos, 
    ses_dwelling, ses_dwelling_cat, 
    ses_ethnicity, ses_ethnicityWB, ses_ethnicityWhite,
    ses_sexOrientation, ses_sexOrientationHetero, 
    ses_gender, ses_genderFemale, 
    lifestyle_clothingStyle, lifestyle_clothingStyleGroups,
    lifestyle_typeTransport, lifestyle_consClothes, lifestyle_exercise, 
    lifestyle_eatMeatFreq, lifestyle_favAlcool, lifestyle_consCoffee, 
    ses_language, lifestyle_smokeFreq, 
    lifestyle_ownPet, lifestyle_ownPet_bin,
    lifestyle_goHuntingFreq, lifestyle_goHuntingFreq_bin, lifestyle_goHuntingFreq_factor, lifestyle_goHuntingFreq_numeric,
    lifestyle_goFishingFreq, lifestyle_goFishingFreq_bin, lifestyle_goFishingFreq_factor, lifestyle_goFishingFreq_numeric, 
    lifestyle_goMuseumsFreq, lifestyle_goMuseumsFreq_bin, lifestyle_goMuseumsFreq_factor, lifestyle_goMuseumsFreq_numeric,
    lifestyle_volunteeringFreq, lifestyle_volunteeringFreq_bin, lifestyle_volunteeringFreq_factor, lifestyle_volunteeringFreq_numeric,
    ses_educ, ses_educ_3Cat, ses_educ_5Cat, 
    ses_income, ses_income3Cat, ses_incomeCensus,
    lifestyle_motorizedActFreq, lifestyle_motorizedActFreq_bin, lifestyle_motorizedActFreq_factor, lifestyle_motorizedActFreq_numeric,
    dv_voteChoice
  ) %>%
  drop_na() %>%
  filter(dv_voteChoice != "other")

# Convert all ordered factors to unordered factors
DataModel <- DataModel %>%
  mutate(across(where(is.ordered), ~ factor(., ordered = FALSE)))

# Conversion de quelques variables en facteur
DataModel$dv_voteChoice <- factor(DataModel$dv_voteChoice)

# Appliquer un contraste somme à dv_voteChoice afin d'obtenir un coefficient pour chaque niveau
contrasts(DataModel$dv_voteChoice) <- contr.sum(nlevels(DataModel$dv_voteChoice))

DataModel$lifestyle_consCoffee <- factor(DataModel$lifestyle_consCoffee, ordered = FALSE)
DataModel$lifestyle_consClothes <- factor(DataModel$lifestyle_consClothes, ordered = FALSE)

# ------------------------------------------------------------------------
# 3) Séparation Train/Test
# ------------------------------------------------------------------------
set.seed(42)
trainIndex <- createDataPartition(DataModel$dv_voteChoice, p = 0.8, list = FALSE)
DfTrain    <- DataModel[trainIndex, ]
DfTest     <- DataModel[-trainIndex, ]

# ------------------------------------------------------------------------
# 4) Définition des différentes codifications possibles
# ------------------------------------------------------------------------
variable_options <- list(
  age = c("ses_age", "ses_age_4Cat", "ses_ageGroup5Years"),
  dwelling = c("ses_dwelling", "ses_dwelling_cat"),
  ethnicity = c("ses_ethnicity", "ses_ethnicityWB", "ses_ethnicityWhite"),
  sexOrientation = c("ses_sexOrientation", "ses_sexOrientationHetero"),
  gender = c("ses_gender", "ses_genderFemale"),
  clothingStyle = c("lifestyle_clothingStyle", "lifestyle_clothingStyleGroups"),
  goHuntingFreq = c("lifestyle_goHuntingFreq", "lifestyle_goHuntingFreq_bin", 
                    "lifestyle_goHuntingFreq_factor", "lifestyle_goHuntingFreq_numeric"),
  goFishingFreq = c("lifestyle_goFishingFreq", "lifestyle_goFishingFreq_bin", 
                    "lifestyle_goFishingFreq_factor", "lifestyle_goFishingFreq_numeric"),
  goMuseumsFreq = c("lifestyle_goMuseumsFreq", "lifestyle_goMuseumsFreq_bin", 
                    "lifestyle_goMuseumsFreq_factor", "lifestyle_goMuseumsFreq_numeric"),
  volunteeringFreq = c("lifestyle_volunteeringFreq", "lifestyle_volunteeringFreq_bin", 
                       "lifestyle_volunteeringFreq_factor", "lifestyle_volunteeringFreq_numeric"),
  motorizedActFreq = c("lifestyle_motorizedActFreq", "lifestyle_motorizedActFreq_bin", 
                       "lifestyle_motorizedActFreq_factor", "lifestyle_motorizedActFreq_numeric"),
  tattoo = c("lifestyle_hasTattoos", "lifestyle_numberTattoos"),
  educ = c("ses_educ", "ses_educ_3Cat", "ses_educ_5Cat"),
  income = c("ses_income", "ses_income3Cat", "ses_incomeCensus"),
  ownPet = c("lifestyle_ownPet", "lifestyle_ownPet_bin")
)

# Variables fixes utilisées dans tous les modèles
other_variables <- c(
  "ses_region",
  "ses_immigrant",
  "lifestyle_typeTransport",
  "lifestyle_consClothes",
  "lifestyle_exercise",
  "lifestyle_eatMeatFreq",
  "lifestyle_favAlcool",
  "lifestyle_consCoffee",
  "ses_language",
  "lifestyle_smokeFreq"
)

# ------------------------------------------------------------------------
# 5) Fonction de résumé multiclasses (Accuracy, Kappa, LogLoss, F1-score)
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
  penalty_value <- 5  # Coefficient de pénalité (à ajuster)
  if ("npd" %in% lev && "cpc" %in% lev) {
    if ("cpc" %in% rownames(conf) && "npd" %in% colnames(conf)) {
      penalty <- penalty + penalty_value * conf["cpc", "npd"]
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
# 6) Fonction d'une itération de modèle
# ------------------------------------------------------------------------
one_iteration <- function(model_id, DfTrain, var_options, other_vars) {
  selected_vars <- other_vars
  var_codings_used <- list()
  
  for (var_group in names(var_options)) {
    codings <- var_options[[var_group]]
    chosen_coding <- sample(codings, 1)
    selected_vars <- c(selected_vars, chosen_coding)
    var_codings_used[[chosen_coding]] <- var_group
  }
  
  X_train <- DfTrain[, selected_vars, drop = FALSE]
  y_train <- DfTrain$dv_voteChoice
  
  # Utiliser sep = "_" pour avoir un nom de colonne au format questionkey_choicekey
  dummies <- dummyVars(" ~ .", data = X_train, fullRank = TRUE, sep = "_")
  X_train_dummy <- predict(dummies, newdata = X_train) %>% as.data.frame()
  
  df_for_caret <- cbind(y_train, X_train_dummy)
  colnames(df_for_caret)[1] <- "dv_voteChoice"
  
  train_control <- trainControl(
    method = "cv",
    number = 5,
    summaryFunction = multiClassSummary2,
    classProbs = TRUE,
    savePredictions = "final"
  )
  
  cv_model <- train(
    dv_voteChoice ~ .,
    data = df_for_caret,
    method = "multinom",
    trControl = train_control,
    metric = "composite_score",
    MaxNWts = 100000,
    trace = FALSE
  )
  
  results_cv <- cv_model$results
  accuracy_cv <- results_cv$accuracy[1]
  kappa_cv    <- results_cv$kappa[1]
  logloss_cv  <- results_cv$logLoss[1]
  f1_cv       <- results_cv$f1[1]
  
  iteration_results <- data.frame(
    model_id   = model_id,
    variable   = selected_vars,
    coding     = sapply(selected_vars, function(x) {
                    if (x %in% names(var_codings_used)) {
                      return(var_codings_used[[x]])
                    } else {
                      return("fixed")
                    }
                  }),
    accuracy_cv = accuracy_cv,
    kappa_cv    = kappa_cv,
    logloss_cv  = logloss_cv,
    f1_cv       = f1_cv,
    stringsAsFactors = FALSE
  )
  
  return(iteration_results)
}

# ------------------------------------------------------------------------
# 7) Boucle sur M itérations (PARALLELIZED)
# ------------------------------------------------------------------------
M <- 10000
set.seed(2023)

# Set up parallel cluster
cl <- makeCluster(7)

# Load required packages on each worker
clusterEvalQ(cl, {
  library(nnet)
  library(caret)
  library(tidyverse)
  library(yardstick)
  library(pbapply)
})

# Export necessary objects to workers
clusterExport(cl, c('DfTrain', 'variable_options', 'other_variables',
                    'multiClassSummary2', 'one_iteration'))

# Ensure reproducibility in parallel
clusterSetRNGStream(cl, 2023)

# Run iterations in parallel with progress bar
all_iterations <- pblapply(seq_len(M), function(i) {
  one_iteration(
    model_id    = i,
    DfTrain     = DfTrain,
    var_options = variable_options,
    other_vars  = other_variables
  )
}, cl = cl)

# Stop the cluster
stopCluster(cl)

results_train <- bind_rows(all_iterations)
saveRDS(results_train, "_SharedFolder_datagotchi_federal_2024/data/modele/resultsTrainV4_31janvier2025.rds")
results_train <- readRDS("_SharedFolder_datagotchi_federal_2024/data/modele/resultsTrainV4_31janvier2025.rds")

# ------------------------------------------------------------------------
# 8) Synthèse des résultats sur la CV du train
# ------------------------------------------------------------------------
summary_train <- results_train %>%
  group_by(variable, coding) %>%
  summarise(
    mean_accuracy = mean(accuracy_cv),
    sd_accuracy   = sd(accuracy_cv),
    mean_kappa    = mean(kappa_cv),
    sd_kappa      = sd(kappa_cv),
    mean_logloss  = mean(logloss_cv),
    sd_logloss    = sd(logloss_cv),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_accuracy))
print(summary_train)

# ------------------------------------------------------------------------
# 9) Identification du meilleur modèle (selon accuracy)
# ------------------------------------------------------------------------
best_iterations <- results_train %>%
  group_by(model_id) %>%
  summarise(
    score_iter_accuracy = first(accuracy_cv),
    score_iter_kappa    = first(kappa_cv),
    score_iter_logloss  = first(logloss_cv),
    .groups = "drop"
  ) %>%
  arrange(desc(score_iter_accuracy))

best_id <- best_iterations$model_id[1]
cat("Meilleur modèle trouvé (ID) =", best_id, "\n")

best_config <- results_train %>%
  filter(model_id == best_id)

# ------------------------------------------------------------------------
# 10) Construction du modèle final sur TOUT le training set
# ------------------------------------------------------------------------
final_vars <- best_config$variable
X_train_final <- DfTrain[, final_vars, drop = FALSE]
y_train_final <- DfTrain$dv_voteChoice

# Utiliser sep = "_" ici également
dummies_final <- dummyVars(" ~ .", data = X_train_final, fullRank = TRUE, sep = "_")
X_train_dummy <- predict(dummies_final, newdata = X_train_final) %>% as.data.frame()

final_model <- multinom(
  y_train_final ~ ., 
  data = X_train_dummy, 
  trace = FALSE,
  MaxNWts = 100000
)
print(final_model)

coef_names <- final_model$coefnames
# NULL
print(coef_names)
# --- Post-traitement Option 1 : recalcul des coefficients symétriques ---

# Extraire la matrice des coefficients du modèle final
orig_coef <- coef(final_model)
# Récupérer tous les niveaux de la variable réponse
all_levels <- levels(y_train_final)

# Créer une matrice complète pour les coefficients
full_coef <- matrix(0, nrow = length(all_levels), ncol = ncol(orig_coef))
rownames(full_coef) <- all_levels
colnames(full_coef) <- colnames(orig_coef)

# Remplir full_coef pour les niveaux non de référence
for (lvl in rownames(orig_coef)) {
  full_coef[lvl, ] <- orig_coef[lvl, ]
}

cat("Matrice complète des coefficients (avant symétrisation) :\n")
print(full_coef)

# Pour chaque prédicteur, calculer la moyenne des coefficients sur tous les niveaux
m <- colMeans(full_coef)

# Reparamétrer de manière symétrique : 
# Pour chaque coefficient, soustraire la moyenne (pour que la somme sur tous les niveaux soit nulle)
sym_coef <- full_coef - matrix(rep(m, each = length(all_levels)), nrow = length(all_levels))

cat("Matrice des coefficients symétriques (somme nulle pour chaque prédicteur) :\n")
print(sym_coef)

# Ajouter la matrice symétrique au modèle final pour l'enregistrement
final_model$sym_coef <- sym_coef

# ------------------------------------------------------------------------
# 11) Évaluation sur le jeu de test
# ------------------------------------------------------------------------
X_test_final <- DfTest[, final_vars, drop = FALSE]
y_test_final <- DfTest$dv_voteChoice

X_test_dummy <- predict(dummies_final, newdata = X_test_final) %>% as.data.frame()

pred_test_final_class <- predict(final_model, newdata = X_test_dummy)
acc_test_final <- mean(pred_test_final_class == y_test_final)
cat("Accuracy (test) :", acc_test_final, "\n")

pred_test_final_prob <- predict(final_model, newdata = X_test_dummy, type = "probs")

levelz <- levels(y_test_final)
logloss_test <- 0
n_test <- length(y_test_final)
for (i in seq_len(n_test)) {
  obs_class <- y_test_final[i]
  class_idx <- which(levelz == obs_class)
  p <- pred_test_final_prob[i, class_idx]
  p <- max(p, 1e-15)
  logloss_test <- logloss_test - log(p)
}
logloss_test <- logloss_test / n_test
cat("LogLoss (test) :", logloss_test, "\n")

table_test <- table(
  predicted = pred_test_final_class,
  actual = y_test_final
)
print(table_test)
print(final_model)
print(final_model$sym_coef)

# ------------------------------------------------------------------------
# 12) Sauvegarde du modèle final
# ------------------------------------------------------------------------
saveRDS(final_model, "_SharedFolder_datagotchi_federal_2024/data/modele/finalmodel_withOutInteractions.rds")
cat("Modèle sauvegardé avec succès.\n")
