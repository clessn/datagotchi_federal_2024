# Load packages ----------------------------------------------------------
library(nnet)
library(tidyverse)
library(caret)
library(dplyr)
library(tidyr)
library(ggplot2)

# On va utiliser la CV via caret::train, donc on a besoin d'un summaryFunction adapté
library(yardstick)  # pour calculer metrics multiclasses
library(pbapply)    # pour une barre de progression (optionnel)

# ------------------------------------------------------------------------
# 1) Chargement des données
# ------------------------------------------------------------------------
DataModel <- readRDS("_SharedFolder_datagotchi_federal_2024/data/pilote/DataCleanPilot_2025Janv30.rds")

# ------------------------------------------------------------------------
# 2) Sélection des variables
# ------------------------------------------------------------------------
DataModel <- DataModel |> 
  select(
    id, 
    ses_postalCode, 
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
    ses_income, ses_income_3Cat, ses_incomeCensus,
    lifestyle_motorizedActFreq, lifestyle_motorizedActFreq_bin, lifestyle_motorizedActFreq_factor, lifestyle_motorizedActFreq_numeric,
    dv_voteChoice
  ) %>%
  drop_na()

# La cible en facteur
DataModel$dv_voteChoice <- factor(DataModel$dv_voteChoice)

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
  income = c("ses_income", "ses_income_3Cat", "ses_incomeCensus"),
  ownPet = c("lifestyle_ownPet", "lifestyle_ownPet_bin")
)

# Variables qu'on prend toujours
other_variables <- c(
  "ses_postalCode",
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
# 5) Fonction de résumé multiclasses (Accuracy, Kappa, LogLoss)
# ------------------------------------------------------------------------
# Cette fonction sera appelée par caret pour chaque CV-fold.
multiClassSummary2 <- function(data, lev = NULL, model = NULL) {
  # 'data' : data.frame avec colonnes:
  #   - obs = vraie classe
  #   - pred = classe prédite (au label)
  #   - pXYZ = probabilité prédite pour chaque classe 'XYZ'
  # 'lev' : vecteur des classes
  # 'model' : info sur le modèle

  # On va calculer : accuracy, kappa, et logLoss
  # 1) Accuracy
  acc <- yardstick::accuracy_vec(data$obs, data$pred)
  
  # 2) Kappa
  kap <- yardstick::kap_vec(data$obs, data$pred)
  
  # 3) LogLoss
  eps <- 1e-15
  n   <- nrow(data)
  ll  <- 0
  # Pour calculer la logLoss, on regarde pour chaque observation
  # la prob prédites pour la classe réelle:
  for (i in seq_len(n)) {
    obs_class <- as.character(data$obs[i])
    prob_col  <- paste0("prob.", obs_class)  # caret nomme les proba "prob.ClassName"
    p         <- data[[prob_col]][i]
    p         <- max(p, eps)  # éviter log(0)
    ll        <- ll - log(p)
  }
  ll <- ll / n  # on normalise
  
  out <- c(accuracy = acc, kappa = kap, logLoss = ll)
  return(out)
}

# ------------------------------------------------------------------------
# 6) Fonction d'une itération de modèle
# ------------------------------------------------------------------------
one_iteration <- function(model_id, DfTrain, var_options, other_vars) {
  
  # a) Sélection aléatoire des codifications
  selected_vars <- other_vars
  var_codings_used <- list()  # pour stocker la correspondance variable -> groupe
  
  for (var_group in names(var_options)) {
    codings <- var_options[[var_group]]
    chosen_coding <- sample(codings, 1)   # on pioche 1 codage au hasard
    selected_vars <- c(selected_vars, chosen_coding)
    var_codings_used[[chosen_coding]] <- var_group
  }
  
  # b) Préparer X_train et y_train
  X_train <- DfTrain[, selected_vars, drop = FALSE]
  y_train <- DfTrain$dv_voteChoice
  
  # c) Transformer en dummies
  #    On crée un dummyVars puis on applique à X_train
  #    pour avoir un data.frame de variables numériques
  dummies <- dummyVars(" ~ .", data = X_train, fullRank = TRUE)
  X_train_dummy <- predict(dummies, newdata = X_train) %>%
    as.data.frame()
  
  # d) Préparer un data.frame complet pour caret::train (incluant la cible)
  df_for_caret <- cbind(y_train, X_train_dummy)
  colnames(df_for_caret)[1] <- "dv_voteChoice"  # on renomme la première colonne
  
  # e) Définir la validation croisée (k=5, par ex.) + summaryFunction
  train_control <- trainControl(
    method = "cv",
    number = 5,               # 5-fold CV
    summaryFunction = multiClassSummary2,
    classProbs = TRUE,        # indispensable pour logLoss
    savePredictions = "final" # pour qu'on puisse inspecter p.ex. df_for_caret
  )
  
  # f) Entraîner le modèle multinomial via caret
  #    (on force le paramètre MaxNWts pour éviter l'erreur si trop de variables)
  cv_model <- train(
    dv_voteChoice ~ .,
    data       = df_for_caret,
    method     = "multinom",
    trControl  = train_control,
    metric     = "accuracy",    # on cherche à maximiser l'accuracy
    # (on pourrait mettre "kappa" si on préfère, ou logLoss avec "maximize=FALSE")
    MaxNWts    = 100000,
    trace      = FALSE
  )
  
  # g) Récupérer la performance sur la CV
  #    model$results contient typically 1 ligne (pas de tuning),
  #    on y trouve Accuracy, Kappa, logLoss, etc.
  results_cv <- cv_model$results
  accuracy_cv <- results_cv$accuracy[1]
  kappa_cv    <- results_cv$kappa[1]
  logloss_cv  <- results_cv$logLoss[1]
  
  # Pour "sélectionner" un score, on peut en prendre un principal,
  # par exemple accuracy_cv
  # (ou on garde tout si on veut plus tard analyser)
  
  # h) construire le tableau de résultats
  iteration_results <- data.frame(
    model_id  = model_id,
    variable  = selected_vars,
    coding    = sapply(selected_vars, function(x) {
      if (x %in% names(var_codings_used)) {
        return(var_codings_used[[x]])
      } else {
        return("fixed")
      }
    }),
    accuracy_cv = accuracy_cv,
    kappa_cv    = kappa_cv,
    logloss_cv  = logloss_cv,
    stringsAsFactors = FALSE
  )
  
  return(iteration_results)
}

# ------------------------------------------------------------------------
# 7) Boucle sur M itérations (avec barre de progression)
# ------------------------------------------------------------------------
M <- 30
set.seed(2023)

# pbapply::pblapply() affiche une barre de progression
all_iterations <- pblapply(seq_len(M), function(i) {
  cat("Itération n°", i, "sur", M, "\n")
  one_iteration(
    model_id    = i,
    DfTrain     = DfTrain,
    var_options = variable_options,
    other_vars  = other_variables
  )
})

# On assemble tous les résultats dans un data.frame
results_train <- bind_rows(all_iterations)

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
  arrange(desc(mean_accuracy)) # ou tri par Kappa / logLoss

print(summary_train)

# ------------------------------------------------------------------------
# 9) Identification du meilleur modèle (selon accuracy_cv ou kappa_cv, etc.)
# ------------------------------------------------------------------------
#   À chaque itération, toutes les lignes ont la même accuracy_cv
#   => on peut prendre la première ou la moyenne
best_iterations <- results_train %>%
  group_by(model_id) %>%
  summarise(
    score_iter_accuracy = first(accuracy_cv),
    score_iter_kappa    = first(kappa_cv),
    score_iter_logloss  = first(logloss_cv),
    .groups = "drop"
  )

# Par exemple, on sélectionne le meilleur en accuracy
best_iterations <- best_iterations %>% arrange(desc(score_iter_accuracy))
best_id <- best_iterations$model_id[1]

cat("Meilleur modèle trouvé (ID) =", best_id, "\n")

best_config <- results_train %>%
  filter(model_id == best_id)

# ------------------------------------------------------------------------
# 10) Construction du modèle final sur TOUT le training set (avec la config retenue)
# ------------------------------------------------------------------------
final_vars <- best_config$variable

X_train_final <- DfTrain[, final_vars, drop = FALSE]
y_train_final <- DfTrain$dv_voteChoice

# On recrée les dummies
dummies_final <- dummyVars(" ~ .", data = X_train_final, fullRank = TRUE)
X_train_dummy <- predict(dummies_final, newdata = X_train_final) %>%
  as.data.frame()

# On entraîne le "vrai" modèle final (hors CV, en entier)
final_model <- multinom(
  y_train_final ~ ., 
  data    = X_train_dummy, 
  trace   = FALSE,
  MaxNWts = 100000
)

# ------------------------------------------------------------------------
# 11) Évaluation sur le jeu de test (jamais vu jusqu'ici)
# ------------------------------------------------------------------------
X_test_final <- DfTest[, final_vars, drop = FALSE]
y_test_final <- DfTest$dv_voteChoice

X_test_dummy <- predict(dummies_final, newdata = X_test_final) %>% 
  as.data.frame()

pred_test_final_class <- predict(final_model, newdata = X_test_dummy)
acc_test_final <- mean(pred_test_final_class == y_test_final)

cat("Accuracy (test) :", acc_test_final, "\n")

# Pour un critère plus nuancé, on peut prédire les probabilités, puis calculer
# logLoss ou Kappa sur le test :
pred_test_final_prob <- predict(final_model, newdata = X_test_dummy, type = "probs")

# Par exemple, calculer la logLoss manuellement
# On doit faire correspondre prob à la classe observée:
levelz <- levels(y_test_final)
logloss_test <- 0
n_test       <- length(y_test_final)
for (i in seq_len(n_test)) {
  obs_class <- y_test_final[i]
  # index de la classe dans 'levelz'
  class_idx <- which(levelz == obs_class)
  p         <- pred_test_final_prob[i, class_idx]
  p         <- max(p, 1e-15)
  logloss_test <- logloss_test - log(p)
}
logloss_test <- logloss_test / n_test

cat("LogLoss (test) :", logloss_test, "\n")

# On peut aussi faire un tableau de confusion
table_test <- table(
  predicted = pred_test_final_class,
  actual    = y_test_final
)
print(table_test)

# ------------------------------------------------------------------------
# 12) Sauvegarder le modèle final
# ------------------------------------------------------------------------
saveRDS(final_model, file = "_SharedFolder_datagotchi_federal_2024/clustering/data/multinom_model_2022.rds")

cat("Modèle sauvegardé avec succès.\n")
