# Load packages ----------------------------------------------------------
library(nnet)
library(tidyverse)
library(caret)
library(dplyr)
library(tidyr)
library(ggplot2)

# Load data --------------------------------------------------------------
DataModel <- readRDS("_SharedFolder_datagotchi_federal_2024/data/pilote/DataCleanPilot_2025Janv30.rds")

# Select variables -------------------------------------------------------
DataModel <- DataModel |> 
  select(id, ses_postalCode, ses_age, ses_age_4Cat, ses_ageGroup5Years, lifestyle_hasTattoos, lifestyle_numberTattoos, 
  ses_dwelling, ses_dwelling_cat, ses_ethnicity, ses_ethnicityWB, ses_ethnicityWhite,
ses_sexOrientation, ses_sexOrientationHetero, ses_gender, ses_genderFemale, lifestyle_clothingStyle, lifestyle_clothingStyleGroups,
lifestyle_typeTransport, lifestyle_consClothes, lifestyle_exercise, lifestyle_eatMeatFreq, lifestyle_favAlcool,
lifestyle_consCoffee, ses_language, lifestyle_smokeFreq, lifestyle_ownPet, lifestyle_ownPet_bin,
lifestyle_goHuntingFreq, lifestyle_goHuntingFreq_bin, lifestyle_goHuntingFreq_factor, lifestyle_goHuntingFreq_numeric,
lifestyle_goFishingFreq, lifestyle_goFishingFreq_bin, lifestyle_goFishingFreq_factor, lifestyle_goFishingFreq_numeric, 
lifestyle_goMuseumsFreq, lifestyle_goMuseumsFreq_bin, lifestyle_goMuseumsFreq_factor, lifestyle_goMuseumsFreq_numeric,
lifestyle_volunteeringFreq, lifestyle_volunteeringFreq_bin, lifestyle_volunteeringFreq_factor, lifestyle_volunteeringFreq_numeric,
ses_educ, ses_educ_3Cat, ses_educ_5Cat, ses_income, ses_income_3Cat, ses_incomeCensus,
lifestyle_motorizedActFreq, lifestyle_motorizedActFreq_bin, lifestyle_motorizedActFreq_factor, lifestyle_motorizedActFreq_numeric,
dv_voteChoice)

DataModel$dv_voteChoice <- factor(DataModel$dv_voteChoice)

# Création d'un jeu d'entraînement et d'un jeu de test -------------------
set.seed(42)
trainIndex <- createDataPartition(DataModel$dv_voteChoice, p = 0.8, list = FALSE)
DfTrain  <- DataModel[ trainIndex, ]
DfTest   <- DataModel[-trainIndex, ]

# variable_options : pour chaque "groupe", on liste les différentes versions

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

# ------------------------------------------------------
one_iteration <- function(model_id, DfTrain, var_options, other_vars) {
  
  # a) Sélection aléatoire des codifications
  selected_vars <- other_vars
  var_codings_used <- list()  # pour stocker la correspondance variable -> groupe
  
  for (var_group in names(var_options)) {
    codings <- var_options[[var_group]]
    chosen_coding <- sample(codings, 1)
    selected_vars <- c(selected_vars, chosen_coding)
    var_codings_used[[chosen_coding]] <- var_group
  }
  
  # b) Préparer X_train et y_train (en excluant id et la cible)
  X_train <- DfTrain[, selected_vars, drop = FALSE] %>%
    select(-id)  # enlever "id" si présent
  
  y_train <- DfTrain$dv_voteChoice
  
  # c) Créer les dummies (optionnel si vous utilisez une formule ~ .)
  dummies <- dummyVars(" ~ .", data = X_train, fullRank = TRUE)
  X_train_dummy <- predict(dummies, newdata = X_train) %>%
    as.data.frame()
  
  # d) Entraîner le modèle multinomial
  model <- multinom(y_train ~ ., data = X_train_dummy, trace = FALSE)
  
  # e) Évaluer la performance sur le train (ou CV si besoin)
  pred_train <- predict(model, newdata = X_train_dummy)
  accuracy_train <- mean(pred_train == y_train)
  
  # f) Construire le tableau de résultats pour cette itération
  iteration_results <- data.frame(
    model_id = model_id,
    variable = selected_vars,
    coding = sapply(selected_vars, function(x) {
      # On indique le groupe (var_codings_used) ou "fixed" si variable fixe
      if (x %in% names(var_codings_used)) {
        return(var_codings_used[[x]])
      } else {
        return("fixed")
      }
    }),
    score_train = accuracy_train,
    stringsAsFactors = FALSE
  )
  
  return(iteration_results)
}

M <- 30
set.seed(2023)

all_iterations <- lapply(seq_len(M), function(i) {
  one_iteration(
    model_id    = i,
    DfTrain    = DfTrain,
    var_options = variable_options,
    other_vars  = other_variables
  )
})

results_train <- bind_rows(all_iterations)

# Modèle -----------------------------------------------------------------
multinom_model <- multinom(dv_voteChoice ~ ., data = DataModel)

summary <- summary(multinom_model)

# Interprétation des coefficients -----------------------------------------
# Les coefficients peuvent être transformés en probabilités relatives
exp(coef(multinom_model))

summary$coefficients
summary$standard.errors

# Prédictions ------------------------------------------------------------
# Prédiction sur les données utilisées pour l'entraînement
DataModel$predictions <- predict(multinom_model, newdata = DataModel)

# Évaluation de la performance --------------------------------------------
# Matrice de confusion
table(DataModel$vote_intent, DataModel$predictions)

# Sauvegarder en RDS -----------------------------------------------------

saveRDS(multinom_model, file = "_SharedFolder_datagotchi_federal_2024/clustering/data/multinom_model_2022.rds")