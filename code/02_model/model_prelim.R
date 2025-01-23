library(nnet)

data_model <- readRDS("_SharedFolder_datagotchi_federal_2024/data/pilote/data_clean_simulated_23-01-2025.rds")

# Modèle -----------------------------------------------------------------
multinom_model <- multinom(vote_intent ~ ., data = data_model)

summary <- summary(multinom_model)

# Interprétation des coefficients -----------------------------------------
# Les coefficients peuvent être transformés en probabilités relatives
exp(coef(multinom_model))

summary$coefficients
summary$standard.errors

# Prédictions ------------------------------------------------------------
# Prédiction sur les données utilisées pour l'entraînement
data_model$predictions <- predict(multinom_model, newdata = data_model)

# Évaluation de la performance --------------------------------------------
# Matrice de confusion
table(data_model$vote_intent, data_model$predictions)

# Sauvegarder en RDS -----------------------------------------------------

saveRDS(multinom_model, file = "_SharedFolder_datagotchi_federal_2024/clustering/data/multinom_model_2022.rds")