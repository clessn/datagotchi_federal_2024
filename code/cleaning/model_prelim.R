library(nnet)

df <- readRDS("_SharedFolder_datagotchi_federal_2024/data/data_clean.rds")

str(df)
summary(df)


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