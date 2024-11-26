
# Packages ---------------------------------------------------------------




# Data -------------------------------------------------------------------

model <- readRDS("multinom_model.rds")

kmeans_result <- readRDS("multinom_model.rds")

app_data <- read.csv("data")


Loop: pour chaque jour, faire un modèle bayésien vote_intent ~ SES + lifestyle qui prend comme priors les coefficients du modèle du 1er script
Prédire ce modèle pour chaque centroid
Enregistrer une dataframe avec la prédiction de chaque cluster par jour