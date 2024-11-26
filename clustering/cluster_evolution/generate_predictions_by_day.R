
# Packages ---------------------------------------------------------------




# Data -------------------------------------------------------------------

model <- readRDS("_SharedFolder_datagotchi_federal_2024/clustering/data/multinom_model.rds")

kmeans_result <- readRDS("_SharedFolder_datagotchi_federal_2024/clustering/data/kmeans_results8.rds")

app_data <- read.csv("")


Loop: pour chaque jour, faire un modèle bayésien vote_intent ~ SES + lifestyle qui prend comme priors les coefficients du modèle du 1er script
Prédire ce modèle pour chaque centroid
Enregistrer une dataframe avec la prédiction de chaque cluster par jour