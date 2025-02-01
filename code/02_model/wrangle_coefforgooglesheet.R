# Charger les bibliothèques nécessaires
library(nnet)
library(tidyverse)
library(openxlsx)

# Charger le modèle (remplacez le chemin par celui de votre modèle sauvegardé)

final_model <- readRDS("_SharedFolder_datagotchi_federal_2024/data/modele/finalmodelV5.rds")

# Extraire les coefficients sous forme de matrice
coef_matrix <- coef(final_model)

# Convertir la matrice en data.frame
coef_df <- as.data.frame(coef_matrix)

# Ajouter les noms des coefficients comme première colonne
coef_df <- coef_df %>%
  rownames_to_column(var = "coefficient")

# Réorganiser les colonnes pour correspondre à l'ordre de votre image
coef_df <- coef_df %>%
  select(coefficient, everything())  # "everything()" garde toutes les autres colonnes

# 1. Transposer le data.frame sans la colonne 'coefficient'
coef_transposed <- t(coef_df[,-1])  # on retire la première colonne

# 2. Convertir le résultat en data.frame
coef_transposed <- as.data.frame(coef_transposed)

# 3. Donner comme noms de colonnes les valeurs initialement présentes dans la colonne 'coefficient'
names(coef_transposed) <- coef_df$coefficient

# 4. Ajouter les anciens noms de colonnes (ceux issus de coef_df[,-1]) en tant que colonne 'coefficient'
library(tibble)  # pour utiliser rownames_to_column()
coef_transposed <- coef_transposed %>% rownames_to_column(var = "coefficient")

# Visualiser le résultat
print(coef_transposed)
# Exporter en fichier Excel formaté
write.xlsx(coef_df, "_SharedFolder_datagotchi_federal_2024/data/modele/coefficients_final_model.xlsx")

cat("Exportation réussie : coefficients enregistrés dans 'coefficients_final_model.xlsx'.\n")
