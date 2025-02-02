# Charger les bibliothèques nécessaires
library(nnet)
library(tidyverse)
library(openxlsx)
library(tibble)  # Pour rownames_to_column()

# Charger le modèle (remplacez le chemin par celui de votre modèle sauvegardé)
final_model <- readRDS("_SharedFolder_datagotchi_federal_2024/data/modele/finalmodel_withOutInteractions.rds")

# EXTRACTION DES COEFFICIENTS
# Ici, final_model$sym_coef est déjà une matrice des coefficients symétriques.
coef_matrix <- final_model$sym_coef

# Convertir la matrice en data.frame et ajouter les noms des lignes comme première colonne
coef_df <- as.data.frame(coef_matrix) %>% 
  rownames_to_column(var = "Parti")

# Optionnel : si vous préférez avoir un data.frame transposé 
# (chaque prédicteur en ligne et chaque parti en colonne), vous pouvez faire :
coef_transposed <- t(coef_df[,-1])            # Transposer en retirant la colonne "Parti"
coef_transposed <- as.data.frame(coef_transposed)
names(coef_transposed) <- coef_df$Parti        # Les colonnes portent désormais les noms des partis
coef_transposed <- coef_transposed %>% rownames_to_column(var = "Predictor")

# Visualiser le résultat
print(coef_transposed)

# Exporter en fichier Excel
write.xlsx(coef_transposed, "_SharedFolder_datagotchi_federal_2024/data/modele/coefficients_withoutinteractions.xlsx")
cat("Exportation réussie : coefficients enregistrés dans 'coefficients_withoutinteractions.xlsx'.\n")
