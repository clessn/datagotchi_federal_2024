# Charger les bibliothèques nécessaires
library(nnet)
library(tidyverse)
library(openxlsx)
library(tibble)

# Charger le modèle
final_model <- readRDS("_SharedFolder_datagotchi_federal_2024/data/modele/finalmodel_withOutInteractions.rds")

# EXTRACTION DES COEFFICIENTS --------------------------------------------------
coef_matrix <- final_model$sym_coef

# 1. Fichier avec tous les coefficients (sans intercept)
coef_transposed <- coef_matrix %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Party") %>% 
  {t(.[,-1])} %>%  # Transposer sans la colonne Party
  as.data.frame() %>% 
  set_names(coef_matrix %>% rownames()) %>% 
  rownames_to_column(var = "Predictor") %>% 
  select(Predictor, lpc, gpc, cpc, bq, ndp) %>% 
  filter(Predictor != "(Intercept)")

# 2. Fichier des intercepts seulement
intercept_df <- coef_matrix %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Party") %>% 
  select(Party, Intercept = "(Intercept)") %>% 
  filter(Party %in% c("lpc", "gpc", "cpc", "bq", "ndp")) %>% 
  mutate(Intercept = format(Intercept, scientific = FALSE)) %>% 
  arrange(factor(Party, levels = c("lpc", "gpc", "cpc", "bq", "ndp")))

# Exportation des deux fichiers
write.xlsx(list(
  "Coefficients" = coef_transposed,
  "Intercepts" = intercept_df
), "_SharedFolder_datagotchi_federal_2024/data/modele/model_cOeFfIcIeNt.xlsx")

cat("Exportation réussie :\n",
    "- Coefficients dans l'onglet 'Coefficients'\n", 
    "- Intercepts dans l'onglet 'Intercepts'\n",
    "Fichier : model_coefficients.xlsx\n")
