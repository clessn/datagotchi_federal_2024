#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 02 Variables disponibles pour le clustering
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Problème de haute dimensionalité (~70 dim) => observations sont trop éloignés dans l'espace pour faire un bon clustering k-means car variance trop grande (voir l'équation de l'algo)   
## Solution: Inspiration de l'espace social de La Distinction (Bourdieu, 1979)
## Fusion des attributs pour réduire à 2 dimensions
## Capital économique / Capital culturel
## X = ratio Capital économique / Capital culturel
## Y = capital total


# Charger les librairies nécessaires
library(tidyverse)
library(FactoMineR)
library(factoextra)

# Données

df_pilot_2025 <- readRDS(file = "_PrivateFolder_datagotchi_federal_2025/data/clustering/can2025/02_pilot_2025.rds")

#MAPPING
## Sélection des variables pour l'échelle du capital économique

#########################
# CAPITAL ÉCONOMIQUE
#########################
pond_eco <- c(
  income_no_income          = -4,
  income_1_30000            = -2,
  income_30001_60000        = -1,
  income_60001_90000        = 0,
  income_90001_110000       = 1,
  income_110001_150000      = 2,
  income_150001_200000      = 3,
  income_more_than_200000   = 5,
  ses_dwellingApp           = -1,
  ses_dwellingCondo         = 2,
  ses_dwellingDetachedHouse = 3,
  ses_dwellingTownhouse     = 3,
  ses_dwellingDuplex        = 1,
  ses_dwellingOther         = -1,

  lifestyle_typeTransportCar         = 2,
  lifestyle_typeTransportSUV         = 3,
  lifestyle_typeTransportActive      = -1,
  lifestyle_typeTransportPublicTransit = -1,

  # Vêtements
  lifestyle_consClothesFrip         = -1.5,
  lifestyle_consClothesIndependent  = 2,
  lifestyle_consClothesChain        = 1,
  lifestyle_consClothesSuperstores  = -1,
  lifestyle_consClothesDepartment   = 0,
  lifestyle_consClothesOnline       = 0,

  # Conso/lifestyle
  lifestyle_clothingStyleClassic = 0.5,
  lifestyle_motorizedActFreq_numeric = 0.5,
  lifestyle_favAlcoolBubbleDrink = 0.5 #champagne
)

#########################
# CAPITAL CULTUREL
#########################
pond_cult <- c(
  ses_educBHS     = -1,
  ses_educPostHS  = 1,
  ses_educUniv    = 3,

  lifestyle_eatMeatFreq    = -1,
  lifestyle_clothingStyleClassic = 0,
  lifestyle_clothingStyleCasual  = 0,
  lifestyle_clothingStyleSport   = -0.5,
  lifestyle_hasTattoos           = 0, # peut être les + comme -

  # Activités physiques
  lifestyle_exerciseGym    = 1,
  lifestyle_exerciseTeamSport = 1,
  lifestyle_exerciseWalk   = 0,
  lifestyle_exerciseRun    = 1,
  lifestyle_exerciseYoga   = 2,
  lifestyle_exerciseSwim   = 1,
  lifestyle_exerciseOther  = 0.5,
  lifestyle_exerciseNone   = -1,

  # Activités de plein air / culturelles
  lifestyle_goFishingFreq_numeric  = -1,
  lifestyle_goHuntingFreq_numeric  = -1,
  lifestyle_goMuseumsFreq_numeric  = 3,
  lifestyle_motorizedActFreq_numeric = -1,
  lifestyle_volunteeringFreq_numeric = 2,

  # Alcool
  lifestyle_favAlcoolRedWine    = 2,
  lifestyle_favAlcoolWhiteWine  = 1.5,
  lifestyle_favAlcoolRoseWine   = 1.5,
  lifestyle_favAlcoolSpirits    = 1,
  lifestyle_favAlcoolBubbleDrink = 2,
  lifestyle_favAlcoolBeer       = -0.5,
  lifestyle_favAlcoolMicroBeer  = 2,
  lifestyle_favAlcoolCocktail   = 1.5,
  lifestyle_favAlcoolDontDrink  = 0,

  # Tabac
  lifestyle_smokeFreq       = -0.5,

  # Vêtements (frip & co. => dimension culture)
  lifestyle_consClothesFrip         = 2,
  lifestyle_consClothesIndependent  = 2,
  lifestyle_consClothesChain        = 0.5,
  lifestyle_consClothesSuperstores  = -1,
  lifestyle_consClothesDepartment   = -1,
  lifestyle_consClothesOnline       = 0,
  lifestyle_consClothesOther        = 0,

  # Café
  lifestyle_consCoffeeTimHortons    = -1,
  lifestyle_consCoffeeStarbucks     = 1,
  lifestyle_consCoffeeMcDo          = -1,
  lifestyle_consCoffeeOther         = 0.5,
  lifestyle_consCoffeeIndependent   = 2,
  lifestyle_consCoffeeNone          = 0,

  #Transport
  lifestyle_typeTransportActive      = 0.5,
  lifestyle_typeTransportPublicTransit = 0.5
)

# ---- Vérification d'existence des variables ----
# On vérifie que tous les noms de colonnes spécifiés dans pond_eco et pond_cult
# existent dans df_pilot_2025

missing_eco <- setdiff(names(pond_eco), names(df_pilot_2025))
missing_cult <- setdiff(names(pond_cult), names(df_pilot_2025))

if (length(missing_eco) > 0) {
  cat("Variables manquantes (capital éco) :\n")
  print(missing_eco)
}

if (length(missing_cult) > 0) {
  cat("Variables manquantes (capital culturel) :\n")
  print(missing_cult)
}

# ---- Calcul des scores ----
# Pour éviter erreur si variables manquantes, on ne garde que les colonnes existantes
common_eco <- intersect(names(pond_eco), colnames(df_pilot_2025))
common_cult <- intersect(names(pond_cult), colnames(df_pilot_2025))

# Conversion en matrice
mat_eco <- as.matrix(df_pilot_2025[, common_eco])
mat_cult <- as.matrix(df_pilot_2025[, common_cult])

# Extraction des poids correspondants
weights_eco <- pond_eco[common_eco]
weights_cult <- pond_cult[common_cult]

# Création des scores
# (Note : s'il y a des NA, tu peux envisager un remplacement ou un rowMeans avec na.rm)
df_pilot_2025$score_eco <- mat_eco %*% weights_eco

df_pilot_2025$score_cult <- mat_cult %*% weights_cult

# ---- Construction des dimensions souhaitées ----
# Y = somme des deux capitaux (score_eco + score_cult)
# X = différence économique - culturelle (score_eco - score_cult)


# Calculer la différence et la somme
df_pilot_2025$score_diff <- df_pilot_2025$score_eco - df_pilot_2025$score_cult
df_pilot_2025$score_sum  <- df_pilot_2025$score_eco + df_pilot_2025$score_cult


# Visualiser
ggplot(df_pilot_2025, aes(x = score_diff, y = score_sum)) +
  geom_point(alpha = 0.7, color = "steelblue") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Structure du capital (score_eco - score_cult)\n< 0 = culturel dominant, > 0 = économique dominant",
    y = "Volume total du capital (score_eco + score_cult)",
    title = "Espace social : structure (X) vs. volume (Y)"
  ) +
  theme_minimal()

names(df_pilot_2025)

saveRDS(df_pilot_2025, file = "_PrivateFolder_datagotchi_federal_2025/data/clustering/can2025/02_pilot_2025.rds")
