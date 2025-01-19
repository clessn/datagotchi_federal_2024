
# library -----------------------------------------------------------------
library(nnet)


# data --------------------------------------------------------------------
df <- readRDS("_SharedFolder_datagotchi_federal_2024/data/data_clean.rds")

str(df)
summary(df)


## variables d'interet
variables <- c(
  "maritimes",
  "west",
  "ses_languageOther",
  "immigrant",
  "app_noTattoo",
  "ses_dwelling_app",
  "ses_dwelling_condo",
  "ses_dwelling_coop",
  "ses_dwelling_loft",
  "ses_dwelling_tour",
  "ses_dwelling_detachedHouse",
  "ses_dwelling_townHouse",
  "ses_dwelling_semiDetached",
  "ses_dwelling_HLM",
  "ses_dwelling_mobile",
  "act_Run",
  "act_Swimming",
  "act_TeamSport",
  "act_Walk",
  "act_Yoga",
  "act_Other",
  "act_None",
  "act_Gym",
  "act_Fishing",
  "act_Hunting",
  "act_VisitsMuseumsGaleries",
  "act_MotorizedOutdoorActivities",
  "act_Volunteering",
  "act_transport_Car",
  "act_transport_Moto",
  "act_transport_PublicTransportation",
  "act_transport_SUV",
  "act_transport_Taxi",
  "act_transport_Walk",
  "cons_brand_MaR",
  "cons_brand_BInd",
  "cons_brand_Frip",
  "cons_brand_ChainesB",
  "cons_brand_GSurf",
  "cons_brand_Other",
  "cons_Meat",
  "cons_Vege",
  "cons_Smoke",
  "cons_SmokeStopping",
  "cons_SmokeNever",
  "cons_SmokeStopped",
  "cons_noDrink",
  "cons_regBeers",
  "cons_sparklingDrink",
  "cons_spiritDrink",
  "cons_cocktailsDrink",
  "cons_redWineDrink",
  "cons_whiteWineDrink",
  "cons_roseDrink",
  "cons_coffee_place_ind",
  "app_swag_Formel",
  "app_swag_Chic",
  "app_swag_Classique",
  "app_swag_Casual",
  "app_swag_Sport",
  "app_swag_Other",
  "app_swag_VintageHippBoheme",
  "cons_coffee_place_noCoffee",
  "cons_coffee_McDo",
  "cons_coffee_SC",
  "cons_coffee_Starbucks",
  "cons_coffee_Other",
  "female",
  "age34m",
  "age55p",
  "quebec",
  "ses_hetero",
  "langFr",
  "educBHS",
  "educUniv",
  "incomeLow",
  "incomeHigh"
)


# Check for missing values
sum(is.na(df))

# Handle missing values if any
# Option 1: Remove rows with missing data
df_clean <- na.omit(df)

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