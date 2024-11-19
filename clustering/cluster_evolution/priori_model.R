# Load packages ----------------------------------------------------------
library(nnet)
library(dplyr)

# Data -------------------------------------------------------------------
df_pilot_2021_merged <- read.csv("_SharedFolder_datagotchi_federal_2024/clustering/data/pilot2021_merged_clustering.csv")

# Sélection des variables inclues dans le clustering et celles de voteIntent
data_prior <- df_pilot_2021_merged %>%
  select(
    act_VisitsMuseumsGaleries, act_Volunteering, act_Yoga, act_Run, act_Gym, act_MotorizedOutdoorActivities,
    app_noTattoo, app_swag_Casual, app_swag_VintageHippBoheme,
    cons_regBeers, cons_cocktailsDrink, cons_microBeers, cons_redWineDrink, cons_noDrink,
    cons_brand_ChainesB, cons_brand_GSurf, cons_brand_MaR, cons_brand_Frip,
    cons_coffee_Starbucks, cons_coffee_place_noCoffee,
    cons_Meat,
    cons_SmokeNever, cons_Smoke,
    immigrant, educUniv, age55p, male, ses_hetero, langFr, incomeHigh,
    ses_dwelling_condo, ses_dwelling_detachedHouse,
    act_transport_PublicTransportation, act_transport_Car,
    op_voteIntent_Lib, op_voteIntent_Cons, op_voteIntent_Ndp, op_voteIntent_Bloc,
    op_voteIntent_Green, op_voteIntent_PPC, op_voteIntent_NoVote
  ) %>%
  drop_na()

# Création de la variable cible (vote_intent) 
data_prior <- data_prior %>%
  mutate(
    vote_intent = case_when(
      op_voteIntent_Lib == 1 ~ "Liberal",
      op_voteIntent_Cons == 1 ~ "Conservative",
      op_voteIntent_Ndp == 1 ~ "NDP",
      op_voteIntent_Bloc == 1 ~ "Bloc",
      op_voteIntent_Green == 1 ~ "Green",
      op_voteIntent_PPC == 1 ~ "PPC",
      op_voteIntent_NoVote == 1 ~ "NoVote",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-starts_with("op_voteIntent_")) %>% 
  drop_na(vote_intent) 

# Conversion en facteur
data_prior$vote_intent <- as.factor(data_prior$vote_intent)

data_model <- data_prior %>%
  select(
    vote_intent,
    act_VisitsMuseumsGaleries, act_Volunteering, act_Yoga, act_Run, act_Gym, act_MotorizedOutdoorActivities,
    app_noTattoo, app_swag_Casual, app_swag_VintageHippBoheme,
    cons_regBeers, cons_cocktailsDrink, cons_microBeers, cons_redWineDrink, cons_noDrink,
    cons_brand_ChainesB, cons_brand_GSurf, cons_brand_MaR, cons_brand_Frip,
    cons_coffee_Starbucks, cons_coffee_place_noCoffee,
    cons_Meat,
    cons_SmokeNever, cons_Smoke,
    immigrant, educUniv, age55p, male, ses_hetero, langFr, incomeHigh,
    ses_dwelling_condo, ses_dwelling_detachedHouse,
    act_transport_PublicTransportation, act_transport_Car
  )

# Modèle -----------------------------------------------------------------
multinom_model <- multinom(vote_intent ~ ., data = data_model)

summary(multinom_model)

# Interprétation des coefficients -----------------------------------------
# Les coefficients peuvent être transformés en probabilités relatives
exp(coef(multinom_model))

# Prédictions ------------------------------------------------------------
# Prédiction sur les données utilisées pour l'entraînement
data_model$predictions <- predict(multinom_model, newdata = data_model)

# Évaluation de la performance --------------------------------------------
# Matrice de confusion
table(data_model$vote_intent, data_model$predictions)


# Sauvegarder en RDS -----------------------------------------------------

saveRDS(multinom_model, file = "_SharedFolder_datagotchi_federal_2024/clustering/data/multinom_model.rds")
