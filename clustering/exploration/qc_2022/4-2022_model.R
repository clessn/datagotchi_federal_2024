# Load packages ----------------------------------------------------------
library(nnet)
library(dplyr)
library(tidyr)

# Data -------------------------------------------------------------------

df_pilot1_2022 <- read.csv("/home/alexab/Dropbox/Ulaval/CLESSN/_SharedFolder_datagotchi-developpement/quebec_prov_2022/pilotes/pilote-1-quebec-prov-2022.csv")

variables_int <- c(
  # "id",
  # "postal_code",
  # "ses_age",
   "male",
   "female",
   "ses_genderOther",
   "age34m",
   "age3554",
   "age55p",
   "langEn",
   "langFr",
   "ses_languageOther",
   "act_Gym",
   "act_TeamSport",
  # "act_Walk",
   "act_Run",
   "act_Yoga",
  # "act_Swimming",
  # "act_Other",
   "act_None",
  # "answers.sport",
   "act_Fishing",
   "act_Hunting",
   "act_VisitsMuseumsGaleries",
   "act_MotorizedOutdoorActivities",
   "act_Volunteering",
   "animal_cat",
   "animal_dog",
  # "animal_domestic",
  # "animal_farm",
  # "animal_noPet",
  # "answers.pets",
   "cons_brand_MaR",
  # "cons_brand_BInd",
  # "cons_brand_ChainesB",
  # "cons_brand_GSurf",
  # "cons_brand_OnlineOnly",
   "cons_brand_Frip",
  # "cons_brand_Other",
  # "answers.shopping",
   "educBHS",
   "educCollege",
   "educUniv",
   "cons_redWineDrink",
  # "cons_whiteWineDrink",
  # "cons_roseDrink",
  # "cons_sparklingDrink",
   "cons_regBeers",
  # "cons_microBeers",
  # "cons_spiritDrink",
   "cons_cocktailsDrink",
   "cons_noDrink",
  # "answers.alcohol",
  # "ses_income_None",
  # "ses_income_i1to30",
  # "ses_income_i31to60",
  # "ses_income_i61to90",
  # "ses_income_i91to110",
  # "ses_income_i111to150",
  # "ses_income_i151to200",
  # "ses_income_i201toInf",
  # "ses_income_no_answer",
   "incomeLow",
   "incomeMid",
   "incomeHigh",
  # "parent_outside",
  # "parent_canada",
  # "parent_no_answer",
   "ses_dwelling_app",
  # "ses_dwelling_loft",
  # "ses_dwelling_condo",
  # "ses_dwelling_tour",
   "ses_dwelling_detachedHouse",
  # "ses_dwelling_townHouse",
  # "ses_dwelling_semiDetached",
  # "ses_dwelling_coop",
  # "ses_dwelling_HLM",
  # "ses_dwelling_mobile",
  # "ses_dwelling_other",
  # "answers.dwelling",
  # "cons_Smoke_never",
  # "cons_Smoke_few_times_year",
  # "cons_Smoke_month",
  # "cons_Smoke_once_week",
  # "cons_Smoke_few_times_week",
  # "cons_Smoke_once_day",
  # "cons_Smoke_few_times_day",
  # "answers.smoke",
   "act_transport_Car",
  # "act_transport_SUV",
  # "act_transport_Moto",
   "act_transport_Walk",
  # "act_transport_Bicycle",
   "act_transport_PublicTransportation",
  # "act_transport",
  # "answers.transport",
  # "vehicule_4x4",
  # "vehicule_Berline",
  # "vehicule_Cabriolet",
   "vehicule_PickUp",
  # "vehicule_Van",
  # "vehicule_luxury",
  # "vehicule_sport",
  # "vehicule_electric",
  # "vehicule_VUS",
  # "vehicule_other",
   "vehicule_noCar",
  # "act_modelCar",
  # "answers.vehicule",
  # "turnout_odds",
  # "op_intent",
   "op_intent_CAQ",
   "op_intent_PQ",
   "op_intent_PLQ",
   "op_intent_QS",
   "op_intent_PCQ",
   "op_intent_Other",
  # "op_intent_dontKnow",
   "op_intent_wontVote",
  # "op_potentialG_CAQ",
  # "op_potentialG_PLQ",
  # "op_potentialG_PQ",
  # "op_potentialG_QS",
  # "op_potentialG_PCQ",
  # "op_voted_2018",
  # "party_id_caquiste",
  # "party_id_lib",
  # "party_id_pequiste",
  # "party_id_solidaire",
  # "party_id_cons",
  # "party_id_vert",
  # "party_id_another",
  # "party_id_none",
  # "party_id_DK",
   "immigrant",
   "cons_coffee_TimH",
   "cons_coffee_Starbucks",
  # "cons_coffee_SC",
  # "cons_coffee_McDo",
  # "cons_coffee_Other",
  # "cons_coffee_place_ind",
   "cons_coffee_place_noCoffee",
  # "answers.coffee_shop",
  # "app_swag_Formel",
  # "app_swag_Classique",
  # "app_swag_Casual",
  # "app_swag_Sport",
  # "app_swag_Chic",
  # "app_swag_HippBoheme",
  # "app_swag_Punk",
  # "app_swag_Rock",
  # "app_swag_Other",
  # "answers.clothing",
   "app_noTattoo",
  # "cons_meat_never",
  # "cons_meat_almost_never",
  # "cons_meat_once_month",
  # "cons_meat_once_week",
  # "cons_meat_few_week",
  # "cons_meat_daily",
  # "cons_meat_few_daily",
  # "answers.food",
   "cons_low_Meat",
   "cons_mid_Meat",
   "cons_much_Meat",
   "ses_ethn_White",
   "ses_ethn_Black",
  # "ses_ethn_Aboriginals",
   "ses_ethn_Asiatique",
  # "ses_ethn_Hispanique",
  # "ses_ethn_Arabe",
  # "ses_ethn_Other",
   "ses_hetero",
   "ses_gai",
   "ses_bisex"#,
  # "ses_sexOri_other"#,
 )

# Sélection des variables inclues dans le clustering et celles de voteIntent
data_prior <- df_pilot1_2022 %>%
  select(all_of(variables_int)) %>%
  drop_na()

# Création de la variable cible (vote_intent) 
data_prior <- data_prior %>%
  mutate(
    vote_intent = case_when(
      op_intent_CAQ == 1 ~ "CAQ",
      op_intent_PQ == 1 ~ "PQ",
      op_intent_PLQ == 1 ~ "PLQ",
      op_intent_QS == 1 ~ "QS",
      op_intent_PCQ == 1 ~ "PCQ",
      op_intent_Other == 1 ~ "Autre",
      op_intent_wontVote == 1 ~ "NoVote",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-starts_with("op_voteIntent_")) %>% 
  drop_na(vote_intent) 

#diminution marquée du nombre de répondants car:
# > table(df_pilot1_2022$op_intent_dontKnow)
#    0    1 
# 1196  304 


# Conversion en facteur
data_prior$vote_intent <- as.factor(data_prior$vote_intent)
levels(data_prior$vote_intent)
data_prior$vote_intent <- relevel(data_prior$vote_intent, ref = "CAQ")

data_model <- data_prior %>%
  select(
    vote_intent,
    male,
    female,
    ses_genderOther,
    age34m,
    age3554,
    age55p,
    langEn,
    langFr,
    ses_languageOther,
    act_Gym,
    act_TeamSport,
    act_Run,
    act_Yoga,
    act_None,
    act_Fishing,
    act_Hunting,
    act_VisitsMuseumsGaleries,
    act_MotorizedOutdoorActivities,
    act_Volunteering,
    animal_cat,
    animal_dog,
    cons_brand_MaR,
    cons_brand_Frip,
    educBHS,
    educCollege,
    educUniv,
    cons_redWineDrink,
    cons_regBeers,
    cons_cocktailsDrink,
    cons_noDrink,
    incomeLow,
    incomeMid,
    incomeHigh,
    ses_dwelling_app,
    ses_dwelling_detachedHouse,
    act_transport_Car,
    act_transport_Walk,
    act_transport_PublicTransportation,
    vehicule_PickUp,
    vehicule_noCar,
    immigrant,
    cons_coffee_TimH,
    cons_coffee_Starbucks,
    cons_coffee_place_noCoffee,
    app_noTattoo,
    cons_low_Meat,
    cons_mid_Meat,
    cons_much_Meat,
    ses_ethn_White,
    ses_ethn_Black,
    ses_ethn_Asiatique,
    ses_hetero,
    ses_gai,
    ses_bisex
  )

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

# Calcul de l'exactitude globale
correct_predictions <- sum(diag(table(data_model$vote_intent, data_model$predictions)))
total_predictions <- sum(table(data_model$vote_intent, data_model$predictions))
accuracy <- correct_predictions / total_predictions
print(paste("Exactitude Globale :", round(accuracy * 100, 2), "%"))

# Matrice de confusion normalisée par ligne (classes réelles)
confusion_matrix <- table(data_model$vote_intent, data_model$predictions)
confusion_matrix_normalized <- prop.table(confusion_matrix, 1)
print(round(confusion_matrix_normalized, 2))
