# Load packages ----------------------------------------------------------
library(nnet)
library(dplyr)
library(tidyr)

# Data -------------------------------------------------------------------
df_pilot_2021_merged <- read.csv("_SharedFolder_datagotchi_federal_2024/clustering/data/pilot2021_merged_clustering_qc.csv")

# Variables --------------------------------
variables_int <- c(
  "educBHS",
#  "educHS",
  "educUniv",
  "incomeLow",
  "incomeMid",
  "incomeHigh",
  "ses_hetero",
  "ses_gai",
#  "ses_bisex",
#  "ses_sexOri_other",
  "immigrant",
  "male",
#  "female",
#  "ses_genderOther",
  "age34m",
  "age3554",
  "age55p",
  "langFr",
  "langEn",
  "ses_languageOther",
  "act_transport_Car",
#  "act_transport_SUV",
#  "act_transport_Moto",
  "act_transport_Walk",
#  "act_transport_Bicycle",
  "act_transport_PublicTransportation",
#  "act_Walk",
  "act_Gym",
  "act_TeamSport",
  "act_Run",
  "act_Yoga",
#  "act_Swimming",
#  "act_Other",
  "act_None",
  "act_Fishing",
  "act_Hunting",
  "act_VisitsMuseumsGaleries",
  "act_MotorizedOutdoorActivities",
#  "act_Volunteering",
  "cons_brand_MaR",
#  "cons_brand_OnlineOnly",
#  "cons_brand_BInd",
  "cons_brand_ChainesB",
  "cons_brand_GSurf",
  "cons_brand_Frip",
#  "cons_brand_Other",
  "cons_Meat",
  "cons_Vege",
  "cons_Vegan",
  "cons_coffee_TimH",
#  "cons_coffee_Other",
  "cons_coffee_Starbucks",
#  "cons_coffee_SC",
  "cons_coffee_McDo",
#  "cons_coffee_place_ind",
  "cons_coffee_place_noCoffee",
  "cons_Smoke",
#  "cons_SmokeStopping",
  "cons_SmokeStopped",
  "cons_SmokeNever",
#  "cons_VapeNation",
  "cons_noDrink",
  "cons_redWineDrink",
#  "cons_whiteWineDrink",
#  "cons_roseDrink",
#  "cons_sparklingDrink",
  "cons_regBeers",
  "cons_microBeers",
#  "cons_spiritDrink",
  "cons_cocktailsDrink",
#  "app_swag_Formel",
#  "app_swag_Classique",
#  "app_swag_Casual",
#  "app_swag_Sport",
#  "app_swag_Chic",
#  "app_swag_VintageHippBoheme",
#  "app_swag_Other",
#  "app_swag_Rock",
  "app_noTattoo",
  "ses_dwelling_app",
#  "ses_dwelling_loft",
  "ses_dwelling_condo",
#  "ses_dwelling_tour",
  "ses_dwelling_detachedHouse",
#  "ses_dwelling_townHouse",
#  "ses_dwelling_semiDetached",
#  "ses_dwelling_coop",
#  "ses_dwelling_HLM",
#  "ses_dwelling_mobile",
#  "ses_dwelling_other"#,
#  "ses_dwelling_house"
"op_voteIntent_Lib",
"op_voteIntent_Cons",
"op_voteIntent_Ndp",
"op_voteIntent_Bloc",
"op_voteIntent_Green"#,
#"op_voteIntent_PPC",
#"op_voteIntent_NoVote"
)



# Sélection des variables inclues dans le clustering et celles de voteIntent
data_prior <- df_pilot_2021_merged %>%
  select(all_of(variables_int)) %>%
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
    #  op_voteIntent_PPC == 1 ~ "PPC",
    #  op_voteIntent_NoVote == 1 ~ "NoVote",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-starts_with("op_voteIntent_")) %>% 
  drop_na(vote_intent) 

# Conversion en facteur
data_prior$vote_intent <- as.factor(data_prior$vote_intent)
levels(data_prior$vote_intent)
data_prior$vote_intent <- relevel(data_prior$vote_intent, ref = "Liberal")

data_model <- data_prior %>%
  select(
    vote_intent,
    educBHS,
    educUniv,
    incomeLow,
    incomeMid,
    incomeHigh,
    ses_hetero,
    ses_gai,
    immigrant,
male,
age34m,
age3554,
age55p,
langFr,
langEn,
ses_languageOther,
act_transport_Car,
act_transport_Walk,
act_transport_PublicTransportation,
act_Gym,
act_TeamSport,
act_Run,
act_Yoga,
act_None,
act_Fishing,
act_Hunting,
act_VisitsMuseumsGaleries,
act_MotorizedOutdoorActivities,
cons_brand_MaR,
cons_brand_ChainesB,
cons_brand_GSurf,
cons_brand_Frip,
cons_Meat,
cons_Vege,
cons_Vegan,
cons_coffee_TimH,
cons_coffee_Starbucks,
cons_coffee_McDo,
cons_coffee_place_noCoffee,
cons_Smoke,
cons_SmokeStopped,
cons_SmokeNever,
cons_noDrink,
cons_redWineDrink,
cons_regBeers,
cons_microBeers,
cons_cocktailsDrink,
app_noTattoo,
ses_dwelling_app,
ses_dwelling_condo,
ses_dwelling_detachedHouse
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

saveRDS(multinom_model, file = "_SharedFolder_datagotchi_federal_2024/clustering/data/multinom_model.rds")

# Calcul de l'exactitude globale
correct_predictions <- sum(diag(table(data_model$vote_intent, data_model$predictions)))
total_predictions <- sum(table(data_model$vote_intent, data_model$predictions))
accuracy <- correct_predictions / total_predictions
print(paste("Exactitude Globale :", round(accuracy * 100, 2), "%"))

# Matrice de confusion normalisée par ligne (classes réelles)
confusion_matrix <- table(data_model$vote_intent, data_model$predictions)
confusion_matrix_normalized <- prop.table(confusion_matrix, 1)
print(round(confusion_matrix_normalized, 2))
