# Packages ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)
library(tidyr)
library(ggcorrplot)

# Data -------------------------------------------------------------------

app2022 <- readRDS("/home/alexab/Dropbox/Ulaval/CLESSN/_SharedFolder_datagotchi-developpement/quebec_prov_2022/hub/data-hub-clean-2022-10-27_clean.rds")


df_pilot1_2022 <- read.csv("/home/alexab/Dropbox/Ulaval/CLESSN/_SharedFolder_datagotchi-developpement/quebec_prov_2022/pilotes/pilote-1-quebec-prov-2022.csv")
df_pilot2_2022 <- read.csv("/home/alexab/Dropbox/Ulaval/CLESSN/_SharedFolder_datagotchi-developpement/quebec_prov_2022/pilotes/pilote-2-merge-final.csv")

# Le pilot2 a des variables très différentes du pilot1 et de l'app, il ne sera donc pas utilisé pour le clustering.

## identifier les variables d'interet presentes dans les 2 pilots + app
variables_interet <- c(
  "act_VisitsMuseumsGaleries",
  "act_Fishing",
  "act_Hunting",
  "act_MotorizedOutdoorActivities",
  "act_Volunteering",
  "time",
  "voting_probability",
  "postal_code",
  "riding",
  #"pays_qc",
  "immigrant",
  "op_intent_CAQ",
  "op_intent_PQ",
  "op_intent_PLQ",
  "op_intent_QS",
  "op_intent_PCQ",
  "op_intent_Other",
  "op_intent_no_vote",
  "vote_pred_CAQ",
  "vote_pred_PQ",
  "vote_pred_PLQ",
  "vote_pred_QS",
  "vote_pred_PCQ",
  #"people_pred_CAQ",
  #"people_pred_PQ",
  #"people_pred_PLQ",
  #"people_pred_QS",
  #"people_pred_PCQ",
  "ses_ethn_White",
  "ses_ethn_Black",
  "ses_ethn_Asian",
  "ses_ethn_Hispanic",
  "ses_ethn_Indigenous",
  "ses_ethn_Arab",
  "ses_ethn_Other",
  #"health_relation_entourage",
  #"health_stay_home",
  #"health_physical_health",
  #"health_time_nature",
  #"health_mental_health",
  "act_transport_Car",
  "act_transport_SUV",
  "act_transport_Moto",
  "act_transport_Walk",
  "act_transport_Bicycle",
  "act_transport_PublicTransportation",
  "vehicule_4x4",
  "vehicule_other",
  "vehicule_Berline",
  "vehicule_Cabriolet",
  "vehicule_noCar",
  "vehicule_PickUp",
  "vehicule_Van",
  "vehicule_luxury",
  "vehicule_sport",
  "vehicule_electric",
  "vehicule_VUS",
  "app_swag_Formel",
  "app_swag_Classique",
  "app_swag_Casual",
  "app_swag_Sport",
  "app_swag_Chic",
  "app_swag_HippBoheme",
  "app_swag_Punk",
  "app_swag_Rock",
  "app_swag_Other",
  "cons_Smoke_never",
  "cons_Smoke_few_times_year",
  "cons_Smoke_month",
  "cons_Smoke_once_week",
  "cons_Smoke_few_times_week",
  "cons_Smoke_once_day",
  "cons_Smoke_few_times_day",
  "cons_meat_never",
  "cons_meat_almost_never",
  "cons_meat_once_month",
  "cons_meat_once_week",
  "cons_meat_few_week",
  "cons_meat_daily",
  "cons_meat_few_daily",
  "cons_brand_MaR",
  "cons_brand_BInd",
  "cons_brand_ChainesB",
  "cons_brand_GSurf",
  "cons_brand_OnlineOnly",
  "cons_brand_Frip",
  "cons_brand_Other",
  "cons_coffee_TimH",
  "cons_coffee_Starbucks",
  "cons_coffee_SC",
  "cons_coffee_McDo",
  "cons_coffee_Other",
  "cons_coffee_place_ind",
  "cons_coffee_None",
  "act_Walk",
  "act_Gym",
  "act_Team",
  "act_Run",
  "act_Yoga",
  "act_Swim",
  "act_Other",
  "act_None",
  "ses_income_None",
  "ses_income_i1to30",
  "ses_income_i31to60",
  "ses_income_i61to90",
  "ses_income_i91to110",
  "ses_income_i111to150",
  "ses_income_i151to200",
  "ses_income_i201toInf",
  "ses_dwelling_App",
  "ses_dwelling_Loft",
  "ses_dwelling_Condo",
  "ses_dwelling_Tour",
  "ses_dwelling_Duplex",
  "ses_dwelling_townHouse",
  "ses_dwelling_detachedHouse",
  "ses_dwelling_Coop",
  "ses_dwelling_HLM",
  "ses_dwelling_Mobile",
  "ses_dwelling_Other",
  "cons_redWineDrink",
  "cons_whiteWineDrink",
  "cons_bubbleDrink",
  "cons_beerDrink",
  "cons_microDrink",
  "cons_spiritDrink",
  "cons_cocktailDrink",
  "cons_roseDrink",
  "cons_noDrink",
  #"ses_educ_None",
  #"ses_educ_Prim",
  #"ses_educ_Sec",
  #"ses_educ_Coll",
  #"ses_educ_Bacc",
  #"ses_educ_Master",
  #"ses_educ_PhD",
  "educBHS",
  "educCollege",
  "educUniv",
  #"age",
  "age34m",
  "age3554",
  "age55p",
  "male",
  "female",
  "genderOther",
  #"gender_agender",
  #"gender_trans_female",
  #"gender_trans_male",
  #"gender_nonbinary",
  #"gender_queer",
  "ses_hetero",
  "ses_gai",
  "ses_bisex",
  "ses_sexOri_other",
  #"ses_Asexual",
  #"ses_Queer",
  #"ses_Pansexual",
  #"ses_Questionning",
  "langFr",
  "langEn",
  "ses_languageOther",
  "app_noTattoo",
  "animal_farm",
  "animal_domestic",
  #"animal_cat",
  #"animal_catNdog",
  #"animal_dog",
  "animal_noPet",
  "op_intent",
  "vote_pred",
  "people_predict"#,
  #"id",
  #"city_name",
  #"mrc_name",
  #"region_name"
)

variables <- c(
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
  "act_Walk",
  "act_Run",
  "act_Yoga",
  "act_Swimming",
  "act_Other",
  "act_None",
 # "answers.sport",
  "act_Fishing",
  "act_Hunting",
  "act_VisitsMuseumsGaleries",
  "act_MotorizedOutdoorActivities",
  "act_Volunteering",
  "animal_cat",
  "animal_dog",
  "animal_domestic",
  "animal_farm",
  "animal_noPet",
 # "answers.pets",
  "cons_brand_MaR",
  "cons_brand_BInd",
  "cons_brand_ChainesB",
  "cons_brand_GSurf",
  "cons_brand_OnlineOnly",
  "cons_brand_Frip",
  "cons_brand_Other",
 # "answers.shopping",
  "educBHS",
  "educCollege",
  "educUniv",
  "cons_redWineDrink",
  "cons_whiteWineDrink",
  "cons_roseDrink",
  "cons_sparklingDrink",
  "cons_regBeers",
  "cons_microBeers",
  "cons_spiritDrink",
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
  "ses_dwelling_loft",
  "ses_dwelling_condo",
  "ses_dwelling_tour",
  "ses_dwelling_detachedHouse",
  "ses_dwelling_townHouse",
  "ses_dwelling_semiDetached",
  "ses_dwelling_coop",
  "ses_dwelling_HLM",
  "ses_dwelling_mobile",
  "ses_dwelling_other",
 # "answers.dwelling",
  "cons_Smoke_never",
  "cons_Smoke_few_times_year",
  "cons_Smoke_month",
  "cons_Smoke_once_week",
  "cons_Smoke_few_times_week",
  "cons_Smoke_once_day",
  "cons_Smoke_few_times_day",
 # "answers.smoke",
  "act_transport_Car",
  "act_transport_SUV",
  "act_transport_Moto",
  "act_transport_Walk",
  "act_transport_Bicycle",
  "act_transport_PublicTransportation",
 # "act_transport",
 # "answers.transport",
  "vehicule_4x4",
  "vehicule_Berline",
  "vehicule_Cabriolet",
  "vehicule_PickUp",
  "vehicule_Van",
  "vehicule_luxury",
  "vehicule_sport",
  "vehicule_electric",
  "vehicule_VUS",
  "vehicule_other",
  "vehicule_noCar",
 # "act_modelCar",
 # "answers.vehicule",
  "turnout_odds",
  "op_intent",
  "op_intent_CAQ",
  "op_intent_PQ",
  "op_intent_PLQ",
  "op_intent_QS",
  "op_intent_PCQ",
  "op_intent_Other",
  "op_intent_dontKnow",
  "op_intent_wontVote",
  "op_potentialG_CAQ",
  "op_potentialG_PLQ",
  "op_potentialG_PQ",
  "op_potentialG_QS",
  "op_potentialG_PCQ",
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
  "cons_coffee_SC",
  "cons_coffee_McDo",
  "cons_coffee_Other",
  "cons_coffee_place_ind",
  "cons_coffee_place_noCoffee",
 # "answers.coffee_shop",
  "app_swag_Formel",
  "app_swag_Classique",
  "app_swag_Casual",
  "app_swag_Sport",
  "app_swag_Chic",
  "app_swag_HippBoheme",
  "app_swag_Punk",
  "app_swag_Rock",
  "app_swag_Other",
 # "answers.clothing",
  "app_noTattoo",
  "cons_meat_never",
  "cons_meat_almost_never",
  "cons_meat_once_month",
  "cons_meat_once_week",
  "cons_meat_few_week",
  "cons_meat_daily",
  "cons_meat_few_daily",
 # "answers.food",
  "cons_low_Meat",
  "cons_mid_Meat",
  "cons_much_Meat",
 # "ses_urbain",
 # "ses_sururbain",
 # "ses_rural",
 # "ses_single",
 # "ses_married",
 # "ses_common_law_relationship",
 # "ses_widowed",
 # "ses_divorced",
 # "ses_homeowner",
 # "ses_tenant",
 # "ses_childless_couple",
 # "ses_couple_with_child",
 # "ses_single_whitout_child",
 # "ses_single_with_child",
 # "ses_situation_other",
  "ses_ethn_White",
  "ses_ethn_Black",
  "ses_ethn_Aboriginals",
  "ses_ethn_Asiatique",
  "ses_ethn_Hispanique",
  "ses_ethn_Arabe",
  "ses_ethn_Other",
  "ses_hetero",
  "ses_gai",
  "ses_bisex",
  "ses_sexOri_other",
 # "ideology",
 # "disgust_Expired_milk",
 # "disgust_Grubs_meat",
 # "disgust_Urine_smell",
 # "world_conception"
)
