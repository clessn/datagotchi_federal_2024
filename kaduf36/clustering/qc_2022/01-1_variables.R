#++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 01 Préparation initiales des données pour le clustering
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Variables communes entre pilote1 et app

# Déclaration et initialisation du vecteur contenant le nom des 
# variables qui sont communes entre le pilote1 et l'application
#
# Le pilot2 a des variables très différentes du pilot1 et de l'app, 
# il ne sera donc pas utilisé pour le clustering.
#
# Pour des fins de développement itératif :
# Les variables seront regroupées au fil du temps par sens communs
# Le début et la fin du groupement seront clairement indiqués
# Les groupements seront utilisés à l'étape 02 de la préparation des données
#
variables_act <- c(
  "id",
  "act_Gym",
  "act_Walk",
  "act_Run",
  "act_Yoga",
  "act_Other",
  "act_None",
  "act_Fishing",
  "act_Hunting",
  "act_VisitsMuseumsGaleries",
  "act_MotorizedOutdoorActivities",
  "act_Volunteering"
)

variables_style <- c(
  "id",
  "app_swag_Formel",
  "app_swag_Classique",
  "app_swag_Casual",
  "app_swag_Sport",
  "app_swag_Chic",
  "app_swag_HippBoheme",
  "app_swag_Punk",
  "app_swag_Rock",
  "app_swag_Other",
  "app_noTattoo",
  "animal_cat",
  "animal_dog",
  "animal_domestic",
  "animal_farm",
  "animal_noPet"
)

variables_sante <- c(
  "id",
  "cons_meat_never",
  "cons_meat_almost_never",
  "cons_meat_once_month",
  "cons_meat_once_week",
  "cons_meat_few_week",
  "cons_meat_daily",
  "cons_meat_few_daily",
  "cons_redWineDrink",
  "cons_whiteWineDrink",
  "cons_roseDrink",
  "cons_spiritDrink",
  "cons_bubbleDrink",
  "cons_beerDrink",
  "cons_microDrink",
  "cons_cocktailDrink",
  "cons_noDrink",
  "cons_Smoke_never",
  "cons_Smoke_few_times_year",
  "cons_Smoke_month",
  "cons_Smoke_once_week",
  "cons_Smoke_few_times_week",
  "cons_Smoke_once_day",
  "cons_Smoke_few_times_day"
)

variables_mode_de_vie <- c(
  "id",
  "postal_code", # À transformer en rural, urbain, région, banlieue
  "ses_dwelling_App",
  "ses_dwelling_Loft",
  "ses_dwelling_Condo",
  "ses_dwelling_Tour",
  "ses_dwelling_detachedHouse",
  "ses_dwelling_townHouse",
  "ses_dwelling_Coop",
  "ses_dwelling_HLM",
  "ses_dwelling_Mobile",
  "ses_dwelling_Other",
  "act_transport_Car",
  "act_transport_SUV",
  "act_transport_Moto",
  "act_transport_Walk",
  "act_transport_Bicycle",
  "act_transport_PublicTransportation",
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
  "vehicule_noCar"
)

variables_commerce <- c(
  "id",
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
  "cons_coffee_place_ind"
)

variables_ses <- c(
  "id",
  "male",
  "female",
  "age34m",
  "age3554",
  "age55p",
  "langEn",
  "langFr",
  "ses_languageOther",
  "educBHS",
  "educCollege",
  "educUniv",
  "ses_income_None",
  "ses_income_i1to30",
  "ses_income_i31to60",
  "ses_income_i61to90",
  "ses_income_i91to110",
  "ses_income_i111to150",
  "ses_income_i151to200",
  "ses_income_i201toInf",
  "immigrant",
  "ses_ethn_White",
  "ses_ethn_Black",
  "ses_ethn_Other",
  "ses_hetero",
  "ses_gai",
  "ses_bisex",
  "ses_sexOri_other"
)

variables_vote <- c(
  "id",
  "op_intent",
  "op_intent_CAQ",
  "op_intent_PQ",
  "op_intent_PLQ",
  "op_intent_QS",
  "op_intent_PCQ",
  "op_intent_Other"
)

variables_communes <- Reduce(union, list(
  variables_act,
  variables_style,
  variables_sante,
  variables_mode_de_vie,
  variables_commerce,
  variables_ses,
  variables_vote
))