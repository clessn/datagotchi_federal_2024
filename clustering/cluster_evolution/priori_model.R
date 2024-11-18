
# Load packages ----------------------------------------------------------
library(nnet)

# Data -------------------------------------------------------------------
df_pilot_2021_merged <- read.csv("_SharedFolder_datagotchi_federal_2024/clustering/data/pilot2021_merged_clustering.csv")

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
    vo
  ) %>%
  drop_na()

Loader les données du pilote
Créer un modèle multinomial (avec package nnet) qui prédit vote_intent ~ SES + lifestyle
Enregistrer le modèle en RDS