#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 02 Préparation spécifique des données pour le clustering
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(dplyr)

# Déclaration des variables de clustering
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

variables_style_clust <- c(
  "id",
  "app_swag_Classique",
  "app_swag_Casual",
  "app_swag_Sport",
  "app_swag_Other",
  "app_withTattoo",
  "animal_cat",
  "animal_dog",
  "animal_other",
  "animal_noPet"
)

variables_sante_clust <- c(
  "id",
  "cons_Meat",
  "cons_redWineDrink",
  "cons_whiteWineDrink",
  "cons_roseDrink",
  "cons_spiritDrink",
  "cons_bubbleDrink",
  "cons_beerDrink",
  "cons_microDrink",
  "cons_cocktailDrink",
  "cons_noDrink",
  "cons_Smoke"
)

variables_mode_de_vie_clust <- c(
  "id",
  #"postal_code", # À transformer en rural, urbain, région, banlieue
  "ses_dwelling_App",
  "ses_dwelling_Condo",
  "ses_dwelling_detachedHouse",
  "ses_dwelling_townHouse",
  "ses_dwelling_Other",
  "act_transport_Car",
  "act_transport_SUV",
  "act_transport_Walk",
  "act_transport_Bicycle",
  "act_transport_PublicTransportation",
  "vehicule_ToutTerrain",
  "vehicule_Van",
  "vehicule_Voiture",
  "vehicule_electric",
  "vehicule_VUS",
  "vehicule_other",
  "vehicule_noCar"
)

variables_commerce_clust <- c(
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
  "cons_coffee_McDo",
  "cons_coffee_Other",
  "cons_coffee_place_ind"
)

variables_ses_clust <- c(
  "id",
  "male",
  "female",
  "age",
  "langEn",
  "langFr",
  "ses_languageOther",
  "educ",
  "ses_income",
  "immigrant",
  "ses_ethn_White",
  "ses_ethn_Black",
  "ses_ethn_Other",
  "ses_hetero",
  "ses_gai",
  "ses_bisex",
  "ses_sexOri_other"
)

variables_communes_clust <- Reduce(union, list(
  variables_act,
  variables_style_clust,
  variables_sante_clust,
  variables_mode_de_vie_clust,
  variables_commerce_clust,
  variables_ses_clust
))

# Assemblage des fichiers

df_pilot1_2022_act <- readRDS(file = "data/qc2022/preparation_donnees/02_pilot1_2022_act.rds")
df_pilot1_2022_style <- readRDS(file = "data/qc2022/preparation_donnees/02_pilot1_2022_style.rds")
df_pilot1_2022_sante <- readRDS(file = "data/qc2022/preparation_donnees/02_pilot1_2022_sante.rds")
df_pilot1_2022_mode_de_vie <- readRDS(file = "data/qc2022/preparation_donnees/02_pilot1_2022_mode_de_vie.rds")
df_pilot1_2022_commerce <- readRDS(file = "data/qc2022/preparation_donnees/02_pilot1_2022_commerce.rds")
df_pilot1_2022_ses <- readRDS(file = "data/qc2022/preparation_donnees/02_pilot1_2022_ses.rds")

df_pilot1_2022_clust <- df_pilot1_2022_act %>%
  left_join(df_pilot1_2022_style, by = "id") %>%
  left_join(df_pilot1_2022_sante, by = "id") %>%
  left_join(df_pilot1_2022_mode_de_vie, by = "id") %>%
  left_join(df_pilot1_2022_commerce, by = "id") %>%
  left_join(df_pilot1_2022_ses, by = "id")

saveRDS(df_pilot1_2022_clust, file = "data/qc2022/preparation_donnees/02_pilot1_2022.rds")