# Packages ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)
library(tidyr)
library(ggcorrplot)

# Data -------------------------------------------------------------------
df_pilot1_2021 <- read.csv("/home/alexab/Dropbox/Ulaval/CLESSN/_SharedFolder_datagotchi-developpement/federal_can_2021/pilotes/pilote-1-federal-2021.csv")
df_pilot2_2021 <- read.csv("/home/alexab/Dropbox/Ulaval/CLESSN/_SharedFolder_datagotchi-developpement/federal_can_2021/pilotes/pilote-2-federal-2021.csv")
df_datagotchi_2021 <- read.csv("/home/alexab/Dropbox/Ulaval/CLESSN/_SharedFolder_datagotchi-developpement/federal_can_2021/hub/DatagotchiHub-federal-2021-08-03-2022-.csv")

## identifier les variables d'interet presentes dans les 2 pilots + app
variables_interet <- c(
  "act_transport_Car", "act_transport_SUV", "act_transport_Moto", "act_transport_Walk", 
  "act_transport_Bicycle", "act_transport_PublicTransportation", "act_transport_Taxi",
  "age34m", "age3554", "age55p",
  "langFr", "langEn", "ses_languageOther",
  "male", "female", "ses_genderOther",
  "incomeLow", "incomeHigh",
  "immigrant",
  "act_VisitsMuseumsGaleries", "act_Fishing", "act_Hunting", 
  "act_MotorizedOutdoorActivities", "act_Volunteering", "act_Walk", 
  "act_Gym", "act_TeamSport", "act_Run", "act_Yoga", "act_None", "act_Other",
  "app_swag_Formel", "app_swag_Classique", "app_swag_Casual", "app_swag_Sport", 
  "app_swag_Chic", "app_swag_VintageHippBoheme", "app_swag_Other", "app_swag_Rock",
  "app_noTattoo",
  "ses_hetero", "ses_gai", "ses_bisex",
  "cons_brand_MaR", "cons_brand_BInd", "cons_brand_OnlineOnly", 
  "cons_brand_ChainesB", "cons_brand_GSurf", "cons_brand_Frip", "cons_brand_Other",
  "cons_coffee_place_noCoffee", "cons_coffee_TimH", "cons_coffee_Other", 
  "cons_coffee_Starbucks", "cons_coffee_SC", "cons_coffee_McDo", "cons_coffee_place_ind",
  "cons_Meat", "cons_Vege", "cons_Vegan",
  "ses_dwelling_app", "ses_dwelling_loft", "ses_dwelling_condo", 
  "ses_dwelling_tour", "ses_dwelling_detachedHouse", 
  "ses_dwelling_semiDetached", "ses_dwelling_coop", "ses_dwelling_HLM", 
  "ses_dwelling_mobile", "ses_dwelling_other",
  "cons_noDrink", "cons_redWineDrink", "cons_whiteWineDrink", "cons_roseDrink", 
  "cons_sparklingDrink", "cons_regBeers", "cons_microBeers", 
  "cons_spiritDrink", "cons_cocktailsDrink",
  "cons_Smoke", "cons_SmokeStopping", "cons_SmokeStopped", 
  "cons_SmokeNever", "cons_VapeNation",
  "educBHS", "educUniv",
  "op_voteIntent_Lib", "op_voteIntent_Cons", "op_voteIntent_Ndp", "op_voteIntent_Bloc", "op_voteIntent_Green", "op_voteIntent_PPC",
  "op_voteIntent_NoVote"
)

## Sélectionner les colonnes d'intérêt dans chaque dataset
df_pilot1_2021 <- df_pilot1_2021 %>%
  select(any_of(variables_interet))

df_pilot2_2021 <- df_pilot2_2021 %>%
  select(any_of(variables_interet))


## Ajouter une colonne d'identification pour chaque pilot
df_pilot1_2021$source <- "pilot1"
df_pilot2_2021$source <- "pilot2"

## Fusionner les deux datasets en conservant toutes les observations
df_pilot_2021_merged <- bind_rows(df_pilot1_2021, df_pilot2_2021)

# Enregistrer le jeux de donnees merged

# Enregistrer le jeu de données fusionné sous forme de CSV
write.csv(df_pilot_2021_merged, "/home/alexab/Dropbox/Ulaval/CLESSN/datagotchi_federal_2024/_SharedFolder_datagotchi_federal_2024/clustering/data/pilot2021_merged_clustering.csv")
