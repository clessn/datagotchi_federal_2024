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

postal_code	ses_age	male	female	ses_genderOther	age34m	age3554	age55p	langEn	langFr	ses_languageOther	act_Gym	act_TeamSport	act_Walk	act_Run	act_Yoga	act_Swimming	act_Other	act_None
act_Fishing	act_Hunting	act_VisitsMuseumsGaleries	act_MotorizedOutdoorActivities	act_Volunteering

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


