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


## identifier les variables d'interet presentes dans les 2 pilots + app
variables_interet <- c(
  "quebec",
  "educBHS",
  "educHS",
  "educUniv",
  "incomeLow",
  "incomeMid",
  "incomeHigh",
  "ses_hetero",
  "ses_gai",
  "ses_bisex",
  "ses_sexOri_other",
  "immigrant",
  "male",
  "female",
  "ses_genderOther",
  "age34m",
  "age3554",
  "age55p",
  "langFr",
  "langEn",
  "ses_languageOther",
  "act_transport_Car",
  "act_transport_SUV",
  "act_transport_Moto",
  "act_transport_Walk",
  "act_transport_Bicycle",
  "act_transport_PublicTransportation",
  "act_Walk",
  "act_Gym",
  "act_TeamSport",
  "act_Run",
  "act_Yoga",
  "act_Swimming",
  "act_Other",
  "act_None",
  "act_Fishing",
  "act_Hunting",
  "act_VisitsMuseumsGaleries",
  "act_MotorizedOutdoorActivities",
  "act_Volunteering",
  "cons_brand_MaR",
  "cons_brand_OnlineOnly",
  "cons_brand_BInd",
  "cons_brand_ChainesB",
  "cons_brand_GSurf",
  "cons_brand_Frip",
  "cons_brand_Other",
  "cons_Meat",
  "cons_Vege",
  "cons_Vegan",
  "cons_coffee_TimH",
  "cons_coffee_Other",
  "cons_coffee_Starbucks",
  "cons_coffee_SC",
  "cons_coffee_McDo",
  "cons_coffee_place_ind",
  "cons_coffee_place_noCoffee",
  "cons_Smoke",
  "cons_SmokeStopping",
  "cons_SmokeStopped",
  "cons_SmokeNever",
  "cons_VapeNation",
  "cons_noDrink",
  "cons_redWineDrink",
  "cons_whiteWineDrink",
  "cons_roseDrink",
  "cons_sparklingDrink",
  "cons_regBeers",
  "cons_microBeers",
  "cons_spiritDrink",
  "cons_cocktailsDrink",
  #"cons_microDrink", n'existe pas dans pilot2
  #"cons_bubbleDrink", n'existe pas dans pilot2
  "app_swag_Formel",
  "app_swag_Classique",
  "app_swag_Casual",
  "app_swag_Sport",
  "app_swag_Chic",
  "app_swag_VintageHippBoheme",
  "app_swag_Other",
  "app_swag_Rock",
  "app_noTattoo",
  "op_voteIntent_Lib",
  "op_voteIntent_Cons",
  "op_voteIntent_Ndp",
  "op_voteIntent_Bloc",
  "op_voteIntent_Green",
  "op_voteIntent_PPC",
  "op_voteIntent_NoVote",
  #"op_voteIntent_Other", n'existe pas dans pilot2
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
  "ses_dwelling_house"
)

## Sélectionner les colonnes d'intérêt dans chaque dataset
df_pilot1_2021 <- df_pilot1_2021 %>%
  select(any_of(variables_interet))

df_pilot2_2021 <- df_pilot2_2021 %>%
  select(any_of(variables_interet))

#> table(df_pilot1_2021$quebec)
#
#  0   1 
# 859 892 
#> table(df_pilot2_2021$quebec)
#
#   0    1 
# 1192  408 

## Ajouter une colonne d'identification pour chaque pilot
df_pilot1_2021$source <- "pilot1"
df_pilot2_2021$source <- "pilot2"

## Fusionner les deux datasets en conservant toutes les observations
df_pilot_2021_merged <- bind_rows(df_pilot1_2021, df_pilot2_2021)

# Filter the merged dataset to include only respondents from Quebec
df_pilot_2021_merged_qc <- df_pilot_2021_merged %>%
  filter(quebec == 1)

# Enregistrer le jeux de donnees merged

# Enregistrer le jeu de données fusionné sous forme de CSV
write.csv(df_pilot_2021_merged_qc, "/home/alexab/Dropbox/Ulaval/CLESSN/datagotchi_federal_2024/_SharedFolder_datagotchi_federal_2024/clustering/data/pilot2021_merged_clustering_qc.csv")



# Merging App Datagotchi -------------------------------------------------

df_datagotchi_2021 <- read.csv("/home/alexab/Dropbox/Ulaval/CLESSN/_SharedFolder_datagotchi-developpement/federal_can_2021/hub/DatagotchiHub-federal-2021-08-03-2022-.csv") |> 
  filter(quebec == 1) |> 
  select(vote_intent,
    act_VisitsMuseumsGaleries,
    act_Yoga, act_Run, act_Gym, act_TeamSport, act_None,
    act_MotorizedOutdoorActivities, act_Fishing, act_Hunting, 
    app_noTattoo,
    cons_brand_ChainesB, cons_brand_GSurf, cons_brand_MaR, cons_brand_Frip,
    cons_coffee_Starbucks, cons_coffee_place_noCoffee, cons_coffee_TimH, cons_coffee_McDo, 
    cons_Meat, cons_Vege, cons_Vegan,
    cons_Smoke,
    cons_SmokeStopped,
    cons_SmokeNever,
    cons_noDrink,
    cons_redWineDrink,
    cons_regBeers,
    cons_microBeers,
    cons_cocktailsDrink,
    immigrant, 
    educUniv, educBHS,
    age55p, age34m, #age3554,
    male,
    ses_hetero, #ses_gai,
    langEn, langFr, ses_languageOther,
    #incomeHigh, incomeLow, #incomeMid,
    ses_dwelling_condo, ses_dwelling_detachedHouse, ses_dwelling_app,
    act_transport_PublicTransportation, act_transport_Car, act_transport_Walk
    )

df_datagotchi_raw <- read.csv("/home/alexab/Dropbox/Ulaval/CLESSN/_SharedFolder_bav-2021/Data/Raw/RawData-Hub.csv") 

raw_selected <- df_datagotchi_raw |> 
  filter(answers.province == "Québec") |> 
  select(created, is_self, answers.age, correction, answers.sexual_orientation, answers.education, answers.income,
    prediction.1.name, prediction.1.value, prediction.2.name, prediction.2.value, prediction.3.name, prediction.3.value,
  prediction.4.name, prediction.4.value, prediction.5.name, prediction.5.value)

datagotchi_merged <- df_datagotchi_2021 %>% 
   cbind(., raw_selected) |> 
   filter(answers.age >= 18, is_self == TRUE)

datagotchi_merged<- datagotchi_merged %>%
  mutate(age3554 = ifelse(answers.age %in% c(34, 54), 1, 0))

  
  datagotchi_merged <- datagotchi_merged %>%
    mutate(
      incomeLow = ifelse(answers.income %in% c("Aucun revenu", "1 $ à 30 000 $"), 1, 0),
      incomeMid = ifelse(answers.income %in% c("30 001 $ à 60 000 $", "60 001 $ à 90 000 $"), 1, 0),
      incomeHigh = ifelse(answers.income %in% c("90 001 $ à 110 000 $", 
                                                "110 001 $ à 150 000 $", 
                                                "150 001 $ à 200 000 $", 
                                                "Plus de 200 000 $"), 1, 0)
    )

datagotchi_merged <- datagotchi_merged %>%
    mutate(ses_gai = ifelse(answers.sexual_orientation == "Gai ou lesbienne", 1, 0))

# Étape 1 : Convertir les colonnes de valeurs de prédiction en numériques si nécessaire
prediction_value_cols <- c('prediction.1.value', 'prediction.2.value', 'prediction.3.value', 'prediction.4.value', 'prediction.5.value')

datagotchi_merged[prediction_value_cols] <- lapply(datagotchi_merged[prediction_value_cols], function(x) as.numeric(as.character(x)))

# Étape 2 : Trouver les indices des lignes où correction est -1
bad_rows <- which(datagotchi_merged$correction == -1)

# Étape 3 : Boucle sur chaque ligne identifiée pour mettre à jour 'vote_intent'
for (row in bad_rows) {
    # Extraire les valeurs de prédiction pour la ligne courante
    prediction_values <- as.numeric(datagotchi_merged[row, prediction_value_cols])
    
    # Trouver l'indice de la valeur maximale
    max_index <- which.max(prediction_values)
    
    # Construire le nom de la colonne 'prediction.x.name' correspondante
    prediction_name_col <- paste0('prediction.', max_index, '.name')
    
    # Obtenir le nom du parti correspondant
    party <- as.character(datagotchi_merged[row, prediction_name_col])
    
    # Assigner le nom du parti à 'vote_intent'
    datagotchi_merged$vote_intent[row] <- party
}

# Étape 4 : Remplacer les codes numériques dans 'vote_intent' par les noms des partis
# Définir la correspondance entre les codes numériques et les noms des partis
party_mapping <- c("1" = "bloc", "2" = "pcc", "3" = "vert", "4" = "plc", "5" = "npd")

# Convertir 'vote_intent' en caractère si nécessaire
datagotchi_merged$vote_intent <- as.character(datagotchi_merged$vote_intent)

# Remplacer les codes numériques par les noms des partis
datagotchi_merged$vote_intent <- ifelse(datagotchi_merged$vote_intent %in% names(party_mapping),
                                 party_mapping[datagotchi_merged$vote_intent],
                                 datagotchi_merged$vote_intent)

app_datagotchi_clean <- datagotchi_merged |>
  select(created, vote_intent,
    act_VisitsMuseumsGaleries,
    act_Yoga, act_Run, act_Gym, act_TeamSport, act_None,
    act_MotorizedOutdoorActivities, act_Fishing, act_Hunting, 
    app_noTattoo,
    cons_brand_ChainesB, cons_brand_GSurf, cons_brand_MaR, cons_brand_Frip,
    cons_coffee_Starbucks, cons_coffee_place_noCoffee, cons_coffee_TimH, cons_coffee_McDo, 
    cons_Meat, cons_Vege, cons_Vegan,
    cons_Smoke,
    cons_SmokeStopped,
    cons_SmokeNever,
    cons_noDrink,
    cons_redWineDrink,
    cons_regBeers,
    cons_microBeers,
    cons_cocktailsDrink,
    immigrant, 
    educUniv, educBHS,
    age55p, age34m, age3554,
    male,
    ses_hetero, ses_gai,
    langEn, langFr, ses_languageOther,
    incomeHigh, incomeLow, incomeMid,
    ses_dwelling_condo, ses_dwelling_detachedHouse, ses_dwelling_app,
    act_transport_PublicTransportation, act_transport_Car, act_transport_Walk)

    saveRDS(app_datagotchi_clean, file = "_SharedFolder_datagotchi_federal_2024/clustering/data/app_datagotchi_clean.rds")
