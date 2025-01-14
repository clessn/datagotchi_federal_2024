#------------------------------------------------------------------------------
# Préparer les données obtenue de l'application Datagotchi au cours de la
# campagne électorale fédérale 2021

# Étapes :
# 1- Association des données nettoyées et des données brutes
# 2- Nettoyées les données récupérées des données brutes
#------------------------------------------------------------------------------

### Étape 1 : Associer les données nettoyées et les données brutes
###----------------------------------------------------------------------------

# Lecture du fichier contenant les données nettoyées de l'application
df_datagotchi_2021 <- read.csv(
  "data/DatagotchiHub-federal-2021-08-03-2022-.csv"
)

# Ne conserver que les réponses du Québec (comme pour les pilotes)
df_datagotchi_2021 <- df_datagotchi_2021 %>%
  filter(quebec == 1)

# Lecture du fichier contenant les réponses brutes provenant de l'application
df_datagotchi_2021_raw <- read.csv("data/RawData-Hub.csv")

# Ne conserver que les réponses du Québec (comme pour les pilotes)
df_datagotchi_2021_raw <- df_datagotchi_2021_raw |>
  filter(answers.province == "Québec")

# Retirer les lignes qui ne sont pas concordantes - Voir 1.x-verification.R
common_cols <- c("answers.music.genre", "answers.ethnicity")
diff_rows <- which(!apply(df_datagotchi_2021[common_cols] ==
                          df_datagotchi_2021_raw[common_cols], 1, all))
df_datagotchi_2021 <- df_datagotchi_2021[-diff_rows, ]
df_datagotchi_2021_raw <- df_datagotchi_2021_raw[-diff_rows, ]

# Ne conserver que les variables qui ont un nettoyage de qualité
df_datagotchi_2021 <- df_datagotchi_2021 %>%
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

# Conserver les variables brutes à ajouter aux données déjà nettoyées
df_datagotchi_2021_raw <- df_datagotchi_2021_raw %>%
  select(created,
    is_self,
    answers.age,
    correction,
    answers.sexual_orientation,
    answers.education,
    answers.income,
    prediction.1.name, prediction.1.value,
    prediction.2.name, prediction.2.value,
    prediction.3.name, prediction.3.value,
    prediction.4.name, prediction.4.value,
    prediction.5.name, prediction.5.value
  )

# Associer les colonnes nettoyées aux colonnes des données brutes
datagotchi_2021_merged <- df_datagotchi_2021 %>%
  cbind(., df_datagotchi_2021_raw)

### Étape 2 : Création des variables binaires manquantes
###----------------------------------------------------------------------------

# Ne conserver que les données des répondants de plus de 18 ans
# qui ont répondu pour eux-mêmes
datagotchi_2021_merged <- datagotchi_2021_merged %>%
  filter(answers.age >= 18, is_self == TRUE)

# Creation du groupe d'age 34_54
datagotchi_2021_merged<- datagotchi_2021_merged %>%
  mutate(age3554 = ifelse(answers.age %in% c(34, 54), 1, 0))

# Création des variables binaires de salaires
datagotchi_2021_merged <- datagotchi_2021_merged %>%
  mutate(
    incomeLow = ifelse(answers.income %in% c("Aucun revenu", "1 $ à 30 000 $"), 1, 0),
    incomeMid = ifelse(answers.income %in% c("30 001 $ à 60 000 $", "60 001 $ à 90 000 $"), 1, 0),
    incomeHigh = ifelse(answers.income %in% c("90 001 $ à 110 000 $", 
                                              "110 001 $ à 150 000 $", 
                                              "150 001 $ à 200 000 $", 
                                              "Plus de 200 000 $"), 1, 0)
    )

# Creation de la variable binaire gai
datagotchi_2021_merged <- datagotchi_2021_merged %>%
    mutate(ses_gai = ifelse(answers.sexual_orientation == "Gai ou lesbienne", 1, 0))

### Étape 3 : Création des variables relatives au vote
###----------------------------------------------------------------------------

# Étape 3.1 : Convertir les colonnes de valeurs de prédiction en numériques si nécessaire
prediction_value_cols <- c('prediction.1.value', 'prediction.2.value', 'prediction.3.value', 'prediction.4.value', 'prediction.5.value')

datagotchi_2021_merged[prediction_value_cols] <- lapply(datagotchi_2021_merged[prediction_value_cols], function(x) as.numeric(as.character(x)))

# Étape 3.2 : Trouver les indices des lignes où correction est -1
##### ??? Que signifie la correction à -1?
bad_rows <- which(datagotchi_2021_merged$correction == -1)
datagotchi_2021_merged$vote_intent_original <- datagotchi_2021_merged$vote_intent

# Étape 3.3 : Boucle sur chaque ligne identifiée pour mettre à jour 'vote_intent'
##### ??? Pourquoi vote_intent est affecté à la valeur du parti dont la prédiction
#####     de vote est la plus haute ? (lorsqu'il n'y a pas de vote_intent)
for (row in bad_rows) {

    # Extraire les valeurs de prédiction pour la ligne courante
    prediction_values <- as.numeric(datagotchi_2021_merged[row, prediction_value_cols])
    
    # Trouver l'indice de la valeur maximale de prédiction
    max_index <- which.max(prediction_values)
    
    # Construire le nom de la colonne 'prediction.x.name' correspondante à la valeur maximale
    prediction_name_col <- paste0('prediction.', max_index, '.name')
    
    # Obtenir le nom du parti correspondant 
    party <- as.character(datagotchi_2021_merged[row, prediction_name_col])
    
    # Assigner le nom du parti à 'vote_intent'
    datagotchi_2021_merged$vote_intent[row] <- party
}

# Étape 3.4 : Remplacer les codes numériques dans 'vote_intent' par les noms des partis
# Définir la correspondance entre les codes numériques et les noms des partis
party_mapping <- c("1" = "bloc", "2" = "pcc", "3" = "vert", "4" = "plc", "5" = "npd")

# Convertir 'vote_intent' en caractère si nécessaire
datagotchi_2021_merged$vote_intent <- as.character(datagotchi_2021_merged$vote_intent)

# Remplacer les codes numériques par les noms des partis
datagotchi_2021_merged$vote_intent <- ifelse(datagotchi_2021_merged$vote_intent %in% names(party_mapping),
                                 party_mapping[datagotchi_2021_merged$vote_intent],
                                 datagotchi_2021_merged$vote_intent)

app_datagotchi_clean <- datagotchi_2021_merged |>
  select(created, vote_intent, vote_intent_original,
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

saveRDS(app_datagotchi_clean, file = "data/extrant/app_datagotchi_clean_qconly.rds")
