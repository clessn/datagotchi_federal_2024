#------------------------------------------------------------------------------
# Préparer les données obtenue de l'application Datagotchi au cours de la
# campagne électorale fédérale 2021

# Étapes :
# 1- Association des données nettoyées et des données brutes
# 2- Nettoyées les données récupérées des données brutes
#------------------------------------------------------------------------------

### Étape 1 : Associer les données nettoyées et les données brutes

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

###### Assurance qualité des données ######
# ????? Comment on fait pour savoir que les lignes sont associées aux
# mêmes personnes : Pour le moment, les deux fichiers ont le même
# nombre de répondants, il est présumé que l'ordre est le même.

# Vérifions tout de même s'il y a des colonnes communes et,
# le cas échéant, vérifier que les valeurs sont les mêmes

# Identifier les colonnes communes
common_cols <- intersect(colnames(df_datagotchi_2021), colnames(df_datagotchi_2021_raw))
print(common_cols)

# N'utiliser que les colonnes qui ont des valeurs codées de la même façon
common_cols <- c("answers.music.genre", "answers.ethnicity")

# Vérifier si les colonnes communes ont les mêmes valeurs sur les mêmes lignes
same_values <- sapply(common_cols, function(col) all(df_datagotchi_2021[[col]] == df_datagotchi_2021_raw[[col]]))

# Résultat : TRUE si les valeurs sont identiques, FALSE sinon, pour chaque colonne commune
print(same_values)

# Comparer les colonnes communes et identifier les lignes différentes
diff_rows <- which(!apply(df_datagotchi_2021[common_cols] == df_datagotchi_2021_raw[common_cols], 1, all))

# Créer une table combinée pour voir les différences côte à côte
comparison_table <- data.frame(
  ligne = diff_rows,
  df_2021 = df_datagotchi_2021[diff_rows, common_cols, drop = FALSE],
  df_2021_raw = df_datagotchi_2021_raw[diff_rows, common_cols, drop = FALSE]
)

# Voir les différences
View(comparison_table)

###### ----> Pour ces deux variables, il y a deux lignes qui ne correspondent pas (inversées?)
###### ----> Faut-il retirer ces deux lignes?
###### ----> Est-ce que ça vaut la peine de pousser l'analyse plus loin? Vérifier les autres variables?

#######----------- Fin temporaire de l'analyse

# Il semble y avoir des variables qui ont été mal nettoyées,
# ne conserver que les variables qui ont un nettoyage de qualité
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

# Ne conserver que les données des répondants de plus de 18 ans
# qui ont répondu pour eux-mêmes
datagotchi_2021_merged <- datagotchi_2021_merged %>%
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

    saveRDS(app_datagotchi_clean, file = "data/app_datagotchi_clean.rds")
