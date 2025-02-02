

# S'assurer d'avoir déjà le data_prior de 4-2022_model 

#> table(data_prior$vote_intent)
#   CAQ  Autre NoVote    PCQ    PLQ     PQ     QS 
#   523     22     64    149    148    102    188

# Charger le modèle initial et les centroids
model0 <- readRDS("_SharedFolder_datagotchi_federal_2024/clustering/data/multinom_model_2022.rds")
kmeans_result <- readRDS("_SharedFolder_datagotchi_federal_2024/clustering/data/kmeans_results2022.rds")



app2022 <- readRDS("/home/alexab/Dropbox/Ulaval/CLESSN/_SharedFolder_datagotchi-developpement/quebec_prov_2022/hub/data-hub-clean-2022-10-27_clean.rds") |> 
  rename(ses_genderOther = genderOther,
         act_TeamSport = act_Team,
         cons_regBeers = cons_beerDrink,
         cons_cocktailsDrink = cons_cocktailDrink,
         ses_dwelling_app = ses_dwelling_App,
         cons_coffee_place_noCoffee = cons_coffee_None,
         ses_ethn_Asiatique = ses_ethn_Asian) %>%
  
         # Créer les variables de revenu
         mutate(
           incomeLow = if_else(ses_income_None == 1 | ses_income_i1to30 == 1, 1, 0),
           incomeMid = if_else(ses_income_i31to60 == 1 | ses_income_i61to90 == 1 | ses_income_i91to110 == 1, 1, 0),
           incomeHigh = if_else(ses_income_i111to150 == 1 | ses_income_i151to200 == 1, 1, 0)
         ) %>%
  
         # Créer les variables de consommation de viande
         mutate(
           cons_low_Meat = if_else(cons_meat_never == 1 | cons_meat_almost_never == 1 | cons_meat_once_month == 1, 1, 0),
           cons_mid_Meat = if_else(cons_meat_once_week == 1 | cons_meat_few_week == 1, 1, 0),
           cons_much_Meat = if_else(cons_meat_daily == 1 | cons_meat_few_daily == 1, 1, 0)
         )
       

#> table(app_data$op_intent)
#  CAQ           PQ          PLQ           QS          PCQ 
#  9514         7019         3661        25401         4100 
# Other Did not vote 
# 1247     1031 

app_data <- app2022 |> 
  mutate(
    vote_intent = case_when(
      op_intent == "CAQ" ~ "CAQ",
      op_intent == "PQ" ~ "PQ",
      op_intent == "PLQ" ~ "PLQ",
      op_intent == "QS" ~ "QS",
      op_intent == "PCQ" ~ "PCQ",
      op_intent == "Other" ~ "Autre",
      op_intent == "Did not vote" ~ "NoVote",
      TRUE ~ NA_character_  # Exclure les autres valeurs
    ),
    date = as.Date(time)
  ) |> 
    drop_na(vote_intent)

# Fonction pour traiter les données de l'application
process_app_data <- function(data) {
  data %>%
    select(
      vote_intent,
      male,
      female,
      ses_genderOther,
      age34m,
      age3554,
      age55p,
      langEn,
      langFr,
      ses_languageOther,
      act_Gym,
      act_TeamSport,
      act_Run,
      act_Yoga,
      act_None,
      act_Fishing,
      act_Hunting,
      act_VisitsMuseumsGaleries,
      act_MotorizedOutdoorActivities,
      act_Volunteering,
      animal_cat,
      animal_dog,
      cons_brand_MaR,
      cons_brand_Frip,
      educBHS,
      educCollege,
      educUniv,
      cons_redWineDrink,
      cons_regBeers,
      cons_cocktailsDrink,
      cons_noDrink,
      incomeLow,
      incomeMid,
      incomeHigh,
      ses_dwelling_app,
      ses_dwelling_detachedHouse,
      act_transport_Car,
      act_transport_Walk,
      act_transport_PublicTransportation,
      vehicule_PickUp,
      vehicule_noCar,
      immigrant,
      cons_coffee_TimH,
      cons_coffee_Starbucks,
      cons_coffee_place_noCoffee,
      app_noTattoo,
      cons_low_Meat,
      cons_mid_Meat,
      cons_much_Meat,
      ses_ethn_White,
      ses_ethn_Black,
      ses_ethn_Asiatique,
      ses_hetero,
      ses_gai,
      ses_bisex
    ) %>%
    mutate(
      vote_intent = as.factor(vote_intent)
    ) %>%
    drop_na()
}

# Préparer les données initiales pour le modèle
data_model_prior <- data_prior %>%
  select(
    vote_intent,
    male,
    female,
    ses_genderOther,
    age34m,
    age3554,
    age55p,
    langEn,
    langFr,
    ses_languageOther,
    act_Gym,
    act_TeamSport,
    act_Run,
    act_Yoga,
    act_None,
    act_Fishing,
    act_Hunting,
    act_VisitsMuseumsGaleries,
    act_MotorizedOutdoorActivities,
    act_Volunteering,
    animal_cat,
    animal_dog,
    cons_brand_MaR,
    cons_brand_Frip,
    educBHS,
    educCollege,
    educUniv,
    cons_redWineDrink,
    cons_regBeers,
    cons_cocktailsDrink,
    cons_noDrink,
    incomeLow,
    incomeMid,
    incomeHigh,
    ses_dwelling_app,
    ses_dwelling_detachedHouse,
    act_transport_Car,
    act_transport_Walk,
    act_transport_PublicTransportation,
    vehicule_PickUp,
    vehicule_noCar,
    immigrant,
    cons_coffee_TimH,
    cons_coffee_Starbucks,
    cons_coffee_place_noCoffee,
    app_noTattoo,
    cons_low_Meat,
    cons_mid_Meat,
    cons_much_Meat,
    ses_ethn_White,
    ses_ethn_Black,
    ses_ethn_Asiatique,
    ses_hetero,
    ses_gai,
    ses_bisex
  ) 

# Initialiser la dataframe pour les résultats
results_df <- data.frame()

# Convertir les dates en numéros de jour
app_data$date <- as.Date(app_data$time)
unique_dates <- sort(unique(app_data$date))
date_to_day_number <- data.frame(date = unique_dates, day = 1:length(unique_dates))

# Joindre les numéros de jour à app_data
app_data <- app_data %>% left_join(date_to_day_number, by = "date")

# Boucle sur chaque jour
for (current_day in date_to_day_number$day) {
  # Extraire les données du jour courant
  data_day <- app_data %>% filter(day <= current_day)
  
  # Passer au jour suivant si aucune donnée
  if (nrow(data_day) == 0) {
    next
  }
  
  # Traiter les données du jour
  data_day_processed <- process_app_data(data_day)
  
  # Passer au jour suivant si aucune donnée après traitement
  if (nrow(data_day_processed) == 0) {
    next
  }
  
  # Combiner avec les données initiales
  data_model_i <- bind_rows(data_model_prior, data_day_processed)
  
  # S’assurer que les variables sont les mêmes
  data_model_i <- data_model_i %>%
    select(names(data_model_prior))
  
  # Construire le modèle :
  # Pour le premier jour, on utilise le modèle initial (model0).
  # Pour les jours suivants, on construit un nouveau modèle avec les données jusqu'à ce jour.
  if (current_day == 1) {
    model_i <- model0
  } else {
    model_i <- multinom(vote_intent ~ ., data = data_model_i)
  }
  
  # Extraire les centroids
  centroids <- as.data.frame(kmeans_result$centers)
  
  # S’assurer que les variables correspondent
  predictor_vars <- colnames(data_model_i)[colnames(data_model_i) != "vote_intent"]
  centroids <- centroids[, predictor_vars, drop = FALSE]
  
  # Prédire les probabilités pour chaque centroid
  probs <- predict(model_i, newdata = centroids, type = "probs")
  
  # Convertir les probabilités en dataframe
  probs_df <- as.data.frame(probs)
  
  # Ajouter le numéro de cluster et le jour
  probs_df$cluster <- 1:nrow(probs_df)
  probs_df$day <- current_day
  
  # Réorganiser les colonnes
  probs_df <- probs_df %>%
    select(cluster, day, everything())
  
  # Ajouter aux résultats
  results_df <- bind_rows(results_df, probs_df)
}

saveRDS(results_df, file = "_SharedFolder_datagotchi_federal_2024/clustering/data/2022_pred_centroid.rds")
