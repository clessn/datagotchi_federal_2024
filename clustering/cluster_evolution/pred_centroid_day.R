# Charger les packages nécessaires
library(nnet)
library(dplyr)
library(tidyr)
library(lubridate)  # Pour manipuler les dates

# Charger les données de base
df_pilot_2021_merged <- read.csv("_SharedFolder_datagotchi_federal_2024/clustering/data/pilot2021_merged_clustering.csv") %>%
  select(
    act_VisitsMuseumsGaleries, act_Volunteering, act_Yoga, act_Run, act_Gym, act_MotorizedOutdoorActivities, act_None,
    app_noTattoo, app_swag_Casual, app_swag_VintageHippBoheme,
    cons_regBeers, cons_cocktailsDrink, cons_microBeers, cons_redWineDrink, cons_noDrink,
    cons_brand_ChainesB, cons_brand_GSurf, cons_brand_MaR, cons_brand_Frip,
    cons_coffee_Starbucks, cons_coffee_place_noCoffee, cons_coffee_TimH,
    cons_Meat, cons_Vege,
    cons_SmokeNever, cons_Smoke,
    immigrant, 
    educUniv, educBHS,
    age55p, age34m,
    male,
    ses_hetero, ses_gai,
    langEn, langFr, ses_languageOther,
    incomeHigh, incomeLow,
    ses_dwelling_condo, ses_dwelling_detachedHouse, ses_dwelling_app,
    act_transport_PublicTransportation, act_transport_Car, act_transport_Walk,
    op_voteIntent_Lib, op_voteIntent_Cons, op_voteIntent_Ndp, op_voteIntent_Bloc,
    op_voteIntent_Green
    # Exclure op_voteIntent_PPC et op_voteIntent_NoVote
  ) %>%
  drop_na()

# Préparer les données de base
data_prior <- df_pilot_2021_merged %>%
  mutate(
    vote_intent = case_when(
      op_voteIntent_Lib == 1 ~ "Liberal",
      op_voteIntent_Cons == 1 ~ "Conservative",
      op_voteIntent_Ndp == 1 ~ "NDP",
      op_voteIntent_Bloc == 1 ~ "Bloc",
      op_voteIntent_Green == 1 ~ "Green",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-starts_with("op_voteIntent_")) %>% 
  drop_na(vote_intent) %>%
  mutate(
    vote_intent = as.factor(vote_intent)
  )

# Charger le modèle initial et les centroids
model0 <- readRDS("_SharedFolder_datagotchi_federal_2024/clustering/data/multinom_model.rds")
kmeans_result <- readRDS("_SharedFolder_datagotchi_federal_2024/clustering/data/kmeans_results8.rds")

# Charger et préparer les données de l'application
app_data <- readRDS("_SharedFolder_datagotchi_federal_2024/clustering/data/app_datagotchi_clean.rds") %>%
  mutate(
    vote_intent = case_when(
      vote_intent == "bloc" ~ "Bloc",
      vote_intent == "bq" ~ "Bloc",
      vote_intent == "pcc" ~ "Conservative",
      vote_intent == "vert" ~ "Green",
      vote_intent == "npd" ~ "NDP",
      vote_intent == "plc" ~ "Liberal",
      TRUE ~ NA_character_  # Exclure les autres valeurs
    ),
    date = as.Date(created)
  ) %>%
  drop_na(vote_intent)  # Supprimer les lignes avec vote_intent manquant

# Fonction pour traiter les données de l'application
process_app_data <- function(data) {
  data %>%
    select(
      act_VisitsMuseumsGaleries, act_Volunteering, act_Yoga, act_Run, act_Gym, act_MotorizedOutdoorActivities, act_None,
      app_noTattoo, app_swag_Casual, app_swag_VintageHippBoheme,
      cons_regBeers, cons_cocktailsDrink, cons_microBeers, cons_redWineDrink, cons_noDrink,
      cons_brand_ChainesB, cons_brand_GSurf, cons_brand_MaR, cons_brand_Frip,
      cons_coffee_Starbucks, cons_coffee_place_noCoffee, cons_coffee_TimH,
      cons_Meat, cons_Vege,
      cons_SmokeNever, cons_Smoke,
      immigrant, 
      educUniv, educBHS,
      age55p, age34m,
      male,
      ses_hetero, ses_gai,
      langEn, langFr, ses_languageOther,
      incomeHigh, incomeLow,
      ses_dwelling_condo, ses_dwelling_detachedHouse, ses_dwelling_app,
      act_transport_PublicTransportation, act_transport_Car, act_transport_Walk,
      vote_intent
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
    act_VisitsMuseumsGaleries, act_Volunteering, act_Yoga, act_Run, act_Gym, act_MotorizedOutdoorActivities, act_None,
    app_noTattoo, app_swag_Casual, app_swag_VintageHippBoheme,
    cons_regBeers, cons_cocktailsDrink, cons_microBeers, cons_redWineDrink, cons_noDrink,
    cons_brand_ChainesB, cons_brand_GSurf, cons_brand_MaR, cons_brand_Frip,
    cons_coffee_Starbucks, cons_coffee_place_noCoffee, cons_coffee_TimH,
    cons_Meat, cons_Vege,
    cons_SmokeNever, cons_Smoke,
    immigrant, 
    educUniv, educBHS,
    age55p, age34m,
    male,
    ses_hetero, ses_gai,
    langEn, langFr, ses_languageOther,
    incomeHigh, incomeLow,
    ses_dwelling_condo, ses_dwelling_detachedHouse, ses_dwelling_app,
    act_transport_PublicTransportation, act_transport_Car, act_transport_Walk
  )

# Initialiser la dataframe pour les résultats
results_df <- data.frame()

# Convertir les dates en numéros de jour
app_data$date <- as.Date(app_data$created)
unique_dates <- sort(unique(app_data$date))
date_to_day_number <- data.frame(date = unique_dates, day = 1:length(unique_dates))

# Joindre les numéros de jour à app_data
app_data <- app_data %>% left_join(date_to_day_number, by = "date")

# Boucle sur chaque jour
for (current_day in date_to_day_number$day) {
  # Extraire les données du jour courant
  data_day <- app_data %>% filter(day == current_day)
  
  # Passer au jour suivant si aucune donnée
  if(nrow(data_day) == 0) {
    next
  }
  
  # Traiter les données du jour
  data_day_processed <- process_app_data(data_day)
  
  # Passer au jour suivant si aucune donnée après traitement
  if(nrow(data_day_processed) == 0) {
    next
  }
  
  # Combiner avec les données initiales
  data_model_i <- bind_rows(data_model_prior, data_day_processed)
  
  # S'assurer que les variables sont les mêmes
  data_model_i <- data_model_i %>%
    select(names(data_model_prior))
  
  # Construire le modèle multinomial
  model_i <- multinom(vote_intent ~ ., data = data_model_i)
  
  # Extraire les centroids
  centroids <- as.data.frame(kmeans_result$centers)
  
  # S'assurer que les variables correspondent
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

saveRDS(results_df, file = "_SharedFolder_datagotchi_federal_2024/clustering/data/pred_centroid.rds")

# Afficher les premiers résultats
head(results_df)















