# Charger les packages nécessaires
library(nnet)
library(dplyr)
library(tidyr)

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
    op_voteIntent_Green, op_voteIntent_PPC, op_voteIntent_NoVote
  ) %>%
  drop_na()

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
      TRUE ~ vote_intent
    ),
    date = as.Date(created)
  )

# Fonction pour traiter les données
process_data <- function(data) {
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
      op_voteIntent_Lib, op_voteIntent_Cons, op_voteIntent_Ndp, op_voteIntent_Bloc,
      op_voteIntent_Green, op_voteIntent_PPC, op_voteIntent_NoVote
    ) %>%
    drop_na() %>%
    mutate(
      vote_intent = case_when(
        op_voteIntent_Lib == 1 ~ "Liberal",
        op_voteIntent_Cons == 1 ~ "Conservative",
        op_voteIntent_Ndp == 1 ~ "NDP",
        op_voteIntent_Bloc == 1 ~ "Bloc",
        op_voteIntent_Green == 1 ~ "Green",
        op_voteIntent_PPC == 1 ~ "PPC",
        op_voteIntent_NoVote == 1 ~ "NoVote",
        TRUE ~ NA_character_
      )
    ) %>%
    select(-starts_with("op_voteIntent_")) %>% 
    drop_na(vote_intent) %>%
    mutate(
      vote_intent = as.factor(vote_intent)
    )
}

# Préparer les données initiales pour le modèle
data_prior <- process_data(df_pilot_2021_merged)

# Initialiser la dataframe pour les résultats
results_df <- data.frame()

# Obtenir la liste des dates
date_list <- sort(unique(app_data$date))

# Boucle sur chaque jour
for (current_date in date_list) {
  # Extraire les données du jour courant
  data_day <- app_data %>% filter(date == current_date)
  
  # Passer au jour suivant si aucune donnée
  if(nrow(data_day) == 0) {
    next
  }
  
  # Traiter les données du jour
  data_day_processed <- process_data(data_day)
  
  # Passer au jour suivant si aucune donnée après traitement
  if(nrow(data_day_processed) == 0) {
    next
  }
  
  # Combiner avec les données initiales
  data_model_i <- bind_rows(data_prior, data_day_processed)
  
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
  
  # Ajouter le numéro de cluster et la date
  probs_df$cluster <- 1:nrow(probs_df)
  probs_df$date <- current_date
  
  # Réorganiser les colonnes
  probs_df <- probs_df %>%
    select(cluster, date, everything())
  
  # Ajouter aux résultats
  results_df <- bind_rows(results_df, probs_df)
}

# Afficher les premiers résultats
head(results_df)















