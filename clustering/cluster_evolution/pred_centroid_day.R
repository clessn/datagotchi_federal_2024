# Charger les packages nécessaires
library(nnet)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# Charger les données de base
df_pilot_2021_merged <- read.csv("_SharedFolder_datagotchi_federal_2024/clustering/data/pilot2021_merged_clustering_qc.csv") %>%
  select(
    educBHS,
    educHS,
    educUniv,
    incomeLow,
    incomeMid,
    incomeHigh,
    ses_hetero,
    ses_gai,
    ses_sexOri_other,
immigrant,
male,
age34m,
age3554,
age55p,
langFr,
langEn,
ses_languageOther,
act_transport_Car,
act_transport_Walk,
act_transport_PublicTransportation,
act_Gym,
act_TeamSport,
act_Run,
act_Yoga,
act_None,
act_Fishing,
act_Hunting,
act_VisitsMuseumsGaleries,
act_MotorizedOutdoorActivities,
cons_brand_MaR,
cons_brand_ChainesB,
cons_brand_GSurf,
cons_brand_Frip,
cons_Meat,
cons_Vege,
cons_Vegan,
cons_coffee_TimH,
cons_coffee_Starbucks,
cons_coffee_McDo,
cons_coffee_place_noCoffee,
cons_Smoke,
cons_SmokeStopped,
cons_SmokeNever,
cons_noDrink,
cons_redWineDrink,
cons_regBeers,
cons_microBeers,
cons_cocktailsDrink,
app_noTattoo,
ses_dwelling_app,
ses_dwelling_condo,
ses_dwelling_detachedHouse,
op_voteIntent_Lib, op_voteIntent_Cons, op_voteIntent_Ndp, op_voteIntent_Bloc, op_voteIntent_Green   
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


# vestige d'un test pan-canadien -----------------------------------------

# Créer une liste des variables de province
#province_vars <- c("quebec", "ontario", "alberta", "british_colombia", "manitoba", "saskatchewan",
#                   "pei", "new_brunswick", "nova_scotia", "new_foundland", "yukon", "nunavut", "northwest_territories")
#
# Calculer le nombre de répondants par jour et par province
#obs_per_day_province <- app_data %>%
#  select(date, all_of(province_vars)) %>%
#  pivot_longer(cols = all_of(province_vars), names_to = "province", values_to = "value") %>%
#  filter(value == 1) %>%
#  group_by(date, province) %>%
#  summarise(num_obs = n(), .groups = 'drop')
#
# 1. Graphique du nombre total d'observations par jour
# Étape 1 : Calculer le nombre total d'observations par jour
#obs_per_day <- obs_per_day_province %>%
#  group_by(date) %>%
#  summarise(total_obs = sum(num_obs))
#
# Étape 2 : Créer le graphique
#ggplot(obs_per_day, aes(x = date, y = total_obs)) +
#  geom_line(color = "blue") +
#  geom_point(color = "red") +
#  labs(title = "Nombre total d'observations par jour",
#       x = "Date",
#       y = "Nombre d'observations") +
#  theme_minimal() +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. Graphique du nombre total d'observations par province
# Étape 1 : Calculer le nombre total d'observations par province
#obs_per_province <- obs_per_day_province %>%
#  group_by(province) %>%
#  summarise(total_obs = sum(num_obs))

# Étape 2 : Créer le graphique
#ggplot(obs_per_province, aes(x = reorder(province, -total_obs), y = total_obs)) +
#  geom_bar(stat = "identity", fill = "skyblue") +
#  labs(title = "Nombre total d'observations par province",
#       x = "Province",
#       y = "Nombre d'observations") +
#  theme_minimal() +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Fonction pour traiter les données de l'application
process_app_data <- function(data) {
  data %>%
    select(
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
      educUniv, educBHS, educHS,
      age55p, age34m, age3554,
      male,
      ses_hetero, ses_gai,
      langEn, langFr, ses_languageOther,
      incomeHigh, incomeLow, incomeMid,
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
    educUniv, educBHS, educHS,
    age55p, age34m, age3554,
    male,
    ses_hetero, ses_gai,
    langEn, langFr, ses_languageOther,
    incomeHigh, incomeLow, incomeMid,
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
  data_day <- app_data %>% filter(day <= current_day)
  
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
  probs <- marginaleffects::predictions(model_i, newdata = centroids, type = "probs")
  
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

results_df |> 
  tidyr::pivot_longer(
    cols = c("Bloc", "Conservative", "Green", "Liberal", "NDP"),
    names_to = "party",
    values_to = "prob"
  ) |>
  ggplot(aes(x = day, y = prob)) +
  facet_wrap(~cluster) +
  geom_line(aes(group = party, color = party)) +
  scale_x_continuous(limits = c(1, 10))














