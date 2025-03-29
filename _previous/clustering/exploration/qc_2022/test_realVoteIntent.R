# Library ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# 1. Load Clustering Results and Scale Vectors ---------------------------
orig_center <- readRDS("kmeans_orig_center.rds")
orig_scale  <- readRDS("kmeans_orig_scale.rds")

kmeans_result <- readRDS("_SharedFolder_datagotchi_federal_2024/clustering/data/kmeans_results2022.rds")

# 2. Load & Clean 2022 App Data -----------------------------------------
app2022 <- readRDS("/home/alexab/Dropbox/Ulaval/CLESSN/_SharedFolder_datagotchi-developpement/quebec_prov_2022/hub/data-hub-clean-2022-10-27_clean.rds") |> 
  rename(
    ses_genderOther    = genderOther,
    act_TeamSport      = act_Team,
    cons_regBeers      = cons_beerDrink,
    cons_cocktailsDrink = cons_cocktailDrink,
    ses_dwelling_app   = ses_dwelling_App,
    cons_coffee_place_noCoffee = cons_coffee_None,
    ses_ethn_Asiatique = ses_ethn_Asian
  ) |>
  mutate(
    # Income 
    incomeLow  = if_else(ses_income_None == 1 | ses_income_i1to30 == 1, 1, 0),
    incomeMid  = if_else(ses_income_i31to60 == 1 | ses_income_i61to90 == 1 | ses_income_i91to110 == 1, 1, 0),
    incomeHigh = if_else(ses_income_i111to150 == 1 | ses_income_i151to200 == 1, 1, 0),
    
    # Meat consumption 
    cons_low_Meat  = if_else(cons_meat_never == 1 | cons_meat_almost_never == 1 | cons_meat_once_month == 1, 1, 0),
    cons_mid_Meat  = if_else(cons_meat_once_week == 1 | cons_meat_few_week == 1, 1, 0),
    cons_much_Meat = if_else(cons_meat_daily == 1 | cons_meat_few_daily == 1, 1, 0)
  )

app_data <- app2022 |> 
  mutate(
    vote_intent = case_when(
      op_intent == "CAQ"          ~ "CAQ",
      op_intent == "PQ"           ~ "PQ",
      op_intent == "PLQ"          ~ "PLQ",
      op_intent == "QS"           ~ "QS",
      op_intent == "PCQ"          ~ "PCQ",
      op_intent == "Other"        ~ "Autre",
      op_intent == "Did not vote" ~ "NoVote",
      TRUE                        ~ NA_character_
    ),
    date = as.Date(time)
  ) |> 
  drop_na(vote_intent)  # Only keep rows with recognized vote_intent

# 3. Define Functions ----------------------------------------------------

# A) process_app_data() that scales with the original center/scale
process_app_data <- function(data, center_vals, scale_vals) {
  # 1) Select the same columns you used for K-means
  data_selected <- data %>%
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
    drop_na()  # Remove any incomplete rows
  
  # 2) Save vote_intent (do NOT scale)
  vote_int <- data_selected$vote_intent
  
  # 3) Prepare numeric columns (exclude vote_intent)
  numeric_cols <- setdiff(names(data_selected), "vote_intent")
  data_numeric <- data_selected[, numeric_cols, drop = FALSE]
  
  # 4) Scale using the original center/scale
  data_scaled <- sweep(data_numeric, 2, center_vals[numeric_cols], FUN = "-")
  data_scaled <- sweep(data_scaled, 2, scale_vals[numeric_cols], FUN = "/")
  
  # 5) Re-attach vote_intent
  data_scaled <- as.data.frame(data_scaled)
  data_scaled$vote_intent <- vote_int
  
  # Return the scaled dataframe (54 numeric cols + 1 factor/char col for vote)
  return(data_scaled)
}

# B) assign_clusters() pour la distance Euclidienne dans l'espace scaled
assign_clusters <- function(data, centroids) {
  # Convertir en matrices
  data <- as.matrix(data)
  centroids <- as.matrix(centroids)
  
  # Initialiser la matrice des distances
  distances <- matrix(NA, nrow = nrow(data), ncol = nrow(centroids))
  
  # Calculer les distances Euclidiennes
  for (k in seq_len(nrow(centroids))) {
    diff <- sweep(data, 2, centroids[k, ], FUN = "-")
    distances[, k] <- rowSums(diff^2)  # somme des différences au carré
  }
  
  # Assigner le cluster le plus proche
  cluster_assignment <- apply(distances, 1, which.min)
  return(cluster_assignment)
}

# 4. Tracking Each Day's Respondents -------------------------------------
app_data$date <- as.Date(app_data$date)
unique_dates <- sort(unique(app_data$date))
date_to_day_number <- data.frame(
  date = unique_dates,
  day  = seq_along(unique_dates)
)

app_data <- app_data %>%
  left_join(date_to_day_number, by = "date") %>%
  arrange(date)

all_assignments <- data.frame()

# Les centroids de votre K-means scaled
centroids <- as.data.frame(kmeans_result$centers)

for (current_day in date_to_day_number$day) {
  
  # Filtrer jusqu'au jour actuel (approche cumulative)
  data_day <- app_data %>%
    filter(day <= current_day)
  
  if (nrow(data_day) == 0) next
  
  # Process & scale les données de ce jour
  data_day_processed <- process_app_data(data_day, orig_center, orig_scale)
  if (nrow(data_day_processed) == 0) next
  
  # Exclure vote_intent pour le calcul des distances
  feature_cols <- setdiff(names(data_day_processed), "vote_intent")
  
  # Vérifier l'alignement des colonnes
  feature_cols <- intersect(feature_cols, names(centroids))
  data_features <- data_day_processed[, feature_cols, drop = FALSE]
  centroids_features <- centroids[, feature_cols, drop = FALSE]
  
  # Assigner chaque répondant au centroid le plus proche
  cluster_assignment <- assign_clusters(
    data_features,
    centroids_features
  )
  
  # Stocker le cluster et le jour
  data_day_processed$cluster <- cluster_assignment
  data_day_processed$day     <- current_day
  
  # Ajouter à all_assignments
  all_assignments <- bind_rows(all_assignments, data_day_processed)
}

# 'all_assignments' contient maintenant pour chaque répondant :
# - leur cluster
# - leur vote_intent réel
# - (plus toutes les autres colonnes pour référence)
# - l'index du jour (que vous pouvez interpréter comme "toutes les données jusqu'à ce jour" si cumulative)

# 5. Summaries & Visualization --------------------------------------------

# Définir les couleurs pour chaque parti
party_colors <- c(
  "CAQ"     = "#05d2e0",
  "PQ"      = "#0043FE",
  "PLQ"     = "#FF2806",
  "QS"      = "#f88808",
  "PCQ"     = "#311c68",
  "Autre"   = "#73F986",
  "NoVote"  = "#707373"
)

vote_distribution <- all_assignments %>%
  group_by(day, cluster, vote_intent) %>%
  summarise(num_respondents = n(), .groups = "drop") %>%
  group_by(day, cluster) %>%
  mutate(
    cluster_total = sum(num_respondents),
    percent = 100 * num_respondents / cluster_total
  )

# Vérification rapide
head(vote_distribution)

# Graphique avec couleurs personnalisées pour les partis
ggplot(vote_distribution, aes(x = day, y = percent, color = vote_intent)) +
  geom_line() +
  facet_wrap(~ cluster) +
  scale_color_manual(values = party_colors) +  # Appliquer les couleurs personnalisées
  labs(
    title = "Répartition des Votes Réels par Cluster au Fil du Temps",
    x = "Jour",
    y = "Vote Intent (%)"
  ) +
    clessnize::theme_clean_light() +
  theme(
    legend.position = "bottom",  # Positionner la légende en bas
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 10)
  )
