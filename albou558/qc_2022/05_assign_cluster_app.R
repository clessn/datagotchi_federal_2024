# Library ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(lubridate)

# 1. Load Data, Clustering Results and Scale Vectors ---------------------------

kmeans_result <- readRDS("_PrivateFolder_datagotchi_federal_2025/data/clustering/qc2022/03_pilot1_kmeans.rds")

appData <- readRDS("_PrivateFolder_datagotchi_federal_2025/data/clustering/qc2022/01_app_2022.rds")

# pour process_app_data
scale_origins <- readRDS("_PrivateFolder_datagotchi_federal_2025/data/clustering/qc2022/03_pilot1_scale_origins.rds")  # Moyennes
scale_ecarts_types  <- readRDS("_PrivateFolder_datagotchi_federal_2025/data/clustering/qc2022/03_pilot1_scale_ecarts_types.rds")  # Écarts-types

#ajouter fonction process_app_data 
process_app_data <- function(data, center_vals, scale_vals, 
  selected_vars = c(
    # variables_act
    "act_Gym", "act_Walk", "act_Run", "act_Yoga", "act_Other", "act_None",
    "act_Fishing", "act_Hunting", "act_VisitsMuseumsGaleries", "act_MotorizedOutdoorActivities", "act_Volunteering",
    
    # variables_style_clust
    "app_swag_Classique", "app_swag_Casual", "app_swag_Sport", "app_swag_Other", "app_withTattoo",
    "animal_cat", "animal_dog", "animal_other", "animal_noPet",
    
    # variables_sante_clust
    "cons_Meat", "cons_redWineDrink", "cons_whiteWineDrink", "cons_roseDrink",
    "cons_spiritDrink", "cons_bubbleDrink", "cons_beerDrink", "cons_microDrink",
    "cons_cocktailDrink", "cons_noDrink", "cons_Smoke",
    
    # variables_mode_de_vie_clust
    "ses_dwelling_App", "ses_dwelling_Condo", "ses_dwelling_detachedHouse",
    "ses_dwelling_townHouse", "ses_dwelling_Other",
    "act_transport_Car", "act_transport_SUV", "act_transport_Walk",
    "act_transport_Bicycle", "act_transport_PublicTransportation",
    "vehicule_ToutTerrain", "vehicule_Van", "vehicule_Voiture", "vehicule_electric",
    "vehicule_VUS", "vehicule_other", "vehicule_noCar",
    
    # variables_commerce_clust
    "cons_brand_MaR", "cons_brand_BInd", "cons_brand_ChainesB", "cons_brand_GSurf",
    "cons_brand_OnlineOnly", "cons_brand_Frip", "cons_brand_Other",
    "cons_coffee_TimH", "cons_coffee_Starbucks", "cons_coffee_McDo",
    "cons_coffee_Other", "cons_coffee_place_ind",
    
    # variables_ses_clust
    "male", "female", "age", "langEn", "langFr", "ses_languageOther", "educ",
    "ses_income", "immigrant", "ses_ethn_White", "ses_ethn_Black",
    "ses_ethn_Other", "ses_hetero", "ses_gai", "ses_bisex", "ses_sexOri_other"
  )) {
# 1) Sélectionner les colonnes spécifiées et supprimer les lignes avec des NA
data_selected <- data %>%
select(all_of(selected_vars)) %>%
drop_na()

# 2) Extraire la variable cible (ici vote_intent) qui ne sera pas normalisée
vote_int <- data_selected$vote_intent

# 3) Identifier les colonnes numériques à normaliser (toutes sauf vote_intent)
numeric_cols <- setdiff(selected_vars, "vote_intent")
data_numeric <- data_selected[, numeric_cols, drop = FALSE]

# 4) Normaliser en utilisant les paramètres fournis
data_scaled <- sweep(data_numeric, 2, center_vals[numeric_cols], FUN = "-")
data_scaled <- sweep(data_scaled, 2, scale_vals[numeric_cols], FUN = "/")

# 5) Réattacher la variable cible
data_scaled <- as.data.frame(data_scaled)
data_scaled$vote_intent <- vote_int

return(data_scaled)
}

assign_clusters <- function(data, centroids) {
  # Convertir en matrices
  data <- as.matrix(data)
  centroids <- as.matrix(centroids)
  
  # Initialiser la matrice des distances
  distances <- matrix(NA, nrow = nrow(data), ncol = nrow(centroids))
  
  # Calculer les distances euclidiennes
  for (k in seq_len(nrow(centroids))) {
    diff <- sweep(data, 2, centroids[k, ], FUN = "-")
    distances[, k] <- rowSums(diff^2)  # Somme des différences au carré
  }
  
  # Assigner le cluster le plus proche
  cluster_assignment <- apply(distances, 1, which.min)
  return(cluster_assignment)
}

# Fonction pour assigner les clusters à toutes les données de l'app
assign_clusters_to_app_data <- function(appData, scale_origins, scale_ecarts_types, centroids) {
  # 1. Préparer et normaliser les données de l'app
  app_data_processed <- process_app_data(appData, scale_origins, scale_ecarts_types)
  
  # 2. Exclure vote_intent pour le calcul des distances
  feature_cols <- setdiff(names(app_data_processed), "vote_intent")
  data_features <- app_data_processed[, feature_cols, drop = FALSE]
  centroids_features <- centroids[, feature_cols, drop = FALSE]
  
  # 3. Assigner chaque répondant au cluster le plus proche
  cluster_assignment <- assign_clusters(data_features, centroids_features)
  
  # 4. Ajouter les clusters aux données
  app_data_processed$cluster <- cluster_assignment
  
  return(app_data_processed)
}

# Appliquer la fonction à toutes les données de l'app
app_data_with_clusters <- assign_clusters_to_app_data(appData, scale_origins, scale_ecarts_types, centroids)
