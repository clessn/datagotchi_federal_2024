# Library ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(lubridate)

# 1. Load centroids ---------------------------
kmeans_result <- readRDS("_PrivateFolder_datagotchi_federal_2025/data/clustering/qc2022/03_pilot1_kmeans.rds")

# Extraire les centres du kmeans (votre variable centroids)
centroids <- kmeans_result$centers

# Load Data
appData_clust <- readRDS("_PrivateFolder_datagotchi_federal_2025/data/clustering/qc2022/02_app_2022.rds")


# Load moyennes et écarts-types
scale_origins <- readRDS("_PrivateFolder_datagotchi_federal_2025/data/clustering/qc2022/03_pilot1_scale_origins.rds")
scale_ecarts_types  <- readRDS("_PrivateFolder_datagotchi_federal_2025/data/clustering/qc2022/03_pilot1_scale_ecarts_types.rds")


# Fonction pour process les données
fct_05_process_app_data <- function(data, scale_origins, scale_ecarts_types, id_var = "id", 
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
# 1) Si la colonne id existe, on la conserve
if(id_var %in% names(data)){
data_selected <- data %>% 
select(all_of(c(id_var, selected_vars))) %>% 
drop_na()
id_values <- data_selected[[id_var]]
# On retire id pour la normalisation
data_numeric <- data_selected %>% select(-all_of(id_var))
} else {
data_selected <- data %>% 
select(all_of(selected_vars)) %>% 
drop_na()
id_values <- NULL
data_numeric <- data_selected
}

# 2) Normalisation en utilisant les paramètres fournis
# Assurez-vous que les noms des colonnes de data_numeric correspondent à ceux de scale_origins et scale_ecarts_types
data_scaled <- sweep(data_numeric, 2, scale_origins[names(data_numeric)], FUN = "-")
data_scaled <- sweep(data_scaled, 2, scale_ecarts_types[names(data_numeric)], FUN = "/")

data_scaled <- as.data.frame(data_scaled)

# 3) Réattacher la colonne id (si elle existait)
if(!is.null(id_values)){
data_scaled[[id_var]] <- id_values
}

return(data_scaled)
}



# 3. Fonction d'assignation des clusters -------------------------------------
fct_05_assign_clusters <- function(data, centroids, id_var = "id") {
  # Sélectionner les variables pour le calcul (exclure id s'il existe)
  if(id_var %in% names(data)){
    data_features <- data %>% select(-all_of(id_var))
  } else {
    data_features <- data
  }
  
  # Conversion en matrices
  data_mat <- as.matrix(data_features)
  
  # S'assurer que les centroïdes utilisent les mêmes colonnes que data_features
  centroids_features <- centroids[, colnames(data_features), drop = FALSE]
  centroids_mat <- as.matrix(centroids_features)
  
  # Calculer les distances euclidiennes
  distances <- matrix(NA, nrow = nrow(data_mat), ncol = nrow(centroids_mat))
  for (k in seq_len(nrow(centroids_mat))) {
    diff <- sweep(data_mat, 2, centroids_mat[k, ], FUN = "-")
    distances[, k] <- rowSums(diff^2)
  }
  
  # Assigner le cluster le plus proche à chaque observation
  cluster_assignment <- apply(distances, 1, which.min)
  
  # Ajouter la colonne cluster au jeu de données original (qui contient id)
  data$cluster <- cluster_assignment
  
  return(data)
}


# Traitement (normalisation) des données en conservant la colonne id
data_scaled <- fct_05_process_app_data(appData_clust, scale_origins, scale_ecarts_types, id_var = "id")

# Assignation des clusters et ajout de la colonne cluster (id est conservé)
data_with_clusters <- fct_05_assign_clusters(data_scaled, centroids, id_var = "id")

saveRDS(data_with_clusters, file = "_PrivateFolder_datagotchi_federal_2025/data/clustering/qc2022/05_app_2022_clustered.rds")


