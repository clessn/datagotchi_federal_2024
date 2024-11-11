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
df_datagotchi_2021 <- read.csv("/home/alexab/Dropbox/Ulaval/CLESSN/_SharedFolder_datagotchi-developpement/federal_can_2021/hub/DatagotchiHub-federal-2021-08-03-2022-.csv")

#variables de l'app 2021  
app2021 <- c("ses_gender", "ses_age", "ses_post", "ses_educ", "ses_income", "ses_prov", "ses_eth", "ses_orientation", "ses_born", "ses_dwelling", "ses_language", "act_exercise", "act_fishing", "act_visitMuseumsGaleries", "act_performingArts", "act_motorizedOutdoorActivities", "act_volunteering", "act_holidays", "act_transport", "cons_tryCultRecipes", "cons_brand", "cons_meat", "cons_getCoffee", "cons_smoke", "cons_drink", "pref_music", "pref_movie", "appearance_style", "appearance_tattoo") 

varTransport <- c("act_transport_Car", "act_transport_SUV", "act_transport_Moto", "act_transport_Walk", "act_transport_Bicycle", "act_transport_PublicTransportation", "act_transport_Taxi")     
varAge <- c("age34m", "age3554", "age55p")
varLang <- c("langFr", "langEn", "ses_languageOther")
varGender <- c("male", "female", "ses_genderOther")
varIncome <- c("incomeLow", "incomeHigh")
varACT <- c("act_VisitsMuseumsGaleries", "act_Fishing", "act_Hunting", "act_MotorizedOutdoorActivities", "act_Volunteering", "act_Walk", "act_Gym", "act_TeamSport", "act_Run", "act_Yoga", "act_None", "act_Other")
varSwag <- c("app_swag_Formel", "app_swag_Classique", "app_swag_Casual", "app_swag_Sport", "app_swag_Chic", "app_swag_VintageHippBoheme", "app_swag_Other", "app_swag_Rock")
varOrientation <- c("ses_hetero", "ses_gai", "ses_bisex")
varConsBrand <- c("cons_brand_MaR", "cons_brand_BInd", "cons_brand_OnlineOnly", "cons_brand_ChainesB", "cons_brand_GSurf", "cons_brand_Frip", "cons_brand_Other")
varCoffeePlace <- c("cons_coffee_place_noCoffee", "cons_coffee_TimH", "cons_coffee_Other", "cons_coffee_Starbucks", "cons_coffee_SC", "cons_coffee_McDo", "cons_coffee_place_ind")
varBouffe <- c("cons_Meat", "cons_Vege", "cons_Vegan")
ses_dwelling <- c("ses_dwelling_ap", "ses_dwelling_loft", "ses_dwelling_condo", "ses_dwelling_tour", "ses_dwelling_detachedHouse", "ses_dwelling_towhouse", "ses_dwelling_semiDetached", "ses_dwelling_coop", "ses_dwelling_HLM", "ses_dwelling_mobile", "ses_dwelling_other")
cons_drink <- c("cons_noDrink", "cons_redWineDrink", "cons_whiteWineDrink", "cons_roseDrink", "cons_sparklingDrink", "cons_regBeers", "cons_microBeers", "cons_spiritDrink", "cons_cocktailsDrink")
cons_nicotine <- c("cons_Smoke", "cons_SmokeStopping", "cons_SmokeStopped", "cons_SmokeNever", "cons_VapeNation")
educ_level <- c("educBHS", "educHS")

df_pilot1_2021$

# Clustering -------------------------------------------------------------

# Sélection des variables numériques
data_num <- df_pilot1_2021 |> 
  select("app_noTattoo", "immigrant", varTransport, varAge, varLang, varGender, varIncome, varACT, varSwag, varOrientation,
varConsBrand, varCoffeePlace, varBouffe, ses_dwelling, cons_drink, cons_nicotine, educ_level
  ) |> drop_na()
"act_transport_Car", "cons_whiteWineDrink"
data_num_reduit <- df_pilot1_2021 |> 
  select("immigrant", "act_transport_PublicTransportation", "act_transport_Car", "ses_dwelling_house", "age34m", "age55p", varLang, "female", "incomeHigh",
 "act_VisitsMuseumsGaleries", "act_Fishing", "act_Hunting", "act_MotorizedOutdoorActivities", "act_Volunteering", "act_Gym", "cons_coffee_McDo",
 "cons_coffee_place_noCoffee", "cons_coffee_TimH", varBouffe, "cons_regBeers", "cons_redWineDrink", "cons_noDrink", "educHS"
  ) |> drop_na() 

# corr_matrix

cor_matrix <- cor(x = data_num_reduit)

ggcorrplot::ggcorrplot(
  cor_matrix,
  method = "circle",
  hc.order = TRUE,
  type = "upper",
  outline.color = "#00000000",
  colors = c("#d4206b", "#ececec", "#20d48f")
)


# PCA -------------------------------------------------------------

pca_result <- prcomp(
  data_num_reduit,
  scale = TRUE
)

summary(pca_result)

### Environ 3-4 composantes font l'affaire

pca_result$rotation[,1:4]

fviz_eig(pca_result, addlabels = TRUE)

# Interpret dimensions ---------------------------------------------------
fviz_pca_var(
  pca_result,
  col.var = "contrib", # Color by contributions to the PC
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  axes = c(1, 2),
  repel = TRUE
  )

# Normalisation des données (optionnelle, recommandé pour k-means)
data_num_scaled <- scale(data_num_reduit)

# Trouver le nombre de clusters idéal ------------------------------------------

### Coude
wss <- sapply(2:15, function(k){
  kmeans(data_num_scaled, centers = k, nstart = 25)$tot.withinss
})

# Tracer la courbe du coude
plot(2:15, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Nombre de clusters K",
     ylab = "Somme des carrés intra-cluster (withinss)",
     main = "Méthode du coude pour déterminer K")


# Calculer l'indice de silhouette pour différents k
sil_width <- sapply(2:15, function(k){
  km.res <- kmeans(data_num_scaled, centers = k, nstart = 25)
  ss <- cluster::silhouette(km.res$cluster, dist(data_num_scaled))
  mean(ss[, 3])
})

# Tracer la courbe de l'indice de silhouette
plot(2:15, sil_width, type = "b", pch = 19, frame = FALSE,
     xlab = "Nombre de clusters K",
     ylab = "Largeur moyenne de la silhouette",
     main = "Indice de silhouette pour déterminer K")

wss_scaled <- (wss - min(wss)) / (max(wss) - min(wss))
sil_width_scaled <- (sil_width - min(sil_width)) / (max(sil_width) - min(sil_width))

wss_scaled_rev <- rev(wss_scaled)
sil_sum <- wss_scaled_rev + sil_width_scaled

plot(2:15, sil_sum, type = "b")

### 7, 8 or 10 clusters

for (i in c(3, 4, 5, 6, 7, 8, 9, 10)) {
  # Appliquer k-means avec un nombre de clusters k (à définir, ici k = 3)
  set.seed(123)  # Pour rendre les résultats reproductibles
  kmeans_result <- kmeans(data_num_scaled, centers = i, nstart = 25)
  # Ajouter les clusters aux données d'origine
  data_num_reduit[[paste0("cluster_", i)]] <- kmeans_result$cluster
}

table(data_num_reduit$cluster_3)
table(data_num_reduit$cluster_4)
table(data_num_reduit$cluster_5)
table(data_num_reduit$cluster_6)
table(data_num_reduit$cluster_7)
table(data_num_reduit$cluster_9)
table(data_num_reduit$cluster_10)

# Visualisation des clusters sur 2 dimensions ------------------------------------------------

# Calcul de la distance (ici, distance euclidienne)
distance_matrix <- dist(data_num_scaled)

# MDS pour réduction à 2 dimensions
mds_result <- cmdscale(distance_matrix, k = 2)

# Ajouter les coordonnées MDS aux données
data_num_reduit$MDS1 <- mds_result[,1]
data_num_reduit$MDS2 <- mds_result[,2]

plot_data <- data_num_reduit |> 
  tidyr::pivot_longer(
    cols = starts_with("cluster_"),
    names_to = "n_clusters",
    values_to = "cluster",
    names_prefix = "cluster_"
  ) |> 
  mutate(n_clusters = as.numeric(n_clusters))

ggplot(plot_data, aes(x = MDS1, y = MDS2, color = factor(cluster))) +
  geom_point(alpha = 0.3) +
  clessnize::theme_clean_light() +
  facet_wrap(~n_clusters) +
  stat_ellipse()

### 10 clusters me semble être le parfait équilibre où il y a une distinction claire
#### entre les clusters et où chaque cluster couvre un espace significatif

## Checker rapidement la % de variance expliquée par les 2 dimensions
km_res <- kmeans(data_num_scaled, centers = 8, nstart = 25)
fviz_cluster(
  km_res, data = data_num_scaled,
  geom = "point",
  ellipse.type = "convex", 
  ggtheme = theme_bw()
  )

# Description des clusters -----------------------------------------------

#### Function
describe_clusters <- function(data, variables_to_describe, cluster_var){
  data$cluster_var <- data[[cluster_var]]
  ### variables to dummy
  non_numeric_vars <- variables_to_describe[!sapply(data[,variables_to_describe], is.numeric)]
  if (!purrr::is_empty(non_numeric_vars)) {
    df_dummy_only <- data |> 
      select(all_of(non_numeric_vars)) |> 
      fastDummies::dummy_columns(
        select_columns = non_numeric_vars,
        omit_colname_prefix = TRUE,
        remove_selected_columns = TRUE
      ) |> 
      janitor::clean_names()
    variables_to_describe <- c(variables_to_describe[!variables_to_describe %in% non_numeric_vars], names(df_dummy_only))
    df_description <- cbind(data, df_dummy_only) |> 
      tidyr::pivot_longer(
        cols = all_of(variables_to_describe),
        names_to = "variable",
        values_to = "value"
      )
  } else {
      df_description <- data |> 
        tidyr::pivot_longer(
          cols = all_of(variables_to_describe),
          names_to = "variable",
          values_to = "value"
        )
  }
  df_mean_all <- df_description |> 
    group_by(variable) |> 
    summarise(
      mean_value = mean(value),
      sd_value = sd(value)
    )
  df_mean_by_cluster <- df_description |> 
    group_by(cluster_var, variable) |> 
    summarise(mean_cluster = mean(value)) %>% 
    left_join(
      ., df_mean_all, by = "variable"
    ) |> 
    mutate(z_score = (mean_cluster - mean_value) / sd_value)
  return(df_mean_by_cluster)
}

variables_to_describe <- c(
"immigrant",
"act_transport_PublicTransportation",
"act_transport_Car",
"ses_dwelling_house",
"age34m",
"age55p",
"langFr",
"langEn",
"ses_languageOther",
"female",
"incomeHigh",
"act_VisitsMuseumsGaleries",
"act_Fishing",
"act_Hunting",
"act_MotorizedOutdoorActivities",
"act_Volunteering",
"act_Gym",
"cons_coffee_McDo",
"cons_coffee_place_noCoffee",
"cons_coffee_TimH",
"cons_Meat",
"cons_Vege",
"cons_Vegan",
"cons_regBeers",
"cons_redWineDrink",
"cons_noDrink",
"educHS" 
)

describe_clusters(
  data_num_reduit,
  variables_to_describe = variables_to_describe,
  cluster_var = "cluster_5"
) |> print(n = 271) 




