# Packages ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)
library(tidyr)
library(ggcorrplot)
library(tibble)
library(gridExtra)


# Data -------------------------------------------------------------------

df_pilot1_2022 <- read.csv("/home/alexab/Dropbox/Ulaval/CLESSN/_SharedFolder_datagotchi-developpement/quebec_prov_2022/pilotes/pilote-1-quebec-prov-2022.csv")


variables_int <- c(
  # "id",
  # "postal_code",
  # "ses_age",
   "male",
   "female",
   "ses_genderOther",
   "age34m",
   "age3554",
   "age55p",
   "langEn",
   "langFr",
   "ses_languageOther",
   "act_Gym",
   "act_TeamSport",
  # "act_Walk",
   "act_Run",
   "act_Yoga",
  # "act_Swimming",
  # "act_Other",
   "act_None",
  # "answers.sport",
   "act_Fishing",
   "act_Hunting",
   "act_VisitsMuseumsGaleries",
   "act_MotorizedOutdoorActivities",
   "act_Volunteering",
   "animal_cat",
   "animal_dog",
  # "animal_domestic",
  # "animal_farm",
  # "animal_noPet",
  # "answers.pets",
   "cons_brand_MaR",
  # "cons_brand_BInd",
  # "cons_brand_ChainesB",
  # "cons_brand_GSurf",
  # "cons_brand_OnlineOnly",
   "cons_brand_Frip",
  # "cons_brand_Other",
  # "answers.shopping",
   "educBHS",
   "educCollege",
   "educUniv",
   "cons_redWineDrink",
  # "cons_whiteWineDrink",
  # "cons_roseDrink",
  # "cons_sparklingDrink",
   "cons_regBeers",
  # "cons_microBeers",
  # "cons_spiritDrink",
   "cons_cocktailsDrink",
   "cons_noDrink",
  # "answers.alcohol",
  # "ses_income_None",
  # "ses_income_i1to30",
  # "ses_income_i31to60",
  # "ses_income_i61to90",
  # "ses_income_i91to110",
  # "ses_income_i111to150",
  # "ses_income_i151to200",
  # "ses_income_i201toInf",
  # "ses_income_no_answer",
   "incomeLow",
   "incomeMid",
   "incomeHigh",
  # "parent_outside",
  # "parent_canada",
  # "parent_no_answer",
   "ses_dwelling_app",
  # "ses_dwelling_loft",
  # "ses_dwelling_condo",
  # "ses_dwelling_tour",
   "ses_dwelling_detachedHouse",
  # "ses_dwelling_townHouse",
  # "ses_dwelling_semiDetached",
  # "ses_dwelling_coop",
  # "ses_dwelling_HLM",
  # "ses_dwelling_mobile",
  # "ses_dwelling_other",
  # "answers.dwelling",
  # "cons_Smoke_never",
  # "cons_Smoke_few_times_year",
  # "cons_Smoke_month",
  # "cons_Smoke_once_week",
  # "cons_Smoke_few_times_week",
  # "cons_Smoke_once_day",
  # "cons_Smoke_few_times_day",
  # "answers.smoke",
   "act_transport_Car",
  # "act_transport_SUV",
  # "act_transport_Moto",
   "act_transport_Walk",
  # "act_transport_Bicycle",
   "act_transport_PublicTransportation",
  # "act_transport",
  # "answers.transport",
  # "vehicule_4x4",
  # "vehicule_Berline",
  # "vehicule_Cabriolet",
   "vehicule_PickUp",
  # "vehicule_Van",
  # "vehicule_luxury",
  # "vehicule_sport",
  # "vehicule_electric",
  # "vehicule_VUS",
  # "vehicule_other",
   "vehicule_noCar",
  # "act_modelCar",
  # "answers.vehicule",
  # "turnout_odds",
  # "op_intent",
  # "op_intent_CAQ",
  # "op_intent_PQ",
  # "op_intent_PLQ",
  # "op_intent_QS",
  # "op_intent_PCQ",
  # "op_intent_Other",
  # "op_intent_dontKnow",
  # "op_intent_wontVote",
  # "op_potentialG_CAQ",
  # "op_potentialG_PLQ",
  # "op_potentialG_PQ",
  # "op_potentialG_QS",
  # "op_potentialG_PCQ",
  # "op_voted_2018",
  # "party_id_caquiste",
  # "party_id_lib",
  # "party_id_pequiste",
  # "party_id_solidaire",
  # "party_id_cons",
  # "party_id_vert",
  # "party_id_another",
  # "party_id_none",
  # "party_id_DK",
   "immigrant",
   "cons_coffee_TimH",
   "cons_coffee_Starbucks",
  # "cons_coffee_SC",
  # "cons_coffee_McDo",
  # "cons_coffee_Other",
  # "cons_coffee_place_ind",
   "cons_coffee_place_noCoffee",
  # "answers.coffee_shop",
  # "app_swag_Formel",
  # "app_swag_Classique",
  # "app_swag_Casual",
  # "app_swag_Sport",
  # "app_swag_Chic",
  # "app_swag_HippBoheme",
  # "app_swag_Punk",
  # "app_swag_Rock",
  # "app_swag_Other",
  # "answers.clothing",
   "app_noTattoo",
  # "cons_meat_never",
  # "cons_meat_almost_never",
  # "cons_meat_once_month",
  # "cons_meat_once_week",
  # "cons_meat_few_week",
  # "cons_meat_daily",
  # "cons_meat_few_daily",
  # "answers.food",
   "cons_low_Meat",
   "cons_mid_Meat",
   "cons_much_Meat",
   "ses_ethn_White",
   "ses_ethn_Black",
  # "ses_ethn_Aboriginals",
   "ses_ethn_Asiatique",
  # "ses_ethn_Hispanique",
  # "ses_ethn_Arabe",
  # "ses_ethn_Other",
   "ses_hetero",
   "ses_gai",
   "ses_bisex"#,
  # "ses_sexOri_other"#,
 )

# Préparer les données
data_filtered <- df_pilot1_2022 %>%
  select(all_of(variables_int)) %>%
  drop_na()

pca_result0 <- prcomp(
  data_filtered,
  scale = TRUE
)
pca_result0$rotation[,1:4]

fviz_eig(pca_result0, addlabels = TRUE)



# Effectuer la PCA
pca_all <- prcomp(data_filtered, scale. = TRUE)
var_contrib <- factoextra::get_pca_var(pca_all)$contrib

# Nombre d'axes principaux à considérer
num_axes <- 4

# Préparer les contributions par axe sans groupement
all_contrib_dodge <- as.data.frame(var_contrib[, 1:num_axes])
all_contrib_dodge$Variable <- rownames(all_contrib_dodge)

# Réorganiser les données pour ggplot2
all_contrib_dodge <- all_contrib_dodge %>%
  pivot_longer(
    cols = starts_with("Dim"),
    names_to = "Axe",
    values_to = "Contribution"
  )

# Graphique 2 : Contributions des variables par axe sans groupement
ggplot(all_contrib_dodge, aes(x = reorder(Variable, Contribution), y = Contribution, fill = Axe)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  coord_flip() +
  labs(
    title = "Contributions des variables par axe",
    x = "Variables",
    y = "Contribution (%)"
  ) +
  scale_fill_brewer(palette = "Set2", name = "Axes principaux") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 7)
  )

# Clustering -------------------------------------------------------------

## Normalisation des données (optionnelle, recommandé pour k-means)
data_scaled <- scale(data_filtered)


## Checker rapidement la % de variance expliquée par les 2 dimensions
km_res <- kmeans(data_scaled, centers = 11, nstart = 25)
fviz_cluster(
  km_res, data = data_scaled,
  geom = "point",
  ellipse.type = "convex", 
  ggtheme = theme_bw()
  )

#Trouver le nombre de clusters idéal ------------------------------------------

### Coude
wss <- sapply(2:40, function(k){
  kmeans(data_scaled, centers = k, nstart = 25)$tot.withinss
})

# Tracer la courbe du coude
plot(2:40, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Nombre de clusters K",
     ylab = "Somme des carrés intra-cluster (withinss)",
     main = "Méthode du coude pour déterminer K")


# Calculer l'indice de silhouette pour différents k
sil_width <- sapply(2:40, function(k){
  km.res <- kmeans(data_scaled, centers = k, nstart = 25)
  ss <- cluster::silhouette(km.res$cluster, dist(data_scaled))
  mean(ss[, 3])
})

# Tracer la courbe de l'indice de silhouette
plot(2:40, sil_width, type = "b", pch = 19, frame = FALSE,
     xlab = "Nombre de clusters K",
     ylab = "Largeur moyenne de la silhouette",
     main = "Indice de silhouette pour déterminer K")

wss_scaled <- (wss - min(wss)) / (max(wss) - min(wss))
sil_width_scaled <- (sil_width - min(sil_width)) / (max(sil_width) - min(sil_width))

wss_scaled_rev <- rev(wss_scaled)
sil_sum <- wss_scaled_rev + sil_width_scaled

plot(2:40, sil_sum, type = "b")

#K = 4 est un compromis raisonnable entre les deux méthodes.
#La méthode elbow pointe vers ce nombre comme un coude clair,
#et bien que la méthode silhouette favorise initialement K = 2,
#elle reste relativement élevée pour K = 4.

### loop pour cluster

for (i in c(2, 3, 5, 6, 7, 8, 9, 10, 11, 15, 16)){
  # Appliquer k-means avec un nombre de clusters k (à définir, ici k = 3)
  set.seed(123)  # Pour rendre les résultats reproductibles
  kmeans_result <- kmeans(data_scaled, centers = i, nstart = 25)
  # Ajouter les clusters aux données d'origine
  data_filtered[[paste0("cluster_", i)]] <- kmeans_result$cluster
}

kmeans_result11 <- kmeans(data_scaled, centers = 11, nstart = 25)
saveRDS(kmeans_result11, file = "kmeans_results2022.rds")

# Extract & save the center/scale
orig_center <- attr(data_scaled, "scaled:center")
orig_scale  <- attr(data_scaled, "scaled:scale")
saveRDS(orig_center, file = "kmeans_orig_center.rds")
saveRDS(orig_scale,  file = "kmeans_orig_scale.rds")

table(data_filtered$cluster_2)
#  1   2    
# 367 1133 
table(data_filtered$cluster_3)
#  1   2   3 
# 616 132 752  
table(data_filtered$cluster_5)
#  1   2   3   4   5 
# 122 563 174 515 126 
table(data_filtered$cluster_6)
#  1   2   3   4   5   6 
# 166 111  73 468 174 508
table(data_filtered$cluster_8)
#  1   2   3   4   5   6   7   8 
# 125 463  53 350 223  74 187  25 
table(data_filtered$cluster_9)
#  1   2   3   4   5   6   7   8   9 
# 101 324 444 122  43 153 189  72  52 
table(data_filtered$cluster_10)
#  1   2   3   4   5   6   7   8   9  10 
# 50 328 193 445 121  73  25  43 152  70 
table(data_filtered$cluster_11)
#  1   2   3   4   5   6   7   8   9  10  11 
# 73 114 148  46 275   9  25 399 208 135  68
table(data_filtered$cluster_15)
#  1   2   3   4   5   6   7   8   9  10  11  12  13  14  15 
# 43  25 202 145 111  68 126  71  64 137  45 125   9 145 184 

table(data_filtered$cluster_16)
#  1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16 
# 76 134  35 181  25  43   9 120 178  68  79  45 106  40 223 138 

# Visualisation des clusters sur 2 dimensions ------------------------------------------------

# Calcul de la distance (ici, distance euclidienne)
distance_matrix <- dist(data_scaled)

# MDS pour réduction à 2 dimensions
mds_result <- cmdscale(distance_matrix, k = 2)

# Ajouter les coordonnées MDS aux données
data_filtered$MDS1 <- mds_result[,1]
data_filtered$MDS2 <- mds_result[,2]

plot_data <- data_filtered |> 
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

# -------------------------------------------------------------------------
#  Visualisation 3D des clusters K=8 et K=11
# -------------------------------------------------------------------------

# (1) Installer/charger plotly si ce n'est pas déjà fait
# install.packages("plotly")
library(plotly)

# (2) Recalculer ou récupérer la matrice de distances
#     (si vous l'avez déjà calculée auparavant, vous pouvez la réutiliser)
distance_matrix <- dist(data_scaled)

# (3) Calculer le MDS en 3 dimensions
mds_result_3d <- cmdscale(distance_matrix, k = 3)
data_filtered$MDS3_1 <- mds_result_3d[, 1]
data_filtered$MDS3_2 <- mds_result_3d[, 2]
data_filtered$MDS3_3 <- mds_result_3d[, 3]

# (4) Visualisation 3D pour k = 8
p1 <- plot_ly(
  data_filtered,
  x = ~MDS3_1,
  y = ~MDS3_2,
  z = ~MDS3_3,
  color = ~factor(cluster_8),              # On colore selon le cluster_8
  colors = RColorBrewer::brewer.pal(8, "Set1"),
  marker = list(size = 3)
) %>%
  add_markers() %>%
  layout(
    scene = list(
      xaxis = list(title = "MDS1"),
      yaxis = list(title = "MDS2"),
      zaxis = list(title = "MDS3")
    ),
    title = "Visualisation 3D - K=8"
  )

# (5) Visualisation 3D pour k = 11
p2 <- plot_ly(
  data_filtered,
  x = ~MDS3_1,
  y = ~MDS3_2,
  z = ~MDS3_3,
  color = ~factor(cluster_11),             # On colore selon le cluster_11
  colors = RColorBrewer::brewer.pal(11, "Spectral"), 
  marker = list(size = 3)
) %>%
  add_markers() %>%
  layout(
    scene = list(
      xaxis = list(title = "MDS1"),
      yaxis = list(title = "MDS2"),
      zaxis = list(title = "MDS3")
    ),
    title = "Visualisation 3D - K=11"
  )

# (6) Affichage
p1
p2
