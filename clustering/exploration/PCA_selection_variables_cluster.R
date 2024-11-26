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
df_pilot_2021_merged <- read.csv("/home/alexab/Dropbox/Ulaval/CLESSN/datagotchi_federal_2024/_SharedFolder_datagotchi_federal_2024/clustering/data/pilot2021_merged_clustering.csv")
df_datagotchi_2021 <- read.csv("/home/alexab/Dropbox/Ulaval/CLESSN/_SharedFolder_datagotchi-developpement/federal_can_2021/hub/DatagotchiHub-federal-2021-08-03-2022-.csv")


# Définir les groupes de variables --------------------------------
groupes_variables <- list(
  transport = c("act_transport_Car", "act_transport_SUV", "act_transport_Moto", 
                "act_transport_Walk", "act_transport_Bicycle", 
                "act_transport_PublicTransportation", "act_transport_Taxi"),
  demographie1 = c("age34m", "age3554", "age55p", "male", "female", "ses_genderOther", 
                   "ses_hetero", "ses_gai", "ses_bisex"),
  demographie2 = c("langFr", "langEn", "ses_languageOther", "incomeLow", "incomeHigh", 
                   "immigrant", "educBHS", "educUniv"),
  activites = c("act_VisitsMuseumsGaleries", "act_Fishing", "act_Hunting", 
                "act_MotorizedOutdoorActivities", "act_Volunteering", 
                "act_Walk", "act_Gym", "act_TeamSport", "act_Run", 
                "act_Yoga", "act_None", "act_Other"),
  apparence = c("app_swag_Formel", "app_swag_Classique", "app_swag_Casual", 
                "app_swag_Sport", "app_swag_Chic", "app_swag_VintageHippBoheme", 
                "app_swag_Other", "app_swag_Rock", "app_noTattoo"),
  cons_brand = c("cons_brand_MaR", "cons_brand_BInd", "cons_brand_OnlineOnly", 
                 "cons_brand_ChainesB", "cons_brand_GSurf", "cons_brand_Frip", 
                 "cons_brand_Other"),
  cons_coffee = c("cons_coffee_place_noCoffee", "cons_coffee_TimH", "cons_coffee_Other", 
                  "cons_coffee_Starbucks", "cons_coffee_SC", "cons_coffee_McDo", 
                  "cons_coffee_place_ind"),
  cons_food = c("cons_Meat", "cons_Vege", "cons_Vegan"),
  habitat = c("ses_dwelling_app", "ses_dwelling_loft", "ses_dwelling_condo", 
              "ses_dwelling_tour", "ses_dwelling_detachedHouse", 
              "ses_dwelling_semiDetached", 
              "ses_dwelling_coop", "ses_dwelling_HLM", "ses_dwelling_mobile", 
              "ses_dwelling_other"),
  cons_alcool = c("cons_noDrink", "cons_redWineDrink", "cons_whiteWineDrink", 
                  "cons_roseDrink", "cons_sparklingDrink", "cons_regBeers", 
                  "cons_microBeers", "cons_spiritDrink", "cons_cocktailsDrink"),
  cons_tabac = c("cons_Smoke", "cons_SmokeStopping", "cons_SmokeStopped", 
                 "cons_SmokeNever", "cons_VapeNation")
)


# Préparer les données
data_filtered <- df_pilot_2021_merged %>%
  select(-c(X, source)) %>%
  drop_na()

pca_result0 <- prcomp(
  data_filtered,
  scale = TRUE
)

summary(pca_result0)
### Environ 3-4 composantes font l'affaire

pca_result0$rotation[,1:4]

fviz_eig(pca_result0, addlabels = TRUE)


# Effectuer l'ACP
pca_all <- prcomp(data_filtered, scale. = TRUE)
var_contrib <- factoextra::get_pca_var(pca_all)$contrib
variance_explained <- factoextra::get_eigenvalue(pca_all)$variance.percent

num_axes <- 4
all_contrib <- data.frame()

# Calculer les contributions pondérées
for (group_name in names(groupes_variables)) {
  vars_in_group <- groupes_variables[[group_name]]
  vars_present <- vars_in_group[vars_in_group %in% rownames(var_contrib)]
  if (length(vars_present) == 0) next
  
  contrib_group <- var_contrib[vars_present, 1:num_axes, drop = FALSE]
  
  # Calculer les contributions pondérées
  for (i in 1:num_axes) {
    contrib_group[, i] <- contrib_group[, i] * (variance_explained[i] / 100)
  }
  
  df_contrib <- as.data.frame(contrib_group)
  df_contrib$Variable <- rownames(df_contrib)
  df_contrib$Group <- group_name
  
  # Contribution pondérée totale
  df_contrib <- df_contrib %>%
    pivot_longer(cols = starts_with("Dim"), names_to = "Axe", values_to = "Contribution") %>%
    group_by(Group, Variable) %>%
    summarise(TotalContributionWeighted = sum(Contribution), .groups = "drop")
  
  all_contrib <- bind_rows(all_contrib, df_contrib)
}

# Graphique 1 : Contribution pondérée
ggplot(all_contrib, aes(x = reorder(Variable, -TotalContributionWeighted), y = TotalContributionWeighted, fill = Group)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~ Group, scales = "free_y") +
  labs(title = "Contributions pondérées des variables aux axes",
       x = "Variables", y = "Contribution pondérée (%)") +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.y = element_text(size = 7))

# Préparer les contributions par axe pour le second graphique
all_contrib_dodge <- data.frame()
for (group_name in names(groupes_variables)) {
  vars_in_group <- groupes_variables[[group_name]]
  vars_present <- vars_in_group[vars_in_group %in% rownames(var_contrib)]
  if (length(vars_present) == 0) next
  
  contrib_group <- var_contrib[vars_present, 1:num_axes, drop = FALSE]
  
  df_contrib <- as.data.frame(contrib_group)
  df_contrib$Variable <- rownames(df_contrib)
  df_contrib$Group <- group_name
  
  df_contrib <- df_contrib %>%
    pivot_longer(cols = starts_with("Dim"), names_to = "Axe", values_to = "Contribution")
  
  all_contrib_dodge <- bind_rows(all_contrib_dodge, df_contrib)
}

# Graphique 2 : Contributions par axe avec dodge
ggplot(all_contrib_dodge, aes(x = Variable, y = Contribution, fill = Axe)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  coord_flip() +
  facet_wrap(~ Group, scales = "free_y") +
  labs(title = "Contributions des variables par axe",
       x = "Variables", y = "Contribution (%)") +
  scale_fill_brewer(palette = "Set2", name = "Axes principaux") +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.y = element_text(size = 7))

## corr_matrix
cor_matrix <- cor(x = data_filtered)

# Transformer la matrice en un format long (tidy)
cor_df <- as.data.frame(as.table(cor_matrix)) %>%
  rename(Variable1 = Var1, Variable2 = Var2, Correlation = Freq)

# Afficher un aperçu du tableau
print(cor_df)

ggcorrplot::ggcorrplot(
  cor_matrix,
  method = "circle",
  hc.order = TRUE,
  type = "upper",
  outline.color = "#00000000",
  colors = c("#d4206b", "#ececec", "#20d48f")
)

# Variables sélectionnées selon contrib et correlation -------------------

data_select <- df_pilot_2021_merged %>%
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
    act_transport_PublicTransportation, act_transport_Car, act_transport_Walk
  ) %>%
  drop_na()

# PCA --------------------------------------------------------------------

pca_result <- prcomp(
  data_select,
  scale = TRUE
)

summary(pca_result)
### Environ 3-4 composantes font l'affaire

pca_result$rotation[,1:4]

fviz_eig(pca_result, addlabels = TRUE)


# Clustering -------------------------------------------------------------

## Normalisation des données (optionnelle, recommandé pour k-means)
data_scaled <- scale(data_select)


## Checker rapidement la % de variance expliquée par les 2 dimensions
km_res <- kmeans(data_scaled, centers = 8, nstart = 25)
fviz_cluster(
  km_res, data = data_scaled,
  geom = "point",
  ellipse.type = "convex", 
  ggtheme = theme_bw()
  )


# Trouver le nombre de clusters idéal ------------------------------------------

### Coude
wss <- sapply(2:15, function(k){
  kmeans(data_scaled, centers = k, nstart = 25)$tot.withinss
})

# Tracer la courbe du coude
plot(2:15, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Nombre de clusters K",
     ylab = "Somme des carrés intra-cluster (withinss)",
     main = "Méthode du coude pour déterminer K")


# Calculer l'indice de silhouette pour différents k
sil_width <- sapply(2:15, function(k){
  km.res <- kmeans(data_scaled, centers = k, nstart = 25)
  ss <- cluster::silhouette(km.res$cluster, dist(data_scaled))
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



### loop pour cluster

for (i in c(3, 4, 5, 6, 7, 8, 9, 10, 12)){
  # Appliquer k-means avec un nombre de clusters k (à définir, ici k = 3)
  set.seed(123)  # Pour rendre les résultats reproductibles
  kmeans_result <- kmeans(data_scaled, centers = i, nstart = 25)
  # Ajouter les clusters aux données d'origine
  data_select[[paste0("cluster_", i)]] <- kmeans_result$cluster
}

kmeans_result8 <- kmeans(data_scaled, centers = 8, nstart = 25)
saveRDS(kmeans_result8, file = "kmeans_results8.rds")


table(data_select$cluster_3)
table(data_select$cluster_4)
table(data_select$cluster_5)
table(data_select$cluster_6)
table(data_select$cluster_7)
table(data_select$cluster_8)
table(data_select$cluster_9)
table(data_select$cluster_10)
table(data_select$cluster_12)


# Visualisation des clusters sur 2 dimensions ------------------------------------------------

# Calcul de la distance (ici, distance euclidienne)
distance_matrix <- dist(data_scaled)

# MDS pour réduction à 2 dimensions
mds_result <- cmdscale(distance_matrix, k = 2)

# Ajouter les coordonnées MDS aux données
data_select$MDS1 <- mds_result[,1]
data_select$MDS2 <- mds_result[,2]

plot_data <- data_select |> 
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






