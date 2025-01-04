# 1. Chargement des packages --------------------------------------------
library(dplyr)
library(ggplot2)
library(cluster)        # Pour PAM et la distance de Gower
library(factoextra)     # Pour la visualisation des clusters et PCA
library(tidyr)
library(ggcorrplot)
library(tibble)
library(gridExtra)

# 2. Chargement et préparation des données ------------------------------
df_pilot1_2022 <- read.csv("/home/alexab/Dropbox/Ulaval/CLESSN/_SharedFolder_datagotchi-developpement/quebec_prov_2022/pilotes/pilote-1-quebec-prov-2022.csv")

# >>> Ne pas toucher ceci : liste des variables <<<
variables_int <- c(
  # "id",
  # "postal_code",
  # "ses_age",
   "male",
   "female",
   "ses_genderOther",
   "age34m",
  # "age3554",
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
  # "act_None",
  # "answers.sport",
   "act_Fishing",
   "act_Hunting",
   "act_VisitsMuseumsGaleries",
   "act_MotorizedOutdoorActivities",
  # "act_Volunteering",
   "animal_cat",
   "animal_dog",
  # "animal_domestic",
  # "animal_farm",
  # "animal_noPet",
  # "answers.pets",
  # "cons_brand_MaR",
  # "cons_brand_BInd",
  # "cons_brand_ChainesB",
  # "cons_brand_GSurf",
  # "cons_brand_OnlineOnly",
  # "cons_brand_Frip",
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
   "ses_bisex",
   "ses_sexOri_other"#,
)

# On sélectionne les variables ci-dessus et on enlève les lignes incomplètes
data_filtered <- df_pilot1_2022 %>%
  select(all_of(variables_int)) %>%
  drop_na()

# 2.1. (Optionnel) Conversion en factors si ce sont des variables qualitatives / binaires
# Si vous avez de VRAIES variables continues, NE LES convertissez pas.
# Sinon, si tout est vraiment binaire ou catégoriel, faites :
data_filtered <- data_filtered %>%
  mutate(across(everything(), as.factor))

# 3. PCA (facultatif, pour exploration) ----------------------------------
pca_result0 <- prcomp(data_filtered, scale. = TRUE)
pca_result0$rotation[,1:4]   # Pour voir les 4 premiers axes

fviz_eig(pca_result0, addlabels = TRUE)  # Visualisation de la variance

# Contribution des variables aux 4 premiers axes
var_contrib <- factoextra::get_pca_var(pca_result0)$contrib
num_axes <- 4
all_contrib_dodge <- as.data.frame(var_contrib[, 1:num_axes])
all_contrib_dodge$Variable <- rownames(all_contrib_dodge)

all_contrib_dodge <- all_contrib_dodge %>%
  pivot_longer(
    cols = starts_with("Dim"),
    names_to = "Axe",
    values_to = "Contribution"
  )

ggplot(all_contrib_dodge, aes(x = reorder(Variable, Contribution), y = Contribution, fill = Axe)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  coord_flip() +
  labs(
    title = "Contributions des variables par axe",
    x = "Variables",
    y = "Contribution (%)"
  ) +
  scale_fill_brewer(palette = "Set2", name = "Axes") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 7)
  )

# 4. Clustering K-Medoids (PAM) ------------------------------------------

# 4.1 Calcul de la distance de Gower
gower_dist <- daisy(data_filtered, metric = "gower")

# 4.2 Détermination du nombre de clusters
# Méthode du coude
wss_pam <- sapply(2:40, function(k){
  pam_fit <- pam(gower_dist, diss = TRUE, k = k)
  pam_fit$objective  # parfois c'est un vecteur, on peut mettre pam_fit$objective[1] si besoin
})

plot(2:40, wss_pam, type = "b", pch = 19,
     xlab = "Nombre de clusters (K)",
     ylab = "Somme des distances intra-cluster",
     main = "Méthode du coude - PAM (Gower)")

# Indice de silhouette
sil_width_pam <- sapply(2:40, function(k){
  pam_fit <- pam(gower_dist, diss = TRUE, k = k)
  pam_fit$silinfo$avg.width
})

plot(2:40, sil_width_pam, type = "b", pch = 19,
     xlab = "Nombre de clusters (K)",
     ylab = "Largeur moyenne de la silhouette",
     main = "Indice de silhouette - PAM (Gower)")

# Choisir le K en fonction des graphes (ex: coude + silhouette)
K <- 7  # EXEMPLE, à ajuster selon vos résultats

# 4.3 Application de PAM
pam_fit <- pam(gower_dist, diss = TRUE, k = K)

# Ajout du cluster au DataFrame
data_filtered$Cluster <- pam_fit$clustering

# 4.4 Visualisation
# fviz_cluster peut être utilisé, mais attention, vos données sont surtout factorielles.
# L'affichage sera une projection approximative. On peut utiliser "stand = FALSE".
fviz_cluster(
  pam_fit,
  data = data_filtered,
  stand = FALSE,
  geom = "point",
  ellipse.type = "convex",
  ggtheme = theme_bw()
)

fviz_silhouette(pam_fit)  # Visualisation de la silhouette