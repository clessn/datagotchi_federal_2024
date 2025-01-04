# Packages ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)
library(tidyr)
library(ggcorrplot)

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
     "act_Run",
     "act_Yoga",
     "act_None",
     "act_Fishing",
     "act_Hunting",
     "act_VisitsMuseumsGaleries",
     "act_MotorizedOutdoorActivities",
     "act_Volunteering",
     "animal_cat",
     "animal_dog",
     "cons_brand_MaR",
     "cons_brand_Frip",
     "educBHS",
     "educCollege",
     "educUniv",
     "cons_redWineDrink",
     "cons_regBeers",
     "cons_cocktailsDrink",
     "cons_noDrink",
     "incomeLow",
     "incomeMid",
     "incomeHigh",
     "ses_dwelling_app",
     "ses_dwelling_detachedHouse",
     "act_transport_Car",
     "act_transport_Walk",
     "act_transport_PublicTransportation",
     "vehicule_PickUp",
     "vehicule_noCar",
     "immigrant",
     "cons_coffee_TimH",
     "cons_coffee_Starbucks",
     "cons_coffee_place_noCoffee",
     "app_noTattoo",
     "cons_low_Meat",
     "cons_mid_Meat",
     "cons_much_Meat",
     "ses_ethn_White",
     "ses_ethn_Black",
     "ses_ethn_Asiatique",
     "ses_hetero",
     "ses_gai",
     "ses_bisex"
   )  

# Chemin du dossier où enregistrer les graphiques
output_dir <- "/home/alexab/Dropbox/Ulaval/CLESSN/datagotchi_federal_2024/_SharedFolder_datagotchi_federal_2024/clustering/graph/2022/réduit/description"

# S'assurer que le dossier existe
if(!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}

# Charger les bibliothèques nécessaires
library(dplyr)
library(ggplot2)
library(forcats)

# Description des clusters
df_mean_by_cluster <- describe_clusters(
  data_filtered,
  variables_to_describe = variables_to_describe,
  cluster_var = "cluster_11"
)

# Limiter les scores z entre -2 et 2 (optionnel)
df_mean_by_cluster <- df_mean_by_cluster %>%
  mutate(z_score_limited = ifelse(z_score > 2, 2, ifelse(z_score < -2, -2, z_score)))

df_mean_by_cluster <- df_mean_by_cluster %>%
  mutate(
    variable = case_when(
      variable == "male" ~ "Homme",
      variable == "female" ~ "Femme",
      variable == "ses_genderOther" ~ "Autre genre",
      variable == "age34m" ~ "Moins de 34 ans",
      variable == "age3554" ~ "35 à 54 ans",
      variable == "age55p" ~ "55 ans et plus",
      variable == "langEn" ~ "Anglais",
      variable == "langFr" ~ "Français",
      variable == "ses_languageOther" ~ "Autre langue",
      variable == "act_Gym" ~ "Gym",
      variable == "act_TeamSport" ~ "Sports d'équipe",
      variable == "act_Run" ~ "Course",
      variable == "act_Yoga" ~ "Yoga",
      variable == "act_None" ~ "Aucune activité",
      variable == "act_Fishing" ~ "Pêche",
      variable == "act_Hunting" ~ "Chasse",
      variable == "act_VisitsMuseumsGaleries" ~ "Visites musées/galeries",
      variable == "act_MotorizedOutdoorActivities" ~ "Activités motorisées",
      variable == "act_Volunteering" ~ "Bénévolat",
      variable == "animal_cat" ~ "Chat",
      variable == "animal_dog" ~ "Chien",
      variable == "cons_brand_MaR" ~ "Magasins à rayons",
      variable == "cons_brand_Frip" ~ "Friperie",
      variable == "educBHS" ~ "Secondaire",
      variable == "educCollege" ~ "Collégial",
      variable == "educUniv" ~ "Universitaire",
      variable == "cons_redWineDrink" ~ "Vin rouge",
      variable == "cons_regBeers" ~ "Bières régulières",
      variable == "cons_cocktailsDrink" ~ "Cocktails",
      variable == "cons_noDrink" ~ "Ne boit pas d'alcool",
      variable == "incomeLow" ~ "Revenu faible",
      variable == "incomeMid" ~ "Revenu moyen",
      variable == "incomeHigh" ~ "Revenu élevé",
      variable == "ses_dwelling_app" ~ "Appartement",
      variable == "ses_dwelling_detachedHouse" ~ "Maison individuelle",
      variable == "act_transport_Car" ~ "Voiture",
      variable == "act_transport_Walk" ~ "Marche",
      variable == "act_transport_PublicTransportation" ~ "Transport public",
      variable == "vehicule_PickUp" ~ "Pick-up",
      variable == "vehicule_noCar" ~ "Aucun véhicule",
      variable == "immigrant" ~ "Immigrant",
      variable == "cons_coffee_TimH" ~ "Café Tim Hortons",
      variable == "cons_coffee_Starbucks" ~ "Café Starbucks",
      variable == "cons_coffee_place_noCoffee" ~ "Ne boit pas de café",
      variable == "app_noTattoo" ~ "Pas de tatouage",
      variable == "cons_low_Meat" ~ "Faible conso. de viande",
      variable == "cons_mid_Meat" ~ "Conso. modérée de viande",
      variable == "cons_much_Meat" ~ "Conso. élevée de viande",
      variable == "ses_ethn_White" ~ "Blanc(he)",
      variable == "ses_ethn_Black" ~ "Noir(e)",
      variable == "ses_ethn_Asiatique" ~ "Asiatique",
      variable == "ses_hetero" ~ "Hétérosexuel(le)",
      variable == "ses_gai" ~ "Gai(e)",
      variable == "ses_bisex" ~ "Bisexuel(le)",
      TRUE ~ "Autre"  # Au cas où des variables hors liste se glissent
    )
  )

# Obtenir la liste des clusters uniques
clusters <- unique(df_mean_by_cluster$cluster_var)

# Boucle sur chaque cluster
for(cluster_id in clusters){
  # Filtrer les données pour le cluster actuel
  df_cluster <- df_mean_by_cluster %>%
    filter(cluster_var == cluster_id)
  
  # Réordonner les variables par score z absolu
  df_cluster <- df_cluster %>%
    mutate(variable = fct_reorder(variable, abs(z_score), .desc = TRUE))
  
  # Inverser l'ordre des niveaux pour que les variables avec le score z absolu le plus élevé soient en haut
  df_cluster$variable <- fct_rev(df_cluster$variable)
  
# Ajouter une colonne pour la couleur en fonction du signe du z_score
df_cluster <- df_cluster %>%
  mutate(color = ifelse(z_score_limited >= 0, "#26aec6", "#eb1616"))


# Créer le graphique avec une ligne entre 0 et le point
graph <- ggplot(df_cluster, aes(x = z_score_limited, y = variable)) +
  # Lignes entre 0 et les points
  geom_segment(aes(x = 0, xend = z_score_limited, y = variable, yend = variable), 
               color = "grey", size = 1) +  # Épaisseur augmentée
  # Points avec couleur conditionnelle
  geom_point(aes(color = color), size = 3) +  # Points plus grands
  # Ligne verticale pour 0
  geom_vline(xintercept = 0, linetype = "dashed") +
  # Limites sur les axes
  coord_cartesian(xlim = c(-2, 2)) +
  # Personnalisation du thème
  clessnize::theme_clean_light() +
  theme(axis.text.y = element_text(size = 10)) +  # Texte plus grand
  # Étiquettes des axes et titre
  xlab("Score z") +
  ylab(NULL) +
  # Utilisation des couleurs pour les points
  scale_color_identity() +
    ggtitle(paste("Scores z des variables pour le cluster", cluster_id))
  
  # Enregistrer le graphique
  output_file <- file.path(output_dir, paste0("2022_9cluster_", cluster_id, ".png"))
  ggsave(filename = output_file, plot = graph, width = 8, height = 6)
}

generate_persona_prompt <- function(df_all_clusters, cluster_id, z_threshold = 0.30) {
  # Filtrage des données pour le cluster spécifié
  df_cluster <- df_all_clusters %>%
    filter(cluster_var == cluster_id)
  
  # Extraction des variables positives et négatives par rapport au seuil
  variables_positive <- df_cluster %>%
    filter(z_score > z_threshold) %>%
    pull(variable)
  
  variables_negative <- df_cluster %>%
    filter(z_score < -z_threshold) %>%
    pull(variable)
  
  # Construction du prompt
  prompt <- paste0(
    "\nPour le persona ", cluster_id, " :\n",
    "\nLes caractéristiques suivantes définissent ce cluster par leur distinction marquée :\n\n",
    "Variables supérieures (valeurs élevées) :\n",
    if (length(variables_positive) > 0) {
      paste(variables_positive, collapse = "\n")
    } else {
      "Aucune variable supérieure ne se distingue fortement."
    },
    "\n\nVariables inférieures (valeurs faibles) :\n",
    if (length(variables_negative) > 0) {
      paste(variables_negative, collapse = "\n")
    } else {
      "Aucune variable inférieure ne se distingue fortement."
    },
    "\n\nSuggérez un nom qui reflète bien ces caractéristiques.\n",
    "Tu peux donner un prénom significatif suivi par un deux-points (:) et un nom descriptif.\n",
    "Finalement, décris la persona en 2-3 phrases, en tenant compte des scores z des variables pour ce cluster.\n\n"
  )
  
  return(prompt)
}

# Exemple d'utilisation avec df_cluster (remplacer par votre dataframe réel)
prompt_persona <- generate_persona_prompt(df_mean_by_cluster, cluster_id = 5)
cat(prompt_persona)

# Cluster par categories
data_filtered$cluster_labels <- NA
data_filtered$cluster_labels[data_filtered$cluster_11 == 1] <- "1. Jin - Universitaire Immigrant"
data_filtered$cluster_labels[data_filtered$cluster_11 == 2] <- "2. Steve - L'Homme de Plein Air"
data_filtered$cluster_labels[data_filtered$cluster_11 == 3] <- "3. James - Patriarche Anglophone"
data_filtered$cluster_labels[data_filtered$cluster_11 == 4] <- "4. Zoé - Écolo Avant-gardiste"
data_filtered$cluster_labels[data_filtered$cluster_11 == 5] <- "5. "
data_filtered$cluster_labels[data_filtered$cluster_11 == 6] <- "6. Michel - Senior Traditionnaliste"
data_filtered$cluster_labels[data_filtered$cluster_11 == 7] <- "6. Michel - Senior Traditionnaliste"
data_filtered$cluster_labels[data_filtered$cluster_11 == 8] <- "6. Michel - Senior Traditionnaliste"
data_filtered$cluster_labels[data_filtered$cluster_11 == 9] <- "6. Michel - Senior Traditionnaliste"
data_filtered$cluster_labels[data_filtered$cluster_11 == 10] <- "6. Michel - Senior Traditionnaliste"
data_filtered$cluster_labels[data_filtered$cluster_11 == 11] <- "6. Michel - Senior Traditionnaliste"