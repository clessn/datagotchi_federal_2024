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
  "educBHS",
  "educHS",
  "educUniv",
  "incomeLow",
  "incomeMid",
  "incomeHigh",
  "ses_hetero",
  "ses_gai",
#  "ses_bisex",
  "ses_sexOri_other",
  "immigrant",
  "male",
#  "female",
#  "ses_genderOther",
  "age34m",
  "age3554",
  "age55p",
  "langFr",
  "langEn",
  "ses_languageOther",
  "act_transport_Car",
#  "act_transport_SUV",
#  "act_transport_Moto",
  "act_transport_Walk",
#  "act_transport_Bicycle",
  "act_transport_PublicTransportation",
#  "act_Walk",
  "act_Gym",
  "act_TeamSport",
  "act_Run",
  "act_Yoga",
#  "act_Swimming",
#  "act_Other",
  "act_None",
  "act_Fishing",
  "act_Hunting",
  "act_VisitsMuseumsGaleries",
  "act_MotorizedOutdoorActivities",
#  "act_Volunteering",
  "cons_brand_MaR",
#  "cons_brand_OnlineOnly",
#  "cons_brand_BInd",
  "cons_brand_ChainesB",
  "cons_brand_GSurf",
  "cons_brand_Frip",
#  "cons_brand_Other",
  "cons_Meat",
  "cons_Vege",
  "cons_Vegan",
  "cons_coffee_TimH",
#  "cons_coffee_Other",
  "cons_coffee_Starbucks",
#  "cons_coffee_SC",
  "cons_coffee_McDo",
#  "cons_coffee_place_ind",
  "cons_coffee_place_noCoffee",
  "cons_Smoke",
#  "cons_SmokeStopping",
  "cons_SmokeStopped",
  "cons_SmokeNever",
#  "cons_VapeNation",
  "cons_noDrink",
  "cons_redWineDrink",
#  "cons_whiteWineDrink",
#  "cons_roseDrink",
#  "cons_sparklingDrink",
  "cons_regBeers",
  "cons_microBeers",
#  "cons_spiritDrink",
  "cons_cocktailsDrink",
#  "app_swag_Formel",
#  "app_swag_Classique",
#  "app_swag_Casual",
#  "app_swag_Sport",
#  "app_swag_Chic",
#  "app_swag_VintageHippBoheme",
#  "app_swag_Other",
#  "app_swag_Rock",
  "app_noTattoo",
  "ses_dwelling_app",
#  "ses_dwelling_loft",
  "ses_dwelling_condo",
#  "ses_dwelling_tour",
  "ses_dwelling_detachedHouse"#,
#  "ses_dwelling_townHouse",
#  "ses_dwelling_semiDetached",
#  "ses_dwelling_coop",
#  "ses_dwelling_HLM",
#  "ses_dwelling_mobile",
#  "ses_dwelling_other"#,
#  "ses_dwelling_house"
)


# Chemin du dossier où enregistrer les graphiques
output_dir <- "/home/alexab/Dropbox/Ulaval/CLESSN/datagotchi_federal_2024/_SharedFolder_datagotchi_federal_2024/clustering/graph"

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
  cluster_var = "cluster_8"
)

# Limiter les scores z entre -2 et 2 (optionnel)
df_mean_by_cluster <- df_mean_by_cluster %>%
  mutate(z_score_limited = ifelse(z_score > 2, 2, ifelse(z_score < -2, -2, z_score)))

df_mean_by_cluster <- df_mean_by_cluster %>%
  mutate(
    variable = case_when(
      variable == "educBHS" ~ "Secondaire partiel",
      variable == "educHS" ~ "Secondaire",
      variable == "educUniv" ~ "Universitaire",
      variable == "incomeLow" ~ "Revenu faible",
      variable == "incomeMid" ~ "Revenu moyen",
      variable == "incomeHigh" ~ "Revenu élevé",
      variable == "ses_hetero" ~ "Hétéro",
      variable == "ses_gai" ~ "Gai",
      variable == "ses_sexOri_other" ~ "Autre orientation",
      variable == "immigrant" ~ "Immigrant",
      variable == "male" ~ "Homme",
      variable == "age34m" ~ "Moins de 34 ans",
      variable == "age3554" ~ "35 à 54 ans",
      variable == "age55p" ~ "55 ans et plus",
      variable == "langFr" ~ "Français",
      variable == "langEn" ~ "Anglais",
      variable == "ses_languageOther" ~ "Autre langue",
      variable == "act_transport_Car" ~ "Voiture",
      variable == "act_transport_Walk" ~ "Marche",
      variable == "act_transport_PublicTransportation" ~ "Transport public",
      variable == "act_Gym" ~ "Gym",
      variable == "act_TeamSport" ~ "Sports d'équipe",
      variable == "act_Run" ~ "Course",
      variable == "act_Yoga" ~ "Yoga",
      variable == "act_None" ~ "Aucune activité",
      variable == "act_Fishing" ~ "Pêche",
      variable == "act_Hunting" ~ "Chasse",
      variable == "act_VisitsMuseumsGaleries" ~ "Visites de musées/galleries",
      variable == "act_MotorizedOutdoorActivities" ~ "Activités motorisées",
      variable == "cons_brand_MaR" ~ "Magasins à rayon ",
      variable == "cons_brand_ChainesB" ~ "Chaînes de boutique",
      variable == "cons_brand_GSurf" ~ "Grandes surface",
      variable == "cons_brand_Frip" ~ "Friperie",
      variable == "cons_Meat" ~ "Carnivore",
      variable == "cons_Vege" ~ "Végétarien",
      variable == "cons_Vegan" ~ "Végan",
      variable == "cons_coffee_TimH" ~ "Café Tim Hortons",
      variable == "cons_coffee_Starbucks" ~ "Café Starbucks",
      variable == "cons_coffee_McDo" ~ "Café McDonald's",
      variable == "cons_coffee_place_noCoffee" ~ "Pas de café",
      variable == "cons_Smoke" ~ "Fume",
      variable == "cons_SmokeStopped" ~ "A arrêté de fumer",
      variable == "cons_SmokeNever" ~ "N'a jamais fumé",
      variable == "cons_noDrink" ~ "Pas d'alcool",
      variable == "cons_redWineDrink" ~ "Vin rouge",
      variable == "cons_regBeers" ~ "Bières régulières",
      variable == "cons_microBeers" ~ "Microbrasseries",
      variable == "cons_cocktailsDrink" ~ "Cocktails",
      variable == "app_noTattoo" ~ "Pas de tatouages",
      variable == "ses_dwelling_app" ~ "Appartement",
      variable == "ses_dwelling_condo" ~ "Condo",
      variable == "ses_dwelling_detachedHouse" ~ "Maison individuelle",
      TRUE ~ "Autre"
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
    ggtitle(paste("Scores z des variables pour le cluster", cluster_labels))
  
  # Enregistrer le graphique
  output_file <- file.path(output_dir, paste0("2021_9cluster_", cluster_labels, ".png"))
  ggsave(filename = output_file, plot = graph, width = 8, height = 6)
}


generate_persona_prompt <- function(df_all_clusters, cluster_id, z_threshold = 0.25) {
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
prompt_persona <- generate_persona_prompt(df_mean_by_cluster, cluster_id = 8)
cat(prompt_persona)

# Cluster par categories
data_filtered$cluster_labels <- NA
data_filtered$cluster_labels[data_filtered$cluster_8 == 1] <- "1. Gabriel - Urbain Raffiné"
data_filtered$cluster_labels[data_filtered$cluster_8 == 2] <- "2. Susan - Anglophone Citadine"
data_filtered$cluster_labels[data_filtered$cluster_8 == 3] <- "3. Daniela - Allophone Urbaine"
data_filtered$cluster_labels[data_filtered$cluster_8 == 4] <- "4. Guy - Francophone Modeste"
data_filtered$cluster_labels[data_filtered$cluster_8 == 5] <- "5. Nicolas - Sportif Tout-terrain"
data_filtered$cluster_labels[data_filtered$cluster_8 == 6] <- "6. Zoé - Écolo Avant-gardiste"
data_filtered$cluster_labels[data_filtered$cluster_8 == 7] <- "7. Lucie - Professionnelle Discrète"
data_filtered$cluster_labels[data_filtered$cluster_8 == 8] <- "8. Michel - Sage Traditionnel"

#Nom du persona 1: Gabriel : L'Urbain Raffiné
#Description du persona :
#Gabriel est un homme gai urbain et sophistiqué, vivant dans un condo et incarnant un style de vie raffiné.
#Ce cluster se démarque par un fort intérêt pour les activités culturelles comme les musées,
#un goût prononcé pour le vin rouge, et une préférence pour le transport public,
#reflétant une sensibilité esthétique et un engagement envers un mode de vie urbain.


#Nom du persona 2: Susan : L'anglophone
#Description :
#est une anglophone montréalaise typique, préférant la vie en milieu urbain
#et évitant les banlieues avec leurs maisons individuelles. 


#Nom du persona 3: Daniela : Allophone urbaine
#Description :
#35 à 54 ans, est une personne urbaine et active qui adopte un style de vie équilibré.
#Utilisatrice régulière des transports en commun, elle pratique le yoga et préfère le café Starbucks
#pour ses moments de détente. Immigrante parlant une autre langue que le français ou l’anglais,
#elle se distingue par son choix de ne jamais avoir fumé et par des habitudes qui reflètent un intérêt
#pour le bien-être et la simplicité citadine.

#Nom du persona 4: Guy : Le Francophone Modeste
#Description du persona :
#Guy est un individu francophone avec un parcours éducatif secondaire,
#incarnant un mode de vie simple et sans prétention.
#Ce groupe est moins intéressé par des activités culturelles comme les musées ou galeries
#et est éloigné des milieux universitaires et des hauts revenus. 

#Nom du persona 5: Nicolas : Sportif tout-terrain
#Description du persona :
#Nicolas représente un groupe de jeunes adultes de moins de 34 ans,
#diplômés universitaires et ayant un revenu élevé, qui vivent majoritairement dans des maisons individuelles.
#Ce groupe est passionné par des activités comme la pêche, la chasse, les sports d'équipe,
#et la fréquentation du gym. Ils apprécient les microbrasseries, consomment du café Tim Hortons,
#ne fument pas, et sont majoritairement francophones.

#Nom du persona 6: Zoé : L'écolo avant-gardiste 
#Description :
#Zoé incarne une génération jeune et active, très engagée dans des habitudes de vie saines
#et respectueuses de l'environnement.
#Elle privilégie des activités comme le yoga, les visites culturelles et la marche comme mode de transport,
#tout en adoptant un régime alimentaire végétarien ou végan.
#Préférant des choix modernes comme le café Starbucks,
#elle se distingue aussi par son ouverture d'esprit, notamment sur le plan de l'orientation sexuelle.
#Nom du persona 4: Lucie : La francophone accomplie

#Nom du persona 7: Lucie : Professionnelle discrète
#Description du persona :
#Lucie représente un groupe de personnes âgées de 35 à 54 ans, francophones et diplômées universitaires,
#qui n'ont jamais fumé. Ce groupe fréquente principalement des chaînes de boutiques pour leurs achats,
#préférant des options pratiques et accessibles. À l'opposé, ils se démarquent par un faible intérêt pour
#la pêche et incluent peu de personnes de 55 ans et plus, ou ayant arrêté de fumer.


#Nom du persona 8: Michel : Le Classique Cultivé

#Description du persona :
#Michel représente un groupe de personnes âgées de 55 ans et plus, majoritairement francophones,
#éduquées au niveau universitaire, et hétérosexuelles. Ce groupe privilégie des plaisirs raffinés
#comme le vin rouge, tout en s’abstenant de café ou de cocktails, et ayant arrêté de fumer.
#Avec un mode de vie posé, ils n’ont pas de tatouages, participent peu à des activités motorisées ou sportives
#comme la course, et évitent les options plus populaires comme le café Tim Hortons.
#Ce profil illustre une maturité marquée par des choix réfléchis et un penchant pour des goûts classiques
#et sophistiqués.







