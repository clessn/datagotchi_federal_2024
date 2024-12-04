# variables à décrire ----------------------------------------------------
variables_to_describe <- c(
  "educBHS",
#  "educHS",
  "educUniv",
  "incomeLow",
  "incomeMid",
  "incomeHigh",
  "ses_hetero",
  "ses_gai",
#  "ses_bisex",
#  "ses_sexOri_other",
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
output_dir <- "/home/alexab/Dropbox/Ulaval/CLESSN/datagotchi_federal_2024/_SharedFolder_datagotchi_federal_2024/clustering/graph/cluster_final"

# Description des clusters
df_mean_by_cluster <- opubliqr::describe_clusters(
  data_filtered,
  variables_to_describe = variables_to_describe,
  cluster_var = "cluster_6"
)

# Limiter les scores z entre -2 et 2 (optionnel)
df_mean_by_cluster <- df_mean_by_cluster %>%
  mutate(z_score_limited = ifelse(z_score > 2, 2, ifelse(z_score < -2, -2, z_score)))

# Filtrer les données pour le cluster 1
df_cluster <- df_mean_by_cluster %>%
  filter(cluster_var == 2)

df_cluster <- df_cluster %>%
  mutate(
    variable = case_when(
      variable == "educBHS" ~ "Secondaire",
      variable == "educUniv" ~ "Universitaire",
      variable == "incomeLow" ~ "Revenu faible",
      variable == "incomeMid" ~ "Revenu moyen",
      variable == "incomeHigh" ~ "Revenu élevé",
      variable == "ses_hetero" ~ "Hétéro",
      variable == "ses_gai" ~ "Gai",
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
    ggtitle("Scores z des variables pour le cluster 2. Stéphane - Travailleur Discret")
  
  # Enregistrer le graphique
  output_file <- file.path(output_dir, paste0("2021_2. Stéphane - Travailleur Discret.png"))
  ggsave(filename = output_file, plot = graph, width = 12, height = 8)

