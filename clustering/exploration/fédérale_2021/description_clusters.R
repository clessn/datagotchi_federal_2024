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
  "act_VisitsMuseumsGaleries", 
  "act_Volunteering", 
  "act_Yoga", 
  "act_Run", 
  "act_Gym", 
  "act_MotorizedOutdoorActivities",
  "act_None",
  "app_noTattoo", 
  "app_swag_Casual", 
  "app_swag_VintageHippBoheme",
  "cons_regBeers", 
  "cons_cocktailsDrink", 
  "cons_microBeers", 
  "cons_redWineDrink", 
  "cons_noDrink",
  "cons_brand_ChainesB", 
  "cons_brand_GSurf", 
  "cons_brand_MaR", 
  "cons_brand_Frip",
  "cons_coffee_Starbucks", 
  "cons_coffee_place_noCoffee",
  "cons_coffee_TimH",
  "cons_Meat",
  "cons_Vege",
  "cons_SmokeNever", 
  "cons_Smoke",
  "immigrant", 
  "educUniv",
  "educBHS", 
  "age55p",
  "age34m", 
  "male", 
  "ses_hetero",
  "ses_gai",
  "langEn", 
  "langFr", 
  "ses_languageOther",
  "incomeHigh",
  "incomeLow",
  "ses_dwelling_condo", 
  "ses_dwelling_detachedHouse",
  "ses_dwelling_app",
  "act_transport_PublicTransportation", 
  "act_transport_Car",
  "act_transport_Walk"
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
  data_select,
  variables_to_describe = variables_to_describe,
  cluster_var = "cluster_8"
)

# Limiter les scores z entre -2 et 2 (optionnel)
df_mean_by_cluster <- df_mean_by_cluster %>%
  mutate(z_score_limited = ifelse(z_score > 2, 2, ifelse(z_score < -2, -2, z_score)))

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
  
  # Créer le graphique avec une ligne entre 0 et le point
  p <- ggplot(df_cluster, aes(x = z_score_limited, y = variable)) +
    geom_segment(aes(x = 0, xend = z_score_limited, y = variable, yend = variable), color = "grey") +
    geom_point(color = "blue") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    coord_cartesian(xlim = c(-2, 2)) +
    theme_bw() +
    theme(axis.text.y = element_text(size = 7)) +
    xlab("Score z") +
    ylab("Variable") +
    ggtitle(paste("Scores z des variables pour le cluster", cluster_id))
  
  # Enregistrer le graphique
  output_file <- file.path(output_dir, paste0("8cluster_", cluster_id, ".png"))
  ggsave(filename = output_file, plot = p, width = 8, height = 6)
}

